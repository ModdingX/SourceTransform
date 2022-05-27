package io.github.noeppi_noeppi.tools.sourcetransform.transform

import com.google.gson.JsonObject
import io.github.noeppi_noeppi.tools.sourcetransform.transform.data.TransformTarget
import io.github.noeppi_noeppi.tools.sourcetransform.util.{Bytecode, Util}
import io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.{InheritanceIO, InheritanceMap}
import joptsimple.util.PathConverter
import joptsimple.{OptionException, OptionParser}
import net.minecraftforge.srgutils.IMappingFile
import org.objectweb.asm.Opcodes

import java.nio.file.Files
import scala.jdk.CollectionConverters.given

object TransformerApply {

  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specInheritance = options.acceptsAll(List("i", "inheritance").asJava, "The inheritance map to use").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specTransformer = options.acceptsAll(List("t", "transformer").asJava, "The transformer file that will be applied").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specMappings = options.acceptsAll(List("m", "mappings").asJava, "The mappings that will be applied").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specOutput = options.acceptsAll(List("o", "output").asJava, "Output for additional mappings").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specRemap = options.acceptsAll(List("r", "remap").asJava, "Reverse-remap the transformer").availableIf(specTransformer, specMappings)
    val specNoParam = options.acceptsAll(List("noparam").asJava, "Suppress param remapping")
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException =>
        System.err.println("Option exception: " + e.getMessage)
        options.printHelpOn(System.err)
        Util.exit(0)
    }
    if (!set.has(specInheritance) || !set.has(specOutput)) {
      if (!set.has(specInheritance)) System.out.println("Missing required option: " + specInheritance)
      if (!set.has(specOutput)) System.out.println("Missing required option: " + specOutput)
      options.printHelpOn(System.out)
      System.exit(1)
    } else if (!set.has(specTransformer) && !set.has(specMappings)) {
      println("Cannot run with neither mappings nor transformer given.")
      System.exit(1)
    } else {
      val inheritanceReader = Files.newBufferedReader(set.valueOf(specInheritance))
      val inheritance = InheritanceIO.read(inheritanceReader)
      inheritanceReader.close()

      val transformer = if (set.has(specTransformer)) {
        val transformerReader = Files.newBufferedReader(set.valueOf(specTransformer))
        val transformerJson = Util.GSON.fromJson(transformerReader, classOf[JsonObject])
        transformerReader.close()
        TransformerReader.read(transformerJson)
      } else {
        Seq()
      }

      val mappings = if (set.has(specMappings)) {
        val mappingInput = Files.newInputStream(set.valueOf(specMappings))
        val mappings = IMappingFile.load(mappingInput)
        mappingInput.close()
        Some(mappings)
      } else {
        None
      }

      val mappedTransformer = if (set.has(specRemap) && mappings.isDefined) {
        val reversed = mappings.get.reverse()
        transformer.map(_.remap(reversed))
      } else {
        transformer
      }

      val applied = inheritance.addData(apply(inheritance, mappedTransformer, mappings, !set.has(specNoParam)))
      applied.write(set.valueOf(specOutput).toAbsolutePath.normalize(), IMappingFile.Format.TSRG2, false)
    }
  }

  def apply(inheritance: InheritanceMap, transformers: Seq[ConfiguredTransformer], mappings: Option[IMappingFile], includeParams: Boolean): IMappingFile = {
    val transformation = new TransformationCollector(inheritance, mappings)

    val transform = TransformUtil.createTransformer(inheritance, transformers)
    
    def findParamFromMappings(method: Bytecode.Method, idx: Int): Option[String] = mappings match {
      case Some(mf) => Option(mf.getClass(method.cls)) match {
        case Some(cls) => Option(cls.getMethod(method.name, method.desc)) match {
          case Some (md) => md.getParameters.asScala.find(p => p.getIndex == idx).map(p => p.getMapped)
          case None => None
        }
        case None => None
      }
      case None => None
    }

    def processAsSubClass(cls: String): Boolean = transform(
      cls, TransformTarget.CHILD_CLASS,
      ct => ct.baseTypes.nonEmpty && ct.matchBaseClass(inheritance, cls),
      newName => transformation.transformClass(cls, newName)
    ).isDefined

    def processAsUtilityClass(cls: String): Unit = transform(
      cls, TransformTarget.UTILITY_CLASS,
      ct => ct.baseTypes.exists(base => transform.isUtilityClass(cls, base)),
      newName => transformation.transformClass(cls, newName)
    )

    def processField(field: Bytecode.Field, desc: String): Unit = transform(
      field.name, TransformTarget.FIELD,
      ct => ct.matchBaseField(field.name) && (ct.matchTypeDescriptor(inheritance, desc) || transform.isClassRelatedTo(field.cls, ct.baseTypes)),
      newName => transformation.transformField(field, newName)
    )

    def processMethod(method: Bytecode.Method): Unit = transform(
      method.name, TransformTarget.METHOD,
      ct => ct.matchBaseMethod(method.name, method.desc) && (ct.matchTypeDescriptor(inheritance, method.desc) || transform.isClassRelatedTo(method.cls, ct.baseTypes)),
      newName => transformation.transformMethod(method, newName)
    )

    def processParam(method: Bytecode.Method, idx: Int): Unit = {
      // Synthetic methods are checked last, methods from classes precede over methods from interfaces
      def sortKey(method: Bytecode.Method) = (inheritance.is(method, Opcodes.ACC_SYNTHETIC), inheritance.isInterface(method.cls))
      
      inheritance.getOverriddenMethods(method)
        .toSeq.sortBy(sortKey)
        .flatMap(overriddenMethod => findParamFromMappings(overriddenMethod, idx))
        .headOption match {
          case Some(newName) => transformation.transformParam(method, idx, newName)
          case None => inheritance.getParam(method, idx) match {
            case Some(oldName) => transform(
              oldName, TransformTarget.PARAMETER,
              ct => ct.matchBaseMethod(method.name, method.desc) && (ct.matchTypeDescriptor(inheritance, TransformUtil.getParamTypeForTransformerMatch(method.desc, idx)) || transform.isClassRelatedTo(method.cls, ct.baseTypes)),
              newName => transformation.transformParam(method, idx, newName))
            case None =>
          }
      }
    }

    for (cls: String <- inheritance.sourceClasses) {
      if (!processAsSubClass(cls)) {
        processAsUtilityClass(cls)
      }
      for (fd: Bytecode.Field <- inheritance.getClassFields(cls)) inheritance.getDescriptor(fd) match {
        case Some(desc) => processField(fd, desc)
        case None =>
      }
      for (md: Bytecode.Method <- inheritance.getClassMethods(cls)) {
        processMethod(md)
        if (includeParams) {
          for (idx <- 0 until Bytecode.paramCount(md.desc)) {
            processParam(md, idx)
          }
        }
      }
    }

    transformation.build(includeParams)
  }
}
