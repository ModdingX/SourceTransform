package io.github.noeppi_noeppi.tools.sourcetransform.transform

import com.google.gson.JsonObject
import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.{FieldInfo, InheritanceMap, MethodInfo, ParamInfo}
import io.github.noeppi_noeppi.tools.sourcetransform.util.Util
import joptsimple.util.PathConverter
import joptsimple.{OptionException, OptionParser}
import net.minecraftforge.srgutils.IMappingFile

import java.nio.file.Files
import scala.jdk.CollectionConverters._

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
      case e: OptionException => System.err.println("Option exception: " + e.getMessage); options.printHelpOn(System.err); Util.exit(0)
    }
    if (!set.has(specInheritance) || !set.has(specOutput)) {
      if (!set.has(specInheritance)) System.out.println("Missing required option: " + specInheritance)
      if (!set.has(specOutput)) System.out.println("Missing required option: " + specOutput)
      options.printHelpOn(System.out)
    } else if (!set.has(specTransformer) && !set.has(specMappings)) {
      println("Cannot run with neither mappings nor transformer given.")
    } else {
      val inheritanceReader = Files.newBufferedReader(set.valueOf(specInheritance))
      val inheritance = InheritanceMap.read(inheritanceReader)
      inheritanceReader.close()
      
      val transformer = if (set.has(specTransformer)) {
        val transformerReader = Files.newBufferedReader(set.valueOf(specTransformer))
        val transformerJson = Util.GSON.fromJson(transformerReader, classOf[JsonObject])
        transformerReader.close()
        TransformerReader.read(transformerJson)
      } else {
        Nil
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
      
      val applied = apply(inheritance, mappedTransformer, mappings, set.has(specNoParam))
      applied.write(set.valueOf(specOutput), IMappingFile.Format.TSRG2, false)
    }
  }
  
  def apply(inheritance: InheritanceMap, transformers: List[ConfiguredTransformer], mappings: Option[IMappingFile], noparam: Boolean): IMappingFile = {
    val transformation = new Transformation(inheritance, mappings)
    
    val transform = TransformUtil.createTransformer(transformers) _

    def checkUtilityType(cls: String, forType: String): Boolean = {
      val members = inheritance.getMemberSignatures(cls)
      val memberCount = members.count(m => m.contains(forType))
      (members.size / memberCount.toDouble) <= 5
    }
    
    def checkClassFor(cls: String, forTypes: Set[String]): Boolean = {
      forTypes.exists(forType => inheritance.isSubType("L" + cls + ";", forType) || checkUtilityType(cls, forType))
    }

    def findMappingParam(info: ParamInfo): Option[String] = {
      mappings
        .flatMap(mf => Option(mf.getClass(info.method.cls)))
        .flatMap(cls => Option(cls.getMethod(info.method.name, info.method.signature)))
        .flatMap(m => m.getParameters.asScala.find(p => p.getIndex == info.idx))
        .map(p => p.getMapped)
    }
    
    def processSubClass(cls: String): Boolean = {
      val result = transform(cls, TransformTarget.CHILD_CLASS, t => t.baseType.nonEmpty && t.matchBaseClass(inheritance, cls), n => transformation.transformClass(cls, n))
      result.isDefined
    }
    
    def processUtilityClass(cls: String): Unit = {
      transform(cls, TransformTarget.UTILITY_CLASS, t => t.baseType.exists(e => checkUtilityType(cls, e)), n => transformation.transformClass(cls, n))
    }
    
    def processField(info: FieldInfo): Unit = {
      transform(info.name, TransformTarget.FIELD, t => {
        t.matchBaseField(info.name) && (t.matchTypeSignature(inheritance, info.fieldType) || checkClassFor(info.cls, t.baseType))
      }, n => transformation.transformField(info, n))
    }
    
    def processMethod(info: MethodInfo): Unit = {
      transform(info.name, TransformTarget.METHOD, t => {
        t.matchBaseMethod(info.name, info.signature) && (t.matchTypeSignature(inheritance, info.signature) || checkClassFor(info.cls, t.baseType))
      }, n => transformation.transformMethod(info, n))
    }
    
    def processParam(info: ParamInfo): Unit = {
      inheritance.getOverridden(info.method)
        .flatMap(m => findMappingParam(ParamInfo(m, info.idx, info.name)))
        .headOption match {
        case Some(name) => transformation.transformParam(info, name)
        case None => transform(info.name, TransformTarget.PARAMETER, t => {
          t.matchBaseMethod(info.name, info.method.signature) && (t.matchTypeSignature(inheritance, Util.getParamTypeForMatch(info.method.signature, info.idx)) || checkClassFor(info.method.cls, t.baseType))
        }, n => transformation.transformParam(info, n))
      }
    }
    
    inheritance.sourceClasses.foreach(cls => {
      if (!processSubClass(cls)) {
        processUtilityClass(cls)
      }
    })
    
    inheritance.fields.foreach(f => {
      processField(f)
    })
    
    inheritance.methods.foreach(m => {
      processMethod(m)
    })
    
    if (!noparam) {
      inheritance.params.foreach(p => {
        processParam(p)
      })
    }
    
    transformation.build(noparam)
  }
}
