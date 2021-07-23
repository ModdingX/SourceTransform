package io.github.noeppi_noeppi.tools.sourcetransform.local

import com.google.gson.JsonObject
import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.InheritanceMap
import io.github.noeppi_noeppi.tools.sourcetransform.transform.TransformerReader
import io.github.noeppi_noeppi.tools.sourcetransform.util.{LanguageLevel, RenameMap, SourceUtil, Util}
import joptsimple.util.{PathConverter, PathProperties}
import joptsimple.{OptionException, OptionParser}
import net.minecraftforge.srgutils.IMappingFile
import org.eclipse.jdt.core.dom.CompilationUnit

import java.io.File
import java.nio.file.{Files, StandardOpenOption}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object LocalMapCreator {

  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specInput = options.acceptsAll(List("s", "sources").asJava, "Folder with source files.").withRequiredArg().withValuesConvertedBy(new PathConverter(PathProperties.DIRECTORY_EXISTING))
    val specInheritance = options.acceptsAll(List("i", "inheritance").asJava, "The inheritance map to use").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specTransformer = options.acceptsAll(List("t", "transformer").asJava, "The transformer file that will be applied").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specMappings = options.acceptsAll(List("m", "mappings").asJava, "The mappings that will be applied").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specClasspath = options.acceptsAll(List("p", "classpath").asJava, "Library classpath. Must also include the jars / jmods from the java installation.").withRequiredArg().withValuesSeparatedBy(File.pathSeparator).withValuesConvertedBy(new PathConverter())
    val specLevel = options.acceptsAll(List("l", "level").asJava, "Source level").withRequiredArg().withValuesConvertedBy(Util.enum[LanguageLevel]).defaultsTo(LanguageLevel.JAVA_16)
    val specOutput = options.acceptsAll(List("o", "output").asJava, "Output for the local rename map.").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specRemap = options.acceptsAll(List("r", "remap").asJava, "Reverse-remap the transformer").availableIf(specTransformer, specMappings)
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException => System.err.println("Option exception: " + e.getMessage); options.printHelpOn(System.err); Util.exit(0)
    }
    if (!set.has(specInput) || !set.has(specInheritance) || !set.has(specTransformer) || !set.has(specOutput)) {
      if (!set.has(specInput)) System.out.println("Missing required option: " + specInput)
      if (!set.has(specInheritance)) System.out.println("Missing required option: " + specInheritance)
      if (!set.has(specTransformer)) System.out.println("Missing required option: " + specTransformer)
      if (!set.has(specOutput)) System.out.println("Missing required option: " + specOutput)
      options.printHelpOn(System.out)
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
      
      val base = set.valueOf(specInput).toAbsolutePath.normalize()
      val files = SourceUtil.getJavaSources(base)
      val createParser = SourceUtil.createParserFactory(set.valueOf(specLevel), base, set.valuesOf(specClasspath).asScala.toSeq) _
      
      val map = Map.newBuilder[String, Set[(Int, Int, String)]]
      files.foreach(file => {
        val parser = createParser(file)
        parser.createAST(null) match {
          case cu: CompilationUnit =>
            val set = mutable.Set[(Int, Int, String)]()
            cu.accept(new SourceVisitor(inheritance, mappedTransformer, set))
            if (set.nonEmpty) {
              map.addOne(file -> set.toSet)
            }
          case x => System.err.println("Parser returned invalid result for " + file + ": " + x.getClass + " " + x)
        }
      })
      
      val writer = Files.newBufferedWriter(set.valueOf(specOutput), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      RenameMap.write(map.result(), writer)
      writer.close()
    }
  }
}
