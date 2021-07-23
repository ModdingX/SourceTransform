package io.github.noeppi_noeppi.tools.sourcetransform.inspect

import com.google.gson.JsonObject
import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.InheritanceMap
import io.github.noeppi_noeppi.tools.sourcetransform.util.{LanguageLevel, SourceUtil, Util}
import joptsimple.util.{PathConverter, PathProperties}
import joptsimple.{OptionException, OptionParser}
import org.eclipse.jdt.core.dom.CompilationUnit

import java.io.File
import java.nio.file.{Files, StandardOpenOption}
import scala.jdk.CollectionConverters._

object SourceInspector {

  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specInput = options.acceptsAll(List("s", "sources").asJava, "Folder with source files.").withRequiredArg().withValuesConvertedBy(new PathConverter(PathProperties.DIRECTORY_EXISTING))
    val specInheritance = options.acceptsAll(List("i", "inheritance").asJava, "The inheritance map to use").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specInspections = options.acceptsAll(List("c", "inspections").asJava, "The inspection file that will be applied").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specClasspath = options.acceptsAll(List("p", "classpath").asJava, "Library classpath. Must also include the jars / jmods from the java installation.").withRequiredArg().withValuesSeparatedBy(File.pathSeparator).withValuesConvertedBy(new PathConverter())
    val specLevel = options.acceptsAll(List("l", "level").asJava, "Source level").withRequiredArg().withValuesConvertedBy(Util.enum[LanguageLevel]).defaultsTo(LanguageLevel.JAVA_16)
    val specOutput = options.acceptsAll(List("o", "output").asJava, "Output for the inspection report.").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specDefault = options.acceptsAll(List("d", "default").asJava, "Include default inspections found by eclipse JDT")
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException => System.err.println("Option exception: " + e.getMessage); options.printHelpOn(System.err); Util.exit(0)
    }
    if (!set.has(specInput) || !set.has(specInheritance) || !set.has(specInspections) || !set.has(specOutput)) {
      if (!set.has(specInput)) System.out.println("Missing required option: " + specInput)
      if (!set.has(specInheritance)) System.out.println("Missing required option: " + specInheritance)
      if (!set.has(specInspections)) System.out.println("Missing required option: " + specInspections)
      if (!set.has(specOutput)) System.out.println("Missing required option: " + specOutput)
      options.printHelpOn(System.out)
    } else {
      val inheritanceReader = Files.newBufferedReader(set.valueOf(specInheritance))
      val inheritance = InheritanceMap.read(inheritanceReader)
      inheritanceReader.close()

      val inspectionReader = Files.newBufferedReader(set.valueOf(specInspections))
      val inspectionJson = Util.GSON.fromJson(inspectionReader, classOf[JsonObject])
      inspectionReader.close()
      val inspections = InspectionReader.read(inspectionJson)

      val base = set.valueOf(specInput).toAbsolutePath.normalize()
      val files = SourceUtil.getJavaSources(base)
      val createParser = SourceUtil.createParserFactory(set.valueOf(specLevel), base, set.valuesOf(specClasspath).asScala.toSeq) _

      val inspector = new Inspector(inheritance)
      val builtInspections = inspections.map(i => i.visitor(inspector))
      files.foreach(file => {
        val parser = createParser(file)
        parser.createAST(null) match {
          case cu: CompilationUnit =>
            // Prepare inspector for this source file
            inspector.source(file, cu)
            cu.accept(inspector.visitor())
            // Add problem found by JDT
            if (set.has(specDefault)) {
              cu.getProblems.foreach(inspector.message)
            }
            // Run inspections
            builtInspections.foreach(cu.accept)
          case x => System.err.println("Parser returned invalid result for " + file + ": " + x.getClass + " " + x)
        }
      })

      val writer = Files.newBufferedWriter(set.valueOf(specOutput), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      inspector.print(writer)
      writer.close()
    }
  }
}
