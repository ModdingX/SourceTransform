package io.github.noeppi_noeppi.tools.sourcetransform.jstype

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.InheritanceMap
import io.github.noeppi_noeppi.tools.sourcetransform.util.cls.{ClassLocator, CompoundIndex, FakeIndex}
import io.github.noeppi_noeppi.tools.sourcetransform.util.{ClassFailer, Util}
import joptsimple.util.PathConverter
import joptsimple.{OptionException, OptionParser}

import java.io.File
import java.nio.file.{Files, StandardOpenOption}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object JavaScriptGenerator {

  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specInheritance = options.acceptsAll(List("i", "inheritance").asJava, "The inheritance map to use").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specClasses = options.acceptsAll(List("c", "classes").asJava, "The classes to generate type definitions for").withRequiredArg().withValuesSeparatedBy(';')
    val specPackages = options.acceptsAll(List("m", "packages").asJava, "The packages to generate type definitions for").withRequiredArg().withValuesSeparatedBy(';')
    val specClasspath = options.acceptsAll(List("p", "classpath").asJava, "Library classpath. Must also include the jars / jmods from the java installation.").withRequiredArg().withValuesSeparatedBy(File.pathSeparator).withValuesConvertedBy(new PathConverter())
    val specJs = options.acceptsAll(List("j", "output-js").asJava, "Output javascript").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specTs = options.acceptsAll(List("t", "output-ts").asJava, "Output typescript").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specNulls = options.acceptsAll(List("n", "nullable").asJava, "Nullability file").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException => System.err.println("Option exception: " + e.getMessage); options.printHelpOn(System.err); Util.exit(0)
    }
    if (!set.has(specInheritance) || (!set.has(specClasses) && !set.has(specPackages)) || !set.has(specClasspath) || !set.has(specJs) || !set.has(specTs)) {
      if (!set.has(specInheritance)) System.out.println("Missing required option: " + specInheritance)
      if (!set.has(specClasses)) System.out.println("Missing required option: " + specClasses)
      if (!set.has(specClasspath)) System.out.println("Missing required option: " + specClasspath)
      if (!set.has(specJs)) System.out.println("Missing required option: " + specJs)
      if (!set.has(specTs)) System.out.println("Missing required option: " + specTs)
      options.printHelpOn(System.out)
      System.exit(1)
    } else {
      val inheritanceReader = Files.newBufferedReader(set.valueOf(specInheritance))
      val inheritance = InheritanceMap.read(inheritanceReader)
      inheritanceReader.close()
      
      val library = new CompoundIndex(Option(set.valuesOf(specClasspath)).map(_.asScala).getOrElse(Nil).map(p => ClassLocator.file(p)).toSeq: _*)
      val classes = new FakeIndex(library,
        set.valuesOf(specClasses).asScala.map(_.replace('.', '/')).toList,
        set.valuesOf(specPackages).asScala.map(_.replace('.', '/')).toList
      ).allClasses
      
      val classNames: Map[String, String] = classes.map(cls => (cls, if (cls.contains('/')) cls.substring(cls.lastIndexOf('/') + 1) else cls)).toMap
      val classMap: Map[String, String] = if (classNames.size != classNames.values.toSet.size) {
        System.err.println("Duplicate class names. Using fully qualified mode.")
        Map()
      } else {
        classNames
      }
      
      val nulls = if (set.has(specNulls)) {
        val nullReader = Files.newBufferedReader(set.valueOf(specNulls))
        val nullLines = nullReader.lines().toArray.map(_.toString).toSet
          .map((str: String) => if (str.contains("//")) str.substring(0, str.indexOf("//")) else str)
          .map((str: String) => str.strip())
          .filter(_.nonEmpty)
        nullReader.close()
        new NullChecker(nullLines)
      } else {
        new NullChecker(Set())
      }
      
      if (!Files.exists(set.valueOf(specJs).getParent)) Files.createDirectories(set.valueOf(specJs).getParent)
      if (!Files.exists(set.valueOf(specTs).getParent)) Files.createDirectories(set.valueOf(specTs).getParent)
      
      val jsWriter = Files.newBufferedWriter(set.valueOf(specJs), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      val tsWriter = Files.newBufferedWriter(set.valueOf(specTs), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

      val env = new JsEnv(inheritance, classes, library, classMap, tsWriter, nulls)
      jsWriter.write("var exports={};\nfunction require(arg){return" + classes.map(cls => env.plainName(cls) + ":Java.type('" + cls.replace('/', '.') + "')").mkString("{", ",", "}") + ";}\n")
      jsWriter.close()
      
      val failer = new ClassFailer
      val processedClasses = mutable.Set[String]()
      
      for (cls <- classes) {
        JsGenerator.processClass(env, cls, failer, processedClasses)
      }
      
      tsWriter.write("\n")
      
      tsWriter.close()
    }
  }
}
