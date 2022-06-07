package org.moddingx.sourcetransform.inheritance

import org.moddingx.sourcetransform.util.Util
import org.moddingx.sourcetransform.util.cls.{ClassLocator, CompoundIndex, FakeIndex, TreeLocator}
import org.moddingx.sourcetransform.util.inheritance.{InheritanceExtractor, InheritanceIO, InheritanceMap}
import joptsimple.{OptionException, OptionParser}
import joptsimple.util.{PathConverter, PathProperties}

import java.io.File
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.jdk.CollectionConverters.given

object InheritanceBuilder {

  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specClasses = options.acceptsAll(List("c", "classes").asJava, "The folder with the compiled classes").withRequiredArg().withValuesConvertedBy(new PathConverter(PathProperties.DIRECTORY_EXISTING))
    val specClassNames = options.acceptsAll(List("n", "class-names").asJava, "The class names that should be treated as source classes").withRequiredArg().withValuesSeparatedBy(';')
    val specPackageNames = options.acceptsAll(List("m", "package-names").asJava, "The package names that should be treated as source classes").withRequiredArg().withValuesSeparatedBy(';')
    val specClasspath = options.acceptsAll(List("p", "classpath").asJava, "Library classpath. Must also include the jars / jmods from the java installation.").withRequiredArg().withValuesSeparatedBy(File.pathSeparator).withValuesConvertedBy(new PathConverter())
    val specOutput = options.acceptsAll(List("o", "output").asJava, "Output file").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException =>
        System.err.println("Option exception: " + e.getMessage)
        options.printHelpOn(System.err)
        Util.exit(0)
    }
    if ((!set.has(specClasses) && !set.has(specClassNames) && !set.has(specPackageNames)) || !set.has(specOutput)) {
      if (!set.has(specClasses)) System.out.println("Missing required option: " + specClasses)
      if (!set.has(specOutput)) System.out.println("Missing required option: " + specOutput)
      options.printHelpOn(System.out)
      Util.exit(1)
    } else if (set.has(specClasses) && (set.has(specClassNames) || set.has(specPackageNames))) {
      if (!set.has(specClasses)) System.out.println("Can't use " + specClasses + " and " + specClassNames + "/" + specPackageNames + " together.")
      options.printHelpOn(System.out)
      Util.exit(1)
    } else {
      val libraryPaths: Seq[Path] = Option(set.valuesOf(specClasspath)).map(_.asScala.toSeq).getOrElse(Seq())
      val library = new CompoundIndex(libraryPaths.map(path => ClassLocator.file(path)): _*)
      
      val classIndex = if (set.has(specClasses)) {
        new TreeLocator(set.valueOf(specClasses))
      } else {
        new FakeIndex(library,
          set.valuesOf(specClassNames).asScala.toList.map(_.replace('.', '/')),
          set.valuesOf(specPackageNames).asScala.toList.map(_.replace('.', '/'))
        )
      }

      val inheritance: InheritanceMap = InheritanceExtractor.extractInheritance(classIndex, library)
      val writer = Files.newBufferedWriter(set.valueOf(specOutput), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      InheritanceIO.write(inheritance, writer)
      writer.close()
    }
  }
}
