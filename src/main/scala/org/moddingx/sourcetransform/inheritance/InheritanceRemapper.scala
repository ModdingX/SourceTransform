package org.moddingx.sourcetransform.inheritance

import org.moddingx.sourcetransform.util.Util
import org.moddingx.sourcetransform.util.inheritance.InheritanceIO
import joptsimple.util.PathConverter
import joptsimple.{OptionException, OptionParser}
import net.minecraftforge.srgutils.IMappingFile

import java.nio.file.{Files, StandardOpenOption}
import scala.jdk.CollectionConverters.given

object InheritanceRemapper {
  
  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specInput = options.acceptsAll(List("i", "input").asJava, "The inheritance map to read").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specMappings = options.acceptsAll(List("m", "mappings").asJava, "The mappings to apply").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specOutput = options.acceptsAll(List("o", "output").asJava, "The output inheritance map file.").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException =>
        System.err.println("Option exception: " + e.getMessage)
        options.printHelpOn(System.err)
        Util.exit(0)
    }
    if (!set.has(specInput) || !set.has(specMappings) || !set.has(specOutput)) {
      if (!set.has(specInput)) System.out.println("Missing required option: " + specInput)
      if (!set.has(specMappings)) System.out.println("Missing required option: " + specMappings)
      if (!set.has(specOutput)) System.out.println("Missing required option: " + specOutput)
      options.printHelpOn(System.out)
      System.exit(1)
    } else {
      val inheritanceReader = Files.newBufferedReader(set.valueOf(specInput))
      val inheritance = InheritanceIO.read(inheritanceReader)
      inheritanceReader.close()
      
      val mappingInput = Files.newInputStream(set.valueOf(specMappings))
      val mappings = IMappingFile.load(mappingInput)
      mappingInput.close()
      
      val remapped = inheritance.remap(mappings)
      
      val writer = Files.newBufferedWriter(set.valueOf(specOutput), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      InheritanceIO.write(remapped, writer)
      writer.close()
    }
  }
}
