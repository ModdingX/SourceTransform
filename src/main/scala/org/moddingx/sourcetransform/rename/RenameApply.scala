package org.moddingx.sourcetransform.rename

import org.moddingx.sourcetransform.util.{RenameMap, Util}
import joptsimple.util.PathConverter
import joptsimple.{OptionException, OptionParser}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import scala.jdk.CollectionConverters.given

object RenameApply {

  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specInput = options.acceptsAll(List("s", "sources").asJava, "Source root").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specRename = options.acceptsAll(List("r", "rename").asJava, "The rename map to apply").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specComments = options.acceptsAll(List("c", "comments").asJava, "If specified together with --rename, add comments to the code that contain the replacements. If used without --rename, apply previously added comments.")
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException => System.err.println("Option exception: " + e.getMessage); options.printHelpOn(System.err); Util.exit(0)
    }
    if (!set.has(specInput) || (!set.has(specRename) && !set.has(specComments))) {
      if (!set.has(specInput)) System.out.println("Missing required option: " + specInput)
      if (!set.has(specRename) && !set.has(specComments)) System.out.println("Missing required option: " + specRename)
      options.printHelpOn(System.out)
      System.exit(1)
    } else {
      if (set.has(specRename)) {
        val renameReader = Files.newBufferedReader(set.valueOf(specRename))
        val renames = RenameMap.read(renameReader)
        renameReader.close()

        if (set.has(specComments)) RenameMap.applyAsComments(renames, set.valueOf(specInput))
        else RenameMap.applyTo(renames, set.valueOf(specInput))
      } else {
        RenameMap.replaceByComments(set.valueOf(specInput))
      }
    }
  }
}
