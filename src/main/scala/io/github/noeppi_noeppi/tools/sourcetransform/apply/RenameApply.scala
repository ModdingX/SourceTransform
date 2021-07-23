package io.github.noeppi_noeppi.tools.sourcetransform.apply

import io.github.noeppi_noeppi.tools.sourcetransform.util.{RenameMap, Util}
import joptsimple.util.PathConverter
import joptsimple.{OptionException, OptionParser}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import scala.jdk.CollectionConverters._

object RenameApply {

  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specInput = options.acceptsAll(List("s", "sources").asJava, "Source root").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specRename = options.acceptsAll(List("r", "rename").asJava, "The rename map to apply").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specComments = options.acceptsAll(List("c", "comments").asJava, "Only add comments to the code to not change the generated classes on recompiling.  You can alter apply the comments.")
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException => System.err.println("Option exception: " + e.getMessage); options.printHelpOn(System.err); Util.exit(0)
    }
    if (!set.has(specInput) || !set.has(specRename)) {
      if (!set.has(specInput)) System.out.println("Missing required option: " + specInput)
      if (!set.has(specRename)) System.out.println("Missing required option: " + specRename)
      options.printHelpOn(System.out)
    } else {
      val renameReader = Files.newBufferedReader(set.valueOf(specRename))
      val renames = RenameMap.read(renameReader)
      renameReader.close()
      
      val base = set.valueOf(specInput).toAbsolutePath.normalize()
      for ((file, renameSet) <- renames) {
        val path = base.resolve(file).toAbsolutePath.normalize()
        if (!Files.isRegularFile(path)) {
          System.err.println("Can't apply " + file + ": Not found: " + path.toString)
        } else {
          val in = Files.newInputStream(path)
          val dataBuffer = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(in.readAllBytes()))
          in.close()
          dataBuffer.rewind()
          var data = dataBuffer.toString
          val initialLength = data.length
          var failed = false
          for ((idx, len, replacement) <- renameSet.toList.sorted.reverse) {
            if (!failed) {
              if (initialLength <= (idx + len)) {
                System.err.println("Can't apply " + file + ": File too short. Expected length of " + (idx + len + 1) + ", got " + initialLength + ".")
                failed = true
              } else if (set.has(specComments)) {
                // We use relative positions as this is meant to be used before Srg2Source
                // and then is applied afterwards.
                data = data.substring(0, idx + len) + "/*rename#" + len + "#" + replacement + "*/" + data.substring(idx + len)
              } else {
                data = data.substring(0, idx) + replacement + data.substring(idx + len)
              }
            }
          }
          if (!failed) {
            val writer = Files.newBufferedWriter(path, StandardOpenOption.TRUNCATE_EXISTING)
            writer.write(data)
            writer.close()
          }
        }
      }
    }
  }
}
