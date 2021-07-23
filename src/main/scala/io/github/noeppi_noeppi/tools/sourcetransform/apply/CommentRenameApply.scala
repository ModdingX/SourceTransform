package io.github.noeppi_noeppi.tools.sourcetransform.apply

import io.github.noeppi_noeppi.tools.sourcetransform.util.Util
import joptsimple.util.PathConverter
import joptsimple.{OptionException, OptionParser}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

object CommentRenameApply {

  private val RENAME_REGEX = "/\\*rename#(\\d+)#(.*?)\\*/".r
  
  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specInput = options.acceptsAll(List("s", "sources").asJava, "Source root").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException => System.err.println("Option exception: " + e.getMessage); options.printHelpOn(System.err); Util.exit(0)
    }
    if (!set.has(specInput)) {
      if (!set.has(specInput)) System.out.println("Missing required option: " + specInput)
      options.printHelpOn(System.out)
    } else {
      val base = set.valueOf(specInput).toAbsolutePath.normalize()
      val files = Files.walk(base).toScala(List)
        .filter(p => Files.isRegularFile(p))
        .filter(p => p.getFileName.toString.endsWith(".java"))
        .filter(p => p.getFileName.toString != "package-info.java")
        .filter(p => p.getFileName.toString != "module-info.java")
        .map(p => p.toAbsolutePath.normalize())
      
      for (path <- files) {
        if (!Files.isRegularFile(path)) {
          System.err.println("Can't apply " + base.relativize(path) + " comments: Not found: " + path.toString)
        } else {
          val in = Files.newInputStream(path)
          val dataBuffer = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(in.readAllBytes()))
          in.close()
          dataBuffer.rewind()
          var data = dataBuffer.toString
          while (RENAME_REGEX.findFirstMatchIn(data) match {
            case Some(m) =>
              val from = m.start - m.group(1).toInt
              val to = m.end
              data = data.substring(0, from) + m.group(2) + data.substring(to)
              true
            case None => false
          }) {}
          
          val writer = Files.newBufferedWriter(path, StandardOpenOption.TRUNCATE_EXISTING)
          writer.write(data)
          writer.close()
        }
      }
    }
  }
}
