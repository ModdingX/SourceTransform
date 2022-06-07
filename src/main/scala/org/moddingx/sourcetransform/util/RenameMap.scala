package org.moddingx.sourcetransform.util

import java.io.{BufferedReader, Writer}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.collection.mutable

object RenameMap {

  type Renames = Map[String, Set[(Int, Int, String)]]
  
  private val RENAME_REGEX = "^(?U)\\s*\\[\\s*(\\d+)\\s*:\\s*(\\d+)\\s*]\\s*((?:\\w|\\d|_)+?)\\s*$".r
  private val COMMENT_REGEX = "/\\*rename#(\\d+)#(.*?)\\*/".r

  def read(reader: BufferedReader): Renames = {
    val map = Map.newBuilder[String, Set[(Int, Int, String)]]
    var currentFile: String = null
    var set: mutable.Builder[(Int, Int, String), Set[(Int, Int, String)]] = null
    reader.lines().toArray().map(_.toString).foreach(line => {
      if (line.startsWith("\t")) {
        if (currentFile == null || set == null) throw new IllegalStateException("Invalid local rename map: Rename before file definition.")
        line match {
          case RENAME_REGEX(start, length, value) => set.addOne((start.toInt, length.toInt, value))
          case _ => throw new IllegalStateException("Invalid local rename map: Invalid mapping: " + line.substring(1))
        }
      } else {
        if (currentFile != null && set != null) {
          map.addOne((currentFile, set.result()))
        }
        currentFile = line
        set = Set.newBuilder
      }
    })
    if (currentFile != null && set != null) {
      map.addOne((currentFile, set.result()))
    }
    map.result()
  }

  def write(map: Renames, writer: Writer): Unit = {
    for (file <- map.keySet.toList.sorted) {
      writer.write(file + "\n")
      for (entry <- map(file).toList.sorted) {
        writer.write("\t[" + entry._1 + ":" + entry._2 + "] " + entry._3 + "\n")
      }
    }
  }
  
  def applyAsComments(map: Renames, base: Path): Unit = applyTo(map, base, ctx => ctx.original + "/*rename#" + ctx.length + "#" + ctx.replacement + "*/")
  
  def applyTo(map: Renames, base: Path, replacer: ReplaceContext => String = _.replacement): Unit = {
    val basePath = base.toAbsolutePath.normalize()
    for ((file, renameSet) <- map) {
      val path = basePath.resolve(file).toAbsolutePath.normalize()
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
        for ((offset, len, replacement) <- renameSet.toList.sorted.reverse) {
          if (!failed) {
            if (initialLength <= (offset + len)) {
              System.err.println("Can't apply " + file + ": File too short. Expected length of " + (offset + len + 1) + ", got " + initialLength + ".")
              failed = true
            } else {
              data = data.substring(0, offset) + replacer(ReplaceContext(data.substring(offset, offset + len), offset, len, replacement)) + data.substring(offset + len)
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
  
  def replaceByComments(base: Path): Unit = {
    val basePath = base.toAbsolutePath.normalize()
    for (source <- SourceUtil.getJavaSources(basePath)) {
      val path = basePath.resolve(source)
      if (!Files.isRegularFile(path)) {
        System.err.println("Can't apply " + base.relativize(path) + " comments: Not found: " + path.toString)
      } else {
        val in = Files.newInputStream(path)
        val dataBuffer = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(in.readAllBytes()))
        in.close()
        dataBuffer.rewind()
        var data = dataBuffer.toString
        while (COMMENT_REGEX.findFirstMatchIn(data) match {
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

  case class ReplaceContext(original: String, offset: Int, length: Int, replacement: String)
}
