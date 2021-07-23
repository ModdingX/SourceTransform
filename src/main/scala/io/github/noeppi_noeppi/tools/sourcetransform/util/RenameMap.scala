package io.github.noeppi_noeppi.tools.sourcetransform.util

import java.io.{BufferedReader, Writer}
import scala.collection.mutable

object RenameMap {

  private val RENAME_REGEX = "^(?U)\\s*\\[\\s*(\\d+)\\s*:\\s*(\\d+)\\s*]\\s*((?:\\w|\\d|_)+?)\\s*$".r

  def read(reader: BufferedReader): Map[String, Set[(Int, Int, String)]] = {
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

  def write(map: Map[String, Set[(Int, Int, String)]], writer: Writer): Unit = {
    for (file <- map.keySet.toList.sorted) {
      writer.write(file + "\n")
      for (entry <- map(file).toList.sorted) {
        writer.write("\t[" + entry._1 + ":" + entry._2 + "] " + entry._3 + "\n")
      }
    }
  }
}
