package io.github.noeppi_noeppi.tools.sourcetransform.util

import java.io.{BufferedWriter, Writer}

object IOUtil {
  
  def writeBuffered[T](writer: Writer)(action: BufferedWriter => T): T = writer match {
    case w: BufferedWriter => action(w)
    case w =>
      val buffered = new BufferedWriter(w)
      val result = action(buffered)
      buffered.flush()
      result
  }
}
