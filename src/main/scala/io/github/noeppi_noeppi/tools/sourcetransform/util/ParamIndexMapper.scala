package io.github.noeppi_noeppi.tools.sourcetransform.util

object ParamIndexMapper {

  def translateLocalIdx(local: Int, synthetics: Int, sig: String): Option[Int] = {
    if (local < synthetics) {
      // Synthetic value that is a not a real parameter, return None
      None
    } else {
      val args = reducedArgString(sig)
      var bytecodeIdx = synthetics
      var idx = 0
      for (c <- args) {
        if (bytecodeIdx >= local) {
          return Some(idx)
        } else {
          bytecodeIdx += (if (c == 'J' || c == 'D') 2 else 1)
          idx += 1
        }
      }
      None
    }
  }
  
  private def reducedArgString(sig: String): String = {
    val sb = new StringBuilder
    var skipping = false
    for (c <- sig) {
      if (c == ';') {
        skipping = false
      } else if (!skipping && c != '(') {
        if (c == ')') {
          return sb.toString()
        } else {
          sb.append(c)
          if (c == 'L') skipping = true
        }
      }
    }
    sb.toString()
  }
}
