package io.github.noeppi_noeppi.tools.sourcetransform.param

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.MethodInfo

case class ParamSanitizer(forbidden: Set[String], localClassLevel: Int) {
  
  def sanitize(param: String): String = {
    val baseParam = (if (param.endsWith("_")) param + "0" else param) + ("_" * localClassLevel)
    if (forbidden != null && !forbidden.contains(baseParam)) {
      baseParam
    } else {
      var sanitized = "p_" + baseParam
      while (forbidden != null && forbidden.contains(sanitized)) {
        sanitized = "p" + sanitized
      }
      sanitized
    }
  }
}

object ParamSanitizer {
  
  val DEFAULT = new ParamSanitizer(null, 0)
  
  def queryDefault(info: MethodInfo, quiet: Boolean): ParamSanitizer = {
    if (!quiet) System.err.println("Using default sanitizer for method " + info.cls + " " + info.name + info.signature + " Is it missing in the source code?")
    DEFAULT
  }
}
