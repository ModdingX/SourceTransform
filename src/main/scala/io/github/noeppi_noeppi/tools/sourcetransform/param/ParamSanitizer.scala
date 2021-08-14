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
  val NONE = new ParamSanitizer(Set(), 0)
  
  def queryDefault(info: MethodInfo, quiet: Boolean): ParamSanitizer = {
    if (!quiet) println("Using default sanitizer for method " + info.cls + " " + info.name + info.signature)
    DEFAULT
  }
  
  def withRenamed(sanitizer: ParamSanitizer, renames: Set[String]): ParamSanitizer = {
    if (sanitizer.forbidden == null) {
      DEFAULT
    } else {
      ParamSanitizer(sanitizer.forbidden | renames, sanitizer.localClassLevel)
    }
  }
  
  def queryLambda(info: MethodInfo, quiet: Boolean, sanitizers: ParamSanitizer*): Option[ParamSanitizer] = {
    if (sanitizers.exists(s => s.forbidden == null)) {
      if (!quiet) println("Using default sanitizer for lambda implementation " + info.cls + " " + info.name + info.signature)
      None
    } else {
      Some(ParamSanitizer(sanitizers.flatMap(_.forbidden).toSet, sanitizers.map(_.localClassLevel).maxOption.getOrElse(0)))
    }
  }
}
