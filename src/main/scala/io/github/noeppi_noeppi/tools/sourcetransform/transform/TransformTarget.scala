package io.github.noeppi_noeppi.tools.sourcetransform.transform

sealed abstract class TransformTarget(val id: String)

object TransformTarget {
  
  def byId(id: String): TransformTarget = id match {
    case CHILD_CLASS.id => CHILD_CLASS
    case UTILITY_CLASS.id => UTILITY_CLASS
    case FIELD.id => FIELD
    case METHOD.id => METHOD
    case PARAMETER.id => PARAMETER
    case LOCAL.id => LOCAL
    case _ => throw new IllegalArgumentException("Invalid transform target: '" + id + "'")
  }

  case object CHILD_CLASS extends TransformTarget("child_class")
  case object UTILITY_CLASS extends TransformTarget("utility_class")
  case object FIELD extends TransformTarget("field")
  case object METHOD extends TransformTarget("method")
  case object PARAMETER extends TransformTarget("parameter")
  case object LOCAL extends TransformTarget("local")
}
