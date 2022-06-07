package org.moddingx.sourcetransform.transform.data

import org.moddingx.sourcetransform.util.Util

enum TransformTarget(val id: String) {
  case CHILD_CLASS extends TransformTarget("child_class")
  case UTILITY_CLASS extends TransformTarget("utility_class")
  case FIELD extends TransformTarget("field")
  case METHOD extends TransformTarget("method")
  case PARAMETER extends TransformTarget("parameter")
  case LOCAL extends TransformTarget("local")
}

object TransformTarget {

  lazy val targets: Set[TransformTarget] = TransformTarget.values.toSet
  lazy val idMap: Map[String, TransformTarget] = Util.groupDistinct(targets, _.id)
  
  def byId(id: String): TransformTarget = idMap.getOrElse(id, throw new NoSuchElementException("Invalid transform target: " + id))
}
