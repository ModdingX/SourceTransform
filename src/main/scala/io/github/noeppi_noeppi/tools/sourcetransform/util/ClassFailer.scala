package io.github.noeppi_noeppi.tools.sourcetransform.util

import scala.collection.mutable

class ClassFailer {
  
  private val classes = mutable.Set[String]()
  
  def warn(cls: String): Unit = if (!classes.contains(cls)) {
    System.err.println("Failed to load class: " + cls)
    classes.addOne(cls)
  }
}
