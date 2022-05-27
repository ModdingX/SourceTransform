package io.github.noeppi_noeppi.tools.sourcetransform.util.cls

import org.objectweb.asm.ClassReader

import scala.collection.mutable

class FailingLocator(parent: ClassLocator) extends ClassLocator {
  
  private val failedClasses = mutable.Set[String]()

  override def findClass(name: String): Option[ClassReader] = parent.findClass(name) match {
    case Some(cls) => Some(cls)
    case None if !failedClasses.contains(name) =>
      System.err.println("Failed to load class: " + name)
      failedClasses.addOne(name)
      None
    case None => None
  }
}
