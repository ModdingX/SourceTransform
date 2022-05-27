package io.github.noeppi_noeppi.tools.sourcetransform.util.cls

import org.objectweb.asm.ClassReader

class DecoratedIndex(parent: ClassIndex, decorator: ClassLocator => ClassLocator) extends ClassIndex {

  private val locator = decorator(parent)

  override def allClasses: Seq[String] = parent.allClasses
  override def findClass(name: String): Option[ClassReader] = locator.findClass(name)
}
