package org.moddingx.sourcetransform.util.cls

import org.objectweb.asm.ClassReader

object NoneLocator extends ClassIndex {
  
  override def allClasses: Seq[String] = Seq()
  override def findClass(name: String): Option[ClassReader] = None
}
