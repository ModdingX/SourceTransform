package io.github.noeppi_noeppi.tools.sourcetransform.util.cls
import org.objectweb.asm.ClassReader

object NoneLocator extends ClassIndex {
  
  override def allClasses: Seq[String] = Seq()
  override def findClass(name: String): Option[ClassReader] = None
}
