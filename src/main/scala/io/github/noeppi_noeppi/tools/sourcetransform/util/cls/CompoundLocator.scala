package io.github.noeppi_noeppi.tools.sourcetransform.util.cls
import org.objectweb.asm.ClassReader

class CompoundLocator(parents: ClassLocator*) extends ClassLocator {
  
  override def findClass(name: String): Option[ClassReader] = {
    for (parent <- parents) {
      val result = parent.findClass(name)
      if (result.isDefined) return result
    }
    None
  }
}
