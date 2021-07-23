package io.github.noeppi_noeppi.tools.sourcetransform.util.cls

import org.objectweb.asm.ClassReader

trait ClassLocator {
  
  def findClass(name: String): Option[ClassReader]
}