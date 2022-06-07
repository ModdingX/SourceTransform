package org.moddingx.sourcetransform.util.cls

import org.objectweb.asm.ClassReader

import java.nio.file.{Files, Path}

trait ClassLocator {
  
  def findClass(name: String): Option[ClassReader]
}

object ClassLocator {
  
  def file(path: Path): ClassIndex = {
    if (Files.isDirectory(path)) {
      new TreeLocator(path)
    } else {
      new JarLocator(path)
    }
  }
}