package org.moddingx.sourcetransform.util.cls

import org.objectweb.asm.ClassReader

class FakeIndex(val parent: ClassIndex, val classes: List[String], val packages: List[String]) extends ClassIndex {
  
  override lazy val allClasses: Seq[String] = {
    val builder = Set.newBuilder[String]
    builder.addAll(classes)
    builder.addAll(parent.allClasses.filter(cls => packages.contains(packageName(cls))))
    builder.result().toSeq
  }
  
  private def packageName(cls: String): String = {
    if (cls.contains('/')) {
      cls.substring(0, cls.lastIndexOf('/'))
    } else {
      ""
    }
  }

  override def findClass(name: String): Option[ClassReader] = parent.findClass(name)
}
