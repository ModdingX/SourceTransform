package org.moddingx.sourcetransform.util.cls

import org.objectweb.asm.ClassReader

import scala.annotation.tailrec

class CompoundLocator(parents: ClassLocator*) extends ClassLocator {

  private val parentList: List[ClassLocator] = parents.toList

  override def findClass(name: String): Option[ClassReader] = {
    @tailrec
    def findIn(list: List[ClassLocator]): Option[ClassReader] = list match {
      case Nil => None
      case h :: t => h.findClass(name) match {
        case Some(cls) => Some(cls)
        case None => findIn(t)
      }
    }
    findIn(parentList)
  }
}
