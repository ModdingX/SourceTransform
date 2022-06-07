package org.moddingx.sourcetransform.util.cls

class CompoundIndex(parents: ClassIndex*) extends CompoundLocator(parents: _*) with ClassIndex {
  
  override def allClasses: List[String] = parents.flatMap(_.allClasses).distinct.toList
}
