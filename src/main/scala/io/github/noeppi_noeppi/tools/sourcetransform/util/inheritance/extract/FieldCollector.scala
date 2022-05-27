package io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.extract

import io.github.noeppi_noeppi.tools.sourcetransform.util.Bytecode
import io.github.noeppi_noeppi.tools.sourcetransform.util.cls.ClassLocator
import io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.FieldInfo

import scala.collection.mutable

class FieldCollector(val classInheritance: ClassInheritance) {

  val locator: ClassLocator = classInheritance.locator

  case class FieldEntry(desc: String, access: Int)

  private val fields = mutable.Map[Bytecode.Field, FieldEntry]()

  def addField(cls: String, name: String, desc: String, access: Int): Unit = {
    classInheritance.addClass(cls)
    fields.put(Bytecode.Field(cls, name), FieldEntry(desc, access))
  }
  
  def build(cls: String): Set[FieldInfo] = {
    fields.filter(entry => entry._1.cls == cls).map(entry => {
      val (fd, FieldEntry(desc, access)) = entry
      FieldInfo(fd.name, desc, access)
    }).toSet
  }
}
