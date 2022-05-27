package io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.extract

import io.github.noeppi_noeppi.tools.sourcetransform.util.Bytecode
import io.github.noeppi_noeppi.tools.sourcetransform.util.cls.ClassLocator
import io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.{ClassInfo, FieldInfo, InheritanceMap, MethodInfo}
import org.objectweb.asm.{ClassReader, ClassVisitor}

import scala.collection.mutable
import scala.util.matching.Regex

class ClassInheritance(val locator: ClassLocator) {

  case class ClassEntry(parent: String, interfaces: Set[String], nest: Option[String], access: Int)
  
  private val classes = mutable.Map[String, ClassEntry]()
  private val sourceClasses = mutable.Set[String]()
  
  private val CLS_DESC: Regex = "L([^;]+);".r
  
  def addClass(cls: String): Unit = {
    if (cls != Bytecode.ROOT) {
      locator.findClass(cls) match {
        case Some(result) => addClass(result)
        case None =>
      }
    }
  }
  
  def addClass(cls: ClassReader): Unit = {
    if (cls.getClassName != Bytecode.ROOT && !classes.contains(cls.getClassName)) {
      var nest: Option[String] = None
      cls.accept(new ClassVisitor(Bytecode.TARGET) {
        override def visitOuterClass(owner: String, name: String, descriptor: String): Unit = {
          super.visitOuterClass(owner, name, descriptor)
          nest = Some(owner)
        }
      }, ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
      val entry = ClassEntry(
        if cls.getSuperName == null then Bytecode.ROOT else cls.getSuperName,
        cls.getInterfaces.toSet, nest, cls.getAccess
      )
      classes.put(cls.getClassName, entry)
      addClass(entry.parent)
      entry.interfaces.foreach(iface => addClass(iface))
    }
  }
  
  def addDescriptor(desc: String): Unit = {
    CLS_DESC.findAllMatchIn(desc).foreach(m => addClass(m.group(1)))
  }
  
  def addSource(cls: String): Unit = sourceClasses.add(cls)
  
  def build(fields: String => Set[FieldInfo], methods: String => Set[MethodInfo]): InheritanceMap = {
    val builtClasses = classes.map(entry => {
      val (name, ClassEntry(parent, interfaces, nest, access)) = entry
      ClassInfo(
        name, access, sourceClasses.contains(name),
        parent, interfaces, nest,
        fields(name),
        methods(name)
      )
    })
    new InheritanceMap(builtClasses.toSet)
  }
}
