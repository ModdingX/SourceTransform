package io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.extract.visitor

import io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.extract.MethodCollector
import io.github.noeppi_noeppi.tools.sourcetransform.util.{Bytecode, ParamIndexMapper}
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

import scala.collection.mutable

class ParamVisitor(method: Bytecode.Method, methodAccess: Int, methods: MethodCollector, parent: MethodVisitor) extends MethodVisitor(Bytecode.TARGET, parent) {
  
  private val visited = mutable.Set[Int]()
  
  override def visitLocalVariable(name: String, descriptor: String, signature: String, start: Label, end: Label, index: Int): Unit = {
    super.visitLocalVariable(name, descriptor, signature, start, end, index)
    methods.classInheritance.addDescriptor(descriptor)
    ParamIndexMapper.lvtToIdx(methodAccess, method.name, method.desc, index) match {
      case Some(idx) if !visited.contains(idx) =>
        visited.addOne(idx)
        val cleanedParameter = if (name.nonEmpty && Character.isJavaIdentifierStart(name.head) && name.tail.forall(Character.isJavaIdentifierPart)) {
          name
        } else {
          "o" + idx
        }
        methods.addParam(method.cls, method.name, method.desc, idx, cleanedParameter, descriptor)
      case _ =>
    }
  }
}

object ParamVisitor {
  
  def create(method: Bytecode.Method, methodAccess: Int, methods: MethodCollector): MethodVisitor => ParamVisitor = {
    mv => new ParamVisitor(method, methodAccess, methods, mv)
  }
}
