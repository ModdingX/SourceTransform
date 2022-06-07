package org.moddingx.sourcetransform.util.inheritance.extract.visitor

import org.moddingx.sourcetransform.util.inheritance.extract.{MethodCollector, OverrideMatcher}
import org.moddingx.sourcetransform.util.{Bytecode, ParamIndexMapper}
import org.objectweb.asm.*

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.collection.mutable

class OverrideVisitor(method: Bytecode.Method, methodAccess: Int, resolver: Bytecode.Method => OverrideMatcher, parent: MethodVisitor) extends MethodVisitor(Bytecode.TARGET, parent) {
  
  private var failedSynthetic: Boolean = false
  private var currentSyntheticTarget: Option[Bytecode.Method] = None
  
  override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean): Unit = {
    super.visitMethodInsn(opcode, owner, name, descriptor, isInterface)
    if (!failedSynthetic && (methodAccess & Opcodes.ACC_SYNTHETIC) != 0 && method.cls == owner && method.name == name) {
      if (currentSyntheticTarget.isDefined) {
        failedSynthetic = true
        currentSyntheticTarget = None
      } else {
        currentSyntheticTarget = Some(Bytecode.Method(owner, name, descriptor))
      }
    }
  }

  override def visitEnd(): Unit = {
    super.visitEnd()
    resolver(method).addTarget(method)
    currentSyntheticTarget match {
      case Some(target) => resolver(target).addTarget(method)
      case None =>
    }
  }
}

object OverrideVisitor {

  def create(method: Bytecode.Method, methodAccess: Int, resolver: Bytecode.Method => OverrideMatcher): MethodVisitor => MethodVisitor = {
    if ((methodAccess & Opcodes.ACC_STATIC) == 0 && method.name != "<init>") {
      mv => new OverrideVisitor(method, methodAccess, resolver, mv)
    } else {
      identity
    }
  }
}
