package org.moddingx.sourcetransform.util.inheritance.extract.visitor

import org.moddingx.sourcetransform.util.{Bytecode, Util}
import org.moddingx.sourcetransform.util.inheritance.extract.{ClassInheritance, FieldCollector, MethodCollector, OverrideMatcher}
import org.objectweb.asm.{ClassVisitor, FieldVisitor, MethodVisitor}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class InheritanceVisitor(
                          cls: String,
                          classes: ClassInheritance,
                          fields: FieldCollector,
                          methods: MethodCollector
                        ) extends ClassVisitor(Bytecode.TARGET) {

  private val lambdaIdx = new AtomicInteger(0)
  private val overrideMap = mutable.Map[Bytecode.Method, OverrideMatcher]()

  override def visitField(access: Int, name: String, descriptor: String, signature: String, value: Any): FieldVisitor = {
    super.visitField(access, name, descriptor, signature, value)
    fields.addField(cls, name, descriptor, access)
    null
  }

  override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor = {
    super.visitMethod(access, name, descriptor, signature, exceptions)
    methods.addMethod(cls, name, descriptor, access)
    val current = Bytecode.Method(cls, name, descriptor)
    Util.chain(new MethodVisitor(Bytecode.TARGET) {},
      ParamVisitor.create(current, access, methods),
      LambdaVisitor.create(current, lambdaIdx, methods),
      OverrideVisitor.create(current, access, md => getOverrideMatcher(md))
    )
  }

  override def visitEnd(): Unit = {
    super.visitEnd()
    for (matcher <- overrideMap.values) {
      matcher.findOverrides()
    }
  }

  private def getOverrideMatcher(method: Bytecode.Method): OverrideMatcher = {
    if (cls != method.cls) {
      throw new IllegalArgumentException("Override matcher for method out of class scope: " + method)
    }
    overrideMap.getOrElseUpdate(method, new OverrideMatcher(method, methods))
  }
}
