package io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.extract.visitor

import io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.extract.MethodCollector
import io.github.noeppi_noeppi.tools.sourcetransform.util.{Bytecode, ParamIndexMapper}
import org.objectweb.asm.{Handle, Label, MethodVisitor, Opcodes, Type}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class LambdaVisitor(method: Bytecode.Method, lambdaIdx: AtomicInteger, methods: MethodCollector, parent: MethodVisitor) extends MethodVisitor(Bytecode.TARGET, parent) {
  
  override def visitInvokeDynamicInsn(name: String, descriptor: String, bootstrapMethodHandle: Handle, bootstrapMethodArguments: Any*): Unit = {
    super.visitInvokeDynamicInsn(name, descriptor, bootstrapMethodHandle, bootstrapMethodArguments: _*)
    if (bootstrapMethodHandle.getOwner == "java/lang/invoke/LambdaMetafactory" && bootstrapMethodHandle.getName == "metafactory") {
      val lambdaId = "lambda$" + lambdaIdx.getAndIncrement()
      val implementedType = Type.getMethodType(descriptor).getReturnType
      if (implementedType.getSort == Type.OBJECT) {
        val implementedCls = implementedType.getInternalName
        (if bootstrapMethodArguments.size >= 2 then Some(bootstrapMethodArguments(1)) else None) match {
          case Some(handle: Handle) => methods.addLambda(method.cls, lambdaId, handle.getOwner, handle.getName, handle.getDesc, implementedCls)
          case _ => System.err.println("Failed to resolve method handle for lambda " + lambdaId + "@" + name + descriptor)
        }
      }
    }
  }
}

object LambdaVisitor {

  def create(method: Bytecode.Method, lambdaIdx: AtomicInteger, methods: MethodCollector): MethodVisitor => LambdaVisitor = {
    mv => new LambdaVisitor(method, lambdaIdx, methods, mv)
  }
}
