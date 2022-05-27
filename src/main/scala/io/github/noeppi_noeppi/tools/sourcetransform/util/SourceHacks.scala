package io.github.noeppi_noeppi.tools.sourcetransform.util

import org.eclipse.jdt.core.dom.IMethodBinding

// Hacky reflection to get more properties from JDT
// Likely to break at some point
// Should only be used from SourceUtil
object SourceHacks {

  def getBinaryDescriptor(binding: IMethodBinding): Option[String] = {
    if (binding == null) return None
    try {
      val internalCls = Class.forName("org.eclipse.jdt.core.dom.MethodBinding")
      if (!internalCls.isAssignableFrom(binding.getClass)) return None
      val field = internalCls.getDeclaredField("binding")
      field.setAccessible(true)
      val binding2 = field.get(binding)
      val internalCls2 = Class.forName("org.eclipse.jdt.internal.compiler.lookup.MethodBinding")
      if (!internalCls2.isAssignableFrom(binding2.getClass)) return None
      val field2 = internalCls2.getDeclaredField("signature")
      field2.setAccessible(true)
      val signature = field2.get(binding2).asInstanceOf[Array[Char]]
      Some(new String(signature))
    } catch {
      case _: ReflectiveOperationException | _: NoClassDefFoundError => None
    }
  }
  
  // Should be lambda$n with n = unique id for the lambda that (hopefully) matches bytecode
  // However the compiler seems to merge identical lambdas together into one lambda method.
  // So this id needs to be matched with an inheritance map to find the correct method to use
  def getLambdaImplId(binding: IMethodBinding): Option[SourceUtil.Lambda] = {
    if (binding == null) return None
    // Hacky way to match lambdas
    try {
      val internalCls = Class.forName("org.eclipse.jdt.core.dom.MethodBinding$LambdaMethod")
      if (!internalCls.isAssignableFrom(binding.getClass)) return None
      val field = internalCls.getDeclaredField("implementation")
      field.setAccessible(true)
      val impl = field.get(binding).asInstanceOf[IMethodBinding]
      Some(SourceUtil.Lambda(impl.getDeclaringClass.getBinaryName.replace('.', '/'), impl.getName))
    } catch {
      case _: ReflectiveOperationException | _: NoClassDefFoundError => None
    }
  }
}
