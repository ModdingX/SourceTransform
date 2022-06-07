package org.moddingx.sourcetransform.util.inheritance.extract

import org.moddingx.sourcetransform.util.Bytecode
import org.moddingx.sourcetransform.util.cls.ClassLocator
import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class OverrideMatcher(overridingMethod: Bytecode.Method, methods: MethodCollector) {

  private val targets = mutable.Set[(String, String)]()
  private val visitedClasses = mutable.Set[String](overridingMethod.cls)
  
  // Interfaces need to be processed after classes
  private val interfaceList = ListBuffer[String]()
  private var processingInterfaces = false
  
  def addTarget(method: Bytecode.Method): Unit = {
    if (method.cls != overridingMethod.cls) {
      throw new IllegalArgumentException("Can'T add synthetic override target from different class: " + method.cls + " != " + overridingMethod.cls)
    }
    targets.addOne((method.name, method.desc))
  }
  
  def findOverrides(): Unit = {
    if (targets.nonEmpty) {
      methods.locator.findClass(overridingMethod.cls) match {
        case Some(reader) =>
          findOverridesInParents(reader)
          processingInterfaces = true
          for (iface <- interfaceList) findOverridesIn(iface)
        case None => System.err.println("Failed to find class of method to look for overrides: " + overridingMethod + " with " + targets.mkString("[ ", ", ", " ]"))
      }
    }
  }
  
  private def findOverridesIn(cls: String): Unit = {
    if (!visitedClasses.contains(cls)) {
      visitedClasses.add(cls)
      methods.locator.findClass(cls) match {
        case Some(reader) =>
          var hasFound = false
          reader.accept(new ClassVisitor(Bytecode.TARGET) {
            override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor = {
              super.visitMethod(access, name, descriptor, signature, exceptions)
              if (targets.exists(entry => {
                val (targetName, targetDesc) = entry
                name == targetName && descriptor == targetDesc
              })) {
                hasFound = true
                methods.addOverride(Bytecode.Method(reader.getClassName, name, descriptor), overridingMethod)
              }
              null
            }
          }, ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
          if (!hasFound) {
            findOverridesInParents(reader)
          }
        case None =>
      }
    }
  }
  
  private def findOverridesInParents(cls: ClassReader): Unit = {
    if (processingInterfaces) {
      // Just search the super-interfaces
      cls.getInterfaces.foreach(iface => findOverridesIn(iface))
    } else {
      findOverridesIn(if cls.getSuperName == null then Bytecode.ROOT else cls.getSuperName)
      cls.getInterfaces.foreach(iface => {
        if (!interfaceList.contains(iface)) {
          interfaceList.addOne(iface)
        }
      })
    }
  }
}
