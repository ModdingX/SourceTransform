package org.moddingx.sourcetransform.util.inheritance.extract

import org.moddingx.sourcetransform.util.Bytecode
import org.moddingx.sourcetransform.util.cls.ClassLocator
import org.moddingx.sourcetransform.util.inheritance.MethodInfo

import scala.collection.mutable

class MethodCollector(val classInheritance: ClassInheritance) {

  val locator: ClassLocator = classInheritance.locator
  
  case class MethodEntry(access: Int)

  private val methods = mutable.Map[Bytecode.Method, MethodEntry]()
  private val params = mutable.Map[Bytecode.Method, mutable.Map[Int, String]]()
  private val overrides = mutable.Map[Bytecode.Method, mutable.Set[Bytecode.Method]]()
  private val lambdas = mutable.Map[Bytecode.Method, mutable.Set[Bytecode.Lambda]]()

  def addMethod(cls: String, name: String, desc: String, access: Int): Unit = {
    classInheritance.addClass(cls)
    classInheritance.addDescriptor(desc)
    methods.put(Bytecode.Method(cls, name, desc), MethodEntry(access))
  }
  
  def addParam(cls: String, name: String, desc: String, idx: Int, paramName: String, paramDesc: String): Unit = {
    classInheritance.addClass(cls)
    classInheritance.addDescriptor(paramDesc)
    params.getOrElseUpdate(Bytecode.Method(cls, name, desc), mutable.Map()).put(idx, paramName)
  }

  def addOverride(base: Bytecode.Method, overridingMethod: Bytecode.Method): Unit = {
    classInheritance.addClass(base.cls)
    classInheritance.addClass(overridingMethod.cls)
    overrides.getOrElseUpdate(overridingMethod, mutable.Set()).add(base)
  }
  
  def addLambda(cls: String, lambdaId: String, implCls: String, implName: String, implDesc: String, implementedCls: String): Unit = {
    classInheritance.addClass(cls)
    classInheritance.addClass(implCls)
    classInheritance.addDescriptor(implDesc)
    classInheritance.addClass(implementedCls)
    lambdas.getOrElseUpdate(Bytecode.Method(implCls, implName, implDesc), mutable.Set()).add(Bytecode.Lambda(cls, lambdaId, implementedCls))
  }
  
  def build(cls: String): Set[MethodInfo] = {
    methods.filter(entry => entry._1.cls == cls).map(entry => {
      val (md, MethodEntry(access)) = entry
      MethodInfo(
        md.name, md.desc, access,
        params.getOrElse(md, Map()).toMap,
        overrides.getOrElse(md, Set()).toSet,
        lambdas.getOrElse(md, Set()).toSet
      )
    }).toSet
  }
}
