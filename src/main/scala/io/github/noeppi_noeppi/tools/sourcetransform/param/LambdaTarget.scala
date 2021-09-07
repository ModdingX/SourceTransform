package io.github.noeppi_noeppi.tools.sourcetransform.param

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.MethodInfo

sealed trait LambdaTarget {

  def lambdaName(): Option[String]
  def canMatch(lambdaMethod: String): Boolean
  def resolve(factory: MethodInfo => ParamSanitizer): Option[ParamSanitizer]
}

object LambdaTarget {
  
  def forMethod(info: MethodInfo): LambdaTarget = if (info == null) Skip else Method(info)
  
  case class Method(info: MethodInfo) extends LambdaTarget {
    
    private val lambdaId = if (info.name == "<init>") {
        "new"
      } else if (info.name == "<clinit>") {
        "static"
      } else {
        info.name
      }
    
    override def lambdaName(): Option[String] = Some("lambda$" + lambdaId + "$")
    override def canMatch(lambdaMethod: String): Boolean = lambdaMethod.startsWith("lambda$" + lambdaId + "$")
    override def resolve(factory: MethodInfo => ParamSanitizer): Option[ParamSanitizer] = Some(factory(info))
  }
  
  case object Keep extends LambdaTarget {
    override def lambdaName(): Option[String] = None
    override def canMatch(lambdaMethod: String): Boolean = true
    override def resolve(factory: MethodInfo => ParamSanitizer): Option[ParamSanitizer] = Some(ParamSanitizer.NONE)
  }
  
  case object Skip extends LambdaTarget {
    override def lambdaName(): Option[String] = None
    override def canMatch(lambdaMethod: String): Boolean = false
    override def resolve(factory: MethodInfo => ParamSanitizer): Option[ParamSanitizer] = None
  }
}
