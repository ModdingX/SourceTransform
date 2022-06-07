package org.moddingx.sourcetransform.parchment

import org.moddingx.sourcetransform.util.Bytecode

sealed trait LambdaTarget {

  def lambdaName(): Option[String]
  def canMatch(lambdaMethod: String): Boolean
  def resolve(renameResolver: Bytecode.Method => ParamRenamer): Option[ParamRenamer]
}

object LambdaTarget {
  
  case class Method(method: Bytecode.Method) extends LambdaTarget {
    
    private val lambdaId = method.name match {
      case "<init>" => "new"
      case "<clinit>" => "static"
      case _ => method.name
    }
    
    override def lambdaName(): Option[String] = Some("lambda$" + lambdaId + "$")
    override def canMatch(lambdaMethod: String): Boolean = lambdaMethod.startsWith("lambda$" + lambdaId + "$")
    def resolve(renameResolver: Bytecode.Method => ParamRenamer): Option[ParamRenamer] = Some(renameResolver(method))
  }
  
  case object Keep extends LambdaTarget {
    override def lambdaName(): Option[String] = None
    override def canMatch(lambdaMethod: String): Boolean = true
    def resolve(renameResolver: Bytecode.Method => ParamRenamer): Option[ParamRenamer] = Some(ParamRenamer.Keep)
  }
  
  case object Skip extends LambdaTarget {
    override def lambdaName(): Option[String] = None
    override def canMatch(lambdaMethod: String): Boolean = false
    def resolve(renameResolver: Bytecode.Method => ParamRenamer): Option[ParamRenamer] = None
  }
}
