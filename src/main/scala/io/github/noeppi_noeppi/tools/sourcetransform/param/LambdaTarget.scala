package io.github.noeppi_noeppi.tools.sourcetransform.param

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.MethodInfo

sealed trait LambdaTarget {

  def resolve(factory: MethodInfo => ParamSanitizer): Option[ParamSanitizer]
}

object LambdaTarget {
  
  def forMethod(info: MethodInfo): LambdaTarget = if (info == null) Skip else Method(info)
  
  case class Method(info: MethodInfo) extends LambdaTarget {
    override def resolve(factory: MethodInfo => ParamSanitizer): Option[ParamSanitizer] = Some(factory(info))
  }
  
  case object Keep extends LambdaTarget {
    override def resolve(factory: MethodInfo => ParamSanitizer): Option[ParamSanitizer] = Some(ParamSanitizer.NONE)
  }
  
  case object Skip extends LambdaTarget {
    override def resolve(factory: MethodInfo => ParamSanitizer): Option[ParamSanitizer] = None
  }
}
