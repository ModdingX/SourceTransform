package org.moddingx.sourcetransform.util

import net.minecraftforge.srgutils.IMappingFile
import org.objectweb.asm.{Opcodes, Type}

import scala.util.matching.Regex

object Bytecode {

  val TARGET: Int = Opcodes.ASM9
  val ROOT: String = "java/lang/Object"

  def paramCount(desc: String): Int = Type.getMethodType(desc).getArgumentTypes.length
  def simplifiedDescriptorParams(desc: String): String = {
    def simplify(t: Type) = t.getSort match {
      case Type.ARRAY => "["
      case Type.OBJECT => "L"
      case _ => t.getDescriptor.headOption.map("" + _).getOrElse("L")
    }
    Type.getMethodType(desc).getArgumentTypes.map(simplify).mkString
  }

  def returnCls(desc: String): Option[String] = {
    Some(Type.getType(desc))
      .filter(_.getSort == Type.METHOD)
      .map(_.getReturnType)
      .filter(_.getSort == Type.OBJECT)
      .map(_.getInternalName)
  }

  sealed abstract class Content[T <: Content[T]](_cls: String, _name: String) {
    def remap(mappings: IMappingFile): T
    def cls: String = _cls
    def name: String = _name
  }
  
  sealed trait Member(cls: String, name: String) extends Content[?]
  
  object Member {
    def unapply(member: Member): Option[(String, String)] = Some((member.cls, member.name))
  }

  case class Field(override val cls: String, override val name: String) extends Content[Field](cls, name) with Member(cls, name) {
    def remap(mappings: IMappingFile): Field = mappings.getClass(cls) match {
      case null => this
      case cls => Field(cls.getMapped, cls.remapField(name))
    }
  }
  
  object Field {
    given Ordering[Field] = Ordering.by((fd: Field) => (fd.cls, fd.name))
  }

  case class Method(override val cls: String, override val name: String, desc: String) extends Content[Method](cls, name) with Member(cls, name) {
    def remap(mappings: IMappingFile): Method = mappings.getClass(cls) match {
      case null => this
      case cls => Method(cls.getMapped, cls.remapMethod(name, desc), mappings.remapDescriptor(desc))
    }
  }
  
  object Method {
    given Ordering[Method] = Ordering.by((md: Method) => (md.cls, md.name, md.desc))
  }

  // Not lambda implementation methods but lambda ids.
  // lambdaId is typically = lambda$n with a positive int n.
  // The implementing method may be named differently, this is used to identify them uniquely instead of by implementing method.
  case class Lambda(override val cls: String, lambdaId: String, implementedCls: String) extends Content[Lambda](cls, lambdaId) {
    def remap(mappings: IMappingFile): Lambda = Lambda(mappings.remapClass(cls), lambdaId, mappings.remapClass(implementedCls))
  }
  
  object Lambda {
    given Ordering[Lambda] = Ordering.by((lambda: Lambda) => (lambda.cls, lambda.name, lambda.implementedCls))
  }
}
