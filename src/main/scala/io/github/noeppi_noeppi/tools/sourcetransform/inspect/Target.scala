package io.github.noeppi_noeppi.tools.sourcetransform.inspect

import io.github.noeppi_noeppi.tools.sourcetransform.util.{CommonParsers, SourceUtil}
import org.eclipse.jdt.core.dom.Assignment.Operator
import org.eclipse.jdt.core.dom._

import scala.jdk.CollectionConverters._

sealed trait Target {
  
  def string(): String
  def findRead(inspector: Inspector, node: ASTNode): Option[ASTNode]
  def findUpdate(inspector: Inspector, node: ASTNode): Option[Expression]
}

object Target {

  def read(str: String): Target = Parsers.parseIt(Parsers.line, str).getOrElse(throw new IllegalStateException("Expression target expected, got empty string."))

  private object Parsers extends CommonParsers {

    // Param must be before field as field will also match param targets
    def line: Parser[Target] = param | field | failure("Expression target expected")

    def field: Parser[Target] = class_entry ~ "#" ~ ident ^^ { case cls ~ _ ~ name => Field(cls, name) }
    def param: Parser[Target] = class_entry ~ "#" ~ identm ~ msig ~ "@" ~ wholeNumber ^^ { case cls ~ _ ~ name ~ signature ~ _ ~ idx => Parameter(cls, name, signature, idx.toInt) }
  }
  
  case class Field(cls: String, name: String) extends Target {
    override def string(): String = cls + "#" + name
    override def findRead(inspector: Inspector, node: ASTNode): Option[ASTNode] = (node match {
      case x: FieldAccess => Option(x.resolveFieldBinding())
      case x: QualifiedName => Option(x.resolveBinding())
      case x: SimpleName => Option(x.resolveBinding())
      case _ => None
    }) match {
      case Some(binding: IVariableBinding) if binding.isField && binding.getDeclaringClass != null
        && binding.getDeclaringClass.getBinaryName == cls && binding.getName == name =>
        Some(node)
      case _ => None
    }
    override def findUpdate(inspector: Inspector, node: ASTNode): Option[Expression] = node match {
      case x: FieldDeclaration => x.fragments().asScala.flatMap {
        case e: VariableDeclaration => Some(e)
        case _ => None
      }.flatMap(e => findRead(inspector, e.getName).flatMap(_ => Option(e.getInitializer))).headOption
      case x: Assignment if x.getOperator == Operator.ASSIGN =>
        findRead(inspector, x.getLeftHandSide).map(_ => x.getRightHandSide)
      case _ => None
    }
  }
  
  case class Parameter(cls: String, name: String, signature: String, idx: Int) extends Target {
    override def string(): String = cls + "#" + name + signature + "@" + idx
    override def findRead(inspector: Inspector, node: ASTNode): Option[ASTNode] = node match {
      case x: SimpleName => Option(x.resolveBinding()) match {
        case Some(b: IVariableBinding) if b.isParameter && b.getDeclaringMethod != null
          && b.getDeclaringMethod.getDeclaringClass != null && b.getDeclaringMethod.getName == name
          && inspector.inheritance.isSubType(SourceUtil.internal(b.getDeclaringMethod.getDeclaringClass.getBinaryName), "L" + cls + ";")
          && signature.startsWith(b.getDeclaringMethod.getParameterTypes.map(_.getBinaryName).map(SourceUtil.internal).mkString("(", "", ")"))
          && inspector.findParamIdx(b).contains(idx) =>
          Some(node)
        case _ => None
      }
      case _ => None
    }
    override def findUpdate(inspector: Inspector, node: ASTNode): Option[Expression] = (node match {
      case x: ConstructorInvocation if name == "<init>" => Option(x.arguments().asScala, x.resolveConstructorBinding())
      case x: MethodInvocation if name != "<init>" => Option(x.arguments().asScala, x.resolveMethodBinding())
      case _ => None
    }) match {
      case Some((args, b)) if b.getDeclaringClass != null && (name == "<init>" || name == b.getName)
        && inspector.inheritance.isSubType(SourceUtil.internal(b.getDeclaringClass.getBinaryName), "L" + cls + ";")
        && signature.startsWith(b.getParameterTypes.map(_.getBinaryName).map(SourceUtil.internal).mkString("(", "", ")")) =>
        if (args != null && args.indices.contains(idx)) args(idx) match {
          case expr: Expression => Some(expr)
          case _ => None
        } else {
          None
        }
      case _ => None
    }
  }
}



