package org.moddingx.sourcetransform.util

import org.eclipse.jdt.core.dom._

import scala.collection.mutable

class ClassTrackingVisitor extends ASTVisitor {

  private val outerClassElems = mutable.Stack[String]()
  private val innerClassElems = mutable.Stack[String]()
  // Not perfect but it hopefully works in most cases
  private var anonymousIndex = 1

  //noinspection ReverseMap
  protected def currentClass(): String = outerClassElems.reverse.mkString("/") + innerClassElems.reverse.map("$" + _).mkString("")
  
  override def preVisit2(node: ASTNode): Boolean = {
    node match {
      case n: CompilationUnit =>
        outerClassElems.clear()
        if (n.getPackage != null) outerClassElems.addAll(n.getPackage.getName.getFullyQualifiedName.split("\\.").reverse)
        innerClassElems.clear()
      case n: AbstractTypeDeclaration =>
        if (n.isMemberTypeDeclaration) {
          innerClassElems.push(n.getName.getIdentifier)
        } else if (n.isLocalTypeDeclaration) {
          innerClassElems.push(anonymousIndex.toString + n.getName.getIdentifier)
          anonymousIndex += 1
        } else {
          outerClassElems.push(n.getName.getIdentifier)
          innerClassElems.clear()
          anonymousIndex = 1
        }
      case _: AnonymousClassDeclaration =>
        innerClassElems.push(anonymousIndex.toString)
        anonymousIndex += 1
      case _ =>
    }
    true
  }

  override def postVisit(node: ASTNode): Unit = {
    node match {
      case _: CompilationUnit =>
        outerClassElems.clear()
        innerClassElems.clear()
        anonymousIndex = 1
      case n: AbstractTypeDeclaration =>
        if (n.isMemberTypeDeclaration || n.isLocalTypeDeclaration) {
          innerClassElems.pop()
        } else {
          outerClassElems.pop()
          anonymousIndex = 1
        }
      case _: AnonymousClassDeclaration =>
        innerClassElems.pop()
      case _ =>
    }
  }
}
