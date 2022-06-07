package org.moddingx.sourcetransform.local

import org.moddingx.sourcetransform.transform.{ConfiguredTransformer, TransformUtil}
import org.moddingx.sourcetransform.transform.data.TransformTarget
import org.moddingx.sourcetransform.util.{Bytecode, ClassTrackingVisitor, SourceUtil}
import org.moddingx.sourcetransform.util.inheritance.InheritanceMap
import org.eclipse.jdt.core.dom.{ASTNode, IBinding, ITypeBinding, IVariableBinding, Initializer, MethodDeclaration, Modifier, SimpleName, SingleVariableDeclaration, VariableDeclaration, VariableDeclarationFragment}

import scala.collection.mutable

class SourceVisitor(inheritance: InheritanceMap, transform: TransformUtil.AppliedTransformer, renames: mutable.Set[(Int, Int, String)]) extends ClassTrackingVisitor {

  private val methodStack = mutable.Stack[(String, String)]()
  private val renamedParams = mutable.Map[String, String]()
  
  def renameNode(node: ASTNode, binding: IBinding, typeBinding: => ITypeBinding, name: String): Unit = {
    if (methodStack.nonEmpty) {
      binding match {
        case binding: IVariableBinding if binding.getKind == IBinding.VARIABLE && !binding.isField && !binding.isParameter && !binding.isEnumConstant && !binding.isRecordComponent =>
          val renamed = rename(binding.getKey, name, Option(typeBinding).flatMap(bind => SourceUtil.internal(bind)))
          if (renamed != name) {
            renames.addOne((node.getStartPosition, node.getLength, renamed))
          }
        case _ =>
      }
    }
  }
  
  def rename(key: String, name: String, variableType: Option[String]): String = {
    renamedParams.get(key) match {
      case Some(renamed) => renamed
      case None =>
        transform(
          name, TransformTarget.LOCAL,
          ct => checkTransformer(ct, methodStack.headOption, variableType),
          _ => true
        ) match {
          case Some(renamed) =>
            renamedParams.put(key, renamed)
            renamed
          case None =>
            renamedParams.put(key, name)
            name
        }
    }
  }
  
  def checkTransformer(ct: ConfiguredTransformer, currentMethod: Option[(String, String)], variableType: Option[String]): Boolean = {
    currentMethod match {
      case Some((name, desc)) if !ct.matchBaseMethod(name, desc) => return false
      case _ =>
    }
    variableType match {
      case Some(vType) => ct.matchTypeDescriptor(inheritance, vType)
      case None => ct.matchBaseClass(inheritance, currentClass()) || transform.isClassRelatedTo(currentClass(), ct.baseTypes)
    }
  }

  override def visit(node: MethodDeclaration): Boolean = {
    SourceUtil.methodInfo(node.resolveBinding()) match {
      case Left(Bytecode.Method(_, name, desc)) =>
        methodStack.push((name, desc))
        true
      case Right(err) =>
        System.err.println(err + ": " + node.getName.getFullyQualifiedName + " (in " + currentClass() + ")")
        // Dummy element to be popped in endVisit
        methodStack.push(("<invalid>", "()V"))
        false
    }
  }

  override def endVisit(node: MethodDeclaration): Unit = {
    methodStack.pop()
  }

  override def visit(node: Initializer): Boolean = {
    if ((node.getModifiers & Modifier.STATIC) != 0) {
      methodStack.push(("<clinit>", "()V"))
    }
    true
  }

  override def endVisit(node: Initializer): Unit = {
    if ((node.getModifiers & Modifier.STATIC) != 0) {
      methodStack.pop()
    }
  }

  override def visit(node: SimpleName): Boolean = {
    renameNode(node, node.resolveBinding(), node.resolveTypeBinding(), node.getIdentifier)
    true
  }


  override def visit(node: SingleVariableDeclaration): Boolean = variableDeclaration(node)
  override def visit(node: VariableDeclarationFragment): Boolean = variableDeclaration(node)

  private def variableDeclaration(node: VariableDeclaration): Boolean = {
    renameNode(node.getName, node.resolveBinding(), node.getName.resolveTypeBinding(), node.getName.getIdentifier)
    // Don't visit children, already renamed
    false
  }
}
