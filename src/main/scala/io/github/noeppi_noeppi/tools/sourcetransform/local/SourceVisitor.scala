package io.github.noeppi_noeppi.tools.sourcetransform.local

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.InheritanceMap
import io.github.noeppi_noeppi.tools.sourcetransform.transform.{ConfiguredTransformer, TransformTarget, TransformUtil}
import io.github.noeppi_noeppi.tools.sourcetransform.util.SourceUtil
import org.eclipse.jdt.core.dom._

import scala.collection.mutable

class SourceVisitor(private val inheritance: InheritanceMap, transformers: List[ConfiguredTransformer], renames: mutable.Set[(Int, Int, String)]) extends ASTVisitor {
  
  private val outerClassElems = mutable.Stack[String]()
  private val innerClassElems = mutable.Stack[String]()
  // Not perfect but it hopefully works in most cases
  private var anonymousIndex = 1
  private val methodStack = mutable.Stack[(String, String)]()
  private val params = mutable.Map[String, String]()

  private def currentClass() = outerClassElems.reverse.mkString("/") + innerClassElems.reverse.map("$" + _).mkString("")
  
  private val transform = TransformUtil.createTransformer(transformers) _

  private def checkUtilityType(cls: String, forType: String): Boolean = {
    val members = inheritance.getMemberSignatures(cls)
    val memberCount = members.count(m => m.contains(forType))
    (members.size / memberCount.toDouble) <= 5
  }

  private def checkClassFor(cls: String, forTypes: Set[String]): Boolean = {
    forTypes.exists(forType => inheritance.isSubType("L" + cls + ";", forType) || checkUtilityType(cls, forType))
  }
  
  private def rename(key: String, name: String, variableType: String): String = {
    params.get(key) match {
      case Some(renamed) => renamed
      case None =>
        transform(name, TransformTarget.LOCAL, transformer => {
          if (methodStack.nonEmpty && !transformer.matchBaseMethod(methodStack.head._1, methodStack.head._2))  {
            false
          } else if (variableType != null && !transformer.matchTypeSignature(inheritance, variableType)) {
            false
          } else if (variableType == null && !transformer.matchBaseClass(inheritance, currentClass()) && !checkClassFor(currentClass(), transformer.baseType)) {
            false
          } else {
            true
          }
        }, _ => true) match {
          case Some(renamed) => params.put(key, renamed); renamed
          case None => params.put(key, name); name
        }
    }
  }


  override def visit(node: CompilationUnit): Boolean = {
    outerClassElems.clear()
    outerClassElems.addAll(node.getPackage.getName.getFullyQualifiedName.split("\\.").reverse)
    innerClassElems.clear()
    anonymousIndex = 1
    true
  }
  
  override def endVisit(node: CompilationUnit): Unit = {
    outerClassElems.clear()
    innerClassElems.clear()
    anonymousIndex = 1
    params.clear()
  }
  
  override def visit(node: TypeDeclaration): Boolean = startType(node)
  override def endVisit(node: TypeDeclaration): Unit = endType(node)
  override def visit(node: AnnotationTypeDeclaration): Boolean = startType(node)
  override def endVisit(node: AnnotationTypeDeclaration): Unit = endType(node)
  override def visit(node: EnumDeclaration): Boolean = startType(node)
  override def endVisit(node: EnumDeclaration): Unit = endType(node)
  
  private def startType(node: AbstractTypeDeclaration): Boolean = {
    if (node.isMemberTypeDeclaration) {
      innerClassElems.push(node.getName.getIdentifier)
    } else {
      outerClassElems.push(node.getName.getIdentifier)
      innerClassElems.clear()
      anonymousIndex = 1
    }
    true
  }
  
  private def endType(node: AbstractTypeDeclaration): Unit = {
    if (node.isMemberTypeDeclaration) {
      innerClassElems.pop()
    } else {
      outerClassElems.pop()
      anonymousIndex = 1
    }
  }

  override def visit(node: AnonymousClassDeclaration): Boolean = {
    innerClassElems.push("$" + anonymousIndex)
    anonymousIndex += 1
    true
  }

  override def endVisit(node: AnonymousClassDeclaration): Unit = {
    innerClassElems.pop()
  }

  override def visit(node: MethodDeclaration): Boolean = {
    val binding = node.resolveBinding()
    if (binding == null) {
      System.err.println("Failed to resolve method binding: Skipping method: " + node.getName.getFullyQualifiedName + " (in " + currentClass() + ")")
      // The stack is popped in the endVisit method. So we need to push a dummy element
      // even if we skip the method.
      methodStack.push(("<invalid>", "()V"))
      false
    } else {
      val name = if (node.isConstructor) "<init>" else node.getName.getIdentifier
      val ret = if (node.isConstructor) "V" else SourceUtil.internal(binding.getReturnType.getBinaryName)
      val desc = "(" + binding.getParameterTypes.map(str => SourceUtil.internal(str.getBinaryName)).mkString("") + ")" + ret
      methodStack.push((name, desc))
      true
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
    if (methodStack.nonEmpty) {
      node.resolveBinding() match {
        case binding: IVariableBinding if binding.getKind == IBinding.VARIABLE && !binding.isField
          && !binding.isParameter && !binding.isEnumConstant && !binding.isRecordComponent =>
          val typeBinding = Option(node.resolveTypeBinding())
          val renamed = rename(binding.getKey, node.getIdentifier, typeBinding.map(str => SourceUtil.internal(str.getBinaryName)).orNull)
          if (renamed != node.getIdentifier) renames.addOne((node.getStartPosition, node.getLength, renamed))
        case null => System.err.println("Failed to resolve name binding: " + node.getFullyQualifiedName + " (in " + currentClass() + "#" + methodStack.head._1 + methodStack.head._2 + ")")
        case _ =>
      }
    }
    true
  }
  
  override def visit(node: SingleVariableDeclaration): Boolean = variableDeclaration(node)
  override def visit(node: VariableDeclarationFragment): Boolean = variableDeclaration(node)

  private def variableDeclaration(node: VariableDeclaration): Boolean = {
    if (methodStack.nonEmpty) {
      val binding = node.resolveBinding()
      if (binding == null) {
        System.err.println("Failed to resolve variable binding: " + node.getName.getFullyQualifiedName + " (in " + currentClass() + "#" + methodStack.head._1 + methodStack.head._2 + ")")
      } else if (binding.getKind == IBinding.VARIABLE && !binding.isField && !binding.isParameter
        && !binding.isEnumConstant && !binding.isRecordComponent) {
        val typeBinding = Option(node.getName.resolveTypeBinding())
        val renamed = rename(binding.getKey, node.getName.getIdentifier, typeBinding.map(str => SourceUtil.internal(str.getBinaryName)).orNull)
        if (renamed != node.getName.getIdentifier) renames.addOne((node.getName.getStartPosition, node.getName.getLength, renamed))
      }
    }
    // Don't descend into it, we already have it renamed
    false
  }
}
