package io.github.noeppi_noeppi.tools.sourcetransform.param

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.{InheritanceMap, MethodInfo}
import io.github.noeppi_noeppi.tools.sourcetransform.util.{ClassTrackingVisitor, SourceUtil}
import org.eclipse.jdt.core.dom._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class SourceVisitor(val inheritance: InheritanceMap, val sanitizers: mutable.Map[MethodInfo, ParamSanitizer], val quiet: Boolean) extends ClassTrackingVisitor {
  
  private var localClassLevel = 0
  private val imports = mutable.Set[String]()
  private val nameScopes = mutable.Stack[mutable.Set[String]]()
  
  private def pushLocalClass(): Unit = localClassLevel += 1
  private def popLocalClass(): Unit = if (localClassLevel > 0) localClassLevel -= 1 else throw new IllegalStateException("Negative local class level.")
  
  private def pushScope(): Unit = nameScopes.push(mutable.Set())
  private def popScope(): Unit = if (nameScopes.nonEmpty) nameScopes.pop() else throw new IllegalStateException("Can't pop empty scope stack.")
  
  private def defineImport(name: String): Unit = imports.addOne(name)
  private def defineType(name: String): Unit = if (nameScopes.isEmpty) imports.addOne(name) else nameScopes.head.addOne(name)
  private def defineName(name: String): Unit = if (nameScopes.nonEmpty) nameScopes.head.addOne(name) else throw new IllegalStateException("Can't define name on empty scope stack: " + name)
  
  def checkEnd(): Unit = {
    if (localClassLevel != 0) throw new IllegalStateException("Invalid local class level.")
    if (nameScopes.nonEmpty) throw new IllegalStateException("Invalid open scope.")
  }
  
  private def buildSanitizer() = ParamSanitizer(imports.toSet | nameScopes.headOption.getOrElse(Set()), localClassLevel)
  
  private def startType(node: AbstractTypeDeclaration): Boolean = {
    if (node.isLocalTypeDeclaration) pushLocalClass()
    // Define name before push scope so it stays forbidden after the type ends
    defineType(node.getName.getIdentifier)
    true
  }
  
  private def endType(node: AbstractTypeDeclaration): Unit = {
    if (node.isLocalTypeDeclaration) popLocalClass()
  }


  override def visit(node: CompilationUnit): Boolean = { pushScope(); true }
  override def endVisit(node: CompilationUnit): Unit = popScope()

  override def visit(node: TypeDeclaration): Boolean = startType(node)
  override def endVisit(node: TypeDeclaration): Unit = endType(node)
  
  override def visit(node: AnnotationTypeDeclaration): Boolean = startType(node)
  override def endVisit(node: AnnotationTypeDeclaration): Unit = endType(node)

  override def visit(node: EnumDeclaration): Boolean = startType(node)
  override def endVisit(node: EnumDeclaration): Unit = endType(node)

  override def visit(node: RecordDeclaration): Boolean = startType(node)
  override def endVisit(node: RecordDeclaration): Unit = endType(node)
  
  override def visit(node: AnonymousClassDeclaration): Boolean = {
    pushLocalClass()
    true
  }
  override def endVisit(node: AnonymousClassDeclaration): Unit = {
    popLocalClass()
  }


  
  override def visit(node: ImportDeclaration): Boolean = {
    // Static imports are handled when they are accessed
    if (!node.isStatic) {
      if (node.isOnDemand) {
        node.resolveBinding() match {
          case null => warn("Failed to resolve binding: " + node)
          case b: IPackageBinding => SourceUtil.importedClassesFromPkg(inheritance, b.getName.replace('.', '/')).foreach(defineImport)
          case b: ITypeBinding if b.getBinaryName != null => SourceUtil.importedClassesFromCls(inheritance, b.getBinaryName.replace('.', '/')).foreach(defineImport)
          case b => warn("Failed to process import with binding: " + b)
        }
      } else {
        defineImport(SourceUtil.simplePart(node.getName))
      }
    }
    true
  }
  
  
  
  override def visit(node: Initializer): Boolean = {
    pushScope()
    true
  }
  override def endVisit(node: Initializer): Unit = {
    popScope()
  }
  
  override def visit(node: MethodDeclaration): Boolean = {
    pushScope()
    node.parameters().asScala.foreach {
      case p: VariableDeclaration => defineName(p.getName.getIdentifier)
      case p => warn("Failed to process method parameter: " + p)
    }
    true
  }
  override def endVisit(node: MethodDeclaration): Unit = {
    val sanitizer = buildSanitizer()
    popScope()
    val info = SourceUtil.methodInfo(node.resolveBinding())
    if (info == null) {
      warn("Failed to resolve method binding: Skipping method: " + node.getName.getFullyQualifiedName + " (in " + currentClass() + ")")
    } else {
      sanitizers.put(info, sanitizer)
    }
  }

  override def visit(node: LambdaExpression): Boolean = {
    pushLocalClass()
    // Lambdas live in the same scope
    node.parameters().asScala.foreach {
      case p: VariableDeclaration => defineName(p.getName.getIdentifier)
    }
    true
  }
  override def endVisit(node: LambdaExpression): Unit = {
    val sanitizer = buildSanitizer()
    // Lambdas live in the same scope
    popLocalClass()
    val info = SourceUtil.methodInfo(node.resolveMethodBinding())
    if (info == null) {
      warn("Failed to resolve lambda binding: Skipping lambda (in " + currentClass() + ")")
    } else {
      sanitizers.put(info, sanitizer)
    }
  }

  

  override def visit(node: FieldDeclaration): Boolean = {
    // Field declarations contain variable declaration fragments
    // that we don't want to process later
    false
  }
  override def visit(node: SingleVariableDeclaration): Boolean = {
    defineName(node.getName.getIdentifier)
    true
  }
  override def visit(node: VariableDeclarationExpression): Boolean = {
    node.fragments().asScala.foreach {
      case e: VariableDeclarationFragment => defineName(e.getName.getIdentifier)
    }
    true
  }
  override def visit(node: VariableDeclarationStatement): Boolean = {
    node.fragments().asScala.foreach {
      case e: VariableDeclarationFragment => defineName(e.getName.getIdentifier)
    }
    true
  }
  override def visit(node: VariableDeclarationFragment): Boolean = {
    defineName(node.getName.getIdentifier)
    true
  }

  override def visit(node: SimpleName): Boolean = {
    node.getParent match {
      case n: FieldAccess if n.getName == node => // Skip
      case n: SuperFieldAccess if n.getName == node => // Skip
      case n: LabeledStatement if n.getLabel == node => // Skip
      case _ =>
        node.resolveBinding() match {
          case null =>
            warn("Failed to resolve name binding: Skipping name " + node + " (in " + currentClass() + ")")
            defineName(node.getIdentifier)
          case b: IVariableBinding =>
            defineName(b.getName)
          case _ =>
        }
    }
    true
  }

  override def visit(node: QualifiedName): Boolean = {
    firstQualified(node).resolveBinding() match {
      case null =>
        warn("Failed to resolve qualified binding: Skipping name " + node + " (in " + currentClass() + ")")
        defineName(SourceUtil.simplePart(node))
      case b: IVariableBinding =>
        defineName(b.getName)
      case _ =>
    }
    false // Don't visit simple name inside
  }
  
  @tailrec
  private def firstQualified(node: QualifiedName): Name = {
    node.getQualifier match {
      case q: QualifiedName => firstQualified(q)
      case q => q
    }
  }
  
  private def warn(msg: String): Unit = if (!quiet) System.out.println(msg)
}
