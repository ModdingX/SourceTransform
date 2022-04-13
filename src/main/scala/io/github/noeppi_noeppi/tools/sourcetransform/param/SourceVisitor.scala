package io.github.noeppi_noeppi.tools.sourcetransform.param

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.{InheritanceMap, LambdaInfo, MethodInfo}
import io.github.noeppi_noeppi.tools.sourcetransform.util.{ClassTrackingVisitor, SourceUtil}
import org.eclipse.jdt.core.dom._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class SourceVisitor(
                     val inheritance: InheritanceMap,
                     val sanitizers: mutable.Map[MethodInfo, ParamSanitizer],
                     val lambdaTargets: mutable.Map[LambdaInfo, LambdaTarget],
                     val srgMap: mutable.Map[String, mutable.Set[MethodInfo]],
                     val quiet: Boolean
                   ) extends ClassTrackingVisitor {
  
  private var localClassLevel = 0
  private val imports = mutable.Set[String]()
  private val nameScopes = mutable.Stack[mutable.Set[String]]()
  private val staticInitNames = mutable.Map[String, mutable.Set[ParamSanitizer]]()
  private val lambdaTargetStack = mutable.Stack[LambdaTarget]()
  
  private def pushLocalClass(): Unit = localClassLevel += 1
  private def popLocalClass(): Unit = if (localClassLevel > 0) localClassLevel -= 1 else throw new IllegalStateException("Negative local class level.")
  
  private def pushScope(): Unit = nameScopes.push(mutable.Set())
  private def popScope(): Unit = if (nameScopes.nonEmpty) nameScopes.pop() else throw new IllegalStateException("Can't pop empty scope stack.")
  
  private def pushLambdaTarget(info: LambdaTarget): Unit = lambdaTargetStack.push(info)
  private def popLambdaTarget(): Unit = if (lambdaTargetStack.nonEmpty) lambdaTargetStack.pop() else throw new IllegalStateException("Can't pop empty method stack.")
  
  private def defineImport(name: String): Unit = imports.addOne(name)
  private def defineType(name: String): Unit = if (nameScopes.isEmpty) imports.addOne(name) else nameScopes.head.addOne(name)
  private def defineName(name: String): Unit = if (nameScopes.nonEmpty) nameScopes.head.addOne(name) else throw new IllegalStateException("Can't define name on empty scope stack: " + name)
  
  def endParse(): Unit = {
    for ((key, value) <- staticInitNames if value.nonEmpty) {
      val info = MethodInfo(key, "<clinit>", "()V")
      val sanitizer = ParamSanitizer(value.flatMap(_.forbidden).toSet, value.map(_.localClassLevel).maxOption.getOrElse(0))
      sanitizers.put(info, sanitizer)
    }
    if (localClassLevel != 0) throw new IllegalStateException("Invalid local class level.")
    if (nameScopes.nonEmpty) throw new IllegalStateException("Invalid open scope.")
    if (lambdaTargetStack.nonEmpty) throw new IllegalStateException("Invalid open method.")
  }
  
  private def buildSanitizer() = ParamSanitizer(imports.toSet | nameScopes.headOption.getOrElse(Set()), localClassLevel)
  
  private def currentLambdaTarget(): LambdaTarget = lambdaTargetStack.headOption.getOrElse(LambdaTarget.Skip)
  
  private def startType(node: AbstractTypeDeclaration): Boolean = {
    if (node.isLocalTypeDeclaration) pushLocalClass()
    // Define name before push scope so it stays forbidden after the type ends
    defineType(node.getName.getIdentifier)
    true
  }
  
  private def endType(node: AbstractTypeDeclaration): Unit = {
    if (node.isLocalTypeDeclaration) popLocalClass()
  }

  private def processSrgParam(param: String, method: MethodInfo): Unit = {
    if (param.startsWith("p_") && param.endsWith("_") && param.substring(2, param.length - 1).toIntOption.isDefined) {
      srgMap.getOrElseUpdate(param, mutable.Set()).add(method)
    }
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
    node.getParent match {
      case t: AbstractTypeDeclaration =>
        val binding = t.resolveBinding()
        if (binding == null || binding.getBinaryName == null) {
          pushLambdaTarget(LambdaTarget.Skip)
        } else {
          pushLambdaTarget(LambdaTarget.Method(MethodInfo(binding.getBinaryName.replace('.', '/'), "<clinit>", "()V")))
        }
      case _ => pushLambdaTarget(LambdaTarget.Skip)
    }
    pushScope()
    true
  }
  override def endVisit(node: Initializer): Unit = {
    val sanitizer = buildSanitizer()
    popScope()
    popLambdaTarget()
    node.getParent match {
      case t: AbstractTypeDeclaration =>
        val binding = t.resolveBinding()
        if (binding == null) {
          warn("Failed to resolve binding for static initializer in class " + currentClass())
        } else if (binding.getBinaryName == null) {
          warn("Failed to resolve binary for static initializer declaration in class " + currentClass())
        } else {
          staticInitNames.getOrElseUpdate(binding.getBinaryName.replace('.', '/'), mutable.Set()).add(sanitizer)
        }
      case _ => warn("Unmatched static initializer found in class " + currentClass())
    }
  }
  
  override def visit(node: MethodDeclaration): Boolean = {
    pushLambdaTarget(LambdaTarget.forMethod(SourceUtil.methodInfo(node.resolveBinding()).left.toOption.orNull))
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
    popLambdaTarget()
    SourceUtil.methodInfo(node.resolveBinding()) match {
      case Left(info) =>
        sanitizers.put(info, sanitizer)
        node.parameters().asScala.foreach {
          case p: VariableDeclaration => processSrgParam(p.getName.getIdentifier, info)
          case p => warn("Failed to post process method parameter: " + p)
        }
      case Right(err) => warn("Failed to resolve method binding: Skipping method: " + node.getName.getFullyQualifiedName + " (in " + currentClass() + "): " + err)
    }
  }

  override def visit(node: LambdaExpression): Boolean = {
    // Lambdas live in the same scope
    node.parameters().asScala.foreach {
      case p: VariableDeclaration => defineName(p.getName.getIdentifier)
      case p => warn("Failed to process lambda parameter: " + p)
    }
    true
  }
  override def endVisit(node: LambdaExpression): Unit = {
    // Lambdas live in the same scope
    val target = currentLambdaTarget()
    val lambda = SourceUtil.getLambdaImplId(node.resolveMethodBinding())
    if (lambda == null) {
      warn("Failed to resolve lambda binding: Skipping lambda (in " + currentClass() + ")")
    } else {
      lambdaTargets.put(lambda, target)
    }
  }

  

  override def visit(node: FieldDeclaration): Boolean = {
    // We want to skip variable declaration fragments from this but
    // further processing is needed to match lambdas.
    pushLambdaTarget(LambdaTarget.Keep)
    true
  }
  override def endVisit(node: FieldDeclaration): Unit = {
    popLambdaTarget()
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
    node.getParent match {
      case _: FieldDeclaration => // Skip names from field declarations
      case _ => defineName(node.getName.getIdentifier)
    }
    true
  }

  override def visit(node: SimpleName): Boolean = {
    node.getParent match {
      case n: FieldAccess if n.getName == node => // Skip
      case n: SuperFieldAccess if n.getName == node => // Skip
      case n: LabeledStatement if n.getLabel == node => // Skip
      case n: BreakStatement if n.getLabel == node => // Skip
      case n: ContinueStatement if n.getLabel == node => // Skip
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
