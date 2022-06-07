package org.moddingx.sourcetransform.parchment

import org.moddingx.sourcetransform.util.{Bytecode, ClassTrackingVisitor, SourceUtil}
import org.moddingx.sourcetransform.util.inheritance.InheritanceMap
import org.eclipse.jdt.core.dom.{AbstractTypeDeclaration, AnnotationTypeDeclaration, AnonymousClassDeclaration, BreakStatement, CompilationUnit, ContinueStatement, EnumDeclaration, FieldAccess, FieldDeclaration, IPackageBinding, ITypeBinding, IVariableBinding, ImportDeclaration, Initializer, LabeledStatement, LambdaExpression, MethodDeclaration, Name, QualifiedName, RecordDeclaration, SimpleName, SingleVariableDeclaration, SuperFieldAccess, TypeDeclaration, VariableDeclaration, VariableDeclarationExpression, VariableDeclarationFragment, VariableDeclarationStatement}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.given

class SourceVisitor(
                     inheritance: InheritanceMap,
                     sanitizers: mutable.Map[Bytecode.Method, ParamRenamer],
                     lambdaTargets: mutable.Map[SourceUtil.Lambda, LambdaTarget],
                     srgMap: mutable.Map[String, mutable.Set[Bytecode.Method]],
                     quiet: Boolean
                   ) extends ClassTrackingVisitor {
  
  private var localClassLevel = 0
  private val imports = mutable.Set[String]()
  
  // Each method / constructor / static init is a scope
  private val nameScopes = mutable.Stack[mutable.Set[String]]()
  
  // Collect renames for all static init blocks by type. Merged in endVisit
  private val staticInitRenames = mutable.Map[String, mutable.Set[ParamRenamer]]()
  
  // Stack of lambda targets (for nested lambdas)
  private val lambdaTargetStack = mutable.Stack[LambdaTarget]()

  
  private def pushLocalClass(): Unit = localClassLevel += 1
  private def popLocalClass(): Unit = {
    if (localClassLevel > 0) localClassLevel -= 1
    else throw new IllegalStateException("Negative local class level.")
  }
  
  private def pushScope(): Unit = nameScopes.push(mutable.Set())
  private def popScope(): Unit = {
    if (nameScopes.nonEmpty) nameScopes.pop()
    else throw new IllegalStateException("Can't pop empty scope stack.")
  }
  
  private def pushLambdaTarget(info: LambdaTarget): Unit = lambdaTargetStack.push(info)
  private def popLambdaTarget(): Unit = {
    if (lambdaTargetStack.nonEmpty) lambdaTargetStack.pop()
    else throw new IllegalStateException("Can't pop empty method stack.")
  }
  
  private def defineImport(name: String): Unit = imports.addOne(name)
  private def defineType(name: String): Unit = {
    if (nameScopes.isEmpty) imports.addOne(name)
    else nameScopes.head.addOne(name)
  }
  private def defineName(name: String): Unit = {
    if (nameScopes.nonEmpty) nameScopes.head.addOne(name)
    else throw new IllegalStateException("Can't define name on empty scope stack: " + name)
  }
  
  
  def endParse(): Unit = {
    // Merge all renames for static init blocks
    for ((cls, renames) <- staticInitRenames) {
      val merged = ParamRenamer.merge(ParamRenamer.Keep, renames.toSet)
      sanitizers.put(Bytecode.Method(cls, "<clinit>", "()V"), merged)
    }
    if (localClassLevel != 0) throw new IllegalStateException("Invalid local class level.")
    if (nameScopes.nonEmpty) throw new IllegalStateException("Invalid open scope.")
    if (lambdaTargetStack.nonEmpty) throw new IllegalStateException("Invalid open method.")
  }

  private def buildRenamer() = ParamRenamer.Default(imports.toSet | nameScopes.headOption.getOrElse(Set()), localClassLevel)

  private def currentLambdaTarget(): LambdaTarget = lambdaTargetStack.headOption.getOrElse(LambdaTarget.Skip)

  private def startType(node: AbstractTypeDeclaration): Boolean = {
    if (node.isLocalTypeDeclaration) pushLocalClass()
    // Define name before push scope so it stays after the type ends
    defineType(node.getName.getIdentifier)
    true
  }

  private def endType(node: AbstractTypeDeclaration): Unit = {
    if (node.isLocalTypeDeclaration) popLocalClass()
  }

  private def processSrgParam(param: String, method: Bytecode.Method): Unit = {
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
    // Static imports can be shadowed by params without stuff breaking
    // If a static import is accessed in a method, it will be found as a name anyway
    if (!node.isStatic) {
      if (node.isOnDemand) {
        node.resolveBinding() match {
          case null => warn("Failed to resolve binding: " + node)
          case b: IPackageBinding => SourceUtil.importedClassesFromPkg(inheritance, b.getName.replace('.', '/')).foreach(defineImport)
          case b: ITypeBinding => SourceUtil.internalCls(b) match {
            case Some(cls) => SourceUtil.importedClassesFromCls(inheritance, cls).foreach(defineImport)
            case None => warn("Failed to process import with type binding: " + b)
          }
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
      case t: AbstractTypeDeclaration => SourceUtil.internalCls(t.resolveBinding()) match {
        case Some(cls) => pushLambdaTarget(LambdaTarget.Method(Bytecode.Method(cls, "<clinit>", "()V")))
        case None => pushLambdaTarget(LambdaTarget.Skip)
      }
      case _ => pushLambdaTarget(LambdaTarget.Skip)
    }
    pushScope()
    true
  }
  override def endVisit(node: Initializer): Unit = {
    val renamer = buildRenamer()
    popScope()
    popLambdaTarget()
    node.getParent match {
      case t: AbstractTypeDeclaration => SourceUtil.internalCls(t.resolveBinding()) match {
        case Some(cls) => staticInitRenames.getOrElseUpdate(cls, mutable.Set()).add(renamer)
        case None => warn("Failed to resolve binding for static initializer in class " + currentClass())
      }
      case _ => warn("Unmatched static initializer found in class " + currentClass())
    }
  }
  
  override def visit(node: MethodDeclaration): Boolean = {
    SourceUtil.methodInfo(node.resolveBinding()) match {
      case Left(method) => pushLambdaTarget(LambdaTarget.Method(method))
      case Right(err) => pushLambdaTarget(LambdaTarget.Skip) // No logging, will be logged in endVisit
    }
    pushScope()
    node.parameters().asScala.foreach {
      case p: VariableDeclaration => defineName(p.getName.getIdentifier)
      case p => warn("Failed to process method parameter: " + p)
    }
    true
  }
  override def endVisit(node: MethodDeclaration): Unit = {
    val renamer = buildRenamer()
    popScope()
    popLambdaTarget()
    SourceUtil.methodInfo(node.resolveBinding()) match {
      case Left(method) =>
        sanitizers.put(method, renamer)
        node.parameters().asScala.foreach {
          case p: VariableDeclaration => processSrgParam(p.getName.getIdentifier, method)
          case p => warn("Failed to post process method parameter: " + p)
        }
      case Right(err) => warn("Failed to resolve method binding: Skipping method: " + node.getName.getFullyQualifiedName + " (in " + currentClass() + "): " + err)
    }
  }

  override def visit(node: LambdaExpression): Boolean = {
    // Don't push a scope, lambdas live in the same scope
    node.parameters().asScala.foreach {
      case p: VariableDeclaration => defineName(p.getName.getIdentifier)
      case p => warn("Failed to process lambda parameter: " + p)
    }
    true
  }
  override def endVisit(node: LambdaExpression): Unit = {
    val target = currentLambdaTarget()
    SourceUtil.getLambdaImplId(node.resolveMethodBinding()) match {
      case Some(lambda) => lambdaTargets.put(lambda, target)
      case None => warn("Failed to resolve lambda binding: Skipping lambda (in " + currentClass() + ")")
    }
  }
  
  
  
  override def visit(node: FieldDeclaration): Boolean = {
    pushLambdaTarget(LambdaTarget.Keep)
    // We want to skip variable declaration fragments from this but
    // further processing is needed to match lambdas.
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
      case n: FieldAccess if n.getName == node => // Won't conflict (this prefixed)
      case n: SuperFieldAccess if n.getName == node => // Won't conflict (super prefixed)
      case n: LabeledStatement if n.getLabel == node => // Labels don't resolve bindings, need to be skipped explicitly
      case n: BreakStatement if n.getLabel == node => // Labels don't resolve bindings, need to be skipped explicitly
      case n: ContinueStatement if n.getLabel == node => // Labels don't resolve bindings, need to be skipped explicitly
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
    @tailrec
    def firstQualified(node: QualifiedName): Name = {
      node.getQualifier match {
        case q: QualifiedName => firstQualified(q)
        case q => q
      }
    }
    
    firstQualified(node).resolveBinding() match {
      case null =>
        warn("Failed to resolve qualified binding: Skipping name " + node + " (in " + currentClass() + ")")
        defineName(SourceUtil.simplePart(node))
      case b: IVariableBinding =>
        defineName(b.getName)
      case _ =>
    }
    false // Don't visit the simple names inside
  }

  private def warn(msg: String): Unit = if (!quiet) System.out.println(msg)
}
