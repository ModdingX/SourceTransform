package org.moddingx.sourcetransform.util

import org.moddingx.sourcetransform.util.inheritance.InheritanceMap
import org.objectweb.asm.Type
import org.eclipse.jdt.core.dom.{Type => _, *}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.jdk.StreamConverters.*

object SourceUtil {

  def getJavaSources(base: Path): Seq[String] = {
    Files.walk(base.toAbsolutePath.normalize()).toScala(Seq)
      .filter(p => Files.isRegularFile(p))
      .filter(p => p.getFileName.toString.endsWith(".java"))
      .filter(p => p.getFileName.toString != "package-info.java")
      .filter(p => p.getFileName.toString != "module-info.java")
      .map(p => p.toAbsolutePath.normalize())
      .map(p => base.relativize(p))
      .map(p => (0 until p.getNameCount).map(i => p.getName(i)).mkString("/"))
  }
  
  def createParserFactory(level: LanguageLevel, base: Path, classpath: Seq[Path])(path: String): ASTParser = {
    val parser = level.createParser()
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setEnvironment(classpath.map(_.toAbsolutePath.normalize().toString).toArray, Array(base.toAbsolutePath.normalize().toString), null, false)
    parser.setResolveBindings(true)
    parser.setBindingsRecovery(true)
    parser.setUnitName(path)
    val in = Files.newInputStream(base.resolve(path))
    val data = StandardCharsets.UTF_8.decode(ByteBuffer.wrap(in.readAllBytes()))
    val array = if (data.hasArray) data.array() else (0 until data.length()).map(i => data.get(i)).toArray
    parser.setSource(array)
    parser
  }

  def internal(binding: ITypeBinding): Option[String] = {
    if (binding == null) return None
    // Translate a type binding into a type descriptor
    val erasure = binding.getErasure
    if (erasure == null) return None
    if (erasure.isArray) {
      Some(("[" * erasure.getDimensions) + internal(erasure.getElementType))
    } else if (erasure.isPrimitive) erasure.getName match {
      case "boolean" => Some("Z")
      case "byte" => Some("B")
      case "char" => Some("C")
      case "short" => Some("S")
      case "int" => Some("I")
      case "long" => Some("J")
      case "float" => Some("F")
      case "double" => Some("D")
      case "void" => Some("V")
      case _ => None
    } else if (erasure.isNullType) {
      Some("L" + Bytecode.ROOT + ";")
    } else {
      Option(erasure.getBinaryName)
        .map(desc => "L" + desc.replace('.', '/') + ";")
    }
  }
  
  def internalCls(binding: ITypeBinding): Option[String] = internal(binding) match {
    case Some(binary) =>
      val binaryType = Type.getType(binary)
      binaryType.getSort match {
        case Type.OBJECT => Some(binaryType.getInternalName)
        case _ => None
      }
    case None => None
  }
  
  @tailrec
  def simplePart(name: Name): String = name match {
    case n: SimpleName => n.getIdentifier
    case n: QualifiedName => n.getName.getIdentifier
    case n: ModuleQualifiedName => simplePart(n.getName)
    case n =>
      val fqn = n.getFullyQualifiedName
      if (fqn.contains(".")) {
        fqn.substring(fqn.indexOf('.') + 1)
      } else {
        fqn
      }
  }
  
  def importedClassesFromPkg(inheritance: InheritanceMap, pkg: String): Set[String] = {
    inheritance.getPackageMembers(pkg).map(_.substring(pkg.length)).filter(!_.contains("$"))
  }

  def importedClassesFromCls(inheritance: InheritanceMap, cls: String): Set[String] = {
    val pkg = if (cls.contains("/")) cls.substring(0, cls.lastIndexOf('/')) else ""
    inheritance.getPackageMembers(pkg).filter(_.startsWith(cls + "$")).map(_.substring(cls.length + 1))
      .filter(!_.contains("$")).filter(_.toIntOption.isEmpty)
  }
  
  def methodInfo(binding: IMethodBinding): Either[Bytecode.Method, String] = {
    if (binding == null) return Right("Null binding")
    val cls = binding.getDeclaringClass
    if (cls == null) return Right("No declaring class for method found.")
    if (cls.getBinaryName == null) return Right("Failed to get binary name")
    val name = if (binding.isConstructor) "<init>" else binding.getName
    SourceHacks.getBinaryDescriptor(binding) match {
      case Some(sig) => Left(Bytecode.Method(cls.getBinaryName.replace('.', '/'), name, sig))
      case None =>
        if (binding.getReturnType.getBinaryName == null) return Right("Failed to get return type binding")
        if (binding.getParameterTypes.exists(str => str.getBinaryName == null)) return Right("Failed to get signature binding")
        val ret = if (binding.isConstructor) "V" else SourceUtil.internal(binding.getReturnType)
        val desc = "(" + binding.getParameterTypes.map(ptype => SourceUtil.internal(ptype)).mkString("") + ")" + ret
        Left(Bytecode.Method(cls.getBinaryName.replace('.', '/'), name, desc))
    }
  }
  
  def getLambdaImplId(binding: IMethodBinding): Option[Lambda] = SourceHacks.getLambdaImplId(binding)
  def wrapLambda(lambda: Bytecode.Lambda): Lambda = Lambda(lambda.cls, lambda.lambdaId)
  
  case class Lambda(cls: String, lambdaId: String)
}
