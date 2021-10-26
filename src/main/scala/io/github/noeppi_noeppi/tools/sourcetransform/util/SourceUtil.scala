package io.github.noeppi_noeppi.tools.sourcetransform.util

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.{InheritanceMap, LambdaInfo, MethodInfo}
import org.eclipse.jdt.core.dom._

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.jdk.StreamConverters._

object SourceUtil {

  def getJavaSources(base: Path): List[String] = {
    Files.walk(base.toAbsolutePath.normalize()).toScala(List)
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
  
  private val types = Set("Z", "B", "C", "S", "I", "J", "F", "D", "V")
  
  def internal(binary: String): String = {
    if (types.contains(binary.dropWhile(_ == '['))) {
      binary
    } else {
      binary.takeWhile(_ == '[') + "L" + binary.dropWhile(_ == '[').replace('.', '/') + ";"
    }
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
    inheritance.getClasses(pkg).map(_.substring(pkg.length)).filter(!_.contains("$"))
  }
  
  def importedClassesFromCls(inheritance: InheritanceMap, cls: String): Set[String] = {
    val pkg = if (cls.contains("/")) cls.substring(0, cls.lastIndexOf('/')) else ""
    inheritance.getClasses(pkg).filter(_.startsWith(cls + "$")).map(_.substring(cls.length + 1))
      .filter(!_.contains("$")).filter(_.toIntOption.isEmpty)
  }
  
  def methodInfo(binding: IMethodBinding): Either[MethodInfo, String] = {
    if (binding == null) return Right("Null binding")
    val cls = binding.getDeclaringClass
    if (cls == null) return Right("No declaring class for method found.")
    if (cls.getBinaryName == null) return Right("Failed to get binary name")
    val name = if (binding.isConstructor) "<init>" else binding.getName
    SourceHacks.getBinaryDescriptor(binding) match {
      case Some(sig) => Left(MethodInfo(cls.getBinaryName.replace('.', '/'), name, sig))
      case None =>
        if (binding.getReturnType.getBinaryName == null) return Right("Failed to get return type binding")
        if (binding.getParameterTypes.exists(str => str.getBinaryName == null)) return Right("Failed to get signature binding")
        val ret = if (binding.isConstructor) "V" else SourceUtil.internal(binding.getReturnType.getBinaryName)
        val desc = "(" + binding.getParameterTypes.map(str => SourceUtil.internal(str.getBinaryName)).mkString("") + ")" + ret
        Left(MethodInfo(cls.getBinaryName.replace('.', '/'), name, desc))
    }
  }
  
  def getLambdaImplId(binding: IMethodBinding): LambdaInfo = SourceHacks.getLambdaImplId(binding)
}
