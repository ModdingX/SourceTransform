package io.github.noeppi_noeppi.tools.sourcetransform.util

import org.eclipse.jdt.core.dom.{ASTParser, SingleVariableDeclaration}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
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
    val unit = if (path.endsWith(".java")) path.substring(0, path.length - 5) else path
    val unitPath = base.resolve(unit)
    val unitId = (0 until unitPath.getNameCount).map(i => unitPath.getName(i)).mkString("/")
    parser.setUnitName(unitId)
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
}
