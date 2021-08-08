package io.github.noeppi_noeppi.tools.sourcetransform.util.cls

import org.objectweb.asm.ClassReader

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

class TreeLocator(thePath: Path) extends ClassIndex {
  
  private val path: Path = thePath.toAbsolutePath.normalize()
  
  override def allClasses: List[String] = {
    Files.walk(path).toScala(List)
      .filter(p => Files.isRegularFile(p))
      .filter(p => p.getFileName.toString.endsWith(".class"))
      .filter(p => p.getFileName.toString != "package-info.class")
      .filter(p => p.getFileName.toString != "module-info.class")
      .map(p => p.toAbsolutePath.normalize())
      .map(p => path.relativize(p))
      .map(p => (0 until p.getNameCount).map(i => p.getName(i).toString.strip()).mkString("/"))
      .map(p => p.substring(0, p.length - 6))
  }

  override def findClass(name: String): Option[ClassReader] = {
    val target = path.resolve(name + ".class")
    if (Files.exists(target)) {
      Some(new ClassReader(Files.readAllBytes(target)))
    } else {
      None
    }
  }
}
