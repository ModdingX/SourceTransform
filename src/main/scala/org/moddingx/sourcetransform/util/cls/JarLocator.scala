package org.moddingx.sourcetransform.util.cls

import org.objectweb.asm.ClassReader

import java.nio.file.{FileSystem, FileSystems, Files, Path}
import scala.jdk.StreamConverters._

// Jars and JMods
class JarLocator(path: Path) extends ClassIndex {

  private val fs: FileSystem = FileSystems.newFileSystem(path, null.asInstanceOf[ClassLoader])
  private val jmod: Boolean = path.getFileName.toString.endsWith(".jmod")

  override def allClasses: Seq[String] = {
    val basePath = fs.getPath(if (jmod) "/classes/" else "/").toAbsolutePath.normalize()
    Files.walk(basePath).toScala(Seq)
      .filter(p => Files.isRegularFile(p))
      .filter(p => p.getFileName.toString.endsWith(".class"))
      .filter(p => p.getFileName.toString != "package-info.class")
      .filter(p => p.getFileName.toString != "module-info.class")
      .map(p => p.toAbsolutePath.normalize())
      .map(p => basePath.relativize(p))
      .map(p => (0 until p.getNameCount).map(i => p.getName(i).toString.strip()).mkString("/"))
      .map(p => p.substring(0, p.length - 6))
  }
  
  override def findClass(name: String): Option[ClassReader] = {
    val path = fs.getPath((if (jmod) "classes/" else "") + name + ".class")
    if (Files.exists(path)) {
      Some(new ClassReader(Files.readAllBytes(path)))
    } else {
      None
    }
  }
}
