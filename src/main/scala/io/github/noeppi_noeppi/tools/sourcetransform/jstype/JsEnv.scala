package io.github.noeppi_noeppi.tools.sourcetransform.jstype

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.InheritanceMap
import io.github.noeppi_noeppi.tools.sourcetransform.util.cls.ClassLocator
import io.github.noeppi_noeppi.tools.sourcetransform.util.signature.SignatureNode

import java.io.Writer
import scala.jdk.CollectionConverters._

class JsEnv(val inheritance: InheritanceMap, private val classes: Seq[String], val classpath: ClassLocator,
            private val classMap: Map[String, String], val tsWriter: Writer, val nulls: NullChecker) {
  
  private lazy val allSupported: Set[String] = classes.flatMap(cls => {
    val builder = Set.newBuilder[String]
    builder.addOne(cls)
    builder.addAll(inheritance.getAllSuperClasses(cls))
    builder.addAll(inheritance.classes.keySet.filter(_.startsWith(cls + "$")))
    builder.result()
  }).toSet
  
  def isSource(cls: String): Boolean = classes.contains(cls)
  def supports(cls: String): Boolean = allSupported.contains(cls)
  
  def plainName(cls: String): String = classMap.getOrElse(cls, "Java_" + cls.replace('/', '_'))
  
  def typeVars(sig: SignatureNode): String = {
    if (sig == null || sig.formalTypeParameters.isEmpty) {
      ""
    } else {
      sig.formalTypeParameters.asScala
        .map(p => {
          val allBounds = (p.classBound :: p.interfaceBounds.asScala.toList).flatMap(this.jsTypeSig)
          if (allBounds.isEmpty) {
            p.name
          } else {
            p.name + " extends " + allBounds.map("(" + _ + ")").mkString("&")
          }
        })
        .mkString("<", ",", ">")
    }
  }
  
  def typeVarDefs(sig: SignatureNode): String = {
    if (sig == null || sig.formalTypeParameters.isEmpty) {
      ""
    } else {
      sig.formalTypeParameters.asScala
        .map(p => p.name)
        .mkString("<", ",", ">")
    }
  }
  
  def jsTypeField(desc: String, sig: SignatureNode, nullable: Boolean): String = {
    val realSig = if (sig == null) null else sig.superClass; // Weird but field type seems to be stored in the superClass property.
    jsType(desc, realSig, nullable)
  }
  
  def jsType(desc: String, sig: SignatureNode, nullable: Boolean): String = if (nullable) {
    jsType(desc, sig) + "|null"
  } else {
    jsType(desc, sig)
  }
  
  private def jsType(desc: String, sig: SignatureNode): String = {
    if (sig != null) jsTypeSig(sig) match {
      case Some(t) => return t
      case None =>
    }
    jsTypeRaw(desc)
  }
  
  private def jsTypeRaw(desc: String): String = desc match {
    case "V" => "void"
    case "Z" => "boolean"
    case "B" | "C" | "S" | "I" | "J" | "F" | "D" => "number"
    case t if t.startsWith("[") => "Array<" + jsTypeRaw(t.substring(1)) + ">"
    case t if t.startsWith("L") && t.endsWith(";") => jsClass(t.substring(1, t.length - 1))
    case _ => throw new IllegalArgumentException("Invalid java type: " + desc)
  }
  
  private def jsTypeSig(sig: SignatureNode): Option[String] = {
    if (sig.baseType != 0 && sig.baseType != 'L') {
      return Some(sig.baseType match {
        case 'V' => "void"
        case 'Z' => "boolean"
        case 'B' | 'C' | 'S' | 'I' | 'J' | 'F' | 'D' => "number"
        case _ => "unknown"
      })
    }
    if (sig.arrayOf != null) {
      return Some("Array<" + jsTypeSig(sig.arrayOf).getOrElse("unknown") + ">")
    }
    if (sig.classType != null) {
      val base = jsClass(sig.classType.desc)
      if (base != "never" && base != "unknown" && base != "any") {
        return Some(base + typeDefs(sig))
      }
    }
    if (sig.typeVariable != null) {
      return Some(sig.typeVariable)
    }
    None
  }

  def typeDefs(sig: SignatureNode): String = {
    if (sig == null || sig.classType == null || sig.classType.arguments.isEmpty) {
      ""
    } else {
      sig.classType.arguments.asScala
        .map(p => jsTypeSig(p).getOrElse("unknown"))
        .mkString("<", ",", ">")
    }
  }
  
  def jsClass(cls: String): String = cls match {
    case InheritanceMap.ROOT => "any"
    case "java/lang/String" => "string"
    case "java/util/List" => "Array"
    case _ if cls.contains('$') => plainName(cls.substring(0, cls.indexOf('$'))) + cls.substring(cls.indexOf('$')).replace('$', '.')
    case _ if allSupported.contains(cls) => plainName(cls)
    case _ => "never"
  }
}
