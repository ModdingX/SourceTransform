package io.github.noeppi_noeppi.tools.sourcetransform.transform

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.InheritanceMap
import io.github.noeppi_noeppi.tools.sourcetransform.util.Util
import net.minecraftforge.srgutils.IMappingFile

class ConfiguredTransformer(private val transformer: Transformer, private val targets: Set[TransformTarget], val baseType: Set[String], val baseMember: Set[Member], val exactType: Boolean) {
  
  def transform(name: String, target: TransformTarget): Option[String] = {
    if (targets.contains(target)) {
      target match {
        case TransformTarget.CHILD_CLASS | TransformTarget.UTILITY_CLASS =>
          val className = if (name.contains("/")) name.substring(name.lastIndexOf('/') + 1) else name
          val packageName = if (name.contains("/")) Some(name.substring(0, name.lastIndexOf('/'))) else None
          val sourceName = if (className.contains("$")) className.substring(className.lastIndexOf('$') + 1) else className
          val enclosingName = if (className.contains("$")) Some(className.substring(0, className.lastIndexOf('$'))) else None
          if (sourceName.toIntOption.isDefined) {
            // Class has no name in source code, we won't rename it.
            None
          } else {
            transformer.applyOn(className).map(transformed => packageName.map(_ + "/").getOrElse("") + enclosingName.map(_ + "$").getOrElse("") + transformed)
          }
        case _ => transformer.applyOn(name)
      }
    } else {
      None
    }
  }
  
  def remap(mappings: IMappingFile): ConfiguredTransformer = {
    new ConfiguredTransformer(
      transformer.remap(mappings), targets,
      baseType.map(e => mappings.remapDescriptor(e)),
      baseMember, exactType
    )
  }
  
  def matchBaseClass(inheritance: InheritanceMap, testCls: String): Boolean = {
    matchBaseType(inheritance, "L" + testCls + ";")
  }
  
  def matchBaseType(inheritance: InheritanceMap, testType: String): Boolean = {
    if (baseType.isEmpty) {
      true
    } else {
      if (exactType) {
        baseType.contains(testType)
      } else {
        baseType.exists(str => inheritance.isSubType(testType, str))
      }
    }
  }
  
  def matchTypeSignature(inheritance: InheritanceMap, signature: String): Boolean = {
    if (baseType.isEmpty || baseType.exists(e => signature.contains(e))) {
      true
    } else {
      // Check for subclasses as well
      val sigTypes: Set[String] = Util.CLS_TYPE.findAllMatchIn(signature).map(m => m.group(1)).toSet
      if (exactType) {
        baseType.exists(b => sigTypes.contains(b))
      } else {
        baseType.exists(b => sigTypes.exists(sigType => inheritance.isSubType("L" + sigType + ";", b)))
      }
    }
  }
  
  def matchBaseField(name: String): Boolean = baseMember.isEmpty || baseMember.exists {
    case Member.Field(n) if name == n => true
    case _ => false
  }
  
  def matchBaseMethod(name: String, signature: String): Boolean = {
    val params = signature
      .replaceAll("L[^;]*;", "L")
      .replace("(", "")
      .replaceAll("\\).", "")
      .length
    baseMember.isEmpty || baseMember.exists {
      case Member.Method(n, None) if name == n => true
      case Member.Method(n, Some(p)) if name == n && params == p => true
      case _ => false
    }
  }
  
  override def toString: String = "[ " + transformer.string() + " @ " + targets.mkString("(",",",")") + " # " + baseType.mkString("|") + " # " + baseMember.mkString("|") + " ]"
}
