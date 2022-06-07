package org.moddingx.sourcetransform.transform

import org.moddingx.sourcetransform.transform.data.{TransformMember, TransformTarget, Transformer}
import org.moddingx.sourcetransform.util.{Bytecode, Util}
import org.moddingx.sourcetransform.util.inheritance.InheritanceMap
import net.minecraftforge.srgutils.IMappingFile

import scala.util.matching.Regex

class ConfiguredTransformer(private val transformer: Transformer, private val targets: Set[TransformTarget], val baseTypes: Set[String], val baseMember: Set[TransformMember], val exactType: Boolean) {

  def transform(name: String, target: TransformTarget, inheritance: InheritanceMap): Option[String] = {
    target match {
      case _ if !targets.contains(target) => None
      case TransformTarget.CHILD_CLASS | TransformTarget.UTILITY_CLASS => inheritance.getSourceName(name) match {
        case Some(sourceName) => transformer.applyTo(sourceName)
        case None => None
      }
      case _ => transformer.applyTo(name)
    }
  }

  def remap(mappings: IMappingFile): ConfiguredTransformer = {
    new ConfiguredTransformer(
      transformer.remap(mappings), targets,
      baseTypes.map(e => mappings.remapDescriptor(e)),
      baseMember, exactType
    )
  }

  def matchBaseClass(inheritance: InheritanceMap, testCls: String): Boolean = {
    matchBaseType(inheritance, "L" + testCls + ";")
  }

  def matchBaseType(inheritance: InheritanceMap, testType: String): Boolean = {
    if (baseTypes.isEmpty) {
      true
    } else {
      if (exactType) baseTypes.contains(testType)
      else baseTypes.exists(str => inheritance.isSubType(testType, str))
    }
  }

  def matchTypeDescriptor(inheritance: InheritanceMap, desc: String): Boolean = {
    if (baseTypes.isEmpty || baseTypes.exists(e => desc.contains(e))) {
      true
    } else {
      // Check for subclasses as well
      val descTypes: Set[String] = ConfiguredTransformer.CLS_TYPE.findAllMatchIn(desc).map(m => m.group(1)).toSet
      if (exactType) baseTypes.exists(b => descTypes.contains(b))
      else baseTypes.exists(b => descTypes.exists(sigType => inheritance.isSubType("L" + sigType + ";", b)))
    }
  }

  def matchBaseField(name: String): Boolean = baseMember.isEmpty || baseMember.exists {
    case TransformMember.Field(n) if name == n => true
    case _ => false
  }

  def matchBaseMethod(name: String, desc: String): Boolean = {
    val params = Bytecode.simplifiedDescriptorParams(desc).length
    baseMember.isEmpty || baseMember.exists {
      case TransformMember.Method(n, None) if name == n => true
      case TransformMember.Method(n, Some(p)) if name == n && params == p => true
      case _ => false
    }
  }

  override def toString: String = "[ " + transformer.string() + " @ " + targets.mkString("(",",",")") + " # " + baseTypes.mkString("|") + " # " + baseMember.mkString("|") + " ]"
}

object ConfiguredTransformer {
  
  private val CLS_TYPE: Regex = "L([^;]+);".r
}
