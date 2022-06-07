package org.moddingx.sourcetransform.transform

import org.moddingx.sourcetransform.transform.data.TransformTarget
import org.moddingx.sourcetransform.util.Bytecode
import org.moddingx.sourcetransform.util.inheritance.InheritanceMap
import org.objectweb.asm.Type

import scala.annotation.tailrec

object TransformUtil {
  
  class AppliedTransformer private[TransformUtil] (inheritance: InheritanceMap, transformers: Seq[ConfiguredTransformer]) {

    def apply(name: String, target: TransformTarget, filter: ConfiguredTransformer => Boolean, action: String => Boolean): Option[String] = {
      for (ct <- transformers if filter(ct)) {
        ct.transform(name, target, inheritance) match {
          case Some(newName) =>
            if (action(newName)) {
              return Some(newName)
            }
          case None =>
        }
      }
      None
    }

    def isUtilityClass(cls: String, forType: String): Boolean = {
      val members: Set[Bytecode.Member] = inheritance.getClassMembers(cls)
      val memberDescriptors: Set[String] = members.flatMap(m => inheritance.getDescriptor(m))
      val memberCount = memberDescriptors.count(desc => desc.contains(forType))
      (members.size / memberCount.toDouble) <= 5
    }

    def isClassRelatedTo(cls: String, forTypes: Set[String]): Boolean = {
      forTypes.exists(forType => inheritance.isSubType("L" + cls + ";", forType) || isUtilityClass(cls, forType))
    }
  }

  def createTransformer(inheritance: InheritanceMap, transformers: Seq[ConfiguredTransformer]): AppliedTransformer = {
    new AppliedTransformer(inheritance, transformers)
  }

  def getParamTypeForTransformerMatch(desc: String, idx: Int): String = {
    @tailrec
    def typeForMatch(t: Type): String = t.getSort match {
      case Type.ARRAY => typeForMatch(t.getElementType)
      case Type.METHOD => throw new IllegalArgumentException("Method type as descriptor arg")
      case _ => t.getDescriptor
    }

    val args = Type.getMethodType(desc).getArgumentTypes.toSeq
    
    if (args.indices.contains(idx)) typeForMatch(args(idx))
    else "V"
  }
}
