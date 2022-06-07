package org.moddingx.sourcetransform.jstype

import org.moddingx.sourcetransform.util.{Bytecode, Util}
import org.objectweb.asm.tree.{AnnotationNode, FieldNode, MethodNode}

import scala.jdk.CollectionConverters.given

class NullChecker(val nulls: Set[String]) {
  
  def nullable(cls: String, node: FieldNode): Boolean = nulls.contains(cls + ";" + node.name) || ann(node.visibleAnnotations, node.invisibleAnnotations).exists(n => NullChecker.NULL_ANNOTATIONS.contains(n.desc))
  def nullable(cls: String, node: MethodNode): Boolean = nulls.contains(cls + ";" + node.name + node.desc) || ann(node.visibleAnnotations, node.invisibleAnnotations).exists(n => NullChecker.NULL_ANNOTATIONS.contains(n.desc))
  def nullable(cls: String, node: MethodNode, idx: Int): Boolean = nulls.contains(cls + ";" + node.name + node.desc + "#" + idx) || ann(node, idx).exists(n => NullChecker.NULL_ANNOTATIONS.contains(n.desc)) 
  
  private def ann(visible: java.util.List[AnnotationNode], invisible: java.util.List[AnnotationNode]): Seq[AnnotationNode] = {
    val a1 = if (visible == null) Nil else visible.asScala
    val a2 = if (invisible == null) Nil else invisible.asScala
    Seq.newBuilder.addAll(a1).addAll(a2).result()
  }
  
  private def ann(node: MethodNode, idx: Int): Seq[AnnotationNode] = {
    val a1 = if (node.visibleParameterAnnotations == null) Nil else paramAnn(
      node.visibleAnnotableParameterCount, node.visibleParameterAnnotations, idx, Bytecode.simplifiedDescriptorParams(node.desc).length
    )
    val a2 = if (node.invisibleParameterAnnotations == null) Nil else paramAnn(
      node.invisibleAnnotableParameterCount, node.invisibleParameterAnnotations, idx, Bytecode.simplifiedDescriptorParams(node.desc).length
    )
    Seq.newBuilder.addAll(a1).addAll(a2).result()
  }
  
  private def paramAnn(amount: Int, array: Array[java.util.List[AnnotationNode]], idx: Int, maxIdx: Int): Seq[AnnotationNode] = {
    val off = if (amount == 0) 0 else maxIdx - amount
    val target = idx + off
    if (array.indices.contains(target)) {
      Option(array(target)).map(_.asScala).getOrElse(Nil).toSeq
    } else {
      Nil
    }
  }
}

object NullChecker {

  val NULL_ANNOTATIONS: Set[String] = Set(
    "Ljavax/annotation/Nullable;",
    "Lorg/jetbrains/annotations/Nullable;"
  )
}
