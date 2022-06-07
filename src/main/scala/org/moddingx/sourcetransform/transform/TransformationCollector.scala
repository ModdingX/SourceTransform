package org.moddingx.sourcetransform.transform

import org.moddingx.sourcetransform.util.Bytecode
import org.moddingx.sourcetransform.util.inheritance.InheritanceMap
import net.minecraftforge.srgutils.{IMappingBuilder, IMappingFile}
import org.moddingx.sourcetransform.util.Bytecode.Method
import org.objectweb.asm.Opcodes

import scala.collection.mutable
import scala.jdk.CollectionConverters.given

class TransformationCollector(private val inheritance: InheritanceMap, private val mappings: Option[IMappingFile]) {

  case class Parameter(method: Method, idx: Int)
  
  private val classes = mutable.Map[String, String]()
  private val fields = mutable.Map[Bytecode.Field, String]()
  private val methods = mutable.Map[Bytecode.Method, String]()
  private val params = mutable.Map[Parameter, String]()

  private def canTransformClass(cls: String): Boolean = inheritance.isSource(cls) && {mappings match {
    case Some(mf) => mf.getClass(cls) == null
    case None => true
  }}
  
  private def canTransformField(field: Bytecode.Field): Boolean = inheritance.isSource(field) && {mappings match {
    case Some(mf) if mf.getClass(field.cls) != null => mf.getClass(field.cls).getField(field.name) == null
    case _ => true
  }}
  
  private def canTransformMethod(method: Bytecode.Method): Boolean = inheritance.isSource(method) && {mappings match {
    case Some(mf) if mf.getClass(method.cls) != null => mf.getClass(method.cls).getMethod(method.name, method.desc) == null
    case _ => true
  }}
  
  def transformClass(cls: String, newName: String): Boolean = {
    if (classes.contains(cls) || !canTransformClass(cls)) {
      false
    } else {
      classes.put(cls, newName)
      true
    }
  }

  def transformField(field: Bytecode.Field, newName: String): Boolean = {
    if (fields.contains(field) || !canTransformField(field) || inheritance.is(field, Opcodes.ACC_ENUM)) {
      false
    } else {
      fields.put(field, newName)
      true
    }
  }

  def transformMethod(method: Bytecode.Method, newName: String): Boolean = {
    if (methods.contains(method) || !canTransformMethod(method) || method.name == "<init>" || method.name == "<clinit>") {
      false
    } else if (inheritance.getOverriddenMethods(method).nonEmpty) {
      // We can't rename methods that override other methods.
      false
    } else {
      // Transform the whole override group
      val allMethods = inheritance.getOverrideGroup(method)
      if (allMethods.forall(m => !methods.contains(m) && canTransformMethod(method))) {
        methods.put(method, newName)
        allMethods.foreach(m => methods.put(m, newName))
        true
      } else {
        false
      }
    }
  }
  
  private def forceTransformMethod(method: Bytecode.Method, newName: String): Unit = {
    inheritance.getOverrideGroup(method)
      .filter(m => inheritance.isSource(m.cls))
      .foreach(m => methods.put(m, newName))
  }

  def transformParam(method: Bytecode.Method, idx: Int, newName: String): Boolean = {
    if (params.contains(Parameter(method, idx)) || !canTransformMethod(method)) {
      false
    } else {
      params.put(Parameter(method, idx), newName)
      true
    }
  }

  private def addTransformationsFromMappings(): Unit = mappings match {
    case Some(m) =>
      // Rename everything that overrides stuff from the mappings
      for (
        cls: IMappingFile.IClass <- m.getClasses.asScala;
        md: IMappingFile.IMethod <- cls.getMethods.asScala;
        method = Bytecode.Method(cls.getOriginal, md.getOriginal, md.getDescriptor)
        if !inheritance.isStandalone(method)
      ) forceTransformMethod(method, md.getMapped)
    case None =>
  }
  
  def build(includeParams: Boolean): IMappingFile = {
    addTransformationsFromMappings()
    
    val allClasses = Set.newBuilder[String]
      .addAll(classes.keys)
      .addAll(fields.keys.map(_.cls))
      .addAll(methods.keys.map(_.cls))
      .addAll(if includeParams then params.keys.map(_.method.cls) else Set())
      .result()
      .filter(cls => inheritance.isSource(cls))
    
    val fieldMap: Map[String, Map[Bytecode.Field, String]] = fields.toMap.groupBy(_._1.cls)
    val methodMap: Map[String, Map[Bytecode.Method, String]] = methods.toMap.groupBy(_._1.cls)
    val paramMap: Map[Bytecode.Method, Map[Parameter, String]] = if includeParams then params.toMap.groupBy(_._1.method) else Map()
    val methodsFromParams: Map[String, Set[Bytecode.Method]] = paramMap.keySet.groupBy(_.cls)
    
    val builder = IMappingBuilder.create("from", "to")
    for (clsName <- allClasses) {
      val cls: IMappingBuilder.IClass = builder.addClass(clsName, getFullTransformedClassName(clsName))
      fieldMap.get(clsName) match {
        case Some(fields) => for (fd <- fields) cls.field(fd._1.name, fd._2)
        case None =>
      }
      val allMethods = Set.newBuilder[Bytecode.Method]
        .addAll(methodMap.getOrElse(clsName, Set[Nothing]()).map(_._1))
        .addAll(methodsFromParams.getOrElse(clsName, Set[Nothing]()))
        .result()
      for (md <- allMethods) {
        val methodBuilder = methods.get(md) match {
          case Some(newName) => cls.method(md.desc, md.name, newName)
          case None => cls.method(md.desc, md.name, md.name)
        }
        for ((Parameter(_, idx), newName) <- paramMap.getOrElse(md, Set[Nothing]())) {
          methodBuilder.parameter(idx, inheritance.getParam(md, idx).getOrElse("o" + idx), newName)
        }
      }
    }
    builder.build().getMap("from", "to")
  }
  
  private def getFullTransformedClassName(cls: String): String = {
    mappings match {
      case Some(mf) if mf.getClass(cls) != null => mf.remapClass(cls)
      case _ => inheritance.getOuterClass(cls) match {
        case Some(oc) if cls.startsWith(oc + "$") => getFullTransformedClassName(oc) + "$" + getSimpleTransformedClassName(cls)
        case _ if classes.contains(cls) => getPackageName(cls) + getSimpleTransformedClassName(cls)
        case _ => cls
      }
    }
  }
  
  private def getPackageName(cls: String) = if cls.contains("/") then cls.substring(0, cls.lastIndexOf('/') + 1) else ""
  private def getSimpleTransformedClassName(cls: String): String = classes.getOrElse(cls, inheritance.getSimpleName(cls))
}
