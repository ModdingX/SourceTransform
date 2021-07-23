package io.github.noeppi_noeppi.tools.sourcetransform.transform

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.{FieldInfo, InheritanceMap, MethodInfo, ParamInfo}
import net.minecraftforge.srgutils.{IMappingBuilder, IMappingFile}

import scala.collection.mutable

// package transforms go after class transforms
class Transformation(private val inheritance: InheritanceMap, private val mappings: Option[IMappingFile]) {
  
  private val classes = mutable.Map[String, String]()
  private val fields = mutable.Map[FieldInfo, String]()
  private val methods = mutable.Map[MethodInfo, String]()
  private val params = mutable.Map[ParamInfo, String]()
  
  def transformClass(from: String, to: String): Boolean = {
    if (classes.contains(from)) {
      false
    } else {
      classes.put(from, to)
      true
    }
  }
  
  def transformField(from: FieldInfo, to: String): Boolean = {
    if (fields.contains(from)) {
      false
    } else {
      fields.put(from, to)
      true
    }
  }
  
  def transformMethod(from: MethodInfo, to: String): Boolean = {
    if (inheritance.getOverridden(from).nonEmpty) {
      // We can't rename methods that override other methods.
      false
    } else if (methods.contains(from)) {
      false
    } else {
      // We don't just need to transform that one method. If there exists a method that overrides
      // this method and a distinct in one, we need to rename the distinct other method as well.
      val overriding = getAllOverriding(from)
      val base = overriding.flatMap(m => getOverriddenBase(m)).excl(from)
      if (base.forall(m => !methods.contains(m))) {
        methods.put(from, to)
        base.foreach(m => methods.put(m, to))
        true
      } else {
        false
      }
    }
  }
  
  def transformParam(from: ParamInfo, to: String): Boolean = {
    if (params.contains(from)) {
      false
    } else {
      params.put(from, to)
      true
    }
  }

  private def getAllOverriding(method: MethodInfo): Set[MethodInfo] = {
    val overriding = inheritance.getOverriding(method)
    val sub = overriding.flatMap(getAllOverriding)
    Set.newBuilder.addAll(overriding).addAll(sub).result()
  }

  private def getOverriddenBase(method: MethodInfo): Set[MethodInfo] = {
    val overridden = inheritance.getOverridden(method)
    val base = overridden.find(m => inheritance.getOverridden(m).isEmpty)
    val other = overridden.flatMap(m => getOverriddenBase(m))
    Set.newBuilder.addAll(base).addAll(other).result()
  }
  
  def build(): IMappingFile = {
    val builder = IMappingBuilder.create("from", "to")
    val overrideMethods = mutable.Map[MethodInfo, String]()
    addMethodOverridesFromMappings(overrideMethods)
    addMethodOverridesFromInheritance(overrideMethods)
    val allClasses = Set.newBuilder
      .addAll(classes.keySet)
      .addAll(fields.map(_._1.cls))
      .addAll(methods.map(_._1.cls))
      .addAll(overrideMethods.map(_._1.cls))
      .addAll(params.map(_._1.method.cls))
      .result()
    val classMap = buildFullyRenamedClassMap(classes.toMap)
    val fieldMap = fields.toMap.groupBy(_._1.cls)
    val methodMap = methods.toMap.groupBy(_._1.cls)
    val overrideMap = overrideMethods.toMap.groupBy(_._1.cls)
    val paramMap = params.toMap.groupBy(_._1.method)
    val paramMethodMap = paramMap.keySet.groupBy(_.cls)
      allClasses.foreach(clsName => {
      val cls = builder.addClass(clsName, classMap.getOrElse(clsName, clsName))
      fieldMap.getOrElse(clsName, Map()).foreach(field => {
        cls.field(field._1.name, field._2)
      })
      val clsMethods: Map[MethodInfo, String] = methodMap.getOrElse(clsName, Map())
      val clsOverrides: Map[MethodInfo, String] = overrideMap.getOrElse(clsName, Map())
      val methodsWithParams: Map[MethodInfo, String] = paramMethodMap.getOrElse(clsName, Set()).map(e => (e, e.name)).toMap
      val allMethods = Set.newBuilder
        .addAll(clsMethods.keySet)
        .addAll(clsOverrides.keySet)
        .addAll(methodsWithParams.keySet)
        .result()
      allMethods.foreach(mid => {
        clsMethods.get(mid)
          .orElse(clsOverrides.get(mid))
          .orElse(methodsWithParams.get(mid))
          .map(name => (mid, name)) match {
          case Some(method) =>
            val m = cls.method(method._1.signature, method._1.name, method._2)
            addParameters(m, paramMap.getOrElse(method._1, Map()))
          case None =>
        }
      })
    })
    builder.build().getMap("from", "to")
  }
  
  def buildFullyRenamedClassMap(classes: Map[String, String]): Map[String, String] = {
    // As we only renamed the source part of the class (the part after the last $ in the class name) we
    // need to match nested classes up with their enclosing class renames here
    val fullyRenamed = mutable.Map[String, String]()
    def findRename(cls: String): String = {
      fullyRenamed.getOrElseUpdate(cls, {
        val name = classes.getOrElse(cls, cls)
        // Front part of renamed class always stays the same. We use it now to look up the new name for that class
        val className = if (name.contains("/")) cls.substring(name.lastIndexOf('/') + 1) else cls
        val packageName = if (name.contains("/")) Some(cls.substring(0, name.lastIndexOf('/'))) else None
        val sourceName = if (className.contains("$")) className.substring(className.lastIndexOf('$') + 1) else className
        val enclosingName = if (className.contains("$")) Some(className.substring(0, className.lastIndexOf('$'))) else None
        if (enclosingName.isDefined) {
          findRename(packageName.map(_ + "/").getOrElse("") + enclosingName.get) + "$" + sourceName
        } else {
          name
        }
      })
    }
    classes.keySet.foreach(cls => findRename(cls))
    fullyRenamed.toMap
  }
  
  def addMethodOverridesFromMappings(map: mutable.Map[MethodInfo, String]): Unit = {
    mappings match {
      case Some(mappings) =>
        inheritance.methods.foreach(method => {
          inheritance.getOverridden(method).flatMap(m => getMappedName(m, mappings)).headOption match {
            case Some(name) => map.put(method, name)
            case None =>
          }
        })
      case None =>
    }
  }
  
  def addMethodOverridesFromInheritance(map: mutable.Map[MethodInfo, String]): Unit = {
    val processed = mutable.Set[MethodInfo]()
    def process(method: MethodInfo): Unit = {
      if (!processed.contains(method) && !map.contains(method)) {
        val overridden = inheritance.getOverridden(method)
        overridden.foreach(m => process(m))
        overridden.flatMap(m => map.get(m)).headOption match {
          case Some(name) => map.put(method, name)
          case None =>
        }
        processed.addOne(method)
      }
    }
    inheritance.methods.foreach(process)
  }
  
  def addParameters(method: IMappingBuilder.IMethod, params: Map[ParamInfo, String]): Unit = {
    val indices = mutable.Set[Int]()
    params.foreach(param => {
      if (!indices.contains(param._1.idx)) {
        method.parameter(param._1.idx, param._1.name, param._2)
        indices.addOne(param._1.idx)
      }
    })
  }
  
  def getMappedName(method: MethodInfo, mappings: IMappingFile): Option[String] = {
    Option(mappings.getClass(method.cls)).flatMap(cls => Option(cls.getMethod(method.name, method.signature)).map(_.getMapped))
  }
}
