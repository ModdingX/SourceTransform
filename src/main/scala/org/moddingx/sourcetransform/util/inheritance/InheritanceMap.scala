package org.moddingx.sourcetransform.util.inheritance

import org.moddingx.sourcetransform.util.{Bytecode, Util}
import net.minecraftforge.srgutils.{IMappingBuilder, IMappingFile}
import org.objectweb.asm.{Opcodes, Type}

import java.lang.reflect.Modifier
import scala.collection.mutable

import scala.jdk.CollectionConverters.given

class InheritanceMap(private val classSet: Set[ClassInfo]) {

  val classes: Map[String, ClassInfo] = Util.groupDistinct(classSet, _.name)

  private lazy val overridingMap: Map[Bytecode.Method, Set[Bytecode.Method]] = classSet
    .flatMap(cls => cls.methodMap)
    .flatMap(entry => {
      val (method, info) = entry
      info.overrides.map(overridden => (overridden, method))
    })
    .groupMap(pair => pair._1)(pair => pair._2)

  private val overrideGroups = mutable.Map[Bytecode.Method, Set[Bytecode.Method]]()

  private def fieldInfo(fd: Bytecode.Field): Option[FieldInfo] = classes.get(fd.cls).flatMap(info => info.fields.get(fd.name))
  private def methodInfo(md: Bytecode.Method): Option[MethodInfo] = classes.get(md.cls).flatMap(info => info.methods.get((md.name, md.desc)))

  def remap(mappings: IMappingFile): InheritanceMap = new InheritanceMap(classSet.map(_.remap(mappings)))

  def isSubType(child: String, parent: String): Boolean = isSubType(Type.getType(child), Type.getType(parent))

  def isSubType(child: Type, parent: Type): Boolean = {
    if (child.getSort == parent.getSort) {
      child.getSort match {
        case Type.ARRAY if child.getDimensions == parent.getDimensions => isSubType(child.getElementType, parent.getElementType)
        case Type.ARRAY if child.getDimensions > parent.getDimensions =>
          // A higher dimensional array is a subtype of a lower dimensional Object array
          parent.getElementType.getSort == Type.OBJECT && parent.getElementType.getInternalName == Bytecode.ROOT
        case Type.ARRAY => false
        case Type.OBJECT => isSubClass(child.getInternalName, parent.getInternalName)
        case Type.METHOD =>
          // Get whether the child descriptor can override the parent in source code
          child.getArgumentTypes.toVector == parent.getArgumentTypes.toVector && isSubType(child.getReturnType, parent.getReturnType)
        case _ => child == parent
      }
    } else {
      child.getSort match {
        case Type.ARRAY if parent.getSort == Type.OBJECT =>
          // Any array is a subtype of Object
          parent.getInternalName == Bytecode.ROOT
        case _ => false
      }
    }
  }

  def isSubClass(cls: String, parent: String): Boolean = (cls, parent) match {
    case (_, Bytecode.ROOT) => true
    case (Bytecode.ROOT, _) => false
    case (cls, parent) if cls == parent => true
    case (cls, parent) => classes.get(cls) match {
      case Some(info) => isSubClass(info.parent, parent) || info.interfaces.exists(iface => isSubClass(iface, parent))
      case None => false
    }
  }

  def getAllSuperClasses(cls: String): Set[String] = cls match {
    case Bytecode.ROOT => Set()
    case _ => classes.get(cls) match {
      case Some(info) => Set.concat(
        Set(info.parent),
        info.interfaces,
        getAllSuperClasses(info.parent),
        info.interfaces.flatMap(iface => getAllSuperClasses(iface))
      )
      case None => Set(Bytecode.ROOT)
    }
  }
  
  def getOuterClass(cls: String): Option[String] = classes.get(cls).flatMap(info => info.nestedIn)
  def getSimpleName(cls: String): String = getOuterClass(cls) match {
    case Some(outer) if cls.startsWith(outer + "$") => cls.substring(outer.length + 1)
    case _ if cls.contains("/") => cls.substring(cls.lastIndexOf('/') + 1)
    case _ => cls
  }
  def getSourceName(cls: String): Option[String] = {
    val simpleName = getSimpleName(cls)
    if simpleName.toIntOption.isDefined then None else Some(simpleName)
  }

  def getPackageMembers(pkg: String): Set[String] = classes.keySet.filter(name => name.startsWith(pkg + "/"))

  def getClassFields(cls: String): Set[Bytecode.Field] = classes.get(cls).map(info => info.fieldMap.keySet).getOrElse(Set())
  def getClassMethods(cls: String): Set[Bytecode.Method] = classes.get(cls).map(info => info.methodMap.keySet).getOrElse(Set())
  def getClassMembers(cls: String): Set[Bytecode.Member] = classes.get(cls).map(info => Set.concat(info.fieldMap.keys, info.methodMap.keys)).getOrElse(Set())

  def getImplementedLambdas(method: Bytecode.Method): Set[Bytecode.Lambda] = methodInfo(method).map(_.implementsLambdas).getOrElse(Set())
  def getParam(method: Bytecode.Method, idx: Int): Option[String] = methodInfo(method).flatMap(_.params.get(idx))
  def getParams(method: Bytecode.Method): Map[Int, String] = methodInfo(method).map(info => info.params).getOrElse(Map())

  // Whether the method is never overridden and overrides nothing.
  def isStandalone(method: Bytecode.Method): Boolean = methodInfo(method).forall(_.overrides.isEmpty) && !overridingMap.contains(method)
  
  // All methods, the given method overrides
  def getOverriddenMethods(method: Bytecode.Method): Set[Bytecode.Method] = methodInfo(method).map(_.overrides).getOrElse(Set())

  // All methods, that override the given method
  def getOverridingMethods(method: Bytecode.Method): Set[Bytecode.Method] = overridingMap.getOrElse(method, Set())

  // All methods that override each other / are overridden by each other
  // A group of methods can only be remapped to the same name or stuff breaks
  def getOverrideGroup(method: Bytecode.Method): Set[Bytecode.Method] = this.synchronized {
    if (overrideGroups.contains(method)) {
      overrideGroups(method)
    } else {
      val allMethods = mutable.Set[Bytecode.Method]()
      def addMethod(md: Bytecode.Method): Unit = {
        if (!allMethods.contains(md)) {
          allMethods.addOne(md)
          getOverriddenMethods(md).foreach(addMethod)
          getOverridingMethods(md).foreach(addMethod)
        }
      }
      addMethod(method)
      // Only convert to set once and use the same object as map entry for all keys
      val result = allMethods.toSet
      result.foreach(key => overrideGroups.put(key, result))
      result
    }
  }

  def isSource(cls: String): Boolean = classes.get(cls).exists(_.source)
  def isSource(member: Bytecode.Member): Boolean = isSource(member.cls)
  lazy val sourceClasses: Set[String] = classes.values.filter(_.source).map(_.name).toSet

  def is(cls: String, access: Int): Boolean = classes.get(cls).exists(info => (info.access & access) == access)
  def is(member: Bytecode.Member, access: Int): Boolean = member match {
    case fd @ Bytecode.Field(_, _) => fieldInfo(fd).exists(info => (info.access & access) == access)
    case md @ Bytecode.Method(cls, _, _) => methodInfo(md).exists(info => {
      // Mark non-static methods of final classes as final
      val methodAccess = if ((info.access & Opcodes.ACC_STATIC) == 0 && is(cls, Opcodes.ACC_FINAL)) {
        info.access | Opcodes.ACC_FINAL
      } else {
        info.access
      }
      (methodAccess & access) == access
    })
  }

  def isInterface(cls: String): Boolean = is(cls, Opcodes.ACC_INTERFACE)
  def isEnum(cls: String): Boolean = isSubClass(cls, "java/lang/Enum")
  def isRecord(cls: String): Boolean = isSubClass(cls, "java/lang/Record")
  
  def getDescriptor(member: Bytecode.Member): Option[String] = member match {
    case fd @ Bytecode.Field(_, _) => fieldInfo(fd).map(info => info.desc)
    case Bytecode.Method(_, _, desc) => Some(desc)
  }

  // Adds static markers and field desc
  // Assumes that the inheritance map has the data of original name-set
  def addData(mappings: IMappingFile): IMappingFile = {
    def copyMeta(node: IMappingFile.INode, add: (String, String) => Unit): Unit = for ((key: String, value: String) <- node.getMetadata.asScala) add(key, value)
    
    val builder = IMappingBuilder.create("from", "to")
    
    for (pkg: IMappingFile.IPackage <- mappings.getPackages.asScala) {
      val pkgBuilder = builder.addPackage(pkg.getOriginal, pkg.getMapped)
      copyMeta(pkg, pkgBuilder.meta)
    }
    
    for (cls: IMappingFile.IClass <- mappings.getClasses.asScala) {
      val clsBuilder = builder.addClass(cls.getOriginal, cls.getMapped)
      copyMeta(cls, clsBuilder.meta)
      
      for (fd: IMappingFile.IField <- cls.getFields.asScala) {
        val fdBuilder = clsBuilder.field(fd.getOriginal, fd.getMapped)
        copyMeta(fd, fdBuilder.meta)
        if (fd.getDescriptor != null) {
          fdBuilder.descriptor(fd.getDescriptor)
        } else getDescriptor(Bytecode.Field(cls.getOriginal, fd.getOriginal)) match {
          case Some(desc) => fdBuilder.descriptor(desc)
          case None =>
        }
      }
      
      for (md: IMappingFile.IMethod <- cls.getMethods.asScala) {
        val mdBuilder = clsBuilder.method(md.getDescriptor, md.getOriginal, md.getMapped)
        copyMeta(md, mdBuilder.meta)
        if (!md.getMetadata.containsKey("is_static") && is(Bytecode.Method(cls.getOriginal, md.getOriginal, md.getDescriptor), Opcodes.ACC_STATIC)) {
          mdBuilder.meta("is_static", "true")
        }
        
        for (param: IMappingFile.IParameter <- md.getParameters.asScala) {
          val paramBuilder = mdBuilder.parameter(param.getIndex, param.getOriginal, param.getMapped)
          copyMeta(param, paramBuilder.meta)
        }
      }
    }
    
    builder.build().getMap("from", "to")
  }
}
