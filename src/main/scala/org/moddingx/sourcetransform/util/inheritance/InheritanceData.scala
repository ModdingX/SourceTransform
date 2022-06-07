package org.moddingx.sourcetransform.util.inheritance

import org.moddingx.sourcetransform.util.{Bytecode, Util}
import net.minecraftforge.srgutils.IMappingFile
import net.minecraftforge.srgutils.IMappingFile.IField

class ClassInfo(
                 val name: String,
                 val access: Int,
                 val source: Boolean,
                 val parent: String,
                 val interfaces: Set[String],
                 val nestedIn: Option[String],
                 private val fieldSet: Set[FieldInfo],
                 private val methodSet: Set[MethodInfo]
               ) {
  
  lazy val fields: Map[String, FieldInfo] = Util.groupDistinct(fieldSet, _.name)
  lazy val methods: Map[(String, String), MethodInfo] = Util.groupDistinct(methodSet, m => (m.name, m.desc))
  
  lazy val fieldMap: Map[Bytecode.Field, FieldInfo] = fields.map(entry => {
    val (fieldName, value) = entry
    (Bytecode.Field(name, fieldName), value)
  })
  
  lazy val methodMap: Map[Bytecode.Method, MethodInfo] = methods.map(entry => {
    val ((methodName, methodDesc), value) = entry
    (Bytecode.Method(name, methodName, methodDesc), value)
  })
  
  def remap(mappings: IMappingFile): ClassInfo = {
    val cls = Option(mappings.getClass(name))
    ClassInfo(
      cls.map(_.getMapped).getOrElse(name),
      access,
      source,
      if parent == Bytecode.ROOT then Bytecode.ROOT else mappings.remapClass(parent),
      interfaces.map(iface => mappings.remapClass(iface)),
      nestedIn.map(nested => mappings.remapClass(nested)),
      fieldSet.map(fd => fd.remap(mappings, cls)),
      methodSet.map(md => md.remap(mappings, cls))
    )
  }
}

class FieldInfo(
                 val name: String,
                 val desc: String,
                 val access: Int
               ) {
  
    def remap(mappings: IMappingFile, cls: Option[IMappingFile.IClass]): FieldInfo = FieldInfo(
      cls.map(_.remapField(name)).getOrElse(name),
      mappings.remapDescriptor(desc),
      access
    )
}

class MethodInfo(
                  val name: String,
                  val desc: String,
                  val access: Int,
                  val params: Map[Int, String],
                  val overrides: Set[Bytecode.Method],
                  val implementsLambdas: Set[Bytecode.Lambda]
                ) {
  
  def remap(mappings: IMappingFile, cls: Option[IMappingFile.IClass]): MethodInfo = {
    val method = cls.map(_.getMethod(name, desc))
    MethodInfo(
      method.map(_.getMapped).getOrElse(name),
      method.map(_.getMappedDescriptor).getOrElse(desc),
      access,
      params.map(param => {
        val (idx, name) = param
        (idx, method.map(_.remapParameter(idx, name)).getOrElse(name))
      }),
      overrides.map(_.remap(mappings)),
      implementsLambdas.map(_.remap(mappings))
    )
  }
}
