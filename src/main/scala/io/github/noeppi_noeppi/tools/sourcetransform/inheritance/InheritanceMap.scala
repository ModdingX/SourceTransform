package io.github.noeppi_noeppi.tools.sourcetransform.inheritance

import io.github.noeppi_noeppi.tools.sourcetransform.util.{CommonParsers, Util}
import net.minecraftforge.srgutils.IMappingFile

import java.io.{BufferedReader, Writer}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class InheritanceMap private (
                               val classes: Map[String, ClassInfo],
                               val sourceClasses: Set[String],
                               private val fieldsExtended: Map[FieldInfo, Boolean],
                               private val methodsExtended: Map[MethodInfo, Boolean],
                               val params: Set[ParamInfo],
                               private val overrides: Map[MethodInfo, Set[MethodInfo]]
                             ) {
  
  val fields: Set[FieldInfo] = fieldsExtended.keySet
  val methods: Set[MethodInfo] = methodsExtended.keySet
  
  private lazy val fieldMap: Map[String, Map[String, FieldInfo]] = fields.groupBy(_.cls)
    .map(e => (e._1, e._2.groupBy(_.name).map(e => (e._1, e._2.head))))
  
  private lazy val methodMap: Map[String, Map[(String, String), MethodInfo]] = methods.groupBy(_.cls)
    .map(e => (e._1, e._2.groupBy(m => (m.name, m.signature)).map(e => (e._1, e._2.head))))
  
  private lazy val paramMap: Map[MethodInfo, Map[Int, ParamInfo]] = params.groupBy(_.method)
    .map(e => (e._1, e._2.groupBy(_.idx).flatMap(p => p._2.headOption.map(x => (p._1, x)))))
  
  private lazy val overridesReverse: Map[MethodInfo, Set[MethodInfo]] = overrides.toSet.flatMap((e: (MethodInfo, Set[MethodInfo])) => e._2.map(x => (e._1, x)))
    .map(_.swap).groupBy(_._1).map(e => (e._1, e._2.map(_._2)))

  def isSubType(typeElem: String, parent: String): Boolean = {
    if (typeElem.startsWith("[") && parent.startsWith("[")) {
      isSubType(typeElem.substring(1), parent.substring(1))
    } else if (typeElem.startsWith("[") || parent.startsWith("[")) {
      false
    } else if (typeElem.startsWith("L") && typeElem.endsWith(";") && parent.startsWith("L") && parent.endsWith(";")) {
      isSubClass(typeElem.substring(1, typeElem.length - 1), parent.substring(1, parent.length - 1))
    } else {
      typeElem == parent
    }
  }
  
  def isSubClass(cls: String, parent: String): Boolean = {
    if (parent == InheritanceMap.ROOT) {
      true
    } else if (cls == InheritanceMap.ROOT) {
      false
    } else if (cls == parent) {
      true
    } else {
      classes.get(cls).exists(info => isSubClass(info.parent, parent) || info.interfaces.exists(isSubClass(_, parent)))
    }
  }
  
  def getAllSuperClasses(cls: String): List[String] = {
    if (cls == InheritanceMap.ROOT) {
      Nil
    } else if (classes.contains(cls)) {
      val lb = ListBuffer[String]()
      lb.addAll(getAllSuperClasses(classes(cls).parent))
      lb.addAll(classes(cls).interfaces.flatMap(getAllSuperClasses))
      lb.distinct.toList
    } else {
      List(InheritanceMap.ROOT)
    }
  }

  def getClasses(pkg: String): Set[String] = classes.keySet.filter(name => name.startsWith(pkg))

  def getMemberNames(cls: String): Set[String] = {
    val set = mutable.Set[String]()
    fieldMap.get(cls).foreach(map => set.addAll(map.values.map(_.name)))
    methodMap.get(cls).foreach(map => set.addAll(map.values.map(_.name)))
    set.toSet
  }

  def getMemberSignatures(cls: String): Set[String] = {
    val set = mutable.Set[String]()
    fieldMap.get(cls).foreach(map => set.addAll(map.values.map(_.fieldType)))
    methodMap.get(cls).foreach(map => set.addAll(map.values.map(_.signature)))
    set.toSet
  }
  
  def getOverridden(m: MethodInfo): Set[MethodInfo] = overrides.getOrElse(m, Set())
  
  def getOverriding(m: MethodInfo): Set[MethodInfo] = overridesReverse.getOrElse(m, Set())
  
  def isStatic(f: FieldInfo): Boolean = fieldsExtended.getOrElse(f, false)
  
  def isStatic(m: MethodInfo): Boolean = methodsExtended.getOrElse(m, false)
  
  def bytecodeToIdx(m: MethodInfo, bytecodeIdx: Int): Int = {
    var bytecodeCounter = if (isStatic(m)) 0 else 1
    var idxCounter = 0
    val simplified = Util.simplifiedSignatureParams(m.signature)
    while (bytecodeCounter < bytecodeIdx) {
      val simplifiedType = if (idxCounter < simplified.length) simplified.charAt(idxCounter) else 'L'
      bytecodeCounter += (if (simplifiedType == 'J' || simplifiedType == 'D') 2 else 1)
      idxCounter += 1
    }
    idxCounter
  }
  
  def idxToBytecode(m: MethodInfo, idx: Int): Int = {
    var bytecodeCounter = if (isStatic(m)) 0 else 1
    var idxCounter = 0
    val simplified = Util.simplifiedSignatureParams(m.signature)
    while (idxCounter < idx) {
      val simplifiedType = if (idxCounter < simplified.length) simplified.charAt(idxCounter) else 'L'
      bytecodeCounter += (if (simplifiedType == 'J' || simplifiedType == 'D') 2 else 1)
      idxCounter += 1
    }
    idxCounter
  }
  
  def sourcecodeToIdx(m: MethodInfo, sourcecodeIdx: Int): Int = {
    if (isSubClass(m.cls, "java/lang/Enum")) {
      sourcecodeIdx + 2
    } else {
      sourcecodeIdx
    }
  }
  
  def idxToSourcecode(m: MethodInfo, idx: Int): Int = {
    if (isSubClass(m.cls, "java/lang/Enum")) {
      0 max (idx - 2)
    } else {
      idx
    }
  }
  
  def getParam(m: MethodInfo, idx: Int): Option[ParamInfo] = paramMap.getOrElse(m, Map()).get(idx)
  
  def remap(mappings: IMappingFile): InheritanceMap = new InheritanceMap(
    classes.map(e => (mappings.remapClass(e._1), e._2.remap(mappings))),
    sourceClasses.map(cls => mappings.remapClass(cls)),
    fieldsExtended.map(f => (f._1.remap(mappings), f._2)),
    methodsExtended.map(m => (m._1.remap(mappings), m._2)),
    params.map(p => p.remap(mappings)),
    overrides.map(e => (e._1.remap(mappings), e._2.map(m => m.remap(mappings))))
  )
  
  def write(w: Writer): Unit = {
    this.classes.toList.sortBy(x => (!sourceClasses.contains(x._1), x._1)).foreach(e => {
      w.write((if (sourceClasses.contains(e._1)) "source " else "") +  "class " + e._1 + " extends " + e._2.parent + (if (e._2.interfaces.nonEmpty) e._2.interfaces.toList.sorted.mkString(" implements ", ", ", "") else "") + "\n")
    })
    this.fieldsExtended.toList.sorted(InheritanceMap.ExtendedFieldOrdering).foreach(f => {
      w.write((if (f._2) "static " else "") + "field " + f._1.cls + " " + f._1.name + " " + f._1.fieldType + "\n")
    })
    this.methodsExtended.toList.sorted(InheritanceMap.ExtendedMethodOrdering).foreach(m => {
      w.write((if (m._2) "static " else "") + "method " + m._1.cls + " " + m._1.name + " " + m._1.signature + "\n")
    })
    this.params.toList.sorted(InheritanceMap.ParamOrdering).foreach(p => {
      w.write("parameter " + p.method.cls + " " + p.method.name + " " + p.method.signature + " " + p.idx + " " + p.name + "\n")
    })
    this.overrides.toList.sortBy(_._1)(InheritanceMap.MethodOrdering).foreach(e => {
      e._2.toList.sorted(InheritanceMap.MethodOrdering).foreach(m => {
        w.write("override " + e._1.cls + " " + e._1.name + " " + e._1.signature + " from " + m.cls + " " + m.name + " " + m.signature + "\n")
      })
    })
  }
}

object InheritanceMap {
  
  val ROOT: String = "java/lang/Object"

  private implicit val FieldOrdering: Ordering[FieldInfo] = Ordering.by(f => (f.cls, f.name))
  private implicit val MethodOrdering: Ordering[MethodInfo] = Ordering.by(m => (m.cls, m.name, m.signature))
  private implicit val ExtendedFieldOrdering: Ordering[(FieldInfo, Boolean)] = Ordering.by(f => (!f._2, f._1.cls, f._1.name))
  private implicit val ExtendedMethodOrdering: Ordering[(MethodInfo, Boolean)] = Ordering.by(m => (!m._2, m._1.cls, m._1.name, m._1.signature))
  private implicit val ParamOrdering: Ordering[ParamInfo] = Ordering.by(p => (p.method, p.idx))
  
  def read(r: BufferedReader): InheritanceMap = {
    val builder = new Builder
    r.lines().toArray.map(_.toString).map(_.strip()).flatMap(Parsers.parseIt(Parsers.line, _)).foreach(_(builder))
    builder.build()
  }
  
  private object Parsers extends CommonParsers {
    
    type X = Builder => Unit
    
    def line: Parser[X] = class_line | field_line | method_line | param_line | override_line | failure("Inheritance Map line expected.")
    
    def class_line: Parser[X] = opt("source") ~ "class" ~ class_entry ~ "extends" ~ class_entry ~ opt(interfaces) ^^ { case source ~ _ ~ cls ~ _ ~ parent ~ interfaces => builder => builder.cls(cls, Some(parent), interfaces.getOrElse(Set())); if (source.isDefined) builder.src(cls) }
    def interfaces: Parser[Set[String]] = "implements" ~> rep1sep(class_entry, ",") ^^ (x => x.toSet)
    
    def field_line: Parser[X] = opt("static") ~ "field" ~ class_entry ~ ident ~ type_entry_nv ^^ { case isStatic ~ _ ~ cls ~ name ~ fieldType => builder => builder.field(cls, name, fieldType, isStatic.isDefined) }
    def method_line: Parser[X] = opt("static") ~ "method" ~ class_entry ~ identm ~ msig ^^ { case isStatic ~ _ ~ cls ~ name ~ signature => builder => builder.method(cls, name, signature, isStatic.isDefined) }
    def param_line: Parser[X] = "parameter" ~> class_entry ~ identm ~ msig ~ wholeNumber ~ ident ^^ { case cls ~ mname ~ signature ~ idx ~ name => builder => builder.param(cls, mname, signature, idx.toInt, name)}
    def override_line: Parser[X] = "override" ~> class_entry ~ ident ~ msig ~ "from" ~ class_entry ~ ident ~ msig ^^ { case cls ~ name ~ signature ~ _ ~ fromCls ~ fromName ~ fromSig => builder => builder.overrides(MethodInfo(cls, name, signature), MethodInfo(fromCls, fromName, fromSig)) }
  }
  
  class Builder {
    
    private val classes = mutable.Map[String, ClassInfo]()
    private val sourceClasses = mutable.Set[String]()
    private val fields = mutable.Map[FieldInfo, Boolean]()
    private val methods = mutable.Map[MethodInfo, Boolean]()
    private val params = mutable.Set[ParamInfo]()
    private val overrides = mutable.Map[MethodInfo, mutable.Set[MethodInfo]]()
    
    def cls(cls: String, parent: Option[String], interfaces: Set[String]): this.type = {
      classes.put(cls, ClassInfo(parent.getOrElse(InheritanceMap.ROOT), interfaces))
      this
    }
    
    def src(cls: String): this.type = {
      sourceClasses.addOne(cls)
      this
    }
    
    def hasCls(cls: String): Boolean = classes.contains(cls)
    
    def field(cls: String, name: String, fieldType: String, isStatic: Boolean): this.type = {
      fields.addOne(FieldInfo(cls, name, fieldType) -> isStatic)
      this
    }
    
    def method(cls: String, name: String, signature: String, isStatic: Boolean): this.type = {
      if (name != "<clinit>") {
        methods.addOne(MethodInfo(cls, name, signature) -> isStatic)
      }
      this
    }
    
    def param(cls: String, methodName: String, signature: String, idx: Int, name: String): this.type = {
      params.addOne(ParamInfo(MethodInfo(cls, methodName, signature), idx, name))
      this
    }
    
    def overrides(method: MethodInfo, from: MethodInfo): this.type = {
      overrides.getOrElseUpdate(method, mutable.Set()).addOne(from)
      this
    }
    
    def build(): InheritanceMap = new InheritanceMap(classes.toMap, sourceClasses.toSet, fields.toMap, methods.toMap, params.toSet, overrides.map(e => (e._1, e._2.toSet)).toMap)
  }
}

case class ClassInfo(parent: String, interfaces: Set[String]) {
  
  def remap(mappings: IMappingFile): ClassInfo = {
    def rc(cls: String): String = if (cls == InheritanceMap.ROOT) InheritanceMap.ROOT else mappings.remapClass(cls)
    ClassInfo(rc(parent), interfaces.map(rc))
  }
}

case class FieldInfo(cls: String, name: String, fieldType: String) {
  
  def remap(mappings: IMappingFile): FieldInfo = {
    val mappingClass = mappings.getClass(cls)
    if (mappingClass == null) {
      this
    } else {
      FieldInfo(mappingClass.getMapped, mappingClass.remapField(name), mappings.remapDescriptor(fieldType))
    }
  }
}

case class MethodInfo(cls: String, name: String, signature: String) {
  
  def remap(mappings: IMappingFile): MethodInfo = {
    val mappingClass = mappings.getClass(cls)
    if (mappingClass == null) {
      this
    } else {
      MethodInfo(mappingClass.getMapped, mappingClass.remapMethod(name, signature), mappings.remapDescriptor(signature))
    }
  }
}

case class ParamInfo(method: MethodInfo, idx: Int, name: String) {
  
  def remap(mappings: IMappingFile): ParamInfo = {
    val mappingClass = mappings.getClass(method.cls)
    if (mappingClass == null) {
      this
    } else {
      val mappingFunc = mappingClass.getMethod(method.name, method.signature)
      if (mappingFunc == null) {
        this
      } else {
        val m = MethodInfo(mappingClass.getMapped, mappingFunc.getMapped, mappingFunc.getMappedDescriptor)
        ParamInfo(m, idx, mappingFunc.remapParameter(idx, name))
      }
    }
  }
}