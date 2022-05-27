package io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance

import io.github.noeppi_noeppi.tools.sourcetransform.util.{Bytecode, CommonParsers, IOUtil, Util}

import java.io.{Reader, Writer}
import java.util.Locale

object InheritanceIO {

  def read(reader: Reader): InheritanceMap = {
    new InheritanceMap(Parsers.parseIt(Parsers.classes, reader))
  }
  
  private object Parsers extends CommonParsers {
    
    def access: Parser[Int] = "@" ~> "[\\da-fA-F]+".r ^^ (x => Integer.parseInt(x, 16))
    def index: Parser[Int] = "#" ~> "\\d+".r ^^ (x => Integer.parseInt(x))
    
    def classes: Parser[Set[ClassInfo]] = rep(cls) ^^ (x => x.toSet)
    def cls: Parser[ClassInfo] = classType ~ access ~ class_entry ~ opt(extendsClause) ~ opt(implementsClause) ~ classBlock ^^ { case source ~ access ~ name ~ parent ~ interfaces ~ content => ClassInfo(
      name, access, source,
      parent.getOrElse(Bytecode.ROOT),
      interfaces.getOrElse(Set()),
      Util.takeOnce(content.flatMap(_.nest), "A class can only have on nest parent"),
      content.flatMap(_.field).toSet,
      content.flatMap(_.method).toSet
    )}
    
    def classType: Parser[Boolean] = ("source" ^^ (_ => true)) | ("class" ^^ (_ => false))
    def extendsClause: Parser[String] = "extends" ~> class_entry
    def implementsClause: Parser[Set[String]] = "implements" ~> rep1sep(class_entry, ",") ^^ (x => x.toSet)

    def classBlock: Parser[List[ClassContent]] = ("|" ^^ (_ => Nil)) | "{" ~> rep(classContent) <~ "}"
    def classContent: Parser[ClassContent] = nest | field | method | failure("Class content expected")
    def nest: Parser[ClassContent] = "nested" ~> class_entry <~ "|" ^^ (x => ClassContent.Nest(x))
    def field: Parser[ClassContent] = "field" ~> access ~ ident ~ type_entry_nv <~ "|" ^^ { case access ~ name ~ desc => ClassContent.Field(FieldInfo(name, desc, access)) }
    def method: Parser[ClassContent] = "method" ~> access ~ identm ~ mdesc ~ methodBlock ^^ { case access ~ name ~ desc ~ content => ClassContent.Method(MethodInfo(
      name, desc, access,
      content.flatMap(_.param).toMap,
      content.flatMap(_.overriding).toSet,
      content.flatMap(_.lambda).toSet
    )) }
    
    def methodBlock: Parser[List[MethodContent]] = ("|" ^^ (_ => Nil)) | "{" ~> rep(methodContent) <~ "}"
    def methodContent: Parser[MethodContent] = param | overriding | lambda | failure("Method content expected")
    def param: Parser[MethodContent] = "param" ~> index ~ ident <~ "|" ^^ { case idx ~ name => MethodContent.Param((idx, name)) }
    def overriding: Parser[MethodContent] = "overrides" ~> class_entry ~ identm ~ mdesc <~ "|" ^^ { case cls ~ name ~ desc => MethodContent.Overriding(Bytecode.Method(cls, name, desc)) }
    def lambda: Parser[MethodContent] = "lambda" ~> class_entry ~ identm ~ "implements" ~ class_entry <~ "|" ^^ { case cls ~ name ~ _ ~ implementedCls => MethodContent.Lambda(Bytecode.Lambda(cls, name, implementedCls)) }

    sealed trait ClassContent {
      def nest: Option[String] = None
      def field: Option[FieldInfo] = None
      def method: Option[MethodInfo] = None
    }

    object ClassContent {
      case class Nest(value: String) extends ClassContent { override def nest: Option[String] = Some(value) }
      case class Field(value: FieldInfo) extends ClassContent { override def field: Option[FieldInfo] = Some(value) }
      case class Method(value: MethodInfo) extends ClassContent { override def method: Option[MethodInfo] = Some(value) }
    }
    
    sealed trait MethodContent {
      def param: Option[(Int, String)] = None
      def overriding: Option[Bytecode.Method] = None
      def lambda: Option[Bytecode.Lambda] = None
    }
    
    object MethodContent {
      case class Param(value: (Int, String)) extends MethodContent { override def param: Option[(Int, String)] = Some(value) }
      case class Overriding(value: Bytecode.Method) extends MethodContent { override def overriding: Option[Bytecode.Method] = Some(value) }
      case class Lambda(value: Bytecode.Lambda) extends MethodContent { override def lambda: Option[Bytecode.Lambda] = Some(value) }
    }
  }
  
  def write(inheritance: InheritanceMap, writer: Writer): Unit = {
    IOUtil.writeBuffered(writer) { w =>
      inheritance.classes.values.toSeq.sortBy(info => (!info.source, info.name)).foreach(cls => writeClass(cls, w))
    }
  }

  private def writeClass(cls: ClassInfo, writer: Writer): Unit = {
    writer.write(if cls.source then "source" else "class")
    writer.write("@")
    writer.write(Integer.toHexString(cls.access).toUpperCase(Locale.ROOT))
    writer.write(" ")
    writer.write(cls.name)
    if (cls.parent != Bytecode.ROOT) {
      writer.write(" extends ")
      writer.write(cls.parent)
    }
    if (cls.interfaces.nonEmpty) {
      writer.write(" implements ")
      writer.write(cls.interfaces.mkString(", "))
    }
    if (cls.nestedIn.isDefined || cls.fields.nonEmpty || cls.methods.nonEmpty) {
      writer.write(" {\n")
      cls.nestedIn match {
        case Some(nest) => writer.write("\tnested " + nest + " |\n")
        case None =>
      }
      cls.fields.values.toSeq.sortBy(_.name).foreach(info => writeField(info, writer))
      cls.methods.values.toSeq.sortBy(info => (info.name, info.desc)).foreach(info => writeMethod(info, writer))
      writer.write("}\n")
    } else {
      writer.write(" |")
    }
    writer.write("\n")
  }
  
  private def writeField(fd: FieldInfo, writer: Writer): Unit = {
    writer.write("\tfield@")
    writer.write(Integer.toHexString(fd.access).toUpperCase(Locale.ROOT))
    writer.write(" ")
    writer.write(fd.name)
    writer.write(" ")
    writer.write(fd.desc)
    writer.write(" |\n")
  }
  
  private def writeMethod(md: MethodInfo, writer: Writer): Unit = {
    writer.write("\tmethod@")
    writer.write(Integer.toHexString(md.access).toUpperCase(Locale.ROOT))
    writer.write(" ")
    writer.write(md.name)
    writer.write(" ")
    writer.write(md.desc)
    if (md.params.nonEmpty || md.overrides.nonEmpty || md.implementsLambdas.nonEmpty) {
      writer.write(" {\n")
      md.params.toSeq.sortBy(_._1).foreach(entry => {
        val (idx, name) = entry
        writer.write("\t\tparam #" + idx + " " + name + " |\n")
      })
      md.overrides.toSeq.sorted.foreach(method => {
        writer.write("\t\toverrides " + method.cls + " " + method.name + " " + method.desc + " |\n")
      })
      md.implementsLambdas.toSeq.sorted.foreach(lambda => {
        writer.write("\t\tlambda " + lambda.cls + " " + lambda.name + " implements " + lambda.implementedCls + " |\n")
      })
      writer.write("\t}")
    } else {
      writer.write(" |")
    }
    writer.write("\n")
  }
}
