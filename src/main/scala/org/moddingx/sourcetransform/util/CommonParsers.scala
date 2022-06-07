package org.moddingx.sourcetransform.util

import org.apache.commons.text.StringEscapeUtils

import java.io.Reader
import scala.util.parsing.combinator.JavaTokenParsers

class CommonParsers extends JavaTokenParsers {

  def parseIt[T](parser: Parser[T], reader: Reader): T = {
    parseAll(parser, reader) match {
      case Success(x, _) => x
      case NoSuccess(msg, _) => throw new IllegalStateException(msg)
      case _ => throw new IllegalStateException("No match")
    }
  }
  
  def parseLine[T](parser: Parser[T], line: String): Option[T] = {
    if (line.trim.isEmpty) {
      None
    } else {
      parseAll(parser, line) match {
        case Success(x, _) => Some(x)
        case NoSuccess(msg, _) => throw new IllegalStateException(msg + ": " + line)
        case _ => throw new IllegalStateException("No match")
      }
    }
  }
  
  def class_entry: Parser[String] = rep1sep(ident, "/") ^^ (x => x.mkString("/"))

  def type_entry: Parser[String] = type_primitive | "V" | type_class | type_array | failure("Type entry expected.")
  def type_entry_nv: Parser[String] = type_primitive | type_class | type_array | failure("Non-void type entry expected.")
  def mdesc: Parser[String] = "(" ~> rep(type_entry_nv) ~ ")" ~ type_entry ^^ { case args ~ _ ~ ret => args.mkString("(", "", ")") + ret }
  
  private def type_primitive: Parser[String] = "Z" | "B" | "C" | "D" | "F" | "I" | "J" | "S"
  private def type_class: Parser[String] = "L" ~> class_entry <~ ";" ^^ (x => "L" + x + ";")
  private def type_array: Parser[String] = "[" ~> type_entry_nv ^^ (x => "[" + x)
  
  // Identifier part, no check on first char to be an identifier start
  def identp: Parser[String] = rep1(acceptIf(Character.isJavaIdentifierStart)(_ => "Identifier part expected")) ^^ (x => x.mkString(""))
  def identm: Parser[String] = "<init>" | "<clinit>" | ident
  
  def escapedStringLiteral: Parser[String] = stringLiteral ^^ (x => unquote(x))
  
  private def unquote(quoted: String): String = {
    val raw = if (quoted.startsWith("\"") && quoted.endsWith("\"")) {
      quoted.substring(1, quoted.length - 1)
    } else {
      quoted
    }
    StringEscapeUtils.unescapeJava(raw)
  }
}
