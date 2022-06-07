package org.moddingx.sourcetransform.transform.data

import org.moddingx.sourcetransform.util.CommonParsers
import net.minecraftforge.srgutils.IMappingFile

sealed trait Transformer {

  final def applyTo(name: String): Option[String] = {
    val (c, str) = Case.findCase(name)
    applyToSnakeCase(str).map(c.fromSnakeCase)
  }

  protected def applyToSnakeCase(name: String): Option[String]
  def string(): String
  def remap(mappings: IMappingFile): Transformer = this
}

object Transformer {

  def read(str: String): Transformer = Parsers.parseLine(Parsers.line, str).getOrElse(throw new IllegalStateException("Transformation expected, got empty string."))

  private object Parsers extends CommonParsers {

    def line: Parser[Transformer] = strip | rearrange_r | rearrange_l | replace | failure("Transformation expected")

    def strip: Parser[Transformer] = "-" ~> identp ^^ (x => Strip(x))
    def rearrange_r: Parser[Transformer] = ">" ~> identp ^^ (x => Rearrange(x, toFront = false))
    def rearrange_l: Parser[Transformer] = "<" ~> identp ^^ (x => Rearrange(x, toFront = true))
    def replace: Parser[Transformer] = identp ~ "->" ~ identp ^^ { case from ~ _ ~ to => Replace(from, to) }
  }

  case class Strip(str: String) extends Transformer {

    override def applyToSnakeCase(name: String): Option[String] = {
      if (name.contains(str)) Some(name.replace(str, "").replace("__", "_"))
      else None
    }

    override def string(): String = "-" + str
  }

  case class Replace(from: String, to: String) extends Transformer {

    override def applyToSnakeCase(name: String): Option[String] = {
      if (name.contains(from)) Some(name.replace(from, to))
      else None
    }

    override def string(): String = from + "->" + to
  }

  case class Rearrange(part: String, toFront: Boolean) extends Transformer {

    override def applyToSnakeCase(name: String): Option[String] = {
      if (toFront) {
        if (name.endsWith(part)) {
          if (name.substring(0, name.length - part.length).endsWith("_")) {
            Some(part + "_" + name.substring(0, name.length - (part.length + 1)))
          } else {
            Some(part + name.substring(0, name.length - part.length))
          }
        } else {
          None
        }
      } else {
        if (name.startsWith(part)) {
          if (name.substring(part.length).startsWith("_")) {
            Some(name.substring(part.length + 1) + "_" + part)
          } else {
            Some(name.substring(part.length) + part)
          }
        } else {
          None
        }
      }
    }

    override def string(): String = (if (toFront) "<" else ">") + part
  }
}
