package io.github.noeppi_noeppi.tools.sourcetransform.util.descriptor

import io.github.noeppi_noeppi.tools.sourcetransform.util.CommonParsers

object DescriptorParser {
  
  def parse(desc: String): ParsedDescriptor = {
    Parsers.parseAll(Parsers.instance, desc) match {
      case Parsers.Success(r, _) => r
      case _ => throw new IllegalStateException("Invalid descriptor: " + desc)
    }
  }
  
  private object Parsers extends CommonParsers {

    def instance: Parser[ParsedDescriptor] = "(" ~> rep(type_entry_nv) ~ ")" ~ type_entry ^^ { case args ~ _ ~ ret => ParsedDescriptor(args, ret) }
  }
}

case class ParsedDescriptor(params: List[String], ret: String) {
  override def toString: String = params.mkString("(", "", ")") + ret
}