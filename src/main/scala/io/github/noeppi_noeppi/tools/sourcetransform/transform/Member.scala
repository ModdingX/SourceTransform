package io.github.noeppi_noeppi.tools.sourcetransform.transform

trait Member {
  
}

object Member {
  
  def read(str: String): Member = {
    if (str.contains("#")) {
      val args = str.substring(str.indexOf('#') + 1).strip()
      Method(str.substring(0, str.indexOf('#')), if (args.isEmpty) None else Some(args.toInt))
    } else {
      Field(str)
    }
  }
  
  case class Field(name: String) extends Member
  case class Method(name: String, args: Option[Int]) extends Member
}
