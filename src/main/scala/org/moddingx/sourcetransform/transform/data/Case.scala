package org.moddingx.sourcetransform.transform.data

import java.util.Locale
import scala.collection.mutable

sealed trait Case {
  def fromSnakeCase(str: String): String
}

object Case {
  
  // Gets case and transform to snake case
  def findCase(str: String): (Case, String) = {
    if (str.toUpperCase(Locale.ROOT) == str) {
      (UPPER_SNAKE_CASE, str.toLowerCase(Locale.ROOT))
    } else {
      if (str.contains("_")) {
        if (str.nonEmpty && str.head.isUpper) {
          (CAPITALIZED_SNAKE_CASE, str.toLowerCase(Locale.ROOT))
        } else {
          (SNAKE_CASE, str.toLowerCase(Locale.ROOT))
        }
      } else {
        if (str.nonEmpty && str.head.isUpper) {
          (UPPER_CAMEL_CASE, transformAnyCamelCase(str))
        } else {
          (CAMEL_CASE, transformAnyCamelCase(str))
        }
      }
    }
  }
  
  private def transformAnyCamelCase(str: String): String = {
    val sb = new mutable.StringBuilder
    for (c <- str) {
      if (c.isUpper) {
        sb.append('_')
      }
      sb.append(c.toLower)
    }
    sb.toString().dropWhile(_ == '_')
  }
  
  case object CAMEL_CASE extends Case {
    override def fromSnakeCase(str: String): String = {
      var upper = false
      val sb = new mutable.StringBuilder
      for (c <- str) {
        if (c == '_') {
          upper = true
        } else if (upper) {
          sb.append(c.toUpper)
          upper = false
        } else {
          sb.append(c)
        }
      }
      sb.toString()
    }
  }
  
  case object UPPER_CAMEL_CASE extends Case {
    override def fromSnakeCase(str: String): String = {
      val r = CAMEL_CASE.fromSnakeCase(str)
      if (r.nonEmpty) "" + r.head.toUpper + r.tail else r
    }
  }
  
  case object SNAKE_CASE extends Case {
    override def fromSnakeCase(str: String): String = str
  }
  
  case object UPPER_SNAKE_CASE extends Case {
    override def fromSnakeCase(str: String): String = str.toUpperCase(Locale.ROOT)
  }
  
  case object CAPITALIZED_SNAKE_CASE extends Case {
    override def fromSnakeCase(str: String): String = {
      var upper = true
      val sb = new mutable.StringBuilder
      for (c <- str) {
        if (c == '_') {
          upper = true
          sb.append('_')
        } else if (upper) {
          sb.append(c.toUpper)
          upper = false
        } else {
          sb.append(c)
        }
      }
      sb.toString()
    }
  }
}
