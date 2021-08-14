package io.github.noeppi_noeppi.tools.sourcetransform.util

import com.google.gson.{Gson, GsonBuilder}
import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.{InheritanceMap, MethodInfo}
import joptsimple.util.EnumConverter

import java.util
import scala.jdk.CollectionConverters._
import scala.reflect.{ClassTag, classTag}
import scala.util.matching.Regex

object Util {

  val CLS_TYPE: Regex = "L([^;]+);".r
  
  // Extract param list from msig
  val MSIG_PARAMS: Regex = "\\(((?:\\[*(?:[ZBCSIJFD]|L[^;]+;))*)\\)\\[*(?:[ZBCSIJFD]|L[^;]+;)".r
  
  // Extract array from simplified msig (object refs replaced with L)
  val MSIG_SIMPLIFIED_ARR: Regex = "\\[+[ZBCSIJFDL]+".r

  val GSON: Gson = {
    val builder = new GsonBuilder
    builder.disableHtmlEscaping
    builder.setLenient()
    builder.setPrettyPrinting()
    builder.create
  }
  
  def exit(code: Int): Nothing = {
    System.exit(code)
    throw new Error("System.exit returned.")
  }
  
  // Will strip off arrays
  def getParamTypeForMatch(signature: String, idx: Int): String = {
    try {
      var sig = signature
      if (signature.startsWith("(")) sig = sig.substring(1)
      for (_ <- 0 until idx) {
        while (sig.startsWith("[")) sig = sig.substring(1)
        if (sig.startsWith("L")) {
          sig = sig.substring(sig.indexOf(';') + 1)
        } else {
          sig = sig.substring(1)
        }
      }
      val remaining = sig.dropWhile(_ == '[')
      val part = if (sig.startsWith("L")) {
        remaining.substring(0, remaining.indexOf(';') + 1)
      } else {
        remaining.substring(0, 1)
      }
      sig.takeWhile(_ == '[') + part
    } catch {
      case _: IndexOutOfBoundsException => "V"
      case _: NoSuchElementException => "V"
    }
  }
  
  def simplifiedSignatureParams(sig: String): String = sig match {
    case MSIG_PARAMS(params) => MSIG_SIMPLIFIED_ARR.replaceAllIn(CLS_TYPE.replaceAllIn(params, "L"), "[")
    case _ => ""
  }
  
  def returnCls(msig: String): Option[String] = {
    val ret = if (msig.contains(")")) msig.substring(msig.lastIndexOf(')') + 1) else msig
    ret match {
      case CLS_TYPE(t) => Some(t)
      case _ => None
    }
  }

  def enum[T <: Enum[T] : ClassTag]: EnumConverter[T] = new ConcreteEnumConverter(classTag[T].runtimeClass.asInstanceOf[Class[T]])
}

class ConcreteEnumConverter[T <: Enum[T]](clazz: Class[T]) extends EnumConverter[T](clazz) {

  override def valuePattern(): String = {
    util.EnumSet.allOf(valueType()).asScala.map(_.name().toLowerCase).mkString("|")
  }
}


