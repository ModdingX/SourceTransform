package org.moddingx.sourcetransform.util

import com.google.gson.{Gson, GsonBuilder}
import joptsimple.util.EnumConverter

import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters.given
import scala.reflect.{ClassTag, classTag}
import scala.util.matching.Regex

object Util {

  val GSON: Gson = {
    val builder = new GsonBuilder
    builder.disableHtmlEscaping
    builder.setLenient()
    builder.setPrettyPrinting()
    builder.create
  }
  
  def groupDistinct[K, V](elems: Iterable[V], keys: V => K): Map[K, V] = {
    val grouped = elems.groupBy(keys)
    grouped.find(_._2.sizeIs != 1) match {
      case Some((key, value)) => throw new IllegalArgumentException("Duplicate map key: " + key + " (of " + value.mkString("[ ", ", ", " ]") + ")")
      case None =>
    }
    grouped.view.mapValues(set => set.head).toMap
  }
  
  def chain[T](first: T, next: T => T*): T = next.foldLeft(first)((current, entry) => entry(current))

  def takeOnce[T](elems: Iterable[T], errorMsg: String = "Multiple Values"): Option[T] = {
    if (elems.isEmpty) None
    else if (elems.sizeIs > 1) throw new IllegalStateException(errorMsg + ": " + elems.mkString("[ ", ", ", " ]"))
    else Some(elems.head)
  }
  
  def reverseMulti[K, V](map: Map[K, Set[V]]): Map[V, Set[K]] = {
    val builder = mutable.Map[V, mutable.Builder[K, Set[K]]]()
    for ((key, values) <- map; value <- values) {
      builder.getOrElseUpdate(value, Set.newBuilder[K]).addOne(key)
    }
    builder.map(entry => (entry._1, entry._2.result())).toMap
  }

  def exit(code: Int): Nothing = {
    System.exit(code)
    throw new Error("System.exit() returned.")
  }

  def enumArg[T <: Enum[T] : ClassTag]: EnumConverter[T] = new ConcreteEnumConverter(classTag[T].runtimeClass.asInstanceOf[Class[T]])
}

class ConcreteEnumConverter[T <: Enum[T]](clazz: Class[T]) extends EnumConverter[T](clazz) {

  override def valuePattern(): String = {
    valueType().getEnumConstants.map(_.name().toLowerCase).mkString("|")
  }
}
