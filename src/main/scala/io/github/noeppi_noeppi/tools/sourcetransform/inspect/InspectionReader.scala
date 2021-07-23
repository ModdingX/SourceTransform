package io.github.noeppi_noeppi.tools.sourcetransform.inspect

import com.google.gson.JsonObject
import io.github.noeppi_noeppi.tools.sourcetransform.inspect.inspections.AssignInspection
import io.github.noeppi_noeppi.tools.sourcetransform.inspect.value.{Contract, NumberRange}
import org.eclipse.jdt.core.dom.Expression

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object InspectionReader {

  val MIN_API: Int = 1
  val MAX_API: Int = 1
  
  def read(json: JsonObject): List[Inspection] = {
    val version = json.get("api").getAsInt
    if (version < MIN_API) throw new IllegalStateException("Can't read inspection list: " + version + " not supported. Current min ist " + MIN_API + ".")
    if (version > MAX_API) throw new IllegalStateException("Can't read inspection list: " + version + " not supported. Current max ist " + MAX_API + ".")
    val list = json.get("inspections").getAsJsonArray
    val buffer = ListBuffer[Inspection]()
    for (elem <- list.asScala) {
      val obj = elem.getAsJsonObject
      buffer.addOne(obj.get("type").getAsString match {
        case "assign" => readAssign(obj)
        case x => throw new IllegalStateException("Can't read inspection: Type not found: " + x)
      })
    }
    buffer.result()
  }
  
  def readAssign(json: JsonObject): AssignInspection[_] = {
    if (json.has("target") && json.has("targets")) throw new IllegalStateException("An assign inspection cannot have the two attributes `type` and `types` at the same time.")
    val targets: List[Target] = if (json.has("target")) List(Target.read(json.get("target").getAsString)) else json.get("targets").getAsJsonArray.asScala.map(_.getAsString).map(Target.read).toList
    val strict = json.has("strict") && json.get("strict").getAsBoolean
    val valueArray = json.get("value").getAsJsonArray
    if (valueArray.size() != 2) throw new IllegalStateException("Invalid value for assign inspection: Expected array length of 2")
    case class ValueData[T](extractor: Expression => Contract[T], value: T)
    val data = valueArray.get(0).getAsString match {
      case "number" => ValueData[NumberRange](e => NumberExprUtil.getNumberContract(e), NumberRange.read(valueArray.get(1).getAsString))
      case x => throw new IllegalStateException("Invalid value for assign inspection: Invalid value type: " + x)
    }
    val message = json.get("message").getAsString
    AssignInspection(targets, data.extractor, data.value, strict, message)
  }
}
