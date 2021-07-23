package io.github.noeppi_noeppi.tools.sourcetransform.transform

import com.google.gson.JsonObject

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object TransformerReader {

  val MIN_API: Int = 1
  val MAX_API: Int = 1
  
  val BOXED_TYPES = Map(
    "Z" -> "Ljava/lang/Boolean;",
    "B" -> "Ljava/lang/Byte;",
    "C" -> "Ljava/lang/Character;",
    "S" -> "Ljava/lang/Short;",
    "I" -> "Ljava/lang/Integer;",
    "J" -> "Ljava/lang/Long;",
    "F" -> "Ljava/lang/Float;",
    "D" -> "Ljava/lang/Double;",
    "V" -> "Ljava/lang/Void;"
  )
  
  private val ALL_TARGETS = Set[TransformTarget](
    TransformTarget.CHILD_CLASS, TransformTarget.UTILITY_CLASS,
    TransformTarget.FIELD, TransformTarget.METHOD,
    TransformTarget.PARAMETER, TransformTarget.LOCAL
  )
  
  def read(json: JsonObject): List[ConfiguredTransformer] = {
    val version = json.get("api").getAsInt
    if (version < MIN_API) throw new IllegalStateException("Can't read transform list: " + version + " not supported. Current min ist " + MIN_API + ".")
    if (version > MAX_API) throw new IllegalStateException("Can't read transform list: " + version + " not supported. Current max ist " + MAX_API + ".")
    val list = json.get("transformers").getAsJsonArray
    val buffer = ListBuffer[ConfiguredTransformer]()
    for (elem <- list.asScala) {
      val obj = elem.getAsJsonObject
      if (obj.has("type") && obj.has("types")) throw new IllegalStateException("A transformer cannot have the two attributes `type` and `types` at the same time.")
      if (obj.has("member") && obj.has("members")) throw new IllegalStateException("A transformer cannot have the two attributes `member` and `members` at the same time.")
      val transformer: Transformer = Transformer.read(obj.get("transformer").getAsString)
      val targets: Set[TransformTarget] = if (!obj.has("targets")) ALL_TARGETS else obj.get("targets").getAsJsonArray.asScala.map(e => TransformTarget.byId(e.getAsString)).toSet
      val types: Set[String] = if (obj.has("type")) Set(obj.get("type").getAsString) else if (obj.has("types")) obj.get("types").getAsJsonArray.asScala.map(_.getAsString).toSet else Set()
      val members: Set[Member] = if (obj.has("member")) Set(Member.read(obj.get("member").getAsString)) else if (obj.has("members")) obj.get("members").getAsJsonArray.asScala.map(e => Member.read(e.getAsString)).toSet else Set()
      val exactType = obj.has("exact_type") && obj.get("exact_type").getAsBoolean
      buffer.addOne(new ConfiguredTransformer(transformer, targets, expandTypes(types), members, exactType))
    }
    buffer.result()
  }
  
  def expandTypes(types: Set[String]): Set[String] = {
    types.flatMap(t => BOXED_TYPES
      .get(t.dropWhile(_ == '['))
      .map(e => t.takeWhile(_ == '[') + e)
      .map(e => Set(t, e))
      .getOrElse(Set(t))
    )
  }
}
