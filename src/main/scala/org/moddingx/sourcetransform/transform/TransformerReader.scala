package org.moddingx.sourcetransform.transform

import com.google.gson.JsonObject
import org.moddingx.sourcetransform.transform.data.{TransformMember, TransformTarget, Transformer}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.given

object TransformerReader {

  val MIN_API: Int = 1
  val MAX_API: Int = 1

  val BOXED_TYPES: Map[String, String] = Map(
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
  
  def read(json: JsonObject): Seq[ConfiguredTransformer] = {
    val version = json.get("api").getAsInt
    if (version < MIN_API) throw new IllegalStateException("Can't read transform list: " + version + " not supported. Current min ist " + MIN_API + ".")
    if (version > MAX_API) throw new IllegalStateException("Can't read transform list: " + version + " not supported. Current max ist " + MAX_API + ".")
    val list = json.get("transformers").getAsJsonArray
    val builder = Seq.newBuilder[ConfiguredTransformer]
    for (elem <- list.asScala) {
      val obj = elem.getAsJsonObject
      if (obj.has("type") && obj.has("types")) throw new IllegalStateException("A transformer cannot have the two attributes `type` and `types` at the same time.")
      if (obj.has("member") && obj.has("members")) throw new IllegalStateException("A transformer cannot have the two attributes `member` and `members` at the same time.")
      
      val transformer: Transformer = Transformer.read(obj.get("transformer").getAsString)
      
      val targets: Set[TransformTarget] = if (!obj.has("targets")) TransformTarget.targets
        else obj.get("targets").getAsJsonArray.asScala.map(e => TransformTarget.byId(e.getAsString)).toSet
      
      val types: Set[String] = if (obj.has("type")) Set(obj.get("type").getAsString)
        else if (obj.has("types")) obj.get("types").getAsJsonArray.asScala.map(_.getAsString).toSet
        else Set()
      
      val members: Set[TransformMember] = if (obj.has("member")) Set(TransformMember.read(obj.get("member").getAsString))
        else if (obj.has("members")) obj.get("members").getAsJsonArray.asScala.map(e => TransformMember.read(e.getAsString)).toSet
        else Set()
      
      val exactType = obj.has("exact_type") && obj.get("exact_type").getAsBoolean
      
      builder.addOne(new ConfiguredTransformer(transformer, targets, expandTypes(types), members, exactType))
    }
    builder.result()
  }

  private def expandTypes(types: Set[String]): Set[String] = {
    types.flatMap(t => BOXED_TYPES
      .get(t.dropWhile(_ == '['))
      .map(e => t.takeWhile(_ == '[') + e)
      .map(e => Set(t, e))
      .getOrElse(Set(t))
    )
  }
}
