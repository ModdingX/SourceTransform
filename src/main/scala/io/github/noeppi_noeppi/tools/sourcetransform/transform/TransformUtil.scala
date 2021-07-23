package io.github.noeppi_noeppi.tools.sourcetransform.transform

object TransformUtil {

  def createTransformer(transformers: List[ConfiguredTransformer])(name: String, target: TransformTarget, predicate: ConfiguredTransformer => Boolean, action: String => Boolean): Option[String] = {
    for (ct <- transformers if predicate(ct)) {
      ct.transform(name, target) match {
        case Some(transformed) =>
          if (action(transformed)) {
            return Some(transformed)
          }
        case None =>
      }
    }
    None
  }
}
