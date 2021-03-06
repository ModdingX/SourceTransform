package org.moddingx.sourcetransform

import org.moddingx.sourcetransform.inheritance.{InheritanceBuilder, InheritanceRemapper}
import org.moddingx.sourcetransform.jstype.JavaScriptGenerator
import org.moddingx.sourcetransform.local.LocalVariableRenamer
import org.moddingx.sourcetransform.parchment.ParchmentSanitizer
import org.moddingx.sourcetransform.rename.RenameApply
import org.moddingx.sourcetransform.transform.TransformerApply

object Main {
  
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("SourceTransform")
      println()
      println("Choose a sub-command:")
      println("  inheritance   Create an inheritance map based on class files")
      println("  remap         Remap an inheritance map with a mapping file")
      println("  transform     Create mappings based on a transformer json")
      println("  local         Create a local variable rename map")
      println("  rename        Apply a rename map")
      println("  sanitize      Sanitize a parchment export by given source code so it keeps compilable.")
      println("  jstype        Generate JavaScript/TypeScript definitions for Nashorn")
    } else {
      args(0).toLowerCase match {
        case "inheritance" => InheritanceBuilder.run(args.tail.toIndexedSeq: _*)
        case "remap" => InheritanceRemapper.run(args.tail.toIndexedSeq: _*)
        case "transform" => TransformerApply.run(args.tail.toIndexedSeq: _*)
        case "local" => LocalVariableRenamer.run(args.tail.toIndexedSeq: _*)
        case "rename" => RenameApply.run(args.tail.toIndexedSeq: _*)
        case "sanitize" => ParchmentSanitizer.run(args.tail.toIndexedSeq: _*)
        case "jstype" => JavaScriptGenerator.run(args.tail.toIndexedSeq: _*)
        case x => println("Unknown sub-command: '" + x + "'")
      }
    }
  }
}
