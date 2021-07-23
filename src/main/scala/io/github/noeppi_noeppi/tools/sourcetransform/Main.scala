package io.github.noeppi_noeppi.tools.sourcetransform

import io.github.noeppi_noeppi.tools.sourcetransform.apply.{CommentRenameApply, RenameApply}
import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.{InheritanceBuilder, InheritanceRemapper}
import io.github.noeppi_noeppi.tools.sourcetransform.inspect.SourceInspector
import io.github.noeppi_noeppi.tools.sourcetransform.local.LocalMapCreator
import io.github.noeppi_noeppi.tools.sourcetransform.transform.TransformerApply
import io.github.noeppi_noeppi.tools.sourcetransform.util.Util
import org.eclipse.jdt.core.dom.CompilationUnit

object Main extends App {
  
  if (args.length == 0) {
    println("SourceTransform")
    println()
    println("Choose a sub-command:")
    println("  inheritance   Create an inheritance map based on class files")
    println("  remap         Remap an inheritance map with a mapping file")
    println("  transform     Create some mappings based on a transformer json")
    println("  local         Create a local variable rename map")
    println("  apply         Apply a rename map")
    println("  comments      Apply rename comments written via `apply` to be applied later.")
    println("  inspect       Read inspections from json and apply them to code.")
  } else {
    args(0).toLowerCase match {
      case "inheritance" => InheritanceBuilder.run(args.tail.toIndexedSeq: _*)
      case "remap" => InheritanceRemapper.run(args.tail.toIndexedSeq: _*)
      case "transform" => TransformerApply.run(args.tail.toIndexedSeq: _*)
      case "local" => LocalMapCreator.run(args.tail.toIndexedSeq: _*)
      case "apply" => RenameApply.run(args.tail.toIndexedSeq: _*)
      case "comments" => CommentRenameApply.run(args.tail.toIndexedSeq: _*)
      case "inspect" => SourceInspector.run(args.tail.toIndexedSeq: _*)
      case x => println("Unknown sub-command: '" + x + "'")
    }
  }
}
