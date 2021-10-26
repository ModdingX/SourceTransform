package io.github.noeppi_noeppi.tools.sourcetransform.inspect.inspections

import io.github.noeppi_noeppi.tools.sourcetransform.inspect.value.Contract
import io.github.noeppi_noeppi.tools.sourcetransform.inspect.{Inspection, Inspector, Target}
import org.eclipse.jdt.core.dom.{ASTNode, ASTVisitor, Expression}

case class AssignInspection[T](targets: List[Target], extractor: Expression => Contract[T], value: T, strict: Boolean, message: String) extends Inspection {
  
  override def visitor(inspector: Inspector): ASTVisitor = new ASTVisitor() {
    
    override def preVisit2(node: ASTNode): Boolean = {
      var canDescend = true
      targets.flatMap(_.findUpdate(inspector, node)).foreach(expr => {
        val contract = extractor(expr)
        if ((strict && contract.testStrict(value)) || (!strict && contract.test(value))) {
          inspector.message(node, message)
          canDescend = false
        }
      })
      canDescend
    }
  }
}
