package io.github.noeppi_noeppi.tools.sourcetransform.inspect

import org.eclipse.jdt.core.dom.ASTVisitor

trait Inspection {

  def visitor(inspector: Inspector): ASTVisitor
}
