package io.github.noeppi_noeppi.tools.sourcetransform.inspect

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.InheritanceMap
import org.eclipse.jdt.core.compiler.IProblem
import org.eclipse.jdt.core.dom._

import java.io.Writer
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class Inspector(val inheritance: InheritanceMap) {

  private var currentFile: (String, CompilationUnit) = _
  
  private val inspections = mutable.Map[String, mutable.Set[Inspection]]()
  private val paramIndices = mutable.Map[String, Int]()
  
  def source(file: String, cu: CompilationUnit): Unit = {
    currentFile = (file, cu)
  }
  
  def message(node: ASTNode, message: String): Unit = {
    if (currentFile == null) {
      System.err.println("Dropping message: No source file.")
    } else {
      val cu = currentFile._2
      val inspection = Inspection(cu.getLineNumber(node.getStartPosition), cu.getColumnNumber(node.getStartPosition), node.toString, message)
      inspections.getOrElseUpdate(currentFile._1, mutable.Set()).addOne(inspection)
    }
  }
  
  def message(problem: IProblem): Unit = {
    if (currentFile == null) {
      System.err.println("Dropping message: No source file.")
    } else {
      val cu = currentFile._2
      val inspection = Inspection(problem.getSourceLineNumber, cu.getColumnNumber(problem.getSourceStart), "", problem.getMessage)
      inspections.getOrElseUpdate(currentFile._1, mutable.Set()).addOne(inspection)
    }
  }
  
  def print(writer: Writer): Unit = {
    var hasInspections = false
    for ((file, inspections) <- inspections.toList.sortBy(_._1) if inspections.nonEmpty) {
      for (inspection <- inspections.toList.sortBy(e => (e.line, e.column))) {
        hasInspections = true
        writer.write(file + "@" + inspection.line + ":" + inspection.column + " - " + inspection.message + "\n")
        writer.write("   " + inspection.source.replace("\n", "   \n"))
        writer.write("\n\n")
      }
    }
    if (!hasInspections) {
      writer.write("No problems found.\n")
    }
  }
  
  def findParamIdx(binding: IVariableBinding): Option[Int] = Option(binding).flatMap(b => paramIndices.get(b.getKey))
  
  def visitor(): ASTVisitor = new ASTVisitor() {
    override def visit(node: CompilationUnit): Boolean = {
      paramIndices.clear()
      true
    }

    override def visit(node: RecordDeclaration): Boolean = addParams(node.recordComponents().asScala.toSeq)
    override def visit(node: LambdaExpression): Boolean = addParams(node.parameters().asScala.toSeq)
    override def visit(node: MethodDeclaration): Boolean = addParams(node.parameters().asScala.toSeq)
    
    private def addParams(list: Seq[_]): Boolean = {
      list.zipWithIndex.foreach(e => e._1 match {
        case x: VariableDeclaration => Option(x.resolveBinding()).foreach(b => paramIndices.put(b.getKey, e._2))
        case None =>
      })
      false
    }
  }
  
  case class Inspection(line: Int, column: Int, source: String, message: String)
}
