package io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance

import io.github.noeppi_noeppi.tools.sourcetransform.util.cls.{ClassIndex, ClassLocator, CompoundLocator, FailingLocator}
import io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.extract.visitor.InheritanceVisitor
import io.github.noeppi_noeppi.tools.sourcetransform.util.inheritance.extract.{ClassInheritance, FieldCollector, MethodCollector}

object InheritanceExtractor {

  def extractInheritance(sources: ClassIndex, libraryPath: ClassLocator): InheritanceMap = {
    val locator = new FailingLocator(new CompoundLocator(sources, libraryPath))
    val classes = new ClassInheritance(locator)
    val fields = new FieldCollector(classes)
    val methods = new MethodCollector(classes)
    for (cls <- sources.allClasses) {
      locator.findClass(cls) match {
        case Some(reader) =>
          classes.addClass(reader)
          classes.addSource(reader.getClassName)
          val visitor = new InheritanceVisitor(reader.getClassName, classes, fields, methods)
          reader.accept(visitor, 0)
        case None => System.err.println("Source class not found: " + cls)
      }
    }
    classes.build(cls => fields.build(cls), cls => methods.build(cls))
  }
}
