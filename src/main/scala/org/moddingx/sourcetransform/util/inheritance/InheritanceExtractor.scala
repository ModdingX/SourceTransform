package org.moddingx.sourcetransform.util.inheritance

import org.moddingx.sourcetransform.util.cls.{ClassIndex, ClassLocator, CompoundLocator, FailingLocator}
import org.moddingx.sourcetransform.util.inheritance.extract.visitor.InheritanceVisitor
import org.moddingx.sourcetransform.util.inheritance.extract.{ClassInheritance, FieldCollector, MethodCollector}

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
