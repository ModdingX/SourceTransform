package io.github.noeppi_noeppi.tools.sourcetransform.inheritance

import io.github.noeppi_noeppi.tools.sourcetransform.util.{ParamIndexMapper, Util}
import io.github.noeppi_noeppi.tools.sourcetransform.util.cls._
import joptsimple.util.{PathConverter, PathProperties}
import joptsimple.{OptionException, OptionParser}
import org.objectweb.asm._

import java.io.File
import java.nio.file.{Files, StandardOpenOption}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object InheritanceBuilder {
  
  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specClasses = options.acceptsAll(List("c", "classes").asJava, "The folder with the compiled classes").withRequiredArg().withValuesConvertedBy(new PathConverter(PathProperties.DIRECTORY_EXISTING))
    val specClasspath = options.acceptsAll(List("p", "classpath").asJava, "Library classpath. Must also include the jars / jmods from the java installation.").withRequiredArg().withValuesSeparatedBy(File.pathSeparator).withValuesConvertedBy(new PathConverter())
    val specOutput = options.acceptsAll(List("o", "output").asJava, "Output file").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specLocals = options.acceptsAll(List("l", "locals").asJava, "Whether to include inheritance information for all locals, not just parameters.")
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException => System.err.println("Option exception: " + e.getMessage); options.printHelpOn(System.err); Util.exit(0)
    }
    if (!set.has(specClasses) || !set.has(specOutput)) {
      if (!set.has(specClasses)) System.out.println("Missing required option: " + specClasses)
      if (!set.has(specOutput)) System.out.println("Missing required option: " + specOutput)
      options.printHelpOn(System.out)
      System.exit(1)
    } else {
      val classIndex = new TreeLocator(set.valueOf(specClasses))
      val library = new CompoundLocator(Option(set.valuesOf(specClasspath)).map(_.asScala).getOrElse(Nil).map(p => new JarLocator(p)).toSeq: _*)
      val map = buildInheritance(classIndex, library, set.has(specLocals))
      val writer = Files.newBufferedWriter(set.valueOf(specOutput), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      map.write(writer)
      writer.close()
    }
  }
  
  def buildInheritance(sources: ClassIndex, librarypath: ClassLocator, locals: Boolean): InheritanceMap = {
    val classpath = new CompoundLocator(sources, librarypath)
    val builder = new InheritanceMap.Builder()
    val failer = new ClassFailer
    for (cls <- sources.allClasses) {
      classpath.findClass(cls) match {
        case Some(x) => processClass(failer, cls, builder, x, classpath, locals)
        case None => System.err.println("Failed to load source class: " + cls)
      }
    }
    builder.build()
  }
  
  private def processClass(failer: ClassFailer, cls: String, builder: InheritanceMap.Builder, data: ClassReader, classpath: ClassLocator, locals: Boolean): Unit = {
    addClassInheritance(failer, cls, builder, data, classpath)
    builder.src(cls)
    val overridableMethods = mutable.Set[(String, String)]()
    val syntheticOverrides = mutable.Map[(String, String), (String, String)]()
    var lambdaIdx = 0 // Keep track of lambda indices as they would appear in source code. We use these as lambda ids.
    data.accept(new ClassVisitor(Opcodes.ASM9) {
      
      override def visitField(access: Int, name: String, descriptor: String, signature: String, value: Any): FieldVisitor = {
        builder.field(cls, name, descriptor, (access & Opcodes.ACC_STATIC) != 0)
        addSignatureInheritance(failer, descriptor, builder, classpath)
        null
      }
      
      override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor = {
        builder.method(cls, name, descriptor, (access & Opcodes.ACC_STATIC) != 0)
        addSignatureInheritance(failer, descriptor, builder, classpath)
        val visitor: MethodVisitor = if ((access & Opcodes.ACC_PRIVATE) == 0) {
          overridableMethods.addOne((name, descriptor))
          if ((access & Opcodes.ACC_SYNTHETIC) != 0) {
            // Synthetic method. Might call another method that is the real overriding method
            // We need to scan the code
            new MethodVisitor(Opcodes.ASM9) {
              // Null means multiple invoke opcodes do nothing.
              // None means not yet found
              var resultMethod: Option[(String, String)] = None

              override def visitMethodInsn(opcode: Int, owner: String, invokeName: String, invokeDescriptor: String, isInterface: Boolean): Unit = {
                if (resultMethod != null && resultMethod.isEmpty && owner == cls && name == invokeName) {
                  resultMethod = Some((invokeName, invokeDescriptor))
                } else {
                  resultMethod = null
                }
              }

              override def visitEnd(): Unit = {
                if (resultMethod != null && resultMethod.isDefined) {
                  syntheticOverrides.put((name, descriptor), (resultMethod.get._1, resultMethod.get._2))
                }
              }
            }
          } else {
            null
          }
        } else {
          null
        }
        new MethodVisitor(Opcodes.ASM9, visitor) {
          private val visited = mutable.Set[Int]()
          
          override def visitLocalVariable(paramName: String, paramDescriptor: String, paramSignature: String, start: Label, end: Label, local: Int): Unit = {
            super.visitLocalVariable(paramName, paramDescriptor, paramSignature, start, end, local)
            if (locals) {
              addSignatureInheritance(failer, paramDescriptor, builder, classpath)
            }
            
            val synthetics = if (name == "<init>" && data.getSuperName == "java/lang/Enum") {
              3
            } else if (name == "<init>" || (access & Opcodes.ACC_STATIC) == 0) {
              1
            } else {
              0
            }
            
            ParamIndexMapper.translateLocalIdx(local, synthetics, descriptor) match {
              case Some(idx) if !visited.contains(idx) =>
                visited.addOne(idx)
                val cleanedParameter = if (paramName.nonEmpty && Character.isJavaIdentifierStart(paramName.head) && paramName.tail.forall(Character.isJavaIdentifierPart)) {
                  paramName
                } else {
                  "o" + idx
                }
                builder.param(cls, name, descriptor, idx, cleanedParameter)
              case _ =>
            }
          }

          override def visitInvokeDynamicInsn(name: String, descriptor: String, bootstrapMethodHandle: Handle, bootstrapMethodArguments: AnyRef*): Unit = {
            super.visitInvokeDynamicInsn(name, descriptor, bootstrapMethodHandle, bootstrapMethodArguments: _*)
            if (bootstrapMethodHandle.getOwner == "java/lang/invoke/LambdaMetafactory" && bootstrapMethodHandle.getName == "metafactory") {
              // Lambda
              val lambdaId = "lambda$" + lambdaIdx
              lambdaIdx += 1
              // The implemented class of the lambda is the return type from the descriptor (args are final locals used in the lambda)
              // If we can't get a return type, use Object
              val implementedCls = Util.returnCls(descriptor).getOrElse({
                System.err.println("Failed to resolve implemented class for lambda in " + cls + " " + lambdaId + "@" + name + descriptor + ". Using " + InheritanceMap.ROOT)
                InheritanceMap.ROOT
              })
              // Get the handle passed to the metafactory and try to resolve the implementation method
              val handle = (if (bootstrapMethodArguments.size >= 2) Some(bootstrapMethodArguments(1)) else None) match {
                case Some(h: Handle) => h
                case _ =>
                  System.err.println("Failed to resolve method handle for lambda in " + cls + " " + lambdaId + "@" + name + descriptor)
                  null
              }
              if (handle != null) {
                builder.lambda(cls, lambdaId, MethodInfo(handle.getOwner, handle.getName, handle.getDesc), implementedCls)
              }
            }
          }
        }
      }
    }, 0)
    // Now to find overrides we need to go through th parents recursively. We'll first follow the
    // classes to the bottom and then go through interfaces from top to bottom.
    // We also need to keep a blacklist of types to skip as we only want to find the first override
    // down the tree
    val skipClasses = mutable.Set[MethodInfo]()
    val interfaceList = ListBuffer[String]()
    interfaceList.addAll(data.getInterfaces)
    val parentName = Option(data.getSuperName).getOrElse(InheritanceMap.ROOT)
    classpath.findClass(parentName) match {
      case Some(x) => findOverridesClasses(failer, cls, parentName, builder, overridableMethods.toSet, syntheticOverrides.toMap, skipClasses, x, interfaceList, classpath)
      case None => failer.warn(parentName)
    }
    populateInterfaces(failer, interfaceList, classpath)
    val interfaces = interfaceList.toList.distinct
    for (iface <- interfaces) {
      classpath.findClass(iface) match {
        case Some(x) => findOverridesIn(failer, cls, iface, builder, overridableMethods.toSet, syntheticOverrides.toMap, skipClasses, x, classpath)
        case None => failer.warn(iface)
      }
    }
  }
  
  private def addSignatureInheritance(failer: ClassFailer, signature: String, builder: InheritanceMap.Builder, classpath: ClassLocator): Unit = {
    Util.CLS_TYPE.findAllMatchIn(signature).foreach(m => addClassInheritance(failer, m.group(1), builder, classpath))
  }
  
  private def addClassInheritance(failer: ClassFailer, cls: String, builder: InheritanceMap.Builder, classpath: ClassLocator): Unit = {
    if (!builder.hasCls(cls)) {
      classpath.findClass(cls) match {
        case Some(x) => addClassInheritance(failer, cls, builder, x, classpath)
        case None => failer.warn(cls)
      }
    }
  }
  
  private def addClassInheritance(failer: ClassFailer, cls: String, builder: InheritanceMap.Builder, data: ClassReader, classpath: ClassLocator): Unit = {
    if (!builder.hasCls(cls)) {
      val parent = Option(data.getSuperName)
      val interfaces = data.getInterfaces.toSet
      builder.cls(cls, parent, interfaces)
      parent match {
        case Some(x) if x != InheritanceMap.ROOT => addClassInheritance(failer, x, builder, classpath)
        case _ =>
      }
      interfaces.filter(_ != InheritanceMap.ROOT).foreach(iface => addClassInheritance(failer, iface, builder, classpath))
    }
  }
  
  @tailrec
  private def findOverridesClasses(failer: ClassFailer, scanningCls: String, cls: String, builder: InheritanceMap.Builder, methods: Set[(String, String)], syntheticOverrides: Map[(String, String), (String, String)], skipClasses: mutable.Set[MethodInfo], data: ClassReader, interfaceList: ListBuffer[String], classpath: ClassLocator): Unit = {
    findOverridesIn(failer, scanningCls, cls, builder, methods, syntheticOverrides, skipClasses, data, classpath)
    if (cls != InheritanceMap.ROOT) {
      val parent = Option(data.getSuperName)
      val interfaces = data.getInterfaces.toSet
      interfaceList.addAll(interfaces)
      classpath.findClass(parent.getOrElse(InheritanceMap.ROOT)) match {
        case Some(x) => findOverridesClasses(failer, scanningCls, parent.getOrElse(InheritanceMap.ROOT), builder, methods, syntheticOverrides, skipClasses, x, interfaceList, classpath)
        case None => failer.warn(parent.getOrElse(InheritanceMap.ROOT))
      }
    }
  }
  
  private def populateInterfaces(failer: ClassFailer, interfaceList: ListBuffer[String], classpath: ClassLocator): Unit = {
    val interfaces = interfaceList.toList.distinct
    interfaces.foreach(iface => addAllParentInterfaces(failer, iface, interfaceList, classpath))
  }
  
  private def addAllParentInterfaces(failer: ClassFailer, cls: String, interfaceList: ListBuffer[String], classpath: ClassLocator): Unit = {
    classpath.findClass(cls) match {
      case Some(data) =>
        data.getInterfaces.foreach(iface => {
          if (!interfaceList.contains(iface)) {
            interfaceList.addOne(iface)
            addAllParentInterfaces(failer, iface, interfaceList, classpath)
          }
        })
      case None => failer.warn(cls)
    }
  }
  
  private def findOverridesIn(failer: ClassFailer, scanningClass: String, cls: String, builder: InheritanceMap.Builder, methods: Set[(String, String)], syntheticOverrides: Map[(String, String), (String, String)], skipClasses: mutable.Set[MethodInfo], data: ClassReader, classpath: ClassLocator): Unit = {
    def result(name: String, signature: String): Unit = {
      if (!skipClasses.contains(MethodInfo(cls, name, signature))) {
        builder.overrides(MethodInfo(scanningClass, name, signature), MethodInfo(cls, name, signature))
        skipAll(failer, cls, name, signature, skipClasses, data, classpath)
      }
      if (syntheticOverrides.contains((name, signature))) {
        val syn = syntheticOverrides((name, signature))
        builder.overrides(MethodInfo(scanningClass, syn._1, syn._2), MethodInfo(cls, name, signature))
      }
    }
    data.accept(new ClassVisitor(Opcodes.ASM9) {
      override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor = {
        if (methods.contains((name, descriptor)) && name != "<init>" && name != "<clinit>") {
          result(name, descriptor)
        }
        null
      }
    }, 0)
  }
  
  private def skipAll(failer: ClassFailer, cls: String, name: String, signature: String, skipClasses: mutable.Set[MethodInfo], data: ClassReader, classpath: ClassLocator): Unit = {
    // If a class is skipped al parents a re skipped as well
    if (!skipClasses.contains(MethodInfo(cls, name, signature))) {
      skipClasses.addOne(MethodInfo(cls, name, signature))
      val parent = Option(data.getSuperName)
      val interfaces = data.getInterfaces.toSet
      // Don't skip Object, it has special handling
      parent match {
        case Some(x) if x != InheritanceMap.ROOT =>
          classpath.findClass(x) match {
            case Some(y) => skipAll(failer, x, name, signature, skipClasses, y, classpath)
            case None => failer.warn(x)
          }
        case _ =>
      }
      interfaces.filter(_ != InheritanceMap.ROOT).foreach(iface => classpath.findClass(iface) match {
        case Some(x) => skipAll(failer, iface, name, signature, skipClasses, x, classpath)
        case None => failer.warn(iface)
      })
      // Special handling for Object
      skipClasses.addOne(MethodInfo(InheritanceMap.ROOT, name, signature))
    }
  }
}

private class ClassFailer {
  private val classes = mutable.Set[String]()
  def warn(cls: String): Unit = if (!classes.contains(cls)) { System.err.println("Failed to load class: " + cls); classes.addOne(cls) }
}