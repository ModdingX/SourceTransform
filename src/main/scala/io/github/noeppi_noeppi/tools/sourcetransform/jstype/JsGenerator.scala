package io.github.noeppi_noeppi.tools.sourcetransform.jstype

import io.github.noeppi_noeppi.tools.sourcetransform.util.signature.SignatureNode
import io.github.noeppi_noeppi.tools.sourcetransform.util.{Bytecode, Util}
import org.objectweb.asm.{Opcodes, Type}
import org.objectweb.asm.signature.SignatureReader
import org.objectweb.asm.tree.{ClassNode, MethodNode}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.given

object JsGenerator {

  val KEYWORDS: Set[String] = Set(
    "await", "break", "case", "catch", "class", "const", "continue", "debugger", "default",
    "delete", "do", "else", "enum", "export", "extends", "false", "finally", "for", "function",
    "if", "implements", "import", "in", "instanceof", "interface", "let", "new", "null", "package",
    "private", "protected", "public", "return", "super", "switch", "static", "this", "throw",
    "try", "true", "typeof", "var", "void", "while", "with", "yield"
  )

  val CLASS_BLACKLIST: Set[String] = Set(
    Bytecode.ROOT,
    "java/lang/String",
    "java/util/List"
  )

  def processClass(env: JsEnv, cls: String, processedClasses: mutable.Set[String]): Unit = {
    if (CLASS_BLACKLIST.contains(cls)) return 
    if (processedClasses.contains(cls)) return
    // Don't add to processedClasses now, first process super classes
    for (parent <- env.inheritance.getAllSuperClasses(cls)) processClass(env, parent, processedClasses)
    if (processedClasses.contains(cls)) return
    processedClasses.addOne(cls)

    val node: ClassNode = env.classpath.findClass(cls) match {
      case Some(reader) =>
        val clsNode = new ClassNode(Bytecode.TARGET)
        reader.accept(clsNode, 0)
        clsNode
      case None => return
    }
    
    // inner classes are handled by their containing classes
    if (node.outerClass != null) return

    val statics = env.isSource(cls) && (node.access & Opcodes.ACC_INTERFACE) == 0

    // TS sometimes still complains but these should not be that bad
    // Just shut up TS
    env.tsWriter.write("// @ts-ignore\n")

    if (env.isSource(cls)) {
      if (statics) {
        val abstractStr = if ((node.access & Opcodes.ACC_ABSTRACT) == 1) "abstract " else ""
        env.tsWriter.write("export " + abstractStr + "class " + env.plainName(cls))
      } else {
        env.tsWriter.write("export interface " + env.plainName(cls))
      }
    } else {
      env.tsWriter.write("export interface " + env.jsClass(cls))
    }

    val clsSig = parseSig(node.signature)

    val usedIdentifiers = mutable.Set[String]("constructor")
    generateClassContent(env, cls, node, clsSig, statics, usedIdentifiers)

//    if (env.isSource(cls)) {
//      env.tsWriter.write("export type " + env.jsClass(cls) + env.typeVars(clsSig) + "=" + env.plainName(cls) + env.typeVarDefs(clsSig) + ";")
//    }

    if (env.isSource(cls) && !statics) {
      // Interface. We need to add class with only the statics
      env.tsWriter.write("export " + "class " + env.plainName(cls) + "{")
      generateMethods(env, node, usedIdentifiers, Some(true), warnSkip = true)
      generateFields(env, node, usedIdentifiers, Some(true))
      writeInner(env, cls, node, usedIdentifiers)
      env.tsWriter.write("}")
    }

    env.tsWriter.write("\n")
  }

  private def generateAnonymousClassContent(env: JsEnv, cls: String): Unit = {
    val node: ClassNode = env.classpath.findClass(cls) match {
      case Some(reader) =>
        val clsNode = new ClassNode(Bytecode.TARGET)
        reader.accept(clsNode, 0)
        clsNode
      case None => null
    }
    if (node == null || (node.access & Opcodes.ACC_INTERFACE) != 0) return
    val clsSig = parseSig(node.signature)
    val usedIdentifiers = mutable.Set[String]("constructor")
    generateClassContent(env, cls, node, clsSig, statics = true, usedIdentifiers)
  }

  private def generateClassContent(env: JsEnv, cls: String, node: ClassNode, clsSig: SignatureNode, statics: Boolean, usedIdentifiers: mutable.Set[String]): Unit = {
    env.tsWriter.write(env.typeVars(clsSig))

    val superClsAsItf = if (node.superName != null && node.superName != Bytecode.ROOT
      && !CLASS_BLACKLIST.contains(node.superName) && env.supports(node.superName)) {
      if (env.isSource(node.superName)) {
        if (statics) {
          // Must use plain name for extends as a type can't be implemented
          // For interfaces the type is fine
          env.tsWriter.write(" extends " + env.plainName(node.superName) + env.typeDefs(clsSig.superClass))
        }
        None
      } else {
        Some(node.superName)
      }
    } else {
      None
    }

    val allInterfaces = node.interfaces.asScala.toSeq
      .filter(itf => itf != null && itf != Bytecode.ROOT && !CLASS_BLACKLIST.contains(itf) && env.supports(itf))
    if (superClsAsItf.isDefined || allInterfaces.nonEmpty) {
      if (statics) {
        env.tsWriter.write(" implements ")
      } else {
        env.tsWriter.write(" extends ")
      }
      val allImplements = ListBuffer[String]()
      superClsAsItf match {
        case Some(itf) => allImplements.addOne(env.jsClass(itf) + env.typeDefs(clsSig.superClass))
        case None =>
      }
      for (itf <- allInterfaces) {
        val itfSig = clsSig.superInterfaces.asScala.toSeq
          .find(s => s.classType != null && s.classType.desc == itf)
          .orNull
        allImplements.addOne(env.jsClass(itf) + env.typeDefs(itfSig))
      }
      env.tsWriter.write(allImplements.mkString(","))
    }
    env.tsWriter.write("{")

    generateMethods(env, node, usedIdentifiers, if (statics) None else Some(false), warnSkip = true)
    generateFields(env, node, usedIdentifiers, if (statics) None else Some(false))

    // For classes, we need to redefine all interface methods
    // as classes are sometimes represented as TS interfaces
    // we need to do this for all parent classes and interfaces
    val processedParents = mutable.Set[String]()
    def processParentInfo(parentName: String): Unit = {
      if (parentName != null && parentName != Bytecode.ROOT && !CLASS_BLACKLIST.contains(parentName) && !processedParents.contains(parentName)) {
        processedParents.addOne(parentName)
        env.classpath.findClass(parentName) match {
          case Some(parentReader) =>
            val parent = new ClassNode(Bytecode.TARGET)
            parentReader.accept(parent, 0)
            generateMethods(env, parent, usedIdentifiers, statics = Some(false), warnSkip = false)
            processParentInfo(parent.superName)
            if (parent.interfaces != null) parent.interfaces.asScala.foreach(processParentInfo)
          case None =>
        }
      }
    }

    processParentInfo(node.superName)
    if (node.interfaces != null) node.interfaces.asScala.foreach(processParentInfo)

    if (statics) {
      writeInner(env, cls, node, usedIdentifiers)
    }

    env.tsWriter.write("};")
  }

  private def writeInner(env: JsEnv, cls: String, node: ClassNode, usedIdentifiers: mutable.Set[String]): Unit = {
    if (node.innerClasses != null && !node.innerClasses.isEmpty) {
      for (inner <- node.innerClasses.asScala if inner.innerName != null
           if inner.name.startsWith(cls + "$") && !inner.name.substring(cls.length + 1).contains('$')) {
        if (usedIdentifiers.contains(inner.innerName)) {
          System.err.println("Skipping class " + cls + "$" + inner.innerName)
        } else {
          usedIdentifiers.add(inner.innerName)
          env.tsWriter.write("static readonly " + inner.innerName + "=class")
          generateAnonymousClassContent(env, inner.name)
        }
      }
    }
  }

  private def generateFields(env: JsEnv, cls: ClassNode, usedIdentifiers: mutable.Set[String], statics: Option[Boolean]): Unit = {
    for (field <- cls.fields.asScala) {
      if ((field.access & Opcodes.ACC_SYNTHETIC) != 0 || (field.access & Opcodes.ACC_PUBLIC) == 0) {
        //
      } else if (usedIdentifiers.contains(field.name)) {
        System.err.println("Skipping field " + cls.name + ";" + field.name)
      } else if (checkStatic(statics, field.access)) {
        usedIdentifiers.add(field.name)
        if ((field.access & Opcodes.ACC_STATIC) != 0) env.tsWriter.write("static ")
        if ((field.access & Opcodes.ACC_FINAL) != 0) env.tsWriter.write("readonly ")
        env.tsWriter.write(field.name + ":" + env.jsTypeField(field.desc, parseSig(field.signature), env.nulls.nullable(cls.name, field)) + ";")
      }
    }
  }

  private def generateMethods(env: JsEnv, cls: ClassNode, usedIdentifiers: mutable.Set[String], statics: Option[Boolean], warnSkip: Boolean): Unit = {
    val isEnum = (cls.access & Opcodes.ACC_ENUM) != 0
    val allMethodNames = mutable.Set[String]()
    for (method <- cls.methods.asScala if !isEnum || !skipEnum(method)) {
      if (method.name == "<init>" && statics.isEmpty && (method.access & Opcodes.ACC_PUBLIC) != 0) {
        generateMethod(env, "constructor", cls.name, method, returnType = false)
      } else if (method.name == "<init>" || method.name == "<clinit>" || (method.access & Opcodes.ACC_SYNTHETIC) != 0
        || (method.access & Opcodes.ACC_PUBLIC) == 0) {
        //
      } else if (usedIdentifiers.contains(method.name)) {
        if (warnSkip) System.err.println("Skipping method " + cls.name + ";" + method.name + method.desc)
      } else if (checkStatic(statics, method.access)) {
        allMethodNames.add(method.name)
        generateMethod(env, method.name, cls.name, method, returnType = true)
      }
    }
    usedIdentifiers.addAll(allMethodNames)
  }

  private def skipEnum(method: MethodNode): Boolean = {
    method.name == "<init>"
  }

  private def generateMethod(env: JsEnv, name: String, cls: String, method: MethodNode, returnType: Boolean): Unit = {
    val desc = Type.getMethodType(method.desc)
    val sig = parseSig(method.signature)
    val params: Seq[String] = for ((arg, idx) <- desc.getArgumentTypes.toSeq.map(_.getDescriptor).zipWithIndex;
                                    paramSigs = sig.parameters.asScala;
                                    paramSig = if (paramSigs.indices.contains(idx)) paramSigs(idx) else null)
      yield paramName(method, idx) + ":" + env.jsType(arg, paramSig, env.nulls.nullable(cls, method, idx))
    val ret = if (returnType) ":" + env.jsType(desc.getReturnType.getDescriptor, sig.returnType, env.nulls.nullable(cls, method)) else ""
    if ((method.access & Opcodes.ACC_STATIC) != 0) env.tsWriter.write("static ")
    env.tsWriter.write(name + env.typeVars(sig) + "(" + params.mkString(",") + ")" + ret + ";")
  }

  private def paramName(method: MethodNode, idx: Int): String = {
    val bytecode = bytecodeIdx(method, idx)
    (if (method.localVariables == null) Nil else method.localVariables.asScala).find(n => n.index == bytecode) match {
      case Some(node) if node.name != null && KEYWORDS.contains(node.name) => node.name + "_"
      case Some(node) if node.name != null => node.name
      case _ => "o" + idx
    }
  }

  def bytecodeIdx(method: MethodNode, idx: Int): Int = {
    var bytecodeCounter = if ((method.access & Opcodes.ACC_STATIC) != 0) 0 else 1
    var idxCounter = 0
    val simplified = Bytecode.simplifiedDescriptorParams(method.desc)
    while (idxCounter < idx) {
      val simplifiedType = if (idxCounter < simplified.length) simplified.charAt(idxCounter) else 'L'
      bytecodeCounter += (if (simplifiedType == 'J' || simplifiedType == 'D') 2 else 1)
      idxCounter += 1
    }
    bytecodeCounter
  }

  private def parseSig(sig: String): SignatureNode = {
    if (sig == null) return new SignatureNode(Bytecode.TARGET)
    val node = new SignatureNode(Bytecode.TARGET)
    new SignatureReader(sig).accept(node)
    node
  }

  private def checkStatic(statics: Option[Boolean], access: Int): Boolean = statics match {
    case Some(true) => (access & Opcodes.ACC_STATIC) != 0
    case Some(false) => (access & Opcodes.ACC_STATIC) == 0
    case None => true
  }
}
