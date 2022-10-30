package org.moddingx.sourcetransform.util

import org.objectweb.asm.Opcodes

object ParamIndexMapper {

  def bytecodeToIdx(m: Bytecode.Method, isStatic: Boolean, bytecodeIdx: Int): Int = {
    var bytecodeCounter = if (isStatic) 0 else 1
    var idxCounter = 0
    val simplified = Bytecode.simplifiedDescriptorParams(m.desc)
    while (bytecodeCounter < bytecodeIdx) {
      val simplifiedType = if (idxCounter < simplified.length) simplified.charAt(idxCounter) else 'L'
      bytecodeCounter += (if (simplifiedType == 'J' || simplifiedType == 'D') 2 else 1)
      idxCounter += 1
    }
    idxCounter
  }

  def idxToBytecode(m: Bytecode.Method, isStatic: Boolean, idx: Int): Int = {
    var bytecodeCounter = if (isStatic) 0 else 1
    var idxCounter = 0
    val simplified = Bytecode.simplifiedDescriptorParams(m.desc)
    while (idxCounter < idx) {
      val simplifiedType = if (idxCounter < simplified.length) simplified.charAt(idxCounter) else 'L'
      bytecodeCounter += (if (simplifiedType == 'J' || simplifiedType == 'D') 2 else 1)
      idxCounter += 1
    }
    bytecodeCounter
  }
  
  def lvtToIdx(access: Int, name: String, descriptor: String, lvt: Int): Option[Int] = {
    var lvtCounter = if (name != "<init>" && (access & Opcodes.ACC_STATIC) != 0) 0 else 1
    var idxCounter = 0
    val simplified = Bytecode.simplifiedDescriptorParams(descriptor)
    for (simplifiedType <- simplified if lvtCounter < lvt && idxCounter < simplified.length - 1) {
      lvtCounter += (if (simplifiedType == 'J' || simplifiedType == 'D') 2 else 1)
      idxCounter += 1
    }
    if (lvtCounter == lvt) Some(idxCounter)
    else None
  }
}
