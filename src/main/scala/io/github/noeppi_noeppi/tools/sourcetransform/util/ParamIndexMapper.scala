package io.github.noeppi_noeppi.tools.sourcetransform.util

import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.MethodInfo
import org.objectweb.asm.Opcodes

object ParamIndexMapper {

  def bytecodeToIdx(m: MethodInfo, isStatic: Boolean, bytecodeIdx: Int): Int = {
    var bytecodeCounter = if (isStatic) 0 else 1
    var idxCounter = 0
    val simplified = Util.simplifiedSignatureParams(m.signature)
    while (bytecodeCounter < bytecodeIdx) {
      val simplifiedType = if (idxCounter < simplified.length) simplified.charAt(idxCounter) else 'L'
      bytecodeCounter += (if (simplifiedType == 'J' || simplifiedType == 'D') 2 else 1)
      idxCounter += 1
    }
    idxCounter
  }

  def idxToBytecode(m: MethodInfo, isStatic: Boolean, idx: Int): Int = {
    var bytecodeCounter = if (isStatic) 0 else 1
    var idxCounter = 0
    val simplified = Util.simplifiedSignatureParams(m.signature)
    while (idxCounter < idx) {
      val simplifiedType = if (idxCounter < simplified.length) simplified.charAt(idxCounter) else 'L'
      bytecodeCounter += (if (simplifiedType == 'J' || simplifiedType == 'D') 2 else 1)
      idxCounter += 1
    }
    bytecodeCounter
  }
  
  def lvtToIdx(access: Int, name: String, descriptor: String, lvt: Int): Option[Int] = {
    var lvtCounter = if (name != "<init>" && (access & Opcodes.ACC_STATIC) != 0) 0 else 1
    val simplified = Util.simplifiedSignatureParams(descriptor)
    for ((simplifiedType, idx) <- simplified.zipWithIndex) {
      if (lvtCounter == lvt) return Some(idx)
      lvtCounter += (if (simplifiedType == 'J' || simplifiedType == 'D') 2 else 1)
    }
    None
  }
}
