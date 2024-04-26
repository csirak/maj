package com.maj.ir.handlers

import com.maj.ast._
import com.maj.emitters.Emitter
import com.maj.ir._

class ControlFlowIRGenHandler(val irGenerator: IRGenerator)(implicit emitter: Emitter[IRNode]) {
  def handle(node: Conditional): Option[IRNode] = {
    val conditionAssign = irGenerator.visit(node.condition)
    val ifFalseLabel = irGenerator.nextLabel
    val anonCondition = irGenerator.getResultInAnonVar(conditionAssign)
    emitter.emit(JumpIfNotZero(anonCondition, ifFalseLabel))
    irGenerator.visit(node.ifTrue)
    emitter.emit(ifFalseLabel)
    None
  }

  def handle(node: Loop): Option[IRNode] = {
    val loopLabel = irGenerator.nextLabel
    val endLabel = irGenerator.nextLabel
    emitter.emit(loopLabel)
    val conditionAssign = irGenerator.visit(node.condition)
    val anonCondition = irGenerator.getResultInAnonVar(conditionAssign)
    emitter.emit(JumpIfNotZero(anonCondition, endLabel))
    irGenerator.visit(node.body)
    emitter.emit(JumpIR(loopLabel))
    emitter.emit(endLabel)
    None
  }
}






