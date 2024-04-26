package com.maj.ir.handlers

import com.maj.ast._
import com.maj.emitters.Emitter
import com.maj.ir.{IRGenerator, IRNode, NotIR, WrappedOperatorIR}

class OperatorIRGenHandler(val irGenerator: IRGenerator)(implicit emitter: Emitter[IRNode]) {
  def handle(node: Operator[ASTNode]): Option[IRNode] = {
    val left = irGenerator.visit(node.left)
    val right = irGenerator.visit(node.right)
    val leftRef = irGenerator.getResultInAnonOrScalar(left)
    val rightRef = irGenerator.getResultInAnonOrScalar(right)
    val anonRef = irGenerator.assignToAnonVarAndEmit(WrappedOperatorIR(node.getType, leftRef, rightRef))
    Some(anonRef)
  }

  def handle(node: Not): Option[IRNode] = {
    val value = irGenerator.visit(node.value)
    val valueRef = irGenerator.getResultInAnonOrScalar(value)
    val anonRef = irGenerator.assignToAnonVarAndEmit(NotIR(valueRef))
    Some(anonRef)
  }


}
