package com.maj.ir.handlers

import com.maj.ast._
import com.maj.emitters.Emitter
import com.maj.ir.{IRGenerator, IRNode, IRNot, IROperator}

class OperatorIRGenHandler(val irGenerator: IRGenerator)(implicit emitter: Emitter[IRNode]) {
  def handle(node: Operator[ASTNode]): Option[IRNode] = {
    val left = irGenerator.visit(node.left)
    val right = irGenerator.visit(node.right)
    val leftRef = irGenerator.getResultInAnonOrScalar(left)
    val rightRef = irGenerator.getResultInAnonOrScalar(right)
    Some(IROperator(node.getType, leftRef, rightRef))
  }

  def handle(node: Not): Option[IRNode] = {
    val value = irGenerator.visit(node.value)
    val valueRef = irGenerator.getResultInAnonOrScalar(value)
    Some(IRNot(valueRef))
  }


}
