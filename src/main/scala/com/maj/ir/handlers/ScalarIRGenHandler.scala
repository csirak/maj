package com.maj.ir.handlers

import com.maj.ast._
import com.maj.emitters.Emitter
import com.maj.ir.{IRGenerator, IRNode, IRScalar}

class ScalarIRGenHandler(val irGenerator: IRGenerator)(implicit emitter: Emitter[IRNode]) {
  def handle(node: Iden): Option[IRNode] = Some(irGenerator.getValue(node.value))

  def handle(node: Scalar): Option[IRNode] = Some(IRScalar(node))
}
