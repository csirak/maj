package com.maj.ir.handlers

import com.maj.ast._
import com.maj.emitters.Emitter
import com.maj.ir.{AssignIR, IRGenerator, IRNode}


class VariableIRGenHandler(val irGenerator: IRGenerator)(implicit emitter: Emitter[IRNode]) {
  def handle(node: Assignable): Option[IRNode] = {
    val anonRef = irGenerator.visit(node.value).getOrElse(throw new Exception("VariableIRGenHandler: Assign: Value not found"))
    val namedRef = node match {
      case Assign(name, _) => irGenerator.getVar(name)
      // const and var
      case node => irGenerator.addVar(node.name)
    }
    emitter.emit(AssignIR(namedRef, anonRef))
    None
  }
}
