package com.maj.ir.handlers

import com.maj.ast._
import com.maj.emitters.Emitter
import com.maj.ir.{AssignIR, IRGenerator, IRNode}


class VariableIRGenHandler(val irGenerator: IRGenerator)(implicit emitter: Emitter[IRNode]) {
  def handle(node: Assignable): Option[IRNode] = {

    val anonRef = irGenerator.visit(node.value).getOrElse(throw new Exception("VariableIRGenHandler: Assign: Value not found"))
    val namedRef = node match {
      case Assign(name, _) => Some(irGenerator.getVar(name))
      case MutableVar(name, _) => Some(irGenerator.addVar(name))
      case ConstVar(_, _) => None
      case _ => throw new Exception("Assignable not implemented")
    }

    if (namedRef.isEmpty) {
      irGenerator.addConst(node.name, anonRef)
    } else {
      emitter.emit(AssignIR(namedRef.get, anonRef))
    }
    None
  }
}
