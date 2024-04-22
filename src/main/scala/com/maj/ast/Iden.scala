package com.maj.ast

import com.maj.codegen.Environment

case class Iden(val value: String) extends ASTNode {
  override def equals(node: ASTNode): Boolean = node match {
    case Iden(nodeIdenValue) => value == nodeIdenValue
    case _ => false;
  }

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    val offset = env.get(value)
    if (offset == -1) {
      throw new RuntimeException(s"Variable $value not found")
    } else {
      emitter.emitLine(s"ld a0, -$offset(fp)")
    }
  }
}
