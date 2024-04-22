package com.maj.ast

import com.maj.codegen.Environment

case class Numeric(val value: Number) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = emitter.emitLine(s"li a0, ${value}")
}
