package com.maj.ast

case class Numeric(val value: Number) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(implicit emitter: Emitter): Unit = emitter.emitLine(s"li a0, ${value}")
}
