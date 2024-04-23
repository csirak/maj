package com.maj.ast

import com.maj.codegen.{Emitter, Environment}

case class Numeric(val value: Number) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = emitter.emitLine(s"li a0, ${value}")
}

case class Bool(val value: Boolean) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    emitter.emitLine(s"li a0, ${if (value) 1 else 0}")
  }
}

case class Null() extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    emitter.emitLine("li a0, 0")
  }
}

