package com.maj.ast

trait ASTNode {
  def equals(node: ASTNode): Boolean

  def emit(implicit emitter: Emitter): Unit = println("Not implemented")
}

