package com.maj.ast

import com.maj.codegen.Environment

trait ASTNode {
  def equals(node: ASTNode): Boolean

  def emit(env: Environment)(implicit emitter: Emitter): Unit = println("Not implemented")
}

