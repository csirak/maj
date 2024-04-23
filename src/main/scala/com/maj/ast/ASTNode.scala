package com.maj.ast

import com.maj.codegen.{Emitter, Environment}

trait ASTNode {
  def equals(node: ASTNode): Boolean

  def emit(env: Environment)(implicit emitter: Emitter): Unit = println("Not implemented")
}

