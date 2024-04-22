package com.maj.ast

import com.maj.codegen.Environment

case class Assign(val name: String, val value: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {

    val offset = env.get(name)
    if (offset == -1) {
      throw new RuntimeException(s"Variable $name not found")
    }
    value.emit(env)
    emitter.emitLine(s"sd a0, -$offset(fp)")
  }
}

case class Create(val name: String, val value: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    if (env.get(name) != -1) {
      throw new RuntimeException(s"Variable $name already exists")
    }
    value.emit(env)
    emitter.emitLine(s"addi sp, sp, -8")
    emitter.emitLine(s"sd a0, 0(sp)")

    env.addLocalWithOffset(name, 8)

  }
}

