package com.maj.ast

import com.maj.codegen.{Environment, Label}

case class Loop(val condition: ASTNode, val body: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = {
    node match {
      case loop: Loop => {
        this.condition.equals(loop.condition) && this.body.equals(loop.body)
      }
      case _ => false
    }
  }

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    val conditionLabel = Label.next
    val loopBackLabel = Label.next
    emitter.emitLine(s"j $conditionLabel")
    emitter.emit(s"$loopBackLabel:")
    this.body.emit(env)
    emitter.emit(s"$conditionLabel:")
    this.condition.emit(env)
    emitter.emitLine("seqz a0, a0")
    emitter.emitLine(s"beqz a0, $loopBackLabel")
  }
}
