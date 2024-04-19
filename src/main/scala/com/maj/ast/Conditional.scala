package com.maj.ast

import com.maj.codegen.Label

case class Conditional(val condition: ASTNode, val ifTrue: ASTNode, val elseIfTrue: Option[ASTNode] = None) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(implicit emitter: Emitter): Unit = {
    val ifFalseLabel = Label.next
    val endLabel = Label.next
    this.condition.emit
    emitter.emitLine(s"beqz a0, $ifFalseLabel")
    ifTrue.emit
    emitter.emitLine(s"j $endLabel")
    emitter.emitLine(s"$ifFalseLabel:")
    this.elseIfTrue.foreach(_.emit)
    emitter.emitLine(s"$endLabel:")

  }
}