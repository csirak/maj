package com.maj.ast

import com.maj.codegen.{Emitter, Environment, Label}

case class Conditional(val condition: ASTNode, val ifTrue: ASTNode, val elseIfTrue: Option[ASTNode] = None) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    val ifFalseLabel = Label.next
    val endLabel = Label.next
    this.condition.emit(env)
    emitter.emitLine(s"beqz a0, $ifFalseLabel")
    ifTrue.emit(env)
    emitter.emitLine(s"j $endLabel")
    emitter.emit(s"$ifFalseLabel:")
    this.elseIfTrue.foreach(_.emit(env))
    emitter.emit(s"$endLabel:")
  }
}


case class Loop(val condition: ASTNode, val body: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

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
