package com.maj.ast

case class Conditional(val condition: ASTNode, val ifTrue: ASTNode, val elseIfTrue: Option[Conditional] = None) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false
}