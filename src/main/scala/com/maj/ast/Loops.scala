package com.maj.ast

case class Loop(val condition: ASTNode, val body: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = {
    node match {
      case loop: Loop => {
        this.condition.equals(loop.condition) && this.body.equals(loop.body)
      }
      case _ => false
    }
  }
}
