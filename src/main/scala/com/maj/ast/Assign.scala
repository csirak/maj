package com.maj.ast

case class Assign(val name: String, val value: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false
}

case class Create(val name: String, val value: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false
}

