package com.maj.ast

case class Numeric(val value: Number) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false
}
