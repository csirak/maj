package com.maj.ast

case class Iden(val value: String) extends ASTNode {
  override def equals(node: ASTNode): Boolean = node match {
    case Iden(nodeIdenValue) => value == nodeIdenValue
    case _ => false;
  }
}
