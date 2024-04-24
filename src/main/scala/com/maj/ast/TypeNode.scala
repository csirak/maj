package com.maj.ast

sealed trait TypeNode {
  def accepts(other: TypeNode): Boolean = {
    this == other
  }
}


case class MajIntType() extends TypeNode

case class MajBoolType() extends TypeNode

case class MajVoidType() extends TypeNode

case class MajTypeUndefined() extends TypeNode

case class MajFuncType(val returnType: String, val params: List[String]) extends TypeNode

case class MajStruct(val name: String, val fields: Map[String, String]) extends TypeNode


case class MajTypeComposeOr(val left: TypeNode, val right: TypeNode) extends TypeNode {
  override def accepts(other: TypeNode): Boolean = {
    left.accepts(other) || right.accepts(other)
  }
}

case class MajTypeComposeAnd(val left: TypeNode, val right: TypeNode) extends TypeNode {
  override def accepts(other: TypeNode): Boolean = {
    left.accepts(other) && right.accepts(other)
  }
}