package com.maj.ast

sealed trait TypeNode {
  def accepts(other: TypeNode): Boolean = {
    this == other
  }

  def acceptsWithResolver(other: TypeNode, resolver: (TypeNode) => TypeNode): Boolean = {
    resolver(this).accepts(resolver(other))
  }
}

case class MajIntType() extends TypeNode

case class MajBoolType() extends TypeNode

case class MajVoidType() extends TypeNode

case class MajCharType() extends TypeNode

case class MajTypeUndefined() extends TypeNode

case class MajFuncType(val returnType: TypeNode, val params: List[TypeNode]) extends TypeNode {
  override def accepts(other: TypeNode): Boolean = {
    other match {
      case MajFuncType(otherReturnType, otherParams) => {
        returnType == otherReturnType && params == otherParams
      }
      case _ => false
    }
  }
}

case class MajStruct(val name: String, val fields: Map[String, String]) extends TypeNode

case class MajType(val typ: String) extends TypeNode {
  override def toString: String = typ
}

abstract class TypeOperator extends Operator[TypeNode] with TypeNode {
  def left: TypeNode

  def right: TypeNode

  def get(left: TypeNode, right: TypeNode): TypeOperator
}

case class MajTypeComposeOr(left: TypeNode = null, right: TypeNode = null) extends TypeOperator {
  override def acceptsWithResolver(other: TypeNode, resolver: (TypeNode) => TypeNode): Boolean = {
    resolver(other).acceptsWithResolver(resolver(left), resolver) || resolver(other).acceptsWithResolver(resolver(right), resolver)
  }

  override def get(left: TypeNode, right: TypeNode): TypeOperator = MajTypeComposeOr(left, right)
}

case class MajTypeComposeAnd(left: TypeNode = null, right: TypeNode = null) extends TypeOperator {
  override def acceptsWithResolver(other: TypeNode, resolver: (TypeNode) => TypeNode): Boolean = {
    resolver(other).acceptsWithResolver(resolver(left), resolver) && other.acceptsWithResolver(resolver(right), resolver)
  }

  override def get(left: TypeNode, right: TypeNode): TypeOperator = MajTypeComposeAnd(left, right)
}
