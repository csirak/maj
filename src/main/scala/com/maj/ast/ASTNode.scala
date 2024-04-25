package com.maj.ast

import com.maj.typecheck.{MajFuncType, TypeNode}

sealed trait ASTNode

case class Conditional(val condition: ASTNode, val ifTrue: ASTNode, val elseIfTrue: Option[ASTNode] = None) extends ASTNode

case class Loop(val condition: ASTNode, val body: ASTNode) extends ASTNode

case class Call(val callee: String, val args: List[ASTNode]) extends ASTNode

case class Return(val term: ASTNode) extends ASTNode

case class Block(val statements: List[ASTNode]) extends ASTNode {
  def ++(other: Block): Block = Block(statements ++ other.statements)
}

case class AsmBlock(val statements: List[String]) extends ASTNode

case class Function(val name: String, val params: List[String], val signature: MajFuncType, val body: ASTNode) extends ASTNode

case class MajInt(val value: Number) extends ASTNode

case class MajBool(val value: Boolean) extends ASTNode

case class MajNull() extends ASTNode

case class MajChar(val value: Char) extends ASTNode

case class Assign(val name: String, val value: ASTNode) extends ASTNode

case class MutableVar(val name: String, val value: ASTNode) extends ASTNode

case class ConstVar(val name: String, val value: ASTNode) extends ASTNode

case class Iden(val value: String) extends ASTNode

case class TypeDef(val name: String, val typ: TypeNode) extends ASTNode

case class Not(val node: ASTNode = null) extends ASTNode {
  def get(one: ASTNode): ASTNode = Not(one)
}

abstract class AstOperator extends Operator[ASTNode] with ASTNode {
  def left: ASTNode

  def right: ASTNode

  def get(left: ASTNode, right: ASTNode): AstOperator
}

case class Equals(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = Equals(left, right)
}

case class And(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = And(left, right)
}

case class Or(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = Or(left, right)
}

case class LessThanOrEquals(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = LessThanOrEquals(left, right)
}

case class LessThan(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = LessThan(left, right)
}

case class GreaterThanOrEquals(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = GreaterThanOrEquals(left, right)
}

case class GreaterThan(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = GreaterThan(left, right)
}

case class NotEquals(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = NotEquals(left, right)
}

case class Add(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = Add(left, right)
}

case class Sub(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = Sub(left, right)
}

case class Mul(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = Mul(left, right)
}

case class Div(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = Div(left, right)
}

case class Mod(val left: ASTNode = null, val right: ASTNode = null) extends AstOperator {
  override def get(left: ASTNode, right: ASTNode): AstOperator = Mod(left, right)
}