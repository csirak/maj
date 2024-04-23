package com.maj.ast

sealed trait ASTNode

case class Conditional(val condition: ASTNode, val ifTrue: ASTNode, val elseIfTrue: Option[ASTNode] = None) extends ASTNode

case class Loop(val condition: ASTNode, val body: ASTNode) extends ASTNode

case class Call(val callee: String, val args: List[ASTNode]) extends ASTNode

case class Return(val term: ASTNode) extends ASTNode

case class Block(val statements: List[ASTNode]) extends ASTNode

case class Function(val name: String, val params: List[String], val body: ASTNode) extends ASTNode

case class Main(val body: Block) extends ASTNode

case class Assert(val condition: ASTNode) extends ASTNode

case class Numeric(val value: Number) extends ASTNode

case class Bool(val value: Boolean) extends ASTNode

case class Null() extends ASTNode

case class Assign(val name: String, val value: ASTNode) extends ASTNode

case class Create(val name: String, val value: ASTNode) extends ASTNode

case class Iden(val value: String) extends ASTNode

case class Not(val node: ASTNode = null) extends ASTNode {
  def get(one: ASTNode): ASTNode = Not(one)
}

abstract class Operator extends ASTNode {
  def left: ASTNode

  def right: ASTNode

  def get(left: ASTNode, right: ASTNode): Operator
}

case class Equals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Equals(left, right)
}

case class And(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = And(left, right)
}

case class Or(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Or(left, right)
}

case class LessThanOrEquals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = LessThanOrEquals(left, right)
}

case class LessThan(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = LessThan(left, right)
}

case class GreaterThanOrEquals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = GreaterThanOrEquals(left, right)
}

case class GreaterThan(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = GreaterThan(left, right)
}


case class NotEquals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = NotEquals(left, right)
}

case class Add(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Add(left, right)
}

case class Sub(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Sub(left, right)
}

case class Mul(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Mul(left, right)
}

case class Div(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Div(left, right)
}

case class Mod(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Mod(left, right)
}