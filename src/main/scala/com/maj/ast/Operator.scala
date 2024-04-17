package com.maj.ast


abstract class Operator extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  def get(left: ASTNode, right: ASTNode): Operator
}

case class Not(val node: ASTNode = null) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  def get(one: ASTNode): ASTNode = Not(one)

}

case class Equals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Equals(left, right)
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