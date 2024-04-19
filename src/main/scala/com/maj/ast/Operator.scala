package com.maj.ast

import com.maj.codegen.RiscV


abstract class Operator extends ASTNode {
  def left: ASTNode

  def right: ASTNode

  override def equals(node: ASTNode): Boolean = false

  override def emit(implicit emitter: Emitter): Unit = {
    left.emit
    RiscV.push(List("a0")).foreach(emitter.emitLine)
    right.emit
    RiscV.pop(List("a1")).foreach(emitter.emitLine)
  }

  def get(left: ASTNode, right: ASTNode): Operator

}

case class Not(val node: ASTNode = null) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(implicit emitter: Emitter): Unit = {
    node.emit
    emitter.emitLine("seqz a0, a0")
  }

  def get(one: ASTNode): ASTNode = Not(one)


}

case class Equals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Equals(left, right)

  override def emit(implicit emitter: Emitter): Unit = {
    super.emit
    emitter.emitLine("xor a0, a0, a1")
    emitter.emitLine("seqz a0, a0")
  }
}


case class NotEquals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = NotEquals(left, right)

  override def emit(implicit emitter: Emitter): Unit = {
    super.emit
    emitter.emitLine("xor a0, a0, a1")
  }
}

case class Add(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Add(left, right)

  override def emit(implicit emitter: Emitter): Unit = {
    super.emit
    emitter.emitLine("add a0, a0, a1")
  }
}

case class Sub(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Sub(left, right)

  override def emit(implicit emitter: Emitter): Unit = {
    super.emit
    // a1 is the original value from the stack preserving order
    emitter.emitLine("sub a0, a1, a0")
  }
}

case class Mul(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Mul(left, right)

  override def emit(implicit emitter: Emitter): Unit = {
    super.emit
    emitter.emitLine("mul a0, a0, a1")
  }
}

case class Div(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Div(left, right)

  override def emit(implicit emitter: Emitter): Unit = {
    super.emit
    // a1 is the original value from the stack preserving order
    emitter.emitLine("div a0, a1, a0")
  }
}


case class Mod(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Mod(left, right)

  override def emit(implicit emitter: Emitter): Unit = {
    super.emit
    // a1 is the original value from the stack preserving order
    emitter.emitLine("rem a0, a1, a0")
  }
}