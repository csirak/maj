package com.maj.ast

import com.maj.codegen.Environment


abstract class Operator extends ASTNode {
  def left: ASTNode

  def right: ASTNode

  override def equals(node: ASTNode): Boolean = false

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    left.emit(env)
    emitter.emitLine("addi sp, sp, -8")
    emitter.emitLine("sd a0, 8(sp)")
    right.emit(env)
    emitter.emitLine("ld a1, 8(sp)")
    emitter.emitLine("addi sp, sp, 8")
  }

  def get(left: ASTNode, right: ASTNode): Operator

}

case class Not(val node: ASTNode = null) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    node.emit(env)
    emitter.emitLine("seqz a0, a0")
  }

  def get(one: ASTNode): ASTNode = Not(one)


}

case class Equals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Equals(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    emitter.emitLine("xor a0, a0, a1")
    emitter.emitLine("seqz a0, a0")
  }
}

case class LessThanOrEquals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = LessThanOrEquals(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    // inc so equal to case is met
    emitter.emitLine("addi a0, a0, 1")
    // a1 is the original value from the stack preserving order
    emitter.emitLine("slt a0, a1, a0")
  }
}

case class LessThan(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = LessThan(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    // a1 is the original value from the stack preserving order
    emitter.emitLine("slt a0, a1, a0")
  }
}

case class GreaterThanOrEquals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = GreaterThanOrEquals(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    // inc so equal to case is met
    emitter.emitLine("addi a1, a1, 1")
    // a1 is the original value from the stack preserving order
    emitter.emitLine("slt a0, a0, a1")
  }
}

case class GreaterThan(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = GreaterThan(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    emitter.emitLine("slt a0, a0, a1")

  }
}


case class NotEquals(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = NotEquals(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    emitter.emitLine("xor a0, a0, a1")
  }
}

case class Add(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Add(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    emitter.emitLine("add a0, a0, a1")
  }
}

case class Sub(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Sub(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    // a1 is the original value from the stack preserving order
    emitter.emitLine("sub a0, a1, a0")
  }
}

case class Mul(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Mul(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    emitter.emitLine("mul a0, a0, a1")
  }
}

case class Div(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Div(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    // a1 is the original value from the stack preserving order
    emitter.emitLine("div a0, a1, a0")
  }
}


case class Mod(val left: ASTNode = null, val right: ASTNode = null) extends Operator {
  override def get(left: ASTNode, right: ASTNode): Operator = Mod(left, right)

  override def emit(env: Environment)(implicit emitter: Emitter): Unit = {
    super.emit(env)
    // a1 is the original value from the stack preserving order
    emitter.emitLine("rem a0, a1, a0")
  }
}