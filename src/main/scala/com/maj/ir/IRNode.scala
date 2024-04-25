package com.maj.ir


sealed trait IRNode


abstract class Scalar extends IRNode

case class IntIR(val value: Number) extends Scalar

case class BoolIR(val value: Boolean) extends Scalar

case class NullIR() extends Scalar

case class CharIR(val value: Char) extends Scalar

case class IdenIR(val name: String) extends IRNode

case class BlockIR(val statements: List[IRNode]) extends IRNode

case class AssignIR(val name: String, val value: IRNode) extends IRNode

case class CallIR(val callee: String, val args: List[IdenIR]) extends IRNode

case class FuncIR(val name: String, val params: List[IdenIR], val body: BlockIR) extends IRNode

case class ReturnIR(val term: IRNode) extends IRNode

case class ConditionalJumpIR(val condition: IRNode, val ifTrue: IRNode) extends IRNode


abstract class IRBinaryOperator extends IRNode {
  def left: IdenIR

  def right: IdenIR
}

case class NotIR(val node: IdenIR) extends IRNode

case class EqualsIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class AndIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class OrIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class AddIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class SubIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class MulIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class DivIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class ModIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class LessIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class GreaterIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class LessEqIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class GreaterEqIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator

case class NotEqIR(val left: IdenIR, val right: IdenIR) extends IRBinaryOperator



