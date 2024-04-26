package com.maj.ir

import com.maj.ast._


sealed trait IRNode


case class NullIR() extends IRNode {
  override def toString: String = "null"
}

case class ScalarIR(value: Scalar) extends IRNode {
  override def toString: String = if (value.value == null) "null" else value.value.toString
}

case class IdenIR(val symbolIndex: Int) extends IRNode {
  override def toString: String = s"t$symbolIndex"
}

case class NotIR(val value: IRNode) extends IRNode {
  override def toString: String = s"!$value"
}

case class LabelIR(val value: Int) extends IRNode {
  override def toString: String = s".L$value:"
}

case class JumpIfNotZero(val condition: IdenIR, val label: LabelIR) extends IRNode {
  override def toString: String = s"bneqz $condition $label"
}

case class JumpIR(val label: LabelIR) extends IRNode {
  override def toString: String = s"jump $label"
}

case class AssignIR(val iden: IdenIR, val value: IRNode) extends IRNode {
  override def toString: String = s"$iden = $value"
}

case class CallIR(val callee: String, val args: List[IdenIR]) extends IRNode {
  override def toString: String = s"call $callee(${args.mkString(", ")})"
}

case class FuncIR(val name: String) extends IRNode {
  override def toString: String = s"func $name:"
}

case class ReturnIR(val term: IRNode) extends IRNode {
  override def toString: String = s"return $term"
}


case class ASMBlockIR(val statements: List[String]) extends IRNode {
  override def toString: String = "asm { \n" ++ statements.mkString("\n") ++ "}"
}

case class WrappedOperatorIR(val operator: Operator[ASTNode], left: IRNode, right: IRNode) extends IRNode {
  override def toString: String = {
    val opStr = operator match {
      case n: Add => "+"
      case n: Sub => "-"
      case n: Mul => "*"
      case n: Div => "/"
      case n: Mod => "%"
      case n: Equals => "=="
      case n: NotEquals => "!="
      case n: LessThan => "<"
      case n: LessThanOrEquals => "<="
      case n: GreaterThan => ">"
      case n: GreaterThanOrEquals => ">="
      case n: And => "&&"
      case n: Or => "||"
    }
    s"$left $opStr $right"
  }
}
