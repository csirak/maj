package com.maj.ir

import com.maj.ast._


sealed trait IRNode


case class IRNull() extends IRNode {
  override def toString: String = "null"
}

case class IRScalar(value: Scalar) extends IRNode {
  //  override def toString: String = if (value.value == null) "null" else value.value.toString
  override def toString: String = value match {
    case MajNull() => "null"
    case MajChar(value) => s"'$value'"
    case _ => s"${value.value}"
  }
}

case class IRIden(val symbolIndex: Int) extends IRNode {
  override def toString: String = s"v$symbolIndex"
}

case class IRNot(val value: IRNode) extends IRNode {
  override def toString: String = s"!$value"
}

case class IRLabel(val value: Int) extends IRNode {
  override def toString: String = s"$toCall:"

  def toCall: String = s".L$value"
}

case class IRJumpIf(val condition: IRIden, val label: IRLabel) extends IRNode {
  override def toString: String = s"jump if($condition) ${label.toCall}"
}

case class IRJump(val label: IRLabel) extends IRNode {
  override def toString: String = s"jump ${label.toCall}"
}

case class IRAssign(val iden: IRIden, val value: IRNode) extends IRNode {
  override def toString: String = s"$iden = $value"
}

case class IRCall(val callee: String, val args: List[IRIden]) extends IRNode {
  override def toString: String = s"call $callee(${args.mkString(", ")})"
}

case class IRFunc(val name: String, val params: List[IRIden]) extends IRNode {
  override def toString: String = s"func $name (${params.mkString(", ")}):"
}

case class IRReturn(val term: IRNode) extends IRNode {
  override def toString: String = s"return $term\n"
}


case class IRAsmBlock(val statements: List[String]) extends IRNode {
  override def toString: String = "\t\tasm { \n\t\t" ++ statements.mkString("\n\t\t") ++ "}"
}

case class IROperator(val operator: Operator[ASTNode], left: IRNode, right: IRNode) extends IRNode {
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
