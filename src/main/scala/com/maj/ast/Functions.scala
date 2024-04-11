package com.maj.ast

case class Call(val callee: String, val args: List[ASTNode]) extends ASTNode {
  override def equals(node: ASTNode): Boolean = {
    node match {
      case call: Call => {
        (this.callee == call.callee
          && this.args.length == call.args.length
          && this.args.sameElements(call.args))
      }
      case _ => false
    }
  }
}

case class Return(val term: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false
}

case class Block(val statements: List[ASTNode]) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false
}

case class Function(val name: String, val params: List[String], val body: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false
}