package com.maj

import com.maj.ast._

trait Visitor[T] {
  def visit(node: ASTNode): T
}
