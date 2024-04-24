package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.TypeChecker

class ControlFlowTypeCheckHandler(val typeChecker: TypeChecker) {
  def visit(node: Conditional): TypeNode = {
    typeChecker.assertType(MajBoolType(), typeChecker.visit(node.condition))
    typeChecker.visit(node.ifTrue)
    node.elseIfTrue.map(typeChecker.visit)
    MajVoidType()
  }

  def visit(node: Loop): TypeNode = {
    typeChecker.assertType(MajBoolType(), typeChecker.visit(node.condition))
    typeChecker.visit(node.body)
    MajVoidType()
  }

}
