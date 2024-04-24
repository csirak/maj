package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.{BaseTypeChecker, TypeChecker}

class OperatorTypeCheckHandler(val typeChecker: TypeChecker) {
  def visit(node: Not): MajBoolType = {
    typeChecker.assertType(new MajBoolType(), typeChecker.visit(node.node))
    new MajBoolType()
  }

  def visit(node: AstOperator): MajTypeComposeOr = {
    typeChecker.assertType(BaseTypeChecker.boolOrInt, typeChecker.visit(node.left))
    typeChecker.assertType(BaseTypeChecker.boolOrInt, typeChecker.visit(node.right))
    BaseTypeChecker.boolOrInt
  }


}
