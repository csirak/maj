package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.{BaseTypeChecker, MajBoolType, MajTypeComposeOr, TypeChecker}

class OperatorTypeCheckHandler(val typeChecker: TypeChecker) {
  def handle(node: Not): MajBoolType = {
    typeChecker.assertType(new MajBoolType(), typeChecker.visit(node.value))
    new MajBoolType()
  }

  def handle(node: AstOperator): MajTypeComposeOr = {
    typeChecker.assertType(BaseTypeChecker.boolOrInt, typeChecker.visit(node.left))
    typeChecker.assertType(BaseTypeChecker.boolOrInt, typeChecker.visit(node.right))
    BaseTypeChecker.boolOrInt
  }


}
