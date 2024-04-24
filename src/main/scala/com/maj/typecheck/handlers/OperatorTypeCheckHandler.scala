package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.TypeChecker

class OperatorTypeCheckHandler(val typeChecker: TypeChecker) {
  def visit(node: Not): MajBoolType = {
    typeChecker.assertType(new MajBoolType(), typeChecker.visit(node.node))
    new MajBoolType()
  }

  def visitMath(node: Operator): MajIntType = {
    typeChecker.assertType(new MajIntType(), typeChecker.visit(node.left))
    typeChecker.assertType(new MajIntType(), typeChecker.visit(node.right))
    new MajIntType()
  }

  def visitBool(node: Operator): MajBoolType = {
    typeChecker.assertType(new MajBoolType(), typeChecker.visit(node.left))
    typeChecker.assertType(new MajBoolType(), typeChecker.visit(node.right))
    new MajBoolType()
  }

  def visitEquals(node: Operator): MajBoolType = {
    val left = typeChecker.visit(node.left)
    val right = typeChecker.visit(node.right)
    typeChecker.assertType(left, right)
    new MajBoolType()
  }

  def visitCompare(node: Operator): MajBoolType = {
    typeChecker.assertType(new MajIntType(), typeChecker.visit(node.left))
    typeChecker.assertType(new MajIntType(), typeChecker.visit(node.right))
    new MajBoolType()
  }


}
