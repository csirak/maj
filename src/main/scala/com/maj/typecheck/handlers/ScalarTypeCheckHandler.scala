package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck._

class ScalarTypeCheckHandler(val typeChecker: TypeChecker) {

  def visit(node: MajInt): MajIntType = new MajIntType()

  def visit(node: MajBool): TypeNode = new MajBoolType()

  def visit(node: MajNull): TypeNode = new MajVoidType()

  def visit(node: MajChar): TypeNode = new MajTypeComposeOr(new MajCharType(), new MajIntType())
}
