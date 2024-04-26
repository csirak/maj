package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck._

class ScalarTypeCheckHandler(val typeChecker: TypeChecker) {

  def handle(node: MajInt): MajIntType = new MajIntType()

  def handle(node: MajBool): TypeNode = new MajBoolType()

  def handle(node: MajNull): TypeNode = new MajVoidType()

  def handle(node: MajChar): TypeNode = new MajTypeComposeOr(new MajCharType(), new MajIntType())
}
