package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.TypeChecker

class VariableTypeCheckHandler(val typeChecker: TypeChecker) {

  def visit(node: Assign): TypeNode = {
    val typ = typeChecker.getType(node.name)
    typeChecker.assertType(typ, typeChecker.visit(node.value))
    MajVoidType()
  }

  def visit(node: Create): TypeNode = {
    typeChecker.getType(node.name) match {
      case MajTypeUndefined() =>
        typeChecker.addType(node.name, typeChecker.visit(node.value))
        MajVoidType()
      case _ =>
        throw new RuntimeException(s"Variable ${node.name} already exists")
    }
  }

  def visit(node: Iden): TypeNode = typeChecker.getType(node.value) match {
    case MajTypeUndefined() =>
      throw new RuntimeException(s"Variable ${node.value} not found")
    case t => t
  }
}
