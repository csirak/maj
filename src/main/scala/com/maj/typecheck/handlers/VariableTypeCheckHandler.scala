package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.{MajConstant, MajVoidType, TypeChecker, TypeNode}

class VariableTypeCheckHandler(val typeChecker: TypeChecker) {

  def visit(node: Assign): TypeNode = {
    val typ = typeChecker.getOrThrow(node.name)
    typeChecker.assertType(typ, typeChecker.visit(node.value))
    MajVoidType()
  }

  def visit(node: MutableVar): TypeNode = {
    typeChecker.getType(node.name) match {
      case None =>
        typeChecker.addType(node.name, typeChecker.visit(node.value))
        MajVoidType()
      case Some(_) =>
        throw new RuntimeException(s"Variable ${node.name} already exists")
    }
  }

  def visit(node: ConstVar): TypeNode = {
    typeChecker.getType(node.name) match {
      case None =>
        typeChecker.addType(node.name, MajConstant(typeChecker.visit(node.value)))
        MajVoidType()
      case _ =>
        throw new RuntimeException(s"Variable ${node.name} already exists")
    }
  }

  def visit(node: Iden): TypeNode = typeChecker.getType(node.value) match {
    case Some(t) => t
    case None =>
      throw new RuntimeException(s"Variable ${node.value} not found")
  }

  def visit(node: TypeDef): TypeNode = {
    typeChecker.addType(node.name, node.typ)
    MajVoidType()
  }
}
