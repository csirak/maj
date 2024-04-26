package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.{MajConstant, MajVoidType, TypeChecker, TypeNode}

class VariableTypeCheckHandler(val typeChecker: TypeChecker) {

  def handle(node: Assign): TypeNode = {
    val typ = typeChecker.getTypeOrThrow(node.name)
    typeChecker.assertType(typ, typeChecker.visit(node.value))
    MajVoidType()
  }

  def handle(node: MutableVar): TypeNode = {
    typeChecker.getType(node.name) match {
      case None =>
        typeChecker.addType(node.name, typeChecker.visit(node.value))
        MajVoidType()
      case Some(_) =>
        throw new RuntimeException(s"Variable ${node.name} already exists")
    }
  }

  def handle(node: ConstVar): TypeNode = {
    typeChecker.getType(node.name) match {
      case None =>
        typeChecker.addType(node.name, MajConstant(typeChecker.visit(node.value)))
        MajVoidType()
      case _ =>
        throw new RuntimeException(s"Variable ${node.name} already exists")
    }
  }

  def handle(node: Iden): TypeNode = typeChecker.getType(node.value) match {
    case Some(t) => t
    case None =>
      throw new RuntimeException(s"Variable ${node.value} not found")
  }

  def handle(node: TypeDef): TypeNode = {
    typeChecker.addType(node.name, node.value)
    MajVoidType()
  }
}
