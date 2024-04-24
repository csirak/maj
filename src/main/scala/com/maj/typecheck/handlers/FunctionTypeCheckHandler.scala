package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.TypeChecker

class FunctionTypeCheckHandler(val typeChecker: TypeChecker) {
  def visit(node: Function): TypeNode = {
    typeChecker.addType(node.name, node.signature)

    val localTypeCheck = new TypeChecker(typeChecker)
    val argTypes = node.signature.params
    val args = node.params
    args.zip(argTypes).foreach {
      case (arg, argType) => {
        val typ = localTypeCheck.getOrThrow(argType.toString)
        localTypeCheck.addType(arg, typ)
      }
    }
    localTypeCheck.visit(node.body)
    node.signature
  }

  def visit(node: Call): TypeNode = {
    val expected = typeChecker.getType(node.callee)
    if (expected == MajTypeUndefined()) {
      throw new RuntimeException(s"Function ${node.callee} not found")
    }
    val func = expected.asInstanceOf[MajFuncType]
    val args = node.args.map(typeChecker.visit)
    val combined: List[(TypeNode, TypeNode)] = func.params.map(_.toString).map(typeChecker.getType).zipAll(args, MajTypeUndefined(), MajTypeUndefined())
    combined.foreach {
      case (expected, actual) => typeChecker.assertType(expected, actual)
    }
    typeChecker.getOrThrow(func.returnType.toString)
  }

  def visit(node: Return): TypeNode = {
    typeChecker.visit(node.term)
  }

  def visit(node: Block): TypeNode = {
    node.statements.map(typeChecker.visit)
    MajVoidType()
  }
}
