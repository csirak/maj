package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck._

class FunctionTypeCheckHandler(val typeChecker: TypeChecker) {
  def handle(node: Function): TypeNode = {
    typeChecker.addType(node.name, node.signature)
    typeChecker.setScope(node.name)
    val argTypes = node.signature.params
    val args = node.params
    args.zip(argTypes).foreach {
      case (arg, argType) => {
        val typ = typeChecker.getTypeOrThrow(argType.toString)
        typeChecker.addType(arg, typ)
      }
    }
    typeChecker.visit(node.body)
    node.signature
  }

  def handle(node: Call): TypeNode = {
    val expected = typeChecker.getType(node.callee)
    if (expected.isEmpty) {
      throw new RuntimeException(s"Function ${node.callee} not found")
    }
    val func = expected.get.asInstanceOf[MajFuncType]
    val args = node.args.map(typeChecker.visit)
    val combined = func.params.map(_.toString).map(typeChecker.getType).zip(args)
    combined.foreach {
      case (expected, actual) => typeChecker.assertType(expected.getOrElse(MajTypeUndefined()), actual)
    }
    typeChecker.getTypeOrThrow(func.returnType.toString)
  }

  def handle(node: Return): TypeNode = {
    val nodeType = typeChecker.visit(node.term)


    val scopedReturn = for {
      name <- typeChecker.getScope
      scopeReturnType <- typeChecker.getType(name)
    } yield scopeReturnType

    scopedReturn match {
      case Some(MajFuncType(returnType, _)) => typeChecker.assertType(returnType, nodeType)
      case _ => throw new RuntimeException("Return statement outside of scope")
    }
    nodeType
  }


  def handle(node: Block): TypeNode = {
    val scopedChecker = new TypeChecker(typeChecker)
    val returns = node.statements.flatMap(stmt => scopedChecker.visit(stmt) match {
      case ret@(MajReturnType(_) | MajConditionalReturn(_)) => Some(ret)
      case _ => None
    })

    val out = if (returns.isEmpty) MajVoidType()
    else {
      returns.foldLeft[TypeNode](MajVoidType())((acc, ret) => ret match {
        case MajReturnType(ret) if acc == MajVoidType() => ret
        case MajReturnType(ret) => MajTypeComposeOr(acc, ret)
        case MajConditionalReturn(typ) if acc == MajVoidType() => typ
        case MajConditionalReturn(typ) => MajTypeComposeOr(acc, typ)
      })
    }
    MajReturnType(out)
  }

  def handle(node: AsmBlock): TypeNode = {
    println("WARNING: ASM block not type checked")
    MajVoidType()
  }

}
