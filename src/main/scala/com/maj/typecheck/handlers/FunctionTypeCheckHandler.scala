package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.TypeChecker

class FunctionTypeCheckHandler(val typeChecker: TypeChecker) {
  def visit(node: Function): TypeNode = {
    typeChecker.addType(node.name, node.signature)
    val localTypeCheck = new TypeChecker(node.name, typeChecker)
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
    val nodeType = typeChecker.visit(node.term)
    val expectedScope = typeChecker.getType(typeChecker.scopeTag)
    expectedScope match {
      case MajFuncType(returnType, _) => typeChecker.assertType(returnType, nodeType)
      case _ => throw new RuntimeException("Return statement outside of scope")
    }
    MajReturnType(nodeType)
  }

  def visit(node: Block): TypeNode = {
    //    val returns = node.statements.map(typeChecker.visit).find {
    //      case MajReturnType(_) => true
    //      case MajConditionalReturn(_) => true
    //      case _ => false
    //    }
    //
    //    if (returns.isEmpty) {
    //      MajVoidType()
    //    } else {
    //      var returnAggregator: TypeNode = returns.head
    //      for (returnAble <- returns) {
    //        returnAble match {
    //          case MajReturnType(_) => return returnAggregator
    //          case MajConditionalReturn(typ) => {
    //            returnAggregator = MajTypeComposeOr(returnAggregator, typ)
    //          }
    //          case _ => throw new RuntimeException("Invalid return type")
    //        }
    //      }
    //      returnAggregator
    //    }

    val returns = node.statements.flatMap(stmt => typeChecker.visit(stmt) match {
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
}
