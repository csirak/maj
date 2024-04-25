package com.maj.typecheck.handlers

import com.maj.ast._
import com.maj.typecheck.TypeChecker

class ControlFlowTypeCheckHandler(val typeChecker: TypeChecker) {
  def visit(node: Conditional): TypeNode = {
    typeChecker.assertType(MajBoolType(), typeChecker.visit(node.condition))
    val trueReturn = typeChecker.visit(node.ifTrue)
    val elseReturn = node.elseIfTrue.map(typeChecker.visit).getOrElse(MajVoidType())

    (trueReturn, elseReturn) match {
      case (MajReturnType(trueType), MajReturnType(elseType)) => {
        MajReturnType(MajTypeComposeOr(trueType, elseType))
      }
      case (MajReturnType(trueType), MajVoidType()) => {
        MajConditionalReturn(trueType)
      }
      case (MajVoidType(), MajReturnType(elseType)) => {
        MajConditionalReturn(elseType)
      }
      case (MajVoidType(), MajVoidType()) => {
        MajVoidType()
      }
    }
  }

  def visit(node: Loop): TypeNode = {
    typeChecker.assertType(MajBoolType(), typeChecker.visit(node.condition))
    val loopReturn = typeChecker.visit(node.body)
    loopReturn match {
      case MajReturnType(typ) => MajConditionalReturn(typ)
      case _ => MajVoidType()
    }
  }

}
