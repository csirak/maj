package com.maj.typecheck

import com.maj.Visitor
import com.maj.ast._
import com.maj.typecheck.handlers._

class TypeChecker(parent: TypeEnvironment = null) extends TypeEnvironment(parent) with Visitor[TypeNode] {
  private val scalarHandler = new ScalarTypeCheckHandler(this)
  private val operatorHandler = new OperatorTypeCheckHandler(this)
  private val variableHandler = new VariableTypeCheckHandler(this)
  private val functionHandler = new FunctionTypeCheckHandler(this)
  private val controlFlowHandler = new ControlFlowTypeCheckHandler(this)

  override def visit(node: ASTNode): TypeNode = {
    println(s"Visiting $node")
    node match {
      case (node: Main) => functionHandler.visit(node)
      case (node: Assert) => functionHandler.visit(node)
      case (node: Function) => functionHandler.visit(node)
      case (node: Return) => functionHandler.visit(node)
      case (node: Block) => functionHandler.visit(node)
      case (node: Call) => functionHandler.visit(node)

      case (node: Add) => operatorHandler.visitMath(node)
      case (node: Sub) => operatorHandler.visitMath(node)
      case (node: Mul) => operatorHandler.visitMath(node)
      case (node: Div) => operatorHandler.visitMath(node)
      case (node: Mod) => operatorHandler.visitMath(node)

      case (node: LessThan) => operatorHandler.visitCompare(node)
      case (node: GreaterThan) => operatorHandler.visitCompare(node)
      case (node: LessThanOrEquals) => operatorHandler.visitCompare(node)
      case (node: GreaterThanOrEquals) => operatorHandler.visitCompare(node)

      case (node: Not) => operatorHandler.visit(node)
      case (node: And) => operatorHandler.visitBool(node)
      case (node: Or) => operatorHandler.visitBool(node)

      case (node: Equals) => operatorHandler.visitEquals(node)
      case (node: NotEquals) => operatorHandler.visitEquals(node)

      case (node: MajInt) => scalarHandler.visit(node)
      case (node: MajBool) => scalarHandler.visit(node)
      case (node: MajNull) => scalarHandler.visit(node)

      case (node: Assign) => variableHandler.visit(node)
      case (node: Create) => variableHandler.visit(node)
      case (node: Iden) => variableHandler.visit(node)

      case (node: Conditional) => controlFlowHandler.visit(node)
      case (node: Loop) => controlFlowHandler.visit(node)

      case _ => throw new RuntimeException("Not implemented")
    }
  }

  def assertType(expected: TypeNode, actual: TypeNode): Unit = {
    if (!expected.accepts(actual)) {
      throw new RuntimeException(s"Expected type $expected but got $actual")
    }
  }
}

object BaseTypeChecker {
  def apply(): TypeChecker = {
    val typeChecker = new TypeChecker()
    typeChecker.addType("int", MajIntType())
    typeChecker.addType("bool", MajBoolType())
    typeChecker.addType("void", MajVoidType())
    typeChecker.addType("putchar", MajFuncType("void", List("int")))
    typeChecker
  }

}
