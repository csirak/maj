package com.maj.typecheck

import com.maj.Visitor
import com.maj.ast._
import com.maj.typecheck.handlers._

class TypeChecker(val scopeTag: String = "", parent: TypeEnvironment = null) extends TypeEnvironment(parent) with Visitor[TypeNode] {
  private val scalarHandler = new ScalarTypeCheckHandler(this)
  private val operatorHandler = new OperatorTypeCheckHandler(this)
  private val variableHandler = new VariableTypeCheckHandler(this)
  private val functionHandler = new FunctionTypeCheckHandler(this)
  private val controlFlowHandler = new ControlFlowTypeCheckHandler(this)

  addType("int", MajIntType())
  addType("bool", MajBoolType())
  addType("void", MajVoidType())
  addType("char", MajTypeComposeOr(MajCharType(), MajIntType()))

  override def visit(node: ASTNode): TypeNode = {
    node match {
      case (node: Function) => functionHandler.visit(node)
      case (node: Return) => functionHandler.visit(node)
      case (node: Block) => functionHandler.visit(node)
      case (node: Call) => functionHandler.visit(node)
      case (node: AsmBlock) => functionHandler.visit(node)

      case (node: Add) => operatorHandler.visit(node)
      case (node: Sub) => operatorHandler.visit(node)
      case (node: Mul) => operatorHandler.visit(node)
      case (node: Div) => operatorHandler.visit(node)
      case (node: Mod) => operatorHandler.visit(node)

      case (node: LessThan) => operatorHandler.visit(node)
      case (node: GreaterThan) => operatorHandler.visit(node)
      case (node: LessThanOrEquals) => operatorHandler.visit(node)
      case (node: GreaterThanOrEquals) => operatorHandler.visit(node)

      case (node: Not) => operatorHandler.visit(node)
      case (node: And) => operatorHandler.visit(node)
      case (node: Or) => operatorHandler.visit(node)

      case (node: Equals) => operatorHandler.visit(node)
      case (node: NotEquals) => operatorHandler.visit(node)

      case (node: MajInt) => scalarHandler.visit(node)
      case (node: MajBool) => scalarHandler.visit(node)
      case (node: MajNull) => scalarHandler.visit(node)
      case (node: MajChar) => scalarHandler.visit(node)

      case (node: Assign) => variableHandler.visit(node)
      case (node: MutableVar) => variableHandler.visit(node)
      case (node: Iden) => variableHandler.visit(node)
      case (node: ConstVar) => variableHandler.visit(node)
      case (node: TypeDef) => variableHandler.visit(node)

      case (node: Conditional) => controlFlowHandler.visit(node)
      case (node: Loop) => controlFlowHandler.visit(node)
    }
  }

  def assertType(expected: TypeNode, actual: TypeNode): Unit = {
    def resolveType(t: TypeNode): Option[TypeNode] = {
      t match {
        case MajType(name) => this.getType(name)
        case _ => Some(t)
      }
    }

    if (!(expected.acceptsWithResolver(actual, resolveType) || actual.acceptsWithResolver(expected, resolveType))) {
      throw new RuntimeException(s"Expected type $expected but got $actual")
    }
  }
}


object BaseTypeChecker {
  val boolOrInt: MajTypeComposeOr = MajTypeComposeOr(MajIntType(), MajBoolType())

}
