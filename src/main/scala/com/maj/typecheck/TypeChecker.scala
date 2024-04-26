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
      case (node: Function) => functionHandler.handle(node)
      case (node: Return) => functionHandler.handle(node)
      case (node: Block) => functionHandler.handle(node)
      case (node: Call) => functionHandler.handle(node)
      case (node: AsmBlock) => functionHandler.handle(node)

      case (node: Add) => operatorHandler.handle(node)
      case (node: Sub) => operatorHandler.handle(node)
      case (node: Mul) => operatorHandler.handle(node)
      case (node: Div) => operatorHandler.handle(node)
      case (node: Mod) => operatorHandler.handle(node)

      case (node: LessThan) => operatorHandler.handle(node)
      case (node: GreaterThan) => operatorHandler.handle(node)
      case (node: LessThanOrEquals) => operatorHandler.handle(node)
      case (node: GreaterThanOrEquals) => operatorHandler.handle(node)

      case (node: Not) => operatorHandler.handle(node)
      case (node: And) => operatorHandler.handle(node)
      case (node: Or) => operatorHandler.handle(node)

      case (node: Equals) => operatorHandler.handle(node)
      case (node: NotEquals) => operatorHandler.handle(node)

      case (node: MajInt) => scalarHandler.handle(node)
      case (node: MajBool) => scalarHandler.handle(node)
      case (node: MajNull) => scalarHandler.handle(node)
      case (node: MajChar) => scalarHandler.handle(node)

      case node: Assign => variableHandler.handle(node)
      case (node: MutableVar) => variableHandler.handle(node)
      case (node: Iden) => variableHandler.handle(node)
      case (node: ConstVar) => variableHandler.handle(node)
      case (node: TypeDef) => variableHandler.handle(node)

      case (node: Conditional) => controlFlowHandler.handle(node)
      case (node: Loop) => controlFlowHandler.handle(node)
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
