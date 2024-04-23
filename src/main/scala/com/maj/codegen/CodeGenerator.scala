package com.maj.codegen

import com.maj.Visitor
import com.maj.ast._
import com.maj.codegen.handlers._

class CodeGenerator(implicit emitter: Emitter) extends Environment with Visitor[Unit] {
  private val functionsHandler = new FunctionsCodeGenHandler(this)
  private val operatorsHandler = new OperatorsCodeGenHandler(this)
  private val scalarsHandler = new ScalarsCodeGenHandler(this)
  private val variablesHandler = new VariablesCodeGenHandler(this)
  private val controlFlowHandler = new ControlFlowCodeGenHandler(this)

  def visit(node: ASTNode): Unit = node match {
    case (node: Main) => functionsHandler.visit(node)
    case (node: Assert) => functionsHandler.visit(node)
    case (node: Function) => functionsHandler.visit(node)
    case (node: Return) => functionsHandler.visit(node)
    case (node: Block) => functionsHandler.visit(node)
    case (node: Call) => functionsHandler.visit(node)

    case (node: Add) => operatorsHandler.visit(node)
    case (node: Sub) => operatorsHandler.visit(node)
    case (node: Mul) => operatorsHandler.visit(node)
    case (node: Div) => operatorsHandler.visit(node)
    case (node: Mod) => operatorsHandler.visit(node)

    case (node: Not) => operatorsHandler.visit(node)
    case (node: And) => operatorsHandler.visit(node)
    case (node: Or) => operatorsHandler.visit(node)
    case (node: Equals) => operatorsHandler.visit(node)
    case (node: NotEquals) => operatorsHandler.visit(node)
    case (node: LessThan) => operatorsHandler.visit(node)
    case (node: GreaterThan) => operatorsHandler.visit(node)
    case (node: LessThanOrEquals) => operatorsHandler.visit(node)
    case (node: GreaterThanOrEquals) => operatorsHandler.visit(node)

    case (node: Numeric) => scalarsHandler.visit(node)
    case (node: Bool) => scalarsHandler.visit(node)
    case (node: Null) => scalarsHandler.visit(node)

    case (node: Assign) => variablesHandler.visit(node)
    case (node: Create) => variablesHandler.visit(node)
    case (node: Iden) => variablesHandler.visit(node)

    case (node: Conditional) => controlFlowHandler.visit(node)
    case (node: Loop) => controlFlowHandler.visit(node)
  }


}
