package com.maj.codegen

import com.maj.Visitor
import com.maj.ast._
import com.maj.codegen.emitters.Emitter
import com.maj.codegen.handlers._

class CodeGenerator(parent: OffsetEnvironment = null)(implicit emitter: Emitter) extends OffsetEnvironment(parent) with Visitor[Unit] {
  private val functionsHandler = new FunctionCodeGenHandler(this)
  private val operatorsHandler = new OperatorCodeGenHandler(this)
  private val scalarsHandler = new ScalarCodeGenHandler(this)
  private val variablesHandler = new VariableCodeGenHandler(this)
  private val controlFlowHandler = new ControlFlowCodeGenHandler(this)
  if (parent == null) emitter.emit(RiscVTemplates.start)


  def visit(node: ASTNode): Unit = {
    node match {
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

      case (node: MajInt) => scalarsHandler.visit(node)
      case (node: MajBool) => scalarsHandler.visit(node)
      case (node: MajNull) => scalarsHandler.visit(node)
      case (node: MajChar) => scalarsHandler.visit(node)

      case (node: Assign) => variablesHandler.visit(node)
      case (node: Create) => variablesHandler.visit(node)
      case (node: Iden) => variablesHandler.visit(node)
      case (node: TypeDef) => variablesHandler.visit(node)

      case (node: Conditional) => controlFlowHandler.visit(node)
      case (node: Loop) => controlFlowHandler.visit(node)

      case _ => println("CodeGenerator: Unhandled node: " + node)
    }
  }


}
