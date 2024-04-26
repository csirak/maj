package com.maj.codegen

import com.maj.Visitor
import com.maj.ast._
import com.maj.codegen.handlers._
import com.maj.emitters.Emitter

class CodeGenerator(parent: OffsetEnvironment = null)(implicit emitter: Emitter[String]) extends OffsetEnvironment(parent) with Visitor[Unit] {
  private val functionHandler = new FunctionCodeGenHandler(this)
  private val operatorHandler = new OperatorCodeGenHandler(this)
  private val scalarHandler = new ScalarCodeGenHandler(this)
  private val variableHandler = new VariableCodeGenHandler(this)
  private val controlFlowHandler = new ControlFlowCodeGenHandler(this)
  if (parent == null) emitter.emit(RiscVTemplates.start)


  def visit(node: ASTNode): Unit = {
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

      case (node: Not) => operatorHandler.handle(node)
      case (node: And) => operatorHandler.handle(node)
      case (node: Or) => operatorHandler.handle(node)
      case (node: Equals) => operatorHandler.handle(node)
      case (node: NotEquals) => operatorHandler.handle(node)
      case (node: LessThan) => operatorHandler.handle(node)
      case (node: GreaterThan) => operatorHandler.handle(node)
      case (node: LessThanOrEquals) => operatorHandler.handle(node)
      case (node: GreaterThanOrEquals) => operatorHandler.handle(node)

      case (node: MajInt) => scalarHandler.handle(node)
      case (node: MajBool) => scalarHandler.handle(node)
      case (node: MajNull) => scalarHandler.handle(node)
      case (node: MajChar) => scalarHandler.handle(node)

      case (node: Assign) => variableHandler.handle(node)
      case (node: MutableVar) => variableHandler.handle(node)
      case (node: Iden) => variableHandler.handle(node)
      case (node: TypeDef) => variableHandler.handle(node)
      case (node: ConstVar) => variableHandler.handle(node)

      case (node: Conditional) => controlFlowHandler.handle(node)
      case (node: Loop) => controlFlowHandler.handle(node)

      case _ => println("CodeGenerator: Unhandled node: " + node)
    }
  }


}
