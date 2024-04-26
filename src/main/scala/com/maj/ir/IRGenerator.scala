package com.maj.ir

import com.maj.Visitor
import com.maj.ast._
import com.maj.emitters.Emitter
import com.maj.ir.handlers._


class IRGenerator(parent: IREnviroment = null)(implicit emitter: Emitter[IRNode]) extends IREnviroment(parent) with Visitor[Option[IRNode]] {
  private val functionHandler = new FunctionIRGenHandler(this)
  private val operatorHandler = new OperatorIRGenHandler(this)
  private val controlFlowHandler = new ControlFlowIRGenHandler(this)
  private val scalarHandler = new ScalarIRGenHandler(this)
  private val variableHandler = new VariableIRGenHandler(this)


  def visit(node: ASTNode): Option[IRNode] = {
    node match {
      case (node: Function) => functionHandler.handle(node)
      case (node: Return) => functionHandler.handle(node)
      case (node: Block) => functionHandler.handle(node)
      case (node: Call) => functionHandler.handle(node)
      case (node: AsmBlock) => functionHandler.handle(node)

      case (node: Not) => operatorHandler.handle(node)
      case (node: Operator[ASTNode]) => operatorHandler.handle(node)

      case (node: Iden) => scalarHandler.handle(node)
      case (node: Scalar) => scalarHandler.handle(node)
      case (node: Assignable) => variableHandler.handle(node)

      case (node: Conditional) => controlFlowHandler.handle(node)
      case (node: Loop) => controlFlowHandler.handle(node)

      case (node: TypeDef) => None

      case _ => {
        println("IRGenerator: Unhandled node: " + node)
        None
      }
    }


  }

  private def handleAssign(node: Assignable): Option[IRNode] = {
    val anonRef = visit(node.value).getOrElse(throw new Exception("VariableIRGenHandler: Assign: Value not found"))
    val namedRef = node match {
      case Assign(name, _) => getVar(name)
      // const and var
      case node => addVar(node.name)
    }
    emitter.emit(AssignIR(namedRef, anonRef))
    None
  }


  def assignToAnonVarAndEmit(value: IRNode): IdenIR = {
    val anonRef = addAnonVar()
    emitter.emit(AssignIR(anonRef, value))
    anonRef
  }

  def getResultInAnonVar(value: Option[IRNode]): IdenIR = {
    value.getOrElse(throw new Exception(s"IRGenerator: Operator: Left not found ${value}")) match {
      case node: IdenIR => node
      case node: ScalarIR => assignToAnonVarAndEmit(node)
      case node: NotIR => assignToAnonVarAndEmit(node)
      case node: WrappedOperatorIR => assignToAnonVarAndEmit(node)
      case _ => throw new Exception(s"IRGenerator: Invalid Value: Not operation, variable, or scalar $value")
    }
  }

}
