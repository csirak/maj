package com.maj.codegen.handlers

import com.maj.ast._
import com.maj.codegen.{CodeGenerator, RiscVTemplates}
import com.maj.emitters.Emitter

class OperatorCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter[String]) {
  private def binaryOpPrologue(node: AstOperator): Unit = {
    codeGenerator.visit(node.left)
    emitter.emit(RiscVTemplates.push1("a0"))
    codeGenerator.visit(node.right)
    emitter.emitLine("mv\t\ta1, a0")
    emitter.emit(RiscVTemplates.pop1("a0"))
  }

  def handle(node: Add): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("add\t\ta0, a0, a1")
  }

  def handle(node: Sub): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("sub\t\ta0, a0, a1")
  }

  def handle(node: Mul): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("mul\t\ta0, a0, a1")
  }

  def handle(node: Div): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("div\t\ta0, a0, a1")
  }

  def handle(node: Mod): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("rem\t\ta0, a0, a1")
  }

  def handle(node: And): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("snqz\t\ta0, a0")
    emitter.emitLine("snqz\t\ta1, a1")
    emitter.emitLine("and\t\ta0, a0, a1")
  }

  def handle(node: Or): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("snqz\t\ta0, a0")
    emitter.emitLine("snqz\t\ta1, a1")
    emitter.emitLine("or\t\ta0, a0, a1")
  }

  def handle(node: Not): Unit = {
    codeGenerator.visit(node.value)
    emitter.emitLine("seqz\t\ta0, a0")
  }

  def handle(node: Equals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("xor\t\ta0, a0, a1")
    emitter.emitLine("seqz\t\ta0, a0")
  }

  def handle(node: NotEquals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("xor\t\ta0, a0, a1")
  }

  def handle(node: LessThan): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("slt\t\ta0, a0, a1")
  }

  def handle(node: GreaterThan): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("sgt\t\ta0, a0, a1")
  }

  def handle(node: LessThanOrEquals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("addi\t\ta1, a1, 1")
    emitter.emitLine("slt\t\ta0, a0, a1")
  }

  def handle(node: GreaterThanOrEquals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("addi\t\ta0, a0, 1")
    emitter.emitLine("sgt\t\ta0, a0, a1")
  }


}
