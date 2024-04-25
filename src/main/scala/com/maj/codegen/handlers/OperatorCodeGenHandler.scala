package com.maj.codegen.handlers

import com.maj.ast._
import com.maj.codegen.emitters.Emitter
import com.maj.codegen.{CodeGenerator, RiscVTemplates}

class OperatorCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  private def binaryOpPrologue(node: AstOperator): Unit = {
    codeGenerator.visit(node.left)
    emitter.emit(RiscVTemplates.push1("a0"))
    codeGenerator.visit(node.right)
    emitter.emitLine("mv\t\ta1, a0")
    emitter.emit(RiscVTemplates.pop1("a0"))
  }

  def visit(node: Add): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("add\t\ta0, a0, a1")
  }

  def visit(node: Sub): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("sub\t\ta0, a0, a1")
  }

  def visit(node: Mul): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("mul\t\ta0, a0, a1")
  }

  def visit(node: Div): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("div\t\ta0, a0, a1")
  }

  def visit(node: Mod): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("rem\t\ta0, a0, a1")
  }

  def visit(node: And): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("snqz\t\ta0, a0")
    emitter.emitLine("snqz\t\ta1, a1")
    emitter.emitLine("and\t\ta0, a0, a1")
  }

  def visit(node: Or): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("snqz\t\ta0, a0")
    emitter.emitLine("snqz\t\ta1, a1")
    emitter.emitLine("or\t\ta0, a0, a1")
  }

  def visit(node: Not): Unit = {
    codeGenerator.visit(node.node)
    emitter.emitLine("seqz\t\ta0, a0")
  }

  def visit(node: Equals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("xor\t\ta0, a0, a1")
    emitter.emitLine("seqz\t\ta0, a0")
  }

  def visit(node: NotEquals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("xor\t\ta0, a0, a1")
  }

  def visit(node: LessThan): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("slt\t\ta0, a0, a1")
  }

  def visit(node: GreaterThan): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("sgt\t\ta0, a0, a1")
  }

  def visit(node: LessThanOrEquals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("addi\t\ta1, a1, 1")
    emitter.emitLine("slt\t\ta0, a0, a1")
  }

  def visit(node: GreaterThanOrEquals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("addi\t\ta0, a0, 1")
    emitter.emitLine("sgt\t\ta0, a0, a1")
  }


}
