package com.maj.codegen.handlers

import com.maj.ast._
import com.maj.codegen.{AsmTemplates, CodeGenerator, Emitter}

class OperatorsCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  private def binaryOpPrologue(node: Operator): Unit = {
    codeGenerator.visit(node.left)
    emitter.emit(AsmTemplates.push1("a0"))
    codeGenerator.visit(node.right)
    emitter.emitLine("mv a1, a0")
    emitter.emit(AsmTemplates.pop1("a0"))
  }

  def visit(node: Add): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("add a0, a0, a1")
  }

  def visit(node: Sub): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("sub a0, a0, a1")
  }

  def visit(node: Mul): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("mul a0, a0, a1")
  }

  def visit(node: Div): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("div a0, a0, a1")
  }

  def visit(node: Mod): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("rem a0, a0, a1")
  }

  def visit(node: And): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("snqz a0, a0")
    emitter.emitLine("snqz a1, a1")
    emitter.emitLine("and a0, a0, a1")
  }

  def visit(node: Or): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("snqz a0, a0")
    emitter.emitLine("snqz a1, a1")
    emitter.emitLine("or a0, a0, a1")
  }

  def visit(node: Not): Unit = {
    codeGenerator.visit(node.node)
    emitter.emitLine("seqz a0, a0")
  }

  def visit(node: Equals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("xor a0, a0, a1")
    emitter.emitLine("seqz a0, a0")
  }

  def visit(node: NotEquals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("xor a0, a0, a1")
  }

  def visit(node: LessThan): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("slt a0, a0, a1")
  }

  def visit(node: GreaterThan): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("sgt a0, a0, a1")
  }

  def visit(node: LessThanOrEquals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("addi a1, a1, 1")
    emitter.emitLine("slt a0, a0, a1")
  }

  def visit(node: GreaterThanOrEquals): Unit = {
    binaryOpPrologue(node)
    emitter.emitLine("addi a0, a0, 1")
    emitter.emitLine("sgt a0, a0, a1")
  }


}
