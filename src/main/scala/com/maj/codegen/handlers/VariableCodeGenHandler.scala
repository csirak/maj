package com.maj.codegen.handlers

import com.maj.ast._
import com.maj.codegen.{CodeGenerator, RiscVTemplates}
import com.maj.emitters.Emitter

class VariableCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  def visit(node: Assign): Unit = {
    val offset = codeGenerator.get(node.name)
    if (offset.isEmpty) {
      throw new RuntimeException(s"Variable ${node.name} doesn't exist")
    }
    codeGenerator.getConstant(node.name).foreach(_ => throw new RuntimeException(s"Variable ${node.name} is a constant"))
    codeGenerator.visit(node.value)
    emitter.emitLine(s"sd\t\ta0, -${offset.get}(fp)")
  }

  def visit(node: MutableVar): Unit = {
    throwIfVarExists(node.name)
    codeGenerator.visit(node.value)
    RiscVTemplates.push1("a0")
    codeGenerator.addLocalWithOffset(node.name, 8)
  }

  def visit(node: ConstVar): Unit = {
    codeGenerator.visit(node.value)
    RiscVTemplates.push1("a0")
    codeGenerator.addConstant(node.name, node)
  }

  def visit(node: Iden): Unit = {
    val value = codeGenerator.getValue(node.value)
    if (value.isEmpty) {
      throw new RuntimeException(s"Value ${node.value} not found")
    }

    value.get match {
      case Left(offset) => emitter.emitLine(s"ld\t\ta0, -$offset(fp)")
      case Right(constant) => codeGenerator.visit(constant)
    }
  }

  private def throwIfVarExists(name: String): Unit = {
    if (codeGenerator.get(name).orElse(codeGenerator.getConstant(name)).isDefined) {
      throw new RuntimeException(s"Variable ${name} already exists")
    }
  }

  def visit(node: TypeDef): Unit = {} // Do nothing
}
