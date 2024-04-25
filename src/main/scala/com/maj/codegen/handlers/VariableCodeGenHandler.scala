package com.maj.codegen.handlers

import com.maj.ast.{Assign, Create, Iden, TypeDef}
import com.maj.codegen.emitters.Emitter
import com.maj.codegen.{CodeGenerator, RiscVTemplates}

class VariableCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  def visit(node: Assign): Unit = {
    val offset = codeGenerator.get(node.name)
    if (offset == -1) {
      throw new RuntimeException(s"Variable ${node.name} not found")
    }
    codeGenerator.visit(node.value)
    emitter.emitLine(s"sd\t\ta0, -$offset(fp)")
  }

  def visit(node: Create): Unit = {
    if (codeGenerator.get(node.name) != -1) {
      throw new RuntimeException(s"Variable ${node.name} already exists")
    }
    codeGenerator.visit(node.value)
    RiscVTemplates.push1("a0")
    codeGenerator.addLocalWithOffset(node.name, 8)
  }

  def visit(node: Iden): Unit = {
    val offset = codeGenerator.get(node.value)
    if (offset == -1) {
      throw new RuntimeException(s"Variable ${node.value} not found")
    } else {
      emitter.emitLine(s"ld\t\ta0, -$offset(fp)")
    }
  }

  def visit(node: TypeDef): Unit = {} // Do nothing
}
