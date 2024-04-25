package com.maj.codegen.handlers

import com.maj.ast.{MajBool, MajChar, MajInt, MajNull}
import com.maj.codegen.CodeGenerator
import com.maj.codegen.emitters.Emitter

class ScalarCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  def visit(node: MajInt): Unit = {
    emitter.emitLine(s"li\t\ta0, ${node.value}")
  }

  def visit(node: MajBool): Unit = {
    emitter.emitLine(s"li\t\ta0, ${if (node.value) 1 else 0}")
  }

  def visit(node: MajNull): Unit = {
    emitter.emitLine("li\t\ta0, 0")
  }

  def visit(node: MajChar): Unit = {
    emitter.emitLine(s"li\t\ta0, '${node.value}'")
  }
}
