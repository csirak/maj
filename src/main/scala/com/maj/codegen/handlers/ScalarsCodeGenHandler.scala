package com.maj.codegen.handlers

import com.maj.ast.{Bool, Null, Numeric}
import com.maj.codegen.CodeGenerator
import com.maj.codegen.emitters.Emitter

class ScalarsCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  def visit(node: Numeric): Unit = {
    emitter.emitLine(s"li a0, ${node.value}")
  }

  def visit(node: Bool): Unit = {
    emitter.emitLine(s"li a0, ${if (node.value) 1 else 0}")
  }

  def visit(node: Null): Unit = {
    emitter.emitLine("li a0, 0")
  }
}
