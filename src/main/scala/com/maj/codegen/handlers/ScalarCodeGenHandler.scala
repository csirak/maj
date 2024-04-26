package com.maj.codegen.handlers

import com.maj.ast.{MajBool, MajChar, MajInt, MajNull}
import com.maj.codegen.CodeGenerator
import com.maj.emitters.Emitter

class ScalarCodeGenHandler(codeGenerator: CodeGenerator)(implicit emitter: Emitter[String]) {
  def handle(node: MajInt): Unit = {
    emitter.emitLine(s"li\t\ta0, ${node.value}")
  }

  def handle(node: MajBool): Unit = {
    emitter.emitLine(s"li\t\ta0, ${if (node.value) 1 else 0}")
  }

  def handle(node: MajNull): Unit = {
    emitter.emitLine("li\t\ta0, 0")
  }

  def handle(node: MajChar): Unit = {
    emitter.emitLine(s"li\t\ta0, '${node.value}'")
  }
}
