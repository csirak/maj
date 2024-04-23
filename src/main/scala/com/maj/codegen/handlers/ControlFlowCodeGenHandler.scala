package com.maj.codegen.handlers

import com.maj.ast.{Conditional, Loop}
import com.maj.codegen.{CodeGenerator, Emitter, Label}

class ControlFlowCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  def visit(node: Conditional): Unit = {
    val ifFalseLabel = Label.next
    val endLabel = Label.next
    codeGenerator.visit(node.condition)
    emitter.emitLine(s"beqz a0, $ifFalseLabel")
    codeGenerator.visit(node.ifTrue)
    emitter.emitLine(s"j $endLabel")
    emitter.emit(s"$ifFalseLabel:")
    node.elseIfTrue.foreach(codeGenerator.visit)
    emitter.emit(s"$endLabel:")
  }

  def visit(node: Loop): Unit = {
    val conditionLabel = Label.next
    val endLabel = Label.next
    emitter.emit(s"$conditionLabel:")
    codeGenerator.visit(node.condition)
    emitter.emitLine(s"beqz a0, $endLabel")
    codeGenerator.visit(node.body)
    emitter.emitLine(s"j $conditionLabel")
    emitter.emit(s"$endLabel:")
  }
}
