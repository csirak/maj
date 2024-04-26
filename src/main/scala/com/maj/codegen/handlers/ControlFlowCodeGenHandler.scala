package com.maj.codegen.handlers

import com.maj.ast.{Conditional, Loop}
import com.maj.codegen.{CodeGenerator, Label}
import com.maj.emitters.Emitter

class ControlFlowCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter[String]) {
  def handle(node: Conditional): Unit = {
    val ifFalseLabel = Label.next
    val endLabel = Label.next
    codeGenerator.visit(node.condition)
    emitter.emitLine(s"beqz\t\ta0, $ifFalseLabel")
    codeGenerator.visit(node.ifTrue)
    emitter.emitLine(s"j\t\t$endLabel")
    emitter.emit(s"$ifFalseLabel:")
    node.elseIfTrue.foreach(codeGenerator.visit)
    emitter.emit(s"$endLabel:")
  }

  def handle(node: Loop): Unit = {
    val conditionLabel = Label.next
    val endLabel = Label.next
    emitter.emit(s"$conditionLabel:")
    codeGenerator.visit(node.condition)
    emitter.emitLine(s"beqz\t\ta0, $endLabel")
    codeGenerator.visit(node.body)
    emitter.emitLine(s"j\t\t$conditionLabel")
    emitter.emit(s"$endLabel:")
  }
}
