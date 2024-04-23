package com.maj.codegen.handlers

import com.maj.ast._
import com.maj.codegen._

class FunctionsCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  def visit(node: Main): Unit = {
    emitter.emit(AsmTemplates.startTemplate)
    codeGenerator.visit(node.body)
    emitter.emitLine("j halt")
    emitter.emit(AsmTemplates.assertTemplate)
    emitter.emit(AsmTemplates.putcharTemplate)
  }

  def visit(node: Assert): Unit = {
    codeGenerator.visit(node.condition)
    emitter.emitLine("jal assert")
  }

  def visit(node: Call): Unit = {
    node.args.length match {
      case n if n <= 1 =>
        node.args.foreach(codeGenerator.visit)
      case n if n <= 7 =>
        manageStackForArgs(node.args)
      case _ =>
        throw new RuntimeException("Too many arguments")
    }
    emitter.emitLine(s"jal ${node.callee}")
  }

  private def manageStackForArgs(args: List[ASTNode]): Unit = {
    emitter.emitLine(s"addi sp, sp, -${args.length * 8}")
    args.zipWithIndex.foreach { case (arg, index) =>
      codeGenerator.visit(arg)
      emitter.emitLine(s"sd a0, ${index * 8}(sp)")
    }
    args.indices.foreach { index =>
      emitter.emitLine(s"ld a$index, ${index * 8}(sp)")
    }
    emitter.emitLine(s"addi sp, sp, ${args.length * 8}")
  }


  def visit(node: Block): Unit = node.statements.foreach(codeGenerator.visit)

  def visit(node: Return): Unit = {
    codeGenerator.visit(node.term)
    emitter.emit(AsmTemplates.returnTemplate)
  }

  def visit(node: Function): Unit = {
    if (node.params.length > 7) {
      throw new RuntimeException("Too many arguments")
    }
    val localCodeGen = setupLocalCodeGen(node)
    val stackOffsetDepth = node.stackOffsetDepth
    emitter.emitLine(s"")
    emitter.emit(s".global ${node.name}")
    emitter.emit(s"${node.name}:")
    emitter.emit(AsmTemplates.functionPrologueTemplate)
    emitter.emitLine(s"addi sp, sp, -${stackOffsetDepth}")

    node.params.indices.foreach((index) => {
      emitter.emitLine(s"sd a$index, ${stackOffsetDepth - node.paramOffset(index)}(sp)")
    })
    localCodeGen.visit(node.body)
    emitter.emit(AsmTemplates.returnTemplate)
  }

  private def setupLocalCodeGen(node: Function): CodeGenerator = {
    val codeGen = new CodeGenerator()
    node.params.zipWithIndex.foreach {
      case (param, index) => {
        codeGen.addLocal(param, node.paramOffset(index))
      }
    }
    codeGen.localOffset = node.stackOffsetDepth
    codeGen
  }
}
