package com.maj.codegen.handlers

import com.maj.ast._
import com.maj.codegen._
import com.maj.codegen.emitters.Emitter

class FunctionsCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  def visit(node: Main): Unit = {
    emitter.emit(RiscVTemplates.start)
    codeGenerator.visit(node.body)
    emitter.emitLine("j halt")
    emitter.emit(RiscVTemplates.assert)
    emitter.emit(RiscVTemplates.putchar)
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
    emitter.emit(RiscVTemplates.resetAndReturn)
  }

  def visit(node: Function): Unit = {
    if (node.params.length > 7) {
      throw new RuntimeException("Too many arguments")
    }
    val localCodeGen = setupLocalCodeGen(node)
    emitter.emitLine(s"")
    emitter.emit(s".global ${node.name}")
    emitter.emit(s"${node.name}:")
    emitter.emit(RiscVTemplates.functionPrologue)
    emitter.emitLine(s"addi sp, sp, -${node.params.length * 8}")

    node.params.indices.foreach((index) => {
      emitter.emitLine(s"sd a$index, ${index * 8}(sp)")
    })
    localCodeGen.visit(node.body)
    emitter.emit(RiscVTemplates.resetAndReturn)
  }


  private def setupLocalCodeGen(node: Function): CodeGenerator = {
    val codeGen = new CodeGenerator()
    node.params.foreach(param => {
      codeGen.addLocalWithOffset(param, 8)
    })
    codeGen
  }
}
