package com.maj.codegen.handlers

import com.maj.ast._
import com.maj.codegen._
import com.maj.emitters.Emitter

class FunctionCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter[String]) {
  def handle(node: Call): Unit = {
    node.args.length match {
      case n if n <= 1 =>
        node.args.foreach(codeGenerator.visit)
      case n if n <= 7 =>
        manageStackForArgs(node.args)
      case _ =>
        throw new RuntimeException("Too many arguments")
    }
    emitter.emitLine(s"jal\t\t${node.callee}")
  }

  def handle(node: AsmBlock): Unit = {
    node.statements.foreach(emitter.emit)
  }

  def handle(node: Block): Unit = node.statements.foreach(codeGenerator.visit)

  def handle(node: Return): Unit = {
    codeGenerator.visit(node.term)
    emitter.emit(RiscVTemplates.resetAndReturn)
  }

  def handle(node: Function): Unit = {
    if (node.params.length > 7) {
      throw new RuntimeException("Too many arguments")
    }
    val localCodeGen = setupLocalCodeGen(node)
    emitter.emitLine(s"")
    emitter.emit(s".global ${node.name}")
    emitter.emit(s"${node.name}:")
    emitter.emit(RiscVTemplates.functionPrologue)
    emitter.emitLine(s"addi\t\tsp, sp, -${node.params.length * 8}")

    node.params.indices.foreach((index) => {
      emitter.emitLine(s"sd\t\ta$index, ${index * 8}(sp)")
    })
    localCodeGen.visit(node.body)
    emitter.emit(RiscVTemplates.resetAndReturn)
  }

  private def setupLocalCodeGen(node: Function): CodeGenerator = {
    val codeGen = new CodeGenerator(codeGenerator)
    node.params.foreach(codeGen.addLocalWithOffset(_, 8))
    codeGen
  }

  private def manageStackForArgs(args: List[ASTNode]): Unit = {
    emitter.emitLine(s"addi\t\tsp, sp, -${args.length * 8}")
    args.zipWithIndex.foreach { case (arg, index) =>
      codeGenerator.visit(arg)
      emitter.emitLine(s"sd\t\ta0, ${index * 8}(sp)")
    }
    args.indices.foreach { index =>
      emitter.emitLine(s"ld\t\ta$index, ${index * 8}(sp)")
    }
    emitter.emitLine(s"addi\t\tsp, sp, ${args.length * 8}")
  }


}
