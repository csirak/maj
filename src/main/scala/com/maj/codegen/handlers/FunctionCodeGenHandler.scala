package com.maj.codegen.handlers

import com.maj.ast._
import com.maj.codegen._
import com.maj.emitters.Emitter

class FunctionCodeGenHandler(val codeGenerator: CodeGenerator)(implicit emitter: Emitter) {
  def visit(node: Call): Unit = {
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

  def visit(node: AsmBlock): Unit = {
    node.statements.foreach { line =>
      val cleaned = cleanAsm(line)
      if (cleaned.nonEmpty) {
        if (cleaned.contains(":")) emitter.emit(cleaned) else emitter.emitLine(cleaned)
      }
    }
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

  private def cleanAsm(line: String): String = {
    val regex = "\\s*(\\S+)\\s*".r
    val matches = regex.findAllIn(line).matchData.map(_.group(1)).toList
    if (matches.isEmpty) return ""
    matches.head + "\t\t" + matches.tail.mkString(" ")
  }
}
