package com.maj.ast

case class Call(val callee: String, val args: List[ASTNode]) extends ASTNode {
  override def equals(node: ASTNode): Boolean = {
    node match {
      case call: Call => {
        (this.callee == call.callee
          && this.args.length == call.args.length
          && this.args == call.args)
      }
      case _ => false
    }
  }

  override def emit(implicit emitter: Emitter): Unit = {
    if (args.length <= 1) {
      args.foreach(_.emit)
      emitter.emitLine(s"call $callee")
    } else if (args.length <= 7) {
      emitter.emitLine(s"addi sp, sp, -${args.length * 4}")
      args.zipWithIndex.foreach {
        case (arg, index) => {
          arg.emit
          emitter.emitLine(s"sw a0, ${index * 4}(sp)")
        }
      }
      for (index <- args.indices) {
        emitter.emitLine(s"lw a$index, ${index * 4}(sp)")
      }
      emitter.emitLine(s"addi sp, sp, ${args.length * 4}")

      emitter.emitLine(s"call $callee")
    } else {
      throw new RuntimeException("Too many arguments")
    }
  }
}

case class Return(val term: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false
}

case class Block(val statements: List[ASTNode]) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def toString: String = s"Block{\n${statements.map(_.toString).mkString("\t")}\n}"

  override def emit(implicit emitter: Emitter): Unit = statements.foreach(_.emit)
}

case class Function(val name: String, val params: List[String], val body: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false
}

case class Main(val body: Block) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(implicit emitter: Emitter): Unit = {
    emitter.emit(
      """
        |.align 2
        |.include "cfg.inc"
        |.equ UART_REG_TXFIFO,   0
        |
        |.section .text
        |.global _start
        |_start:
        |        csrr  t0, mhartid             # read hardware thread id (`hart` stands for `hardware thread`)
        |        bnez  t0, halt                # run only on the first hardware thread (hartid == 0), halt all the other threads
        |
        |        la    sp, stack_top           # setup stack pointer
        |""".stripMargin)
    body.emit

    emitter.emit(
      """
        |        j    halt                 # load newline character
        |
        |
        |assert:
        |        beqz     a0, .Lassert_failed
        |        li       a0, '.'
        |        j        .end
        |.Lassert_failed:
        |        li       a0, 'F'
        |.end:
        |
        |putchar:
        |        li       t0, UART_BASE           # load UART base address
        |
        |.Lputchar_loop:
        |        lw       t1, UART_REG_TXFIFO(t0) # read UART TX FIFO status
        |        li       t2, 0x80000000
        |        and      t1, t1, t2
        |        bnez     t1, .Lputchar_loop      # if TX FIFO is full, wait
        |
        |        sw       a0, UART_REG_TXFIFO(t0) # write character to TX FIFO
        |        ret
        |halt:
        |        li a0, 0x100000
        |        li a1, 0x5555
        |        sw a1, 0(a0)
        |""".stripMargin)

  }
}

case class Assert(val condition: ASTNode) extends ASTNode {
  override def equals(node: ASTNode): Boolean = false

  override def emit(implicit emitter: Emitter): Unit = {
    condition.emit
    emitter.emitLine("call assert")
  }
}