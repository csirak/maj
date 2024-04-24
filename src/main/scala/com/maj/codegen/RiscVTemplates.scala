package com.maj.codegen


object RiscVTemplates {
  val start: String =
    """.align 2
      |.include "cfg.inc"
      |.equ UART_REG_TXFIFO,   0
      |
      |.section .text
      |.global _start
      |_start:
      |        csrr  t0, mhartid                # read hardware thread id (`hart` stands for `hardware thread`)
      |        bnez  t0, halt                   # run only on the first hardware thread (hartid == 0), halt all the other threads
      |
      |        la    sp, stack_top              # setup stack pointer
      |        jal     main                     # jump to main function
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
      |
      |halt:
      |        li a0, 0x100000
      |        li a1, 0x5555
      |        sw a1, 0(a0)
      |        j halt
      |""".stripMargin


  val resetAndReturn: String =
    """
      |        mv sp, fp
      |        ld ra, 8(sp)
      |        ld fp, 0(sp)
      |        addi sp, sp, 16
      |        ret
      |""".stripMargin


  val functionPrologue: String =
    """
      |        addi sp, sp, -16
      |        sd ra, 8(sp)
      |        sd fp, 0(sp)
      |        mv fp, sp
      |""".stripMargin


  def push1(reg: String): String = s"\t\taddi sp, sp, -8\n\t\tsd $reg, 0(sp)"

  def pop1(reg: String): String = s"\t\tld $reg, 0(sp)\n\t\taddi sp, sp, 8"
}