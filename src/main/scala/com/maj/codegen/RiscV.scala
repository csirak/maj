package com.maj.codegen

object RiscV {
  def push(regs: List[String]): List[String] = {
    val offset = Math.ceil(regs.length.toDouble / 4).toInt * 4
    "" +: regs.map(reg => s"sw $reg, 0(sp)") :+ s"addi sp, sp, -$offset" :+ ""
  }

  def pop(reg: List[String]): List[String] = {
    // add overflow checks on stack height
    val offset = Math.ceil(reg.length.toDouble / 4).toInt * 4
    "" +: reg.zipWithIndex.map { case (r, i) => s"lw $r, ${(i + 1) * 4}(sp)" } :+ s"addi sp, sp, $offset" :+ ""
  }

  def cmp(left: String, right: String): List[String] = {
    List(
      s"xor      t0, $left, $right",
      s"li $left, 1",
      s"li $left, 0"
    )
  }
}
