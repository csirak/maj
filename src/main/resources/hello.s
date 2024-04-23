
.align 2
.include "cfg.inc"
.equ UART_REG_TXFIFO,   0

.section .text
.global _start
_start:
        csrr  t0, mhartid             # read hardware thread id (`hart` stands for `hardware thread`)
        bnez  t0, halt                # run only on the first hardware thread (hartid == 0), halt all the other threads

        la    sp, stack_top           # setup stack pointer

		li a0, 6
		jal fact
		j halt

assert:
        beqz     a0, .Lassert_failed
        li       a0, '.'
        j        .end
.Lassert_failed:
        li       a0, 'F'
.end:


putchar:
        li       t0, UART_BASE           # load UART base address

.Lputchar_loop:
        lw       t1, UART_REG_TXFIFO(t0) # read UART TX FIFO status
        li       t2, 0x80000000
        and      t1, t1, t2
        bnez     t1, .Lputchar_loop      # if TX FIFO is full, wait

        sw       a0, UART_REG_TXFIFO(t0) # write character to TX FIFO
        ret
halt:
        li a0, 0x100000
        li a1, 0x5555
        sw a1, 0(a0)
        j halt

		
.global fact
fact:

        addi sp, sp, -16
        sd ra, 8(sp)
        sd fp, 0(sp)
        mv fp, sp

		addi sp, sp, -8
		sd a0, 0(sp)
.L1:
		ld a0, -8(fp)
		addi sp, sp, -8
		sd a0, 0(sp)
		li a0, 1
		mv a1, a0
		ld a0, 0(sp)
		addi sp, sp, 8
		addi a0, a0, 1
		sgt a0, a0, a1
		beqz a0, .L2
		ld a0, -8(fp)
		addi sp, sp, -8
		sd a0, 0(sp)
		li a0, 48
		mv a1, a0
		ld a0, 0(sp)
		addi sp, sp, 8
		add a0, a0, a1
		jal putchar
		ld a0, -8(fp)
		addi sp, sp, -8
		sd a0, 0(sp)
		li a0, 1
		mv a1, a0
		ld a0, 0(sp)
		addi sp, sp, 8
		sub a0, a0, a1
		sd a0, -8(fp)
		j .L1
.L2:

        mv sp, fp
        ld ra, 8(sp)
        ld fp, 0(sp)
        addi sp, sp, 16
        ret
