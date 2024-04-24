		
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

		
.global main
main:

        addi sp, sp, -16
        sd ra, 8(sp)
        sd fp, 0(sp)
        mv fp, sp

		addi sp, sp, -0
		li a0, 16
		addi sp, sp, -8
		sd a0, 0(sp)
		li a0, 10
		mv a1, a0
		ld a0, 0(sp)
		addi sp, sp, 8
		sub a0, a0, a1
		addi sp, sp, -8
		sd a0, 0(sp)
		ld a0, -8(fp)
		jal fact

        mv sp, fp
        ld ra, 8(sp)
        ld fp, 0(sp)
        addi sp, sp, 16
        ret
