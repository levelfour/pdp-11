	.text

LC_0:
	.byte 0110,0145,0154,0154,0157,054,040,0167,0157,0162,0154,0144,041,0
	.even
	.globl _main
_main:
	setd
	seti
	mov r5, -(sp)
	mov sp, r5
	jsr pc, ___main
	mov $LC_0, -(sp)
	jsr pc, _puts
	add $02, sp
	clr r0
	mov (sp)+, r5
	rts pc
