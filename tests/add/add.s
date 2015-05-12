	.text

	.even
	.globl _main
_main:
	setd
	seti
	mov r5, -(sp)
	mov sp, r5
	add $-04, sp
	jsr pc, ___main
	mov $012, -02(r5)
	mov $017, -04(r5)
	mov -02(r5), r0
	add -04(r5), r0
	mov r5, sp
	mov (sp)+, r5
	rts pc
