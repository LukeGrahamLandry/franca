.text
.balign 4
_thing2___710:
	hint	#34
	stp	x29, x30, [sp, -16]!
	mov	x29, sp
	ldp	x29, x30, [sp], 16
	ret
/* end function thing2___710 */

.text
.balign 4
.globl _main
_main:
	hint	#34
	stp	x29, x30, [sp, -16]!
	mov	x29, sp
	bl	_thing2___710
	mov	w0, #0
	ldp	x29, x30, [sp], 16
	ret
/* end function main */

.data
.balign 8
_debug_names_len:
	.quad 0
/* end data */

.data
.balign 8
_debug_names:
	.quad 0
/* end data */

