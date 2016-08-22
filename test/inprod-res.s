.data
.balign	8
l.79:	# 6.000000
	.long	0x0
	.long	0x40180000
l.77:	# 5.000000
	.long	0x0
	.long	0x40140000
l.75:	# 4.000000
	.long	0x0
	.long	0x40100000
l.69:	# 3.000000
	.long	0x0
	.long	0x40080000
l.67:	# 2.000000
	.long	0x0
	.long	0x40000000
l.65:	# 1.000000
	.long	0x0
	.long	0x3ff00000
l.63:	# 1000000.000000
	.long	0x0
	.long	0x412e8480
.text
getx.23:
	movsd	0(%eax), %xmm0
	ret
gety.28:
	movsd	8(%eax), %xmm0
	ret
getz.33:
	movsd	16(%eax), %xmm0
	ret
inprod.38:
	movl	%eax, 0(%ebp)
	movl	%ebx, 4(%ebp)
	addl	$8, %ebp
	call	getx.23
	subl	$8, %ebp
	movl	4(%ebp), %eax
	movsd	%xmm0, 8(%ebp)
	addl	$16, %ebp
	call	getx.23
	subl	$16, %ebp
	movsd	8(%ebp), %xmm1
	mulsd	%xmm0, %xmm1
	movl	0(%ebp), %eax
	movsd	%xmm1, 16(%ebp)
	addl	$24, %ebp
	call	gety.28
	subl	$24, %ebp
	movl	4(%ebp), %eax
	movsd	%xmm0, 24(%ebp)
	addl	$32, %ebp
	call	gety.28
	subl	$32, %ebp
	movsd	24(%ebp), %xmm1
	mulsd	%xmm0, %xmm1
	movsd	16(%ebp), %xmm0
	addsd	%xmm1, %xmm0
	movl	0(%ebp), %eax
	movsd	%xmm0, 32(%ebp)
	addl	$40, %ebp
	call	getz.33
	subl	$40, %ebp
	movl	4(%ebp), %eax
	movsd	%xmm0, 40(%ebp)
	addl	$48, %ebp
	call	getz.33
	subl	$48, %ebp
	movsd	40(%ebp), %xmm1
	mulsd	%xmm0, %xmm1
	movsd	32(%ebp), %xmm0
	addsd	%xmm1, %xmm0
	ret
.globl	min_caml_start
min_caml_start:
.globl	_min_caml_start
_min_caml_start: # for cygwin
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%ebp
	movl	36(%esp),%eax
	movl	%eax,min_caml_hp
	movl	$l.63, %eax
	movsd	0(%eax), %xmm0
	movl	$l.65, %eax
	movsd	0(%eax), %xmm1
	movl	$l.67, %eax
	movsd	0(%eax), %xmm2
	movl	$l.69, %eax
	movsd	0(%eax), %xmm3
	movl	min_caml_hp, %eax
	addl	$24, min_caml_hp
	movsd	%xmm3, 16(%eax)
	movsd	%xmm2, 8(%eax)
	movsd	%xmm1, 0(%eax)
	movl	$l.75, %ebx
	movsd	0(%ebx), %xmm1
	movl	$l.77, %ebx
	movsd	0(%ebx), %xmm2
	movl	$l.79, %ebx
	movsd	0(%ebx), %xmm3
	movl	min_caml_hp, %ebx
	addl	$24, min_caml_hp
	movsd	%xmm3, 16(%ebx)
	movsd	%xmm2, 8(%ebx)
	movsd	%xmm1, 0(%ebx)
	movsd	%xmm0, 0(%ebp)
	addl	$8, %ebp
	call	inprod.38
	subl	$8, %ebp
	movsd	0(%ebp), %xmm1
	mulsd	%xmm1, %xmm0
	addl	$8, %ebp
	call	min_caml_truncate
	subl	$8, %ebp
	addl	$8, %ebp
	call	min_caml_print_int
	subl	$8, %ebp
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
