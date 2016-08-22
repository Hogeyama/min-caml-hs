.data
.balign	8
l.48:	# 0.000000
	.long	0x0
	.long	0x0
l.46:	# 1000000.000000
	.long	0x0
	.long	0x412e8480
l.44:	# 4.560000
	.long	0xa3d70a3d
	.long	0x40123d70
l.42:	# 1.230000
	.long	0x7ae147ae
	.long	0x3ff3ae14
.text
inprod.18:
	cmpl	$0, %ecx
	jl	jge_else.53
	movsd	(%eax,%ecx,8), %xmm1
	movsd	(%ebx,%ecx,8), %xmm2
	mulsd	%xmm2, %xmm1
	addsd	%xmm1, %xmm0
	subl	$1, %ecx
	jmp	inprod.18
jge_else.53:
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
	movl	$3, %eax
	movl	$l.42, %ebx
	movsd	0(%ebx), %xmm0
	call	min_caml_create_float_array
	movl	$3, %ebx
	movl	$l.44, %ecx
	movsd	0(%ecx), %xmm0
	movl	%eax, 0(%ebp)
	movl	%ebx, %eax
	addl	$8, %ebp
	call	min_caml_create_float_array
	subl	$8, %ebp
	movl	%eax, %ebx
	movl	$l.46, %eax
	movsd	0(%eax), %xmm0
	movl	$l.48, %eax
	movsd	0(%eax), %xmm1
	movl	$2, %ecx
	movl	0(%ebp), %eax
	movsd	%xmm0, 8(%ebp)
	movsd	%xmm1, %xmm0
	addl	$16, %ebp
	call	inprod.18
	subl	$16, %ebp
	movsd	8(%ebp), %xmm1
	mulsd	%xmm1, %xmm0
	addl	$16, %ebp
	call	min_caml_truncate
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_int
	subl	$16, %ebp
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
