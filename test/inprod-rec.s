.data
.balign	8
l.45:	# 1000000.000000
	.long	0x0
	.long	0x412e8480
l.43:	# 4.560000
	.long	0xa3d70a3d
	.long	0x40123d70
l.41:	# 1.230000
	.long	0x7ae147ae
	.long	0x3ff3ae14
l.39:	# 0.000000
	.long	0x0
	.long	0x0
.text
inprod.17:
	cmpl	$0, %ecx
	jl	jge_else.51
	movsd	(%eax,%ecx,8), %xmm0
	movsd	(%ebx,%ecx,8), %xmm1
	mulsd	%xmm1, %xmm0
	subl	$1, %ecx
	movsd	%xmm0, 0(%ebp)
	addl	$8, %ebp
	call	inprod.17
	subl	$8, %ebp
	movsd	0(%ebp), %xmm1
	addsd	%xmm1, %xmm0
	ret
jge_else.51:
	movl	$l.39, %eax
	movsd	0(%eax), %xmm0
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
	movl	$l.41, %ebx
	movsd	0(%ebx), %xmm0
	call	min_caml_create_float_array
	movl	$3, %ebx
	movl	$l.43, %ecx
	movsd	0(%ecx), %xmm0
	movl	%eax, 0(%ebp)
	movl	%ebx, %eax
	addl	$8, %ebp
	call	min_caml_create_float_array
	subl	$8, %ebp
	movl	%eax, %ebx
	movl	$l.45, %eax
	movsd	0(%eax), %xmm0
	movl	$2, %ecx
	movl	0(%ebp), %eax
	movsd	%xmm0, 8(%ebp)
	addl	$16, %ebp
	call	inprod.17
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
