.data
.balign	8
l.39:	# 48.300300
	.long	0x3eab623f
	.long	0x40482670
l.37:	# 4.500000
	.long	0x0
	.long	0x40120000
l.35:	# -12.300000
	.long	0x9999999a
	.long	0xc0289999
.text
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
	movl	$l.35, %eax
	movsd	0(%eax), %xmm0
	call	min_caml_abs_float
	call	min_caml_sqrt
	call	min_caml_cos
	call	min_caml_sin
	movl	$l.37, %eax
	movsd	0(%eax), %xmm1
	addsd	%xmm1, %xmm0
	movl	$l.39, %eax
	movsd	0(%eax), %xmm1
	subsd	%xmm1, %xmm0
	movl	$1000000, %eax
	movsd	%xmm0, 0(%ebp)
	addl	$8, %ebp
	call	min_caml_float_of_int
	subl	$8, %ebp
	movsd	0(%ebp), %xmm1
	mulsd	%xmm1, %xmm0
	addl	$8, %ebp
	call	min_caml_int_of_float
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
