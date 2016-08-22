.data
.balign	8
l.352:	# 12.000000
	.long	0x0
	.long	0x40280000
l.350:	# 11.000000
	.long	0x0
	.long	0x40260000
l.348:	# 10.000000
	.long	0x0
	.long	0x40240000
l.346:	# 9.000000
	.long	0x0
	.long	0x40220000
l.344:	# 8.000000
	.long	0x0
	.long	0x40200000
l.342:	# 7.000000
	.long	0x0
	.long	0x401c0000
l.340:	# 6.000000
	.long	0x0
	.long	0x40180000
l.338:	# 5.000000
	.long	0x0
	.long	0x40140000
l.336:	# 4.000000
	.long	0x0
	.long	0x40100000
l.334:	# 3.000000
	.long	0x0
	.long	0x40080000
l.332:	# 2.000000
	.long	0x0
	.long	0x40000000
l.330:	# 1.000000
	.long	0x0
	.long	0x3ff00000
l.323:	# 0.000000
	.long	0x0
	.long	0x0
.text
loop3.153:
	movl	20(%edi), %ebx
	movl	16(%edi), %ecx
	movl	12(%edi), %edx
	movl	8(%edi), %esi
	movl	%edi, 0(%ebp)
	movl	4(%edi), %edi
	cmpl	$0, %eax
	jl	jge_else.389
	movl	%esi, 4(%ebp)
	movl	(%edx,%ecx,4), %esi
	movl	(%edx,%ecx,4), %edx
	movsd	(%edx,%ebx,8), %xmm0
	movl	(%edi,%ecx,4), %ecx
	movsd	(%ecx,%eax,8), %xmm1
	movl	4(%ebp), %ecx
	movl	(%ecx,%eax,4), %ecx
	movsd	(%ecx,%ebx,8), %xmm2
	mulsd	%xmm2, %xmm1
	addsd	%xmm1, %xmm0
	movsd	%xmm0, (%esi,%ebx,8)
	subl	$1, %eax
	movl	0(%ebp), %edi
	jmp	*(%edi)
jge_else.389:
	ret
loop2.150:
	movl	20(%edi), %ebx
	movl	16(%edi), %ecx
	movl	12(%edi), %edx
	movl	8(%edi), %esi
	movl	%edi, 0(%ebp)
	movl	4(%edi), %edi
	cmpl	$0, %eax
	jl	jge_else.391
	movl	%ebx, 4(%ebp)
	movl	min_caml_hp, %ebx
	addl	$24, min_caml_hp
	movl	%edi, 8(%ebp)
	movl	$loop3.153, %edi
	movl	%edi, 0(%ebx)
	movl	%eax, 20(%ebx)
	movl	%ecx, 16(%ebx)
	movl	%edx, 12(%ebx)
	movl	%esi, 8(%ebx)
	movl	8(%ebp), %ecx
	movl	%ecx, 4(%ebx)
	movl	4(%ebp), %ecx
	subl	$1, %ecx
	movl	%eax, 12(%ebp)
	movl	%ecx, %eax
	movl	%ebx, %edi
	addl	$16, %ebp
	call	*(%edi)
	subl	$16, %ebp
	movl	12(%ebp), %eax
	subl	$1, %eax
	movl	0(%ebp), %edi
	jmp	*(%edi)
jge_else.391:
	ret
loop1.147:
	movl	20(%edi), %ebx
	movl	16(%edi), %ecx
	movl	12(%edi), %edx
	movl	8(%edi), %esi
	movl	%edi, 0(%ebp)
	movl	4(%edi), %edi
	cmpl	$0, %eax
	jl	jge_else.393
	movl	%ebx, 4(%ebp)
	movl	min_caml_hp, %ebx
	addl	$24, min_caml_hp
	movl	%edi, 8(%ebp)
	movl	$loop2.150, %edi
	movl	%edi, 0(%ebx)
	movl	%ecx, 20(%ebx)
	movl	%eax, 16(%ebx)
	movl	%edx, 12(%ebx)
	movl	%esi, 8(%ebx)
	movl	8(%ebp), %ecx
	movl	%ecx, 4(%ebx)
	movl	4(%ebp), %ecx
	subl	$1, %ecx
	movl	%eax, 12(%ebp)
	movl	%ecx, %eax
	movl	%ebx, %edi
	addl	$16, %ebp
	call	*(%edi)
	subl	$16, %ebp
	movl	12(%ebp), %eax
	subl	$1, %eax
	movl	0(%ebp), %edi
	jmp	*(%edi)
jge_else.393:
	ret
mul.140:
	movl	%eax, 0(%ebp)
	movl	min_caml_hp, %eax
	addl	$24, min_caml_hp
	movl	%edx, 4(%ebp)
	movl	$loop1.147, %edx
	movl	%edx, 0(%eax)
	movl	%ecx, 20(%eax)
	movl	%ebx, 16(%eax)
	movl	%edi, 12(%eax)
	movl	%esi, 8(%eax)
	movl	4(%ebp), %ebx
	movl	%ebx, 4(%eax)
	movl	0(%ebp), %ebx
	subl	$1, %ebx
	movl	%eax, %edi
	movl	%ebx, %eax
	jmp	*(%edi)
init.187:
	movl	8(%edi), %ebx
	movl	4(%edi), %ecx
	cmpl	$0, %eax
	jl	jge_else.395
	movl	$l.323, %edx
	movsd	0(%edx), %xmm0
	movl	%edi, 0(%ebp)
	movl	%eax, 4(%ebp)
	movl	%ecx, 8(%ebp)
	movl	%ebx, %eax
	addl	$16, %ebp
	call	min_caml_create_float_array
	subl	$16, %ebp
	movl	8(%ebp), %ebx
	movl	4(%ebp), %ecx
	movl	%eax, (%ebx,%ecx,4)
	movl	%ecx, %eax
	subl	$1, %eax
	movl	0(%ebp), %edi
	jmp	*(%edi)
jge_else.395:
	ret
make.183:
	movl	4(%edi), %ecx
	movl	%eax, 0(%ebp)
	movl	%ebx, 4(%ebp)
	movl	%ecx, %ebx
	addl	$8, %ebp
	call	min_caml_create_array
	subl	$8, %ebp
	movl	min_caml_hp, %edi
	addl	$16, min_caml_hp
	movl	$init.187, %ebx
	movl	%ebx, 0(%edi)
	movl	4(%ebp), %ebx
	movl	%ebx, 8(%edi)
	movl	%eax, 4(%edi)
	movl	0(%ebp), %ebx
	subl	$1, %ebx
	movl	%eax, 8(%ebp)
	movl	%ebx, %eax
	addl	$16, %ebp
	call	*(%edi)
	subl	$16, %ebp
	movl	8(%ebp), %eax
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
	movl	$0, %eax
	movl	$l.323, %ebx
	movsd	0(%ebx), %xmm0
	call	min_caml_create_float_array
	movl	min_caml_hp, %edi
	addl	$8, min_caml_hp
	movl	$make.183, %ebx
	movl	%ebx, 0(%edi)
	movl	%eax, 4(%edi)
	movl	$2, %eax
	movl	$3, %ebx
	movl	%edi, 0(%ebp)
	addl	$8, %ebp
	call	*(%edi)
	subl	$8, %ebp
	movl	$3, %ebx
	movl	$2, %ecx
	movl	0(%ebp), %edi
	movl	%eax, 4(%ebp)
	movl	%ebx, %eax
	movl	%ecx, %ebx
	addl	$8, %ebp
	call	*(%edi)
	subl	$8, %ebp
	movl	$2, %ebx
	movl	$2, %ecx
	movl	0(%ebp), %edi
	movl	%eax, 8(%ebp)
	movl	%ebx, %eax
	movl	%ecx, %ebx
	addl	$16, %ebp
	call	*(%edi)
	subl	$16, %ebp
	movl	%eax, %edi
	movl	4(%ebp), %edx
	movl	0(%edx), %eax
	movl	$l.330, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	0(%edx), %eax
	movl	$l.332, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	0(%edx), %eax
	movl	$l.334, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 16(%eax)
	movl	4(%edx), %eax
	movl	$l.336, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	4(%edx), %eax
	movl	$l.338, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	4(%edx), %eax
	movl	$l.340, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 16(%eax)
	movl	8(%ebp), %esi
	movl	0(%esi), %eax
	movl	$l.342, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	0(%esi), %eax
	movl	$l.344, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	4(%esi), %eax
	movl	$l.346, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	4(%esi), %eax
	movl	$l.348, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	8(%esi), %eax
	movl	$l.350, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 0(%eax)
	movl	8(%esi), %eax
	movl	$l.352, %ebx
	movsd	0(%ebx), %xmm0
	movsd	%xmm0, 8(%eax)
	movl	$2, %eax
	movl	$3, %ebx
	movl	$2, %ecx
	movl	%edi, 12(%ebp)
	addl	$16, %ebp
	call	mul.140
	subl	$16, %ebp
	movl	12(%ebp), %eax
	movl	0(%eax), %ebx
	movsd	0(%ebx), %xmm0
	addl	$16, %ebp
	call	min_caml_truncate
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_int
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_newline
	subl	$16, %ebp
	movl	12(%ebp), %eax
	movl	0(%eax), %ebx
	movsd	8(%ebx), %xmm0
	addl	$16, %ebp
	call	min_caml_truncate
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_int
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_newline
	subl	$16, %ebp
	movl	12(%ebp), %eax
	movl	4(%eax), %ebx
	movsd	0(%ebx), %xmm0
	addl	$16, %ebp
	call	min_caml_truncate
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_int
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_newline
	subl	$16, %ebp
	movl	12(%ebp), %eax
	movl	4(%eax), %eax
	movsd	8(%eax), %xmm0
	addl	$16, %ebp
	call	min_caml_truncate
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_int
	subl	$16, %ebp
	addl	$16, %ebp
	call	min_caml_print_newline
	subl	$16, %ebp
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
