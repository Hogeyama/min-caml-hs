.data
.balign	8
.text
foo.12:
	movl	%edi, 0(%ebp)
	movl	%esi, 4(%ebp)
	movl	%edx, 8(%ebp)
	movl	%ecx, 12(%ebp)
	movl	%ebx, 16(%ebp)
	addl	$24, %ebp
	call	min_caml_print_int
	subl	$24, %ebp
	movl	16(%ebp), %eax
	addl	$24, %ebp
	call	min_caml_print_int
	subl	$24, %ebp
	movl	12(%ebp), %eax
	addl	$24, %ebp
	call	min_caml_print_int
	subl	$24, %ebp
	movl	8(%ebp), %eax
	addl	$24, %ebp
	call	min_caml_print_int
	subl	$24, %ebp
	movl	4(%ebp), %eax
	addl	$24, %ebp
	call	min_caml_print_int
	subl	$24, %ebp
	movl	0(%ebp), %eax
	jmp	min_caml_print_int
bar.24:
	movl	%edi, 0(%ebp)
	movl	%ecx, %edi
	movl	%edx, %ecx
	movl	%esi, %edx
	movl	0(%ebp), %esi
	movl	%ebx, 0(%ebp)
	movl	%eax, %ebx
	movl	0(%ebp), %eax
	jmp	foo.12
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
	movl	$1, %eax
	movl	$2, %ebx
	movl	$3, %ecx
	movl	$4, %edx
	movl	$5, %esi
	movl	$6, %edi
	call	bar.24
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
