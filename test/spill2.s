.data
.balign	8
.text
f.26:
	movl	$12345, %eax
	ret
g.28:
	addl	$1, %eax
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
	movl	$10, %eax
	movl	$1, %ebx
	call	min_caml_create_array
	movl	%eax, 0(%ebp)
	addl	$8, %ebp
	call	f.26
	subl	$8, %ebp
	movl	$67890, %ebx
	movl	0(%ebp), %ecx
	movl	0(%ecx), %edx
	movl	%edx, %esi
	addl	%edx, %esi
	movl	%esi, %edi
	addl	%esi, %edi
	movl	%eax, 4(%ebp)
	movl	%edi, %eax
	addl	%edi, %eax
	movl	%eax, 8(%ebp)
	addl	%eax, %eax
	movl	%eax, 12(%ebp)
	addl	%eax, %eax
	movl	%eax, 16(%ebp)
	addl	%eax, %eax
	movl	%eax, 20(%ebp)
	addl	%eax, %eax
	movl	%eax, 24(%ebp)
	addl	%eax, %eax
	movl	%eax, 28(%ebp)
	addl	%eax, %eax
	movl	%eax, 32(%ebp)
	addl	%eax, %eax
	movl	%eax, 36(%ebp)
	addl	%eax, %eax
	movl	%eax, 40(%ebp)
	addl	%eax, %eax
	movl	%eax, 44(%ebp)
	addl	%eax, %eax
	movl	%eax, 48(%ebp)
	addl	%eax, %eax
	movl	%eax, 52(%ebp)
	addl	%eax, %eax
	movl	4(%ecx), %ecx
	cmpl	$0, %ecx
	jne	je_else.88
	movl	%ebx, %eax
	addl	$56, %ebp
	call	g.28
	subl	$56, %ebp
	jmp	je_cont.89
je_else.88:
	addl	%esi, %edx
	addl	%edi, %edx
	movl	8(%ebp), %ebx
	addl	%ebx, %edx
	movl	12(%ebp), %ebx
	addl	%ebx, %edx
	movl	16(%ebp), %ebx
	addl	%ebx, %edx
	movl	20(%ebp), %ebx
	addl	%ebx, %edx
	movl	24(%ebp), %ebx
	addl	%ebx, %edx
	movl	28(%ebp), %ebx
	addl	%ebx, %edx
	movl	32(%ebp), %ebx
	addl	%ebx, %edx
	movl	36(%ebp), %ebx
	addl	%ebx, %edx
	movl	40(%ebp), %ebx
	addl	%ebx, %edx
	movl	44(%ebp), %ebx
	addl	%ebx, %edx
	movl	48(%ebp), %ebx
	addl	%ebx, %edx
	movl	52(%ebp), %ebx
	addl	%ebx, %edx
	addl	%eax, %edx
	movl	4(%ebp), %eax
	addl	%edx, %eax
je_cont.89:
	addl	$56, %ebp
	call	min_caml_print_int
	subl	$56, %ebp
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
