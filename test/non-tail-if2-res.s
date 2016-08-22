.data
.balign	8
.text
f.13:
	movl	$12345, %eax
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
	movl	$3, %ebx
	call	min_caml_create_array
	movl	0(%eax), %ebx
	cmpl	$3, %ebx
	jne	je_else.30
	movl	%eax, 0(%ebp)
	addl	$8, %ebp
	call	f.13
	subl	$8, %ebp
	movl	0(%ebp), %ebx
	movl	4(%ebx), %ebx
	addl	%ebx, %eax
	addl	$67890, %eax
	jmp	je_cont.31
je_else.30:
	movl	$7, %eax
je_cont.31:
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
