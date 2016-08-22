.data
.balign	8
.text
ack.15:
	cmpl	$0, %eax
	jg	jle_else.34
	movl	%ebx, %eax
	addl	$1, %eax
	ret
jle_else.34:
	cmpl	$0, %ebx
	jg	jle_else.35
	subl	$1, %eax
	movl	$1, %ebx
	jmp	ack.15
jle_else.35:
	movl	%eax, %ecx
	subl	$1, %ecx
	subl	$1, %ebx
	movl	%ecx, 0(%ebp)
	addl	$8, %ebp
	call	ack.15
	subl	$8, %ebp
	movl	%eax, %ebx
	movl	0(%ebp), %eax
	jmp	ack.15
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
	movl	$10, %ebx
	call	ack.15
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
