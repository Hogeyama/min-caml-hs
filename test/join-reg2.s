.data
.balign	8
.text
f.12:
	movl	$123, %eax
	ret
g.14:
	movl	$456, %eax
	ret
h.16:
	movl	$789, %eax
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
	call	f.12
	cmpl	$0, %eax
	jg	jle_else.30
	movl	%eax, 0(%ebp)
	addl	$8, %ebp
	call	g.14
	subl	$8, %ebp
	movl	0(%ebp), %ebx
	addl	%ebx, %eax
	jmp	jle_cont.31
jle_else.30:
	movl	%eax, 0(%ebp)
	addl	$8, %ebp
	call	h.16
	subl	$8, %ebp
	movl	0(%ebp), %ebx
	subl	%ebx, %eax
jle_cont.31:
	addl	%ebx, %eax
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
