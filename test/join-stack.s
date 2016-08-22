.data
.balign	8
.text
f.14:
	movl	$123, %eax
	ret
g.16:
	movl	$456, %eax
	ret
h.18:
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
	call	f.14
	movl	%eax, 0(%ebp)
	addl	$8, %ebp
	call	g.16
	subl	$8, %ebp
	movl	%eax, 4(%ebp)
	addl	$8, %ebp
	call	h.18
	subl	$8, %ebp
	cmpl	$0, %eax
	jne	je_else.35
	movl	0(%ebp), %eax
	addl	$1, %eax
	jmp	je_cont.36
je_else.35:
	movl	4(%ebp), %eax
	addl	$2, %eax
je_cont.36:
	movl	0(%ebp), %ebx
	addl	%ebx, %eax
	movl	4(%ebp), %ebx
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
