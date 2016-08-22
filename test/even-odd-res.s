.data
.balign	8
.text
odd.19:
	cmpl	$0, %eax
	jg	jle_else.36
	cmpl	$0, %eax
	jl	jge_else.37
	movl	$456, %eax
	ret
jge_else.37:
	addl	$1, %eax
	jmp	even.17
jle_else.36:
	subl	$1, %eax
	jmp	even.17
even.17:
	cmpl	$0, %eax
	jg	jle_else.38
	cmpl	$0, %eax
	jl	jge_else.39
	movl	$123, %eax
	ret
jge_else.39:
	addl	$1, %eax
	jmp	odd.19
jle_else.38:
	subl	$1, %eax
	jmp	odd.19
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
	movl	$789, %eax
	call	even.17
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
