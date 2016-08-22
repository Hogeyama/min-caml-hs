.data
.balign	8
.text
f.9:
	cmpl	$0, %eax
	jl	jge_else.26
	movl	%eax, 0(%ebp)
	movl	%edi, 4(%ebp)
	addl	$8, %ebp
	call	min_caml_print_int
	subl	$8, %ebp
	movl	$1, %eax
	movl	4(%ebp), %ebx
	addl	$8, %ebp
	call	min_caml_create_array
	subl	$8, %ebp
	movl	0(%eax), %edi
	movl	0(%ebp), %eax
	subl	$1, %eax
	jmp	*(%edi)
jge_else.26:
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
	movl	min_caml_hp, %edi
	addl	$8, min_caml_hp
	movl	$f.9, %eax
	movl	%eax, 0(%edi)
	movl	$9, %eax
	call	*(%edi)
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
