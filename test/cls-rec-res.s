.data
.balign	8
.text
f.8:
	movl	4(%edi), %ebx
	cmpl	$0, %eax
	jne	je_else.21
	movl	$0, %eax
	ret
je_else.21:
	subl	$1, %eax
	movl	%ebx, 0(%ebp)
	addl	$8, %ebp
	call	*(%edi)
	subl	$8, %ebp
	movl	0(%ebp), %ebx
	addl	%ebx, %eax
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
	movl	min_caml_hp, %edi
	addl	$8, min_caml_hp
	movl	$f.8, %ebx
	movl	%ebx, 0(%edi)
	movl	%eax, 4(%edi)
	movl	$123, %eax
	call	*(%edi)
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
