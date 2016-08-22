.data
.balign	8
.text
f.6:
	addl	$123, %eax
	ret
g.9:
	movl	4(%edi), %eax
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
	movl	min_caml_hp, %eax
	addl	$8, min_caml_hp
	movl	$f.6, %ebx
	movl	%ebx, 0(%eax)
	movl	min_caml_hp, %edi
	addl	$8, min_caml_hp
	movl	$g.9, %ebx
	movl	%ebx, 0(%edi)
	movl	%eax, 4(%edi)
	movl	$456, %eax
	call	*(%edi)
	movl	%eax, %edi
	movl	$789, %eax
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
