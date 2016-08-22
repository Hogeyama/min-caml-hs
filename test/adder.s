.data
.balign	8
.text
adder.7:
	movl	4(%edi), %ebx
	addl	%ebx, %eax
	ret
make_adder.5:
	movl	min_caml_hp, %ebx
	addl	$8, min_caml_hp
	movl	$adder.7, %ecx
	movl	%ecx, 0(%ebx)
	movl	%eax, 4(%ebx)
	movl	%ebx, %eax
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
	movl	$3, %eax
	call	make_adder.5
	movl	%eax, %edi
	movl	$7, %eax
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
