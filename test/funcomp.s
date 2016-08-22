.data
.balign	8
.text
composed.10:
	movl	8(%edi), %ebx
	movl	4(%edi), %edi
	movl	%ebx, 0(%ebp)
	addl	$8, %ebp
	call	*(%edi)
	subl	$8, %ebp
	movl	0(%ebp), %edi
	jmp	*(%edi)
compose.7:
	movl	min_caml_hp, %ecx
	addl	$16, min_caml_hp
	movl	$composed.10, %edx
	movl	%edx, 0(%ecx)
	movl	%ebx, 8(%ecx)
	movl	%eax, 4(%ecx)
	movl	%ecx, %eax
	ret
dbl.13:
	addl	%eax, %eax
	ret
inc.15:
	addl	$1, %eax
	ret
dec.18:
	subl	$1, %eax
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
	movl	$dbl.13, %ebx
	movl	%ebx, 0(%eax)
	movl	min_caml_hp, %ebx
	addl	$8, min_caml_hp
	movl	$inc.15, %ecx
	movl	%ecx, 0(%ebx)
	movl	min_caml_hp, %ecx
	addl	$8, min_caml_hp
	movl	$dec.18, %edx
	movl	%edx, 0(%ecx)
	movl	%ebx, 0(%ebp)
	movl	%ecx, %ebx
	addl	$8, %ebp
	call	compose.7
	subl	$8, %ebp
	movl	%eax, %ebx
	movl	0(%ebp), %eax
	addl	$8, %ebp
	call	compose.7
	subl	$8, %ebp
	movl	%eax, %edi
	movl	$123, %eax
	addl	$8, %ebp
	call	*(%edi)
	subl	$8, %ebp
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
