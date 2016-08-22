.data
.balign	8
.text
g.38:
	movl	40(%edi), %ebx
	movl	36(%edi), %ecx
	movl	32(%edi), %edx
	movl	28(%edi), %esi
	movl	%eax, 0(%ebp)
	movl	24(%edi), %eax
	movl	%ebx, 4(%ebp)
	movl	20(%edi), %ebx
	movl	%ecx, 8(%ebp)
	movl	16(%edi), %ecx
	movl	%edx, 12(%ebp)
	movl	12(%edi), %edx
	movl	%esi, 16(%ebp)
	movl	8(%edi), %esi
	movl	%edi, 20(%ebp)
	movl	4(%edi), %edi
	addl	%edx, %edi
	addl	%ecx, %edi
	addl	%ebx, %edi
	addl	%eax, %edi
	movl	16(%ebp), %eax
	addl	%eax, %edi
	movl	12(%ebp), %eax
	addl	%eax, %edi
	movl	8(%ebp), %eax
	addl	%eax, %edi
	movl	4(%ebp), %eax
	addl	%eax, %edi
	movl	%edi, %eax
	addl	%esi, %eax
	movl	0(%ebp), %ebx
	cmpl	$0, %ebx
	jg	jle_else.107
	movl	%ebx, %eax
	negl	%eax
	movl	20(%ebp), %edi
	jmp	*(%edi)
jle_else.107:
	ret
h.26:
	movl	36(%eax), %ebx
	movl	32(%eax), %ecx
	movl	28(%eax), %edx
	movl	24(%eax), %esi
	movl	20(%eax), %edi
	movl	%ebx, 0(%ebp)
	movl	16(%eax), %ebx
	movl	%ebx, 4(%ebp)
	movl	12(%eax), %ebx
	movl	%ebx, 8(%ebp)
	movl	8(%eax), %ebx
	movl	%ebx, 12(%ebp)
	movl	4(%eax), %ebx
	movl	0(%eax), %eax
	movl	%eax, 16(%ebp)
	movl	min_caml_hp, %eax
	addl	$48, min_caml_hp
	movl	%ebx, 20(%ebp)
	movl	$g.38, %ebx
	movl	%ebx, 0(%eax)
	movl	%ecx, 40(%eax)
	movl	%edx, 36(%eax)
	movl	%esi, 32(%eax)
	movl	%edi, 28(%eax)
	movl	4(%ebp), %ebx
	movl	%ebx, 24(%eax)
	movl	8(%ebp), %ebx
	movl	%ebx, 20(%eax)
	movl	12(%ebp), %ebx
	movl	%ebx, 16(%eax)
	movl	20(%ebp), %ebx
	movl	%ebx, 12(%eax)
	movl	0(%ebp), %ebx
	movl	%ebx, 8(%eax)
	movl	16(%ebp), %ebx
	movl	%ebx, 4(%eax)
	movl	$1, %ebx
	movl	%eax, %edi
	movl	%ebx, %eax
	jmp	*(%edi)
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
	movl	$1, %eax
	movl	$2, %ebx
	movl	$3, %ecx
	movl	$4, %edx
	movl	$5, %esi
	movl	$6, %edi
	movl	%eax, 0(%ebp)
	movl	$7, %eax
	movl	%ebx, 4(%ebp)
	movl	$8, %ebx
	movl	%ecx, 8(%ebp)
	movl	$9, %ecx
	movl	%edx, 12(%ebp)
	movl	$10, %edx
	movl	%esi, 16(%ebp)
	movl	min_caml_hp, %esi
	addl	$40, min_caml_hp
	movl	%edx, 36(%esi)
	movl	%ecx, 32(%esi)
	movl	%ebx, 28(%esi)
	movl	%eax, 24(%esi)
	movl	%edi, 20(%esi)
	movl	16(%ebp), %eax
	movl	%eax, 16(%esi)
	movl	12(%ebp), %eax
	movl	%eax, 12(%esi)
	movl	8(%ebp), %eax
	movl	%eax, 8(%esi)
	movl	4(%ebp), %eax
	movl	%eax, 4(%esi)
	movl	0(%ebp), %eax
	movl	%eax, 0(%esi)
	movl	%esi, %eax
	addl	$24, %ebp
	call	h.26
	subl	$24, %ebp
	addl	$24, %ebp
	call	min_caml_print_int
	subl	$24, %ebp
	addl	$24, %ebp
	call	min_caml_print_newline
	subl	$24, %ebp
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
