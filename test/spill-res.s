.data
.balign	8
.text
f.43:
	movl	%eax, %esi
	addl	%ebx, %esi
	movl	%eax, %edi
	addl	%ecx, %edi
	movl	%eax, 0(%ebp)
	addl	%edx, %eax
	movl	%eax, 4(%ebp)
	movl	%ebx, %eax
	addl	%ecx, %eax
	movl	%ebx, 8(%ebp)
	addl	%edx, %ebx
	movl	%edx, 12(%ebp)
	addl	%ecx, %edx
	movl	%ecx, 16(%ebp)
	movl	%esi, %ecx
	addl	%edi, %ecx
	movl	%ecx, 20(%ebp)
	movl	4(%ebp), %ecx
	addl	%esi, %ecx
	movl	%ecx, 24(%ebp)
	movl	%esi, %ecx
	addl	%eax, %ecx
	movl	%ecx, 28(%ebp)
	movl	%esi, %ecx
	addl	%ebx, %ecx
	movl	%esi, 32(%ebp)
	addl	%edx, %esi
	movl	%ecx, 36(%ebp)
	movl	%esi, 40(%ebp)
	movl	4(%ebp), %esi
	movl	%edi, %ecx
	addl	%esi, %ecx
	movl	%ecx, 44(%ebp)
	movl	%edi, %ecx
	addl	%eax, %ecx
	movl	%ecx, 48(%ebp)
	movl	%edi, %ecx
	addl	%ebx, %ecx
	movl	%edi, 52(%ebp)
	addl	%edx, %edi
	movl	%edi, 56(%ebp)
	movl	%esi, %edi
	addl	%eax, %edi
	movl	%edi, 60(%ebp)
	movl	%esi, %edi
	addl	%ebx, %edi
	addl	%edx, %esi
	movl	%esi, 64(%ebp)
	movl	%eax, %esi
	addl	%ebx, %esi
	movl	%eax, 68(%ebp)
	addl	%edx, %eax
	movl	%edx, 72(%ebp)
	addl	%ebx, %edx
	movl	%ebx, 76(%ebp)
	movl	20(%ebp), %ebx
	movl	%edx, 80(%ebp)
	movl	24(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 84(%ebp)
	movl	28(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 88(%ebp)
	movl	36(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 92(%ebp)
	movl	40(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 96(%ebp)
	movl	44(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 100(%ebp)
	movl	48(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 104(%ebp)
	movl	%ebx, %edx
	addl	%ecx, %edx
	movl	%edx, 108(%ebp)
	movl	56(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 112(%ebp)
	movl	60(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 116(%ebp)
	movl	%ebx, %edx
	addl	%edi, %edx
	movl	%edx, 120(%ebp)
	movl	64(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 124(%ebp)
	movl	%ebx, %edx
	addl	%esi, %edx
	movl	%edx, 128(%ebp)
	movl	%ebx, %edx
	addl	%eax, %edx
	movl	%edx, 132(%ebp)
	movl	80(%ebp), %edx
	addl	%ebx, %edx
	movl	%edx, 136(%ebp)
	movl	0(%ebp), %edx
	movl	%eax, 140(%ebp)
	movl	8(%ebp), %eax
	addl	%eax, %edx
	movl	16(%ebp), %eax
	addl	%eax, %edx
	movl	12(%ebp), %eax
	addl	%eax, %edx
	movl	32(%ebp), %eax
	addl	%eax, %edx
	movl	52(%ebp), %eax
	addl	%eax, %edx
	movl	4(%ebp), %eax
	addl	%eax, %edx
	movl	68(%ebp), %eax
	addl	%eax, %edx
	movl	76(%ebp), %eax
	addl	%eax, %edx
	movl	72(%ebp), %eax
	addl	%eax, %edx
	addl	%ebx, %edx
	movl	24(%ebp), %eax
	addl	%eax, %edx
	movl	28(%ebp), %eax
	addl	%eax, %edx
	movl	36(%ebp), %eax
	addl	%eax, %edx
	movl	40(%ebp), %eax
	addl	%eax, %edx
	movl	44(%ebp), %eax
	addl	%eax, %edx
	movl	48(%ebp), %eax
	addl	%eax, %edx
	addl	%ecx, %edx
	movl	56(%ebp), %eax
	addl	%eax, %edx
	movl	60(%ebp), %eax
	addl	%eax, %edx
	addl	%edi, %edx
	movl	64(%ebp), %eax
	addl	%eax, %edx
	addl	%esi, %edx
	movl	140(%ebp), %eax
	addl	%eax, %edx
	movl	80(%ebp), %eax
	addl	%eax, %edx
	movl	84(%ebp), %eax
	addl	%eax, %edx
	movl	88(%ebp), %eax
	addl	%eax, %edx
	movl	92(%ebp), %eax
	addl	%eax, %edx
	movl	96(%ebp), %eax
	addl	%eax, %edx
	movl	100(%ebp), %eax
	addl	%eax, %edx
	movl	104(%ebp), %eax
	addl	%eax, %edx
	movl	108(%ebp), %eax
	addl	%eax, %edx
	movl	112(%ebp), %eax
	addl	%eax, %edx
	movl	116(%ebp), %eax
	addl	%eax, %edx
	movl	120(%ebp), %eax
	addl	%eax, %edx
	movl	124(%ebp), %eax
	addl	%eax, %edx
	movl	128(%ebp), %eax
	addl	%eax, %edx
	movl	132(%ebp), %eax
	addl	%eax, %edx
	movl	136(%ebp), %eax
	addl	%eax, %edx
	movl	%edx, %eax
	negl	%eax
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
	movl	$1, %eax
	movl	$2, %ebx
	movl	$3, %ecx
	movl	$4, %edx
	call	f.43
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
