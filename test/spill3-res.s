.data
.balign	8
.text
f.56:
	movl	%eax, %ebx
	addl	$1, %ebx
	movl	%ebx, %ecx
	addl	$1, %ecx
	movl	%ecx, %edx
	addl	$1, %edx
	movl	%edx, %esi
	addl	$1, %esi
	movl	%esi, %edi
	addl	$1, %edi
	movl	%eax, 0(%ebp)
	movl	%edi, %eax
	addl	$1, %eax
	movl	%eax, 4(%ebp)
	addl	$1, %eax
	movl	%eax, 8(%ebp)
	addl	$1, %eax
	movl	%eax, 12(%ebp)
	addl	$1, %eax
	movl	%eax, 16(%ebp)
	addl	$1, %eax
	movl	%eax, 20(%ebp)
	addl	$1, %eax
	movl	%eax, 24(%ebp)
	addl	$1, %eax
	movl	%eax, 28(%ebp)
	addl	$1, %eax
	movl	%eax, 32(%ebp)
	addl	$1, %eax
	movl	%eax, 36(%ebp)
	addl	$1, %eax
	movl	%eax, 40(%ebp)
	addl	$1, %eax
	movl	%eax, 44(%ebp)
	addl	$1, %eax
	movl	%eax, 48(%ebp)
	addl	$1, %eax
	movl	%eax, 52(%ebp)
	addl	%ebx, %eax
	movl	%eax, 56(%ebp)
	addl	%ecx, %eax
	movl	%eax, 60(%ebp)
	addl	%edx, %eax
	movl	%eax, 64(%ebp)
	addl	%esi, %eax
	movl	%eax, 68(%ebp)
	addl	%edi, %eax
	movl	%eax, 72(%ebp)
	movl	%edi, 76(%ebp)
	movl	4(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 80(%ebp)
	movl	8(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 84(%ebp)
	movl	12(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 88(%ebp)
	movl	16(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 92(%ebp)
	movl	20(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 96(%ebp)
	movl	24(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 100(%ebp)
	movl	28(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 104(%ebp)
	movl	32(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 108(%ebp)
	movl	36(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 112(%ebp)
	movl	40(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 116(%ebp)
	movl	44(%ebp), %edi
	addl	%edi, %eax
	movl	%eax, 120(%ebp)
	movl	48(%ebp), %edi
	addl	%edi, %eax
	movl	0(%ebp), %edi
	addl	%eax, %edi
	addl	%ecx, %ebx
	addl	%edx, %ebx
	addl	%esi, %ebx
	movl	76(%ebp), %ecx
	addl	%ecx, %ebx
	movl	4(%ebp), %ecx
	addl	%ecx, %ebx
	movl	8(%ebp), %ecx
	addl	%ecx, %ebx
	movl	12(%ebp), %ecx
	addl	%ecx, %ebx
	movl	16(%ebp), %ecx
	addl	%ecx, %ebx
	movl	20(%ebp), %ecx
	addl	%ecx, %ebx
	movl	24(%ebp), %ecx
	addl	%ecx, %ebx
	movl	28(%ebp), %ecx
	addl	%ecx, %ebx
	movl	32(%ebp), %ecx
	addl	%ecx, %ebx
	movl	36(%ebp), %ecx
	addl	%ecx, %ebx
	movl	40(%ebp), %ecx
	addl	%ecx, %ebx
	movl	44(%ebp), %ecx
	addl	%ecx, %ebx
	movl	48(%ebp), %ecx
	addl	%ecx, %ebx
	movl	52(%ebp), %ecx
	addl	%ecx, %ebx
	movl	56(%ebp), %ecx
	addl	%ecx, %ebx
	movl	60(%ebp), %ecx
	addl	%ecx, %ebx
	movl	64(%ebp), %ecx
	addl	%ecx, %ebx
	movl	68(%ebp), %ecx
	addl	%ecx, %ebx
	movl	72(%ebp), %ecx
	addl	%ecx, %ebx
	movl	80(%ebp), %ecx
	addl	%ecx, %ebx
	movl	84(%ebp), %ecx
	addl	%ecx, %ebx
	movl	88(%ebp), %ecx
	addl	%ecx, %ebx
	movl	92(%ebp), %ecx
	addl	%ecx, %ebx
	movl	96(%ebp), %ecx
	addl	%ecx, %ebx
	movl	100(%ebp), %ecx
	addl	%ecx, %ebx
	movl	104(%ebp), %ecx
	addl	%ecx, %ebx
	movl	108(%ebp), %ecx
	addl	%ecx, %ebx
	movl	112(%ebp), %ecx
	addl	%ecx, %ebx
	movl	116(%ebp), %ecx
	addl	%ecx, %ebx
	movl	120(%ebp), %ecx
	addl	%ecx, %ebx
	addl	%eax, %ebx
	addl	%edi, %ebx
	movl	0(%ebp), %eax
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
	movl	$0, %eax
	call	f.56
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
