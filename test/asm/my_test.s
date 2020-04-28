
  .file "my_test.s"
	.text
	.comm	global,8,8
	.globl	simple
	.type	simple, @function
simple$wew:
	pushq	%rbp
	movq	%rsp, %rbp

	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)

	movq	-8(%rbp), %rdx
	movq	-16(%rbp), %rax
	addq	%rdx, %rax

	popq	%rbp
	ret

	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp

	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$47, %esi
	movl	$5, %edi
	call	simple$wew
	movq	%rax, global(%rip)
	movl	$0, %eax

	leave
  ret
