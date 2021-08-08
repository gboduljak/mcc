	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	_gcd                            ## -- Begin function gcd
	.p2align	4, 0x90
_gcd:                                   ## @gcd
	.cfi_startproc
## %bb.0:                               ## %entry_0
	movl	%edi, -12(%rsp)
	movl	%esi, -8(%rsp)
	cmpl	%esi, %edi
	jne	LBB0_1
LBB0_5:                                 ## %if.merge_0
	movl	-12(%rsp), %eax
	retq
	.p2align	4, 0x90
LBB0_3:                                 ## %if.alt_1
                                        ##   in Loop: Header=BB0_1 Depth=1
	movl	-12(%rsp), %eax
	subl	%eax, -8(%rsp)
LBB0_4:                                 ## %if.merge_1
                                        ##   in Loop: Header=BB0_1 Depth=1
	movl	-12(%rsp), %eax
	cmpl	-8(%rsp), %eax
	je	LBB0_5
LBB0_1:                                 ## %loop.body_0
                                        ## =>This Inner Loop Header: Depth=1
	movl	-12(%rsp), %eax
	cmpl	-8(%rsp), %eax
	jle	LBB0_3
## %bb.2:                               ## %if.conseq_1
                                        ##   in Loop: Header=BB0_1 Depth=1
	movl	-8(%rsp), %eax
	subl	%eax, -12(%rsp)
	jmp	LBB0_4
	.cfi_endproc
                                        ## -- End function
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry_0
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$2, %edi
	movl	$14, %esi
	callq	_gcd
	leaq	_str.0(%rip), %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$3, %edi
	movl	$15, %esi
	callq	_gcd
	leaq	_str.1(%rip), %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$99, %edi
	movl	$121, %esi
	callq	_gcd
	leaq	_str.2(%rip), %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
	.globl	_str.0                          ## @str.0
_str.0:
	.asciz	"%d\n"

	.globl	_str.1                          ## @str.1
_str.1:
	.asciz	"%d\n"

	.globl	_str.2                          ## @str.2
_str.2:
	.asciz	"%d\n"

.subsections_via_symbols
