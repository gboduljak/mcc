	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0
	.globl	_f                              ## -- Begin function f
	.p2align	4, 0x90
_f:                                     ## @f
	.cfi_startproc
## %bb.0:                               ## %entry_0
	pushq	%rax
	.cfi_def_cfa_offset 16
	movsd	%xmm0, (%rsp)
	leaq	_str.0(%rip), %rdi
	movb	$1, %al
	callq	_printf
	movsd	(%rsp), %xmm0                   ## xmm0 = mem[0],zero
	popq	%rax
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3                               ## -- Begin function main
LCPI1_0:
	.quad	0x40283d70a3d70a3d              ## double 12.119999999999999
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry_0
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movabsq	$4623012571490224701, %rax      ## imm = 0x40283D70A3D70A3D
	movq	%rax, 16(%rsp)
	movsd	LCPI1_0(%rip), %xmm0            ## xmm0 = mem[0],zero
	callq	_f
	movsd	%xmm0, 8(%rsp)
	leaq	_str.1(%rip), %rdi
	movb	$1, %al
	callq	_printf
	xorl	%eax, %eax
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
	.globl	_str.0                          ## @str.0
_str.0:
	.asciz	"%f"

	.globl	_str.1                          ## @str.1
_str.1:
	.asciz	"%f\n"

.subsections_via_symbols
