	.file ""
	.section .rodata.cst8,"a",@progbits
	.align	16
caml_negf_mask:
	.quad	0x8000000000000000
	.quad	0
	.align	16
caml_absf_mask:
	.quad	0x7fffffffffffffff
	.quad	-1
	.data
	.globl	camlTuturu__data_begin
camlTuturu__data_begin:
	.text
	.globl	camlTuturu__code_begin
camlTuturu__code_begin:
	.data
	.align	8
	.data
	.align	8
	.quad	3063
	.globl	camlTuturu__tz_compiled_2_399
camlTuturu__tz_compiled_2_399:
	.globl	camlTuturu__tz_compiled_2_370_closure
camlTuturu__tz_compiled_2_370_closure:
	.quad	camlTuturu__tz_compiled_2_370
	.quad	3
	.data
	.align	8
	.quad	3063
	.globl	camlTuturu__tz_compiled_1_398
camlTuturu__tz_compiled_1_398:
	.globl	camlTuturu__tz_compiled_1_64_closure
camlTuturu__tz_compiled_1_64_closure:
	.quad	camlTuturu__tz_compiled_1_64
	.quad	3
	.data
	.align	8
	.quad	3063
	.globl	camlTuturu__tz_compiled_0_397
camlTuturu__tz_compiled_0_397:
	.globl	camlTuturu__tz_compiled_0_11_closure
camlTuturu__tz_compiled_0_11_closure:
	.quad	camlTuturu__tz_compiled_0_11
	.quad	3
	.data
	.align	8
	.globl	camlTuturu__gc_roots
camlTuturu__gc_roots:
	.quad	0
	.text
	.align	16
	.globl	camlTuturu__tz_compiled_2_370
camlTuturu__tz_compiled_2_370:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L100:
	movq	(%rax), %rax
	movq	(%rax), %rbx
	movq	8(%rbx), %rdi
.L101:
	subq	$48, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L102
	leaq	8(%r15), %rsi
	addq	$24, %rsi
	movq	$2048, -8(%rsi)
	movq	camlTuturu__const_block_375@GOTPCREL(%rip), %rax
	movq	%rax, (%rsi)
	movq	(%rbx), %rax
	movq	%rax, 8(%rsi)
	leaq	-24(%rsi), %rax
	movq	$2048, -8(%rax)
	movq	%rsi, (%rax)
	movq	%rdi, 8(%rax)
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
.L102:
	call	caml_call_gc@PLT
.L103:
	jmp	.L101
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.type camlTuturu__tz_compiled_2_370,@function
	.size camlTuturu__tz_compiled_2_370,. - camlTuturu__tz_compiled_2_370
	.text
	.align	16
	.globl	camlTuturu__tz_compiled_1_64
camlTuturu__tz_compiled_1_64:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_adjust_cfa_offset 40
.L111:
	movq	(%rax), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	(%rax), %rax
	movq	(%rax), %rdi
	movq	8(%rax), %rax
	movq	%rax, 24(%rsp)
	movq	(%rdi), %rbx
	movq	%rbx, 16(%rsp)
	movq	8(%rdi), %rax
	movq	%rax, (%rsp)
	movq	(%rsi), %rdi
	movq	8(%rsi), %rax
	movq	camlTezos_raw_protocol_alpha__Script_ir_translator@GOTPCREL(%rip), %rsi
	movq	160(%rsi), %rsi
	call	caml_apply3@PLT
.L104:
	movq	camlTezos_raw_protocol_alpha__Alpha_context@GOTPCREL(%rip), %rbx
	movq	48(%rbx), %rbx
	movq	72(%rbx), %rbx
	movq	(%rbx), %rdi
	call	*%rdi
.L105:
	movq	camlTezos_raw_protocol_alpha__Alpha_context@GOTPCREL(%rip), %rbx
	movq	48(%rbx), %rdi
	movq	8(%rdi), %rbx
	movq	16(%rdi), %rdi
	call	caml_apply2@PLT
.L106:
	movq	camlTezos_protocol_environment_alpha__Environment@GOTPCREL(%rip), %rbx
	movq	120(%rbx), %rbx
	movq	24(%rbx), %rbx
	movq	40(%rbx), %rdi
	movq	$1, %rbx
	call	caml_apply2@PLT
.L107:
	cmpq	$1, %rax
	je	.L110
	movq	camlTezos_raw_protocol_alpha__Alpha_context@GOTPCREL(%rip), %rax
	movq	48(%rax), %rax
	movq	136(%rax), %rdi
	movq	(%rsp), %rax
	movq	16(%rsp), %rbx
	call	caml_apply2@PLT
.L108:
	movq	%rax, (%rsp)
	movq	8(%rsp), %rax
	movq	16(%rax), %rbx
	movq	camlTezos_raw_protocol_alpha__Alpha_context@GOTPCREL(%rip), %rax
	movq	48(%rax), %rax
	movq	128(%rax), %rdi
	movq	16(%rsp), %rax
	call	caml_apply2@PLT
.L109:
	movq	%rax, %rbx
.L112:
	subq	$64, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L113
	leaq	8(%r15), %rax
	addq	$40, %rax
	movq	$2048, -8(%rax)
	movq	%rbx, (%rax)
	movq	(%rsp), %rbx
	movq	%rbx, 8(%rax)
	leaq	-16(%rax), %rbx
	movq	$1024, -8(%rbx)
	movq	%rax, (%rbx)
	leaq	-24(%rbx), %rax
	movq	$2048, -8(%rax)
	movq	%rbx, (%rax)
	movq	24(%rsp), %rbx
	movq	%rbx, 8(%rax)
	addq	$40, %rsp
	.cfi_adjust_cfa_offset -40
	ret
	.cfi_adjust_cfa_offset 40
	.align	4
.L110:
.L115:
	subq	$40, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L116
	leaq	8(%r15), %rbx
	addq	$24, %rbx
	movq	$1025, -8(%rbx)
	movq	(%rsp), %rax
	movq	%rax, (%rbx)
	leaq	-24(%rbx), %rax
	movq	$2048, -8(%rax)
	movq	%rbx, (%rax)
	movq	24(%rsp), %rbx
	movq	%rbx, 8(%rax)
	addq	$40, %rsp
	.cfi_adjust_cfa_offset -40
	ret
	.cfi_adjust_cfa_offset 40
.L116:
	call	caml_call_gc@PLT
.L117:
	jmp	.L115
.L113:
	call	caml_call_gc@PLT
.L114:
	jmp	.L112
	.cfi_adjust_cfa_offset -40
	.cfi_endproc
	.type camlTuturu__tz_compiled_1_64,@function
	.size camlTuturu__tz_compiled_1_64,. - camlTuturu__tz_compiled_1_64
	.text
	.align	16
	.globl	camlTuturu__tz_compiled_0_11
camlTuturu__tz_compiled_0_11:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L118:
	movq	(%rax), %rax
	movq	(%rax), %rbx
	movq	8(%rax), %rdi
	movq	8(%rbx), %rsi
.L119:
	subq	$64, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L120
	leaq	8(%r15), %rax
	addq	$40, %rax
	movq	$2048, -8(%rax)
	movq	(%rbx), %rbx
	movq	(%rbx), %rbx
	movq	%rbx, (%rax)
	movq	%rdi, 8(%rax)
	leaq	-16(%rax), %rbx
	movq	$1024, -8(%rbx)
	movq	%rax, (%rbx)
	leaq	-24(%rbx), %rax
	movq	$2048, -8(%rax)
	movq	%rbx, (%rax)
	movq	%rsi, 8(%rax)
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 8
.L120:
	call	caml_call_gc@PLT
.L121:
	jmp	.L119
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.type camlTuturu__tz_compiled_0_11,@function
	.size camlTuturu__tz_compiled_0_11,. - camlTuturu__tz_compiled_0_11
	.data
	.align	8
	.quad	3840
	.globl	camlTuturu
camlTuturu:
	.quad	camlTuturu__tz_compiled_0_11_closure
	.quad	camlTuturu__tz_compiled_1_64_closure
	.quad	camlTuturu__tz_compiled_2_370_closure
	.data
	.align	8
	.data
	.align	8
	.data
	.align	8
	.data
	.align	8
	.quad	2816
	.globl	camlTuturu__const_block_375
camlTuturu__const_block_375:
	.quad	1
	.quad	1
	.text
	.align	16
	.globl	camlTuturu__entry
camlTuturu__entry:
	.cfi_startproc
.L122:
	movq	$1, %rax
	ret
	.cfi_endproc
	.type camlTuturu__entry,@function
	.size camlTuturu__entry,. - camlTuturu__entry
	.data
	.align	8
	.text
	.globl	camlTuturu__code_end
camlTuturu__code_end:
	.data
				/* relocation table start */
	.align	8
				/* relocation table end */
	.data
	.quad	0
	.globl	camlTuturu__data_end
camlTuturu__data_end:
	.quad	0
	.align	8
	.globl	camlTuturu__frametable
camlTuturu__frametable:
	.quad	10
	.quad	.L121
	.word	16
	.word	3
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L117
	.word	48
	.word	2
	.word	0
	.word	24
	.align	8
	.quad	.L114
	.word	48
	.word	3
	.word	0
	.word	3
	.word	24
	.align	8
	.quad	.L109
	.word	49
	.word	2
	.word	0
	.word	24
	.align	8
	.quad	.L123
	.quad	.L108
	.word	49
	.word	3
	.word	8
	.word	16
	.word	24
	.align	8
	.quad	.L123
	.quad	.L107
	.word	49
	.word	4
	.word	0
	.word	8
	.word	16
	.word	24
	.align	8
	.quad	.L123
	.quad	.L106
	.word	49
	.word	4
	.word	0
	.word	8
	.word	16
	.word	24
	.align	8
	.quad	.L123
	.quad	.L105
	.word	49
	.word	4
	.word	0
	.word	8
	.word	16
	.word	24
	.align	8
	.quad	.L123
	.quad	.L104
	.word	49
	.word	4
	.word	0
	.word	8
	.word	16
	.word	24
	.align	8
	.quad	.L123
	.quad	.L103
	.word	16
	.word	2
	.word	3
	.word	5
	.align	8
	.align	8
.L123:
	.long	(.L124 - .) + -67108864
	.long	-17
	.quad	0
.L124:
	.ascii	"_none_\0"
	.align	8
	.section .note.GNU-stack,"",%progbits
