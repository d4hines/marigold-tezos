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
	.globl	camlTuturu__tz_compiled_2_2091
camlTuturu__tz_compiled_2_2091:
	.globl	camlTuturu__tz_compiled_2_1928_closure
camlTuturu__tz_compiled_2_1928_closure:
	.quad	camlTuturu__tz_compiled_2_1928
	.quad	3
	.data
	.align	8
	.quad	3063
	.globl	camlTuturu__tz_compiled_1_2090
camlTuturu__tz_compiled_1_2090:
	.globl	camlTuturu__tz_compiled_1_199_closure
camlTuturu__tz_compiled_1_199_closure:
	.quad	camlTuturu__tz_compiled_1_199
	.quad	3
	.data
	.align	8
	.quad	3063
	.globl	camlTuturu__tz_compiled_0_2089
camlTuturu__tz_compiled_0_2089:
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
	.globl	camlTuturu__anon_fn_1796
camlTuturu__anon_fn_1796:
	.cfi_startproc
.L101:
	movq	%rbx, %rdi
	movq	8(%rax), %rbx
	addq	$24, %rdi
	movq	(%rax), %rax
	jmp	camlTuturu__anon_fn_1830@PLT
	.cfi_endproc
	.type camlTuturu__anon_fn_1796,@function
	.size camlTuturu__anon_fn_1796,. - camlTuturu__anon_fn_1796
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1703
camlTuturu__anon_fn_1703:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L103:
	movq	%rax, %rdi
	movq	16(%rbx), %rsi
.L104:
	subq	$24, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L105
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	24(%rbx), %rbx
	movq	%rbx, (%rax)
	movq	%rdi, 8(%rax)
	movq	(%rsi), %rdi
	movq	%rsi, %rbx
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	jmp	*%rdi
	.cfi_adjust_cfa_offset 8
.L105:
	call	caml_call_gc@PLT
.L106:
	jmp	.L104
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.type camlTuturu__anon_fn_1703,@function
	.size camlTuturu__anon_fn_1703,. - camlTuturu__anon_fn_1703
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1674
camlTuturu__anon_fn_1674:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L109:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L110:
	subq	$80, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L111
	leaq	8(%r15), %rax
	addq	$64, %rax
	movq	$1024, -8(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, (%rax)
	leaq	-24(%rax), %rsi
	movq	$2048, -8(%rsi)
	movq	%rax, (%rsi)
	movq	40(%rbx), %rax
	movq	%rax, 8(%rsi)
	leaq	-40(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	$4343, -8(%rax)
	movq	camlTuturu__anon_fn_1703@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rax)
	movq	$3, 8(%rax)
	movq	24(%rbx), %rbx
	movq	%rbx, 16(%rax)
	movq	%rsi, 24(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	776(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L107:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L111:
	call	caml_call_gc@PLT
.L112:
	jmp	.L110
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1674,@function
	.size camlTuturu__anon_fn_1674,. - camlTuturu__anon_fn_1674
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1653
camlTuturu__anon_fn_1653:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L115:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L116:
	subq	$56, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L117
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$6391, -8(%rax)
	movq	camlTuturu__anon_fn_1674@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	88(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L113:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L117:
	call	caml_call_gc@PLT
.L118:
	jmp	.L116
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1653,@function
	.size camlTuturu__anon_fn_1653,. - camlTuturu__anon_fn_1653
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1620
camlTuturu__anon_fn_1620:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L121:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L122:
	subq	$80, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L123
	leaq	8(%r15), %rax
	addq	$56, %rax
	movq	$2048, -8(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, (%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 8(%rax)
	leaq	-56(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	$6391, -8(%rsi)
	movq	camlTuturu__anon_fn_1653@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rsi)
	movq	$3, 8(%rsi)
	movq	16(%rbx), %rdx
	movq	%rdx, 16(%rsi)
	movq	24(%rbx), %rdx
	movq	%rdx, 24(%rsi)
	movq	%rax, 32(%rsi)
	movq	48(%rbx), %rax
	movq	%rax, 40(%rsi)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L119:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L123:
	call	caml_call_gc@PLT
.L124:
	jmp	.L122
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1620,@function
	.size camlTuturu__anon_fn_1620,. - camlTuturu__anon_fn_1620
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1599
camlTuturu__anon_fn_1599:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L127:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L128:
	subq	$64, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L129
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$7415, -8(%rax)
	movq	camlTuturu__anon_fn_1620@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rbx
	movq	%rbx, 48(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	56(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L125:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L129:
	call	caml_call_gc@PLT
.L130:
	jmp	.L128
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1599,@function
	.size camlTuturu__anon_fn_1599,. - camlTuturu__anon_fn_1599
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1561
camlTuturu__anon_fn_1561:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L134:
	movq	%rax, (%rsp)
	movq	%rbx, 8(%rsp)
	movq	40(%rbx), %rdi
	movq	32(%rbx), %rax
	movq	%rdi, %rbx
	call	camlTezos_raw_protocol_alpha__Script_int_repr__sub_213@PLT
.L131:
	movq	%rax, %rbx
.L135:
	subq	$64, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L136
	leaq	8(%r15), %rax
	movq	%rax, 16(%rsp)
	movq	$7415, -8(%rax)
	movq	camlTuturu__anon_fn_1599@GOTPCREL(%rip), %rdi
	movq	%rdi, (%rax)
	movq	$3, 8(%rax)
	movq	8(%rsp), %rsi
	movq	16(%rsi), %rdi
	movq	%rdi, 16(%rax)
	movq	24(%rsi), %rdi
	movq	%rdi, 24(%rax)
	movq	%rbx, 32(%rax)
	movq	48(%rsi), %rbx
	movq	%rbx, 40(%rax)
	movq	56(%rsi), %rbx
	movq	%rbx, 48(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	(%rsp), %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L132:
	movq	8(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	16(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L136:
	call	caml_call_gc@PLT
.L137:
	jmp	.L135
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1561,@function
	.size camlTuturu__anon_fn_1561,. - camlTuturu__anon_fn_1561
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1533
camlTuturu__anon_fn_1533:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L141:
	movq	%rax, (%rsp)
	movq	%rbx, 8(%rsp)
	movq	40(%rbx), %rdi
	movq	32(%rbx), %rax
	movq	%rdi, %rbx
	call	camlTezos_raw_protocol_alpha__Michelson_v1_gas__sub_bigint_2797@PLT
.L138:
	movq	%rax, %rbx
.L142:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L143
	leaq	8(%r15), %rax
	movq	%rax, 16(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1561@GOTPCREL(%rip), %rdi
	movq	%rdi, (%rax)
	movq	$3, 8(%rax)
	movq	8(%rsp), %rsi
	movq	16(%rsi), %rdi
	movq	%rdi, 16(%rax)
	movq	24(%rsi), %rdi
	movq	%rdi, 24(%rax)
	movq	32(%rsi), %rdi
	movq	%rdi, 32(%rax)
	movq	40(%rsi), %rdi
	movq	%rdi, 40(%rax)
	movq	48(%rsi), %rdi
	movq	%rdi, 48(%rax)
	movq	56(%rsi), %rdi
	movq	%rdi, 56(%rax)
	movq	(%rsp), %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L139:
	movq	8(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	16(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L143:
	call	caml_call_gc@PLT
.L144:
	jmp	.L142
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1533,@function
	.size camlTuturu__anon_fn_1533,. - camlTuturu__anon_fn_1533
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1494
camlTuturu__anon_fn_1494:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L147:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L148:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L149
	leaq	8(%r15), %rax
	addq	$72, %rax
	movq	$2048, -8(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, (%rax)
	movq	56(%rbx), %rdx
	movq	%rdx, 8(%rax)
	addq	$-72, %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1533@GOTPCREL(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	24(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	48(%rbx), %rcx
	movq	%rcx, 32(%rax)
	movq	32(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	%rsi, 48(%rax)
	movq	%rdx, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L145:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L149:
	call	caml_call_gc@PLT
.L150:
	jmp	.L148
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1494,@function
	.size camlTuturu__anon_fn_1494,. - camlTuturu__anon_fn_1494
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1470
camlTuturu__anon_fn_1470:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L154:
	movq	%rax, %r12
	movq	%rbx, (%rsp)
	movq	$217, %rdi
	call	ml_z_of_int@PLT
	movq	%rax, %rdi
.L155:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L156
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1494@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	%r12, %rax
	movq	%rdi, %rbx
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L152:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L156:
	call	caml_call_gc@PLT
.L157:
	jmp	.L155
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1470,@function
	.size camlTuturu__anon_fn_1470,. - camlTuturu__anon_fn_1470
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1446
camlTuturu__anon_fn_1446:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L160:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L161:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L162
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1470@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L158:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L162:
	call	caml_call_gc@PLT
.L163:
	jmp	.L161
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1446,@function
	.size camlTuturu__anon_fn_1446,. - camlTuturu__anon_fn_1446
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1424
camlTuturu__anon_fn_1424:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L166:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L167:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L168
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1446@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	24(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L164:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L168:
	call	caml_call_gc@PLT
.L169:
	jmp	.L167
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1424,@function
	.size camlTuturu__anon_fn_1424,. - camlTuturu__anon_fn_1424
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1385
camlTuturu__anon_fn_1385:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L173:
	movq	%rax, (%rsp)
	movq	%rbx, 8(%rsp)
	movq	48(%rbx), %rdi
	movq	40(%rbx), %rax
	movq	%rdi, %rbx
	call	camlTezos_raw_protocol_alpha__Script_int_repr__mul_226@PLT
.L170:
	movq	%rax, %rbx
.L174:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L175
	leaq	8(%r15), %rax
	addq	$72, %rax
	movq	$2048, -8(%rax)
	movq	%rbx, (%rax)
	movq	8(%rsp), %rcx
	movq	56(%rcx), %rdi
	movq	%rdi, 8(%rax)
	movq	8(%rdi), %rsi
	movq	(%rdi), %rdi
	addq	$-72, %rax
	movq	%rax, 16(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1424@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rcx), %rdx
	movq	%rdx, 16(%rax)
	movq	24(%rcx), %rdx
	movq	%rdx, 24(%rax)
	movq	32(%rcx), %rdx
	movq	%rdx, 32(%rax)
	movq	%rbx, 40(%rax)
	movq	%rdi, 48(%rax)
	movq	%rsi, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	(%rsp), %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L171:
	movq	8(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	16(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L175:
	call	caml_call_gc@PLT
.L176:
	jmp	.L174
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1385,@function
	.size camlTuturu__anon_fn_1385,. - camlTuturu__anon_fn_1385
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1356
camlTuturu__anon_fn_1356:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L180:
	movq	%rax, (%rsp)
	movq	%rbx, 8(%rsp)
	movq	48(%rbx), %rdi
	movq	40(%rbx), %rax
	movq	%rdi, %rbx
	call	camlTezos_raw_protocol_alpha__Michelson_v1_gas__mul_bigint_2819@PLT
.L177:
	movq	%rax, %rbx
.L181:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L182
	leaq	8(%r15), %rax
	movq	%rax, 16(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1385@GOTPCREL(%rip), %rdi
	movq	%rdi, (%rax)
	movq	$3, 8(%rax)
	movq	8(%rsp), %rsi
	movq	16(%rsi), %rdi
	movq	%rdi, 16(%rax)
	movq	24(%rsi), %rdi
	movq	%rdi, 24(%rax)
	movq	32(%rsi), %rdi
	movq	%rdi, 32(%rax)
	movq	40(%rsi), %rdi
	movq	%rdi, 40(%rax)
	movq	48(%rsi), %rdi
	movq	%rdi, 48(%rax)
	movq	56(%rsi), %rdi
	movq	%rdi, 56(%rax)
	movq	(%rsp), %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L178:
	movq	8(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	16(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L182:
	call	caml_call_gc@PLT
.L183:
	jmp	.L181
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1356,@function
	.size camlTuturu__anon_fn_1356,. - camlTuturu__anon_fn_1356
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1322
camlTuturu__anon_fn_1322:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L186:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L187:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L188
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1356@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L184:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L188:
	call	caml_call_gc@PLT
.L189:
	jmp	.L187
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1322,@function
	.size camlTuturu__anon_fn_1322,. - camlTuturu__anon_fn_1322
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1300
camlTuturu__anon_fn_1300:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L192:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L193:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L194
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1322@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	16(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L190:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L194:
	call	caml_call_gc@PLT
.L195:
	jmp	.L193
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1300,@function
	.size camlTuturu__anon_fn_1300,. - camlTuturu__anon_fn_1300
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1076
camlTuturu__anon_fn_1076:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L197:
	movq	%rax, %rdi
	movq	16(%rbx), %rsi
.L198:
	subq	$24, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L199
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	24(%rbx), %rbx
	movq	%rbx, (%rax)
	movq	%rdi, 8(%rax)
	movq	(%rsi), %rdi
	movq	%rsi, %rbx
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	jmp	*%rdi
	.cfi_adjust_cfa_offset 8
.L199:
	call	caml_call_gc@PLT
.L200:
	jmp	.L198
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.type camlTuturu__anon_fn_1076,@function
	.size camlTuturu__anon_fn_1076,. - camlTuturu__anon_fn_1076
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1260
camlTuturu__anon_fn_1260:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L203:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L204:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L205
	leaq	8(%r15), %rax
	addq	$72, %rax
	movq	$2048, -8(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, (%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 8(%rax)
	leaq	-72(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	$8439, -8(%rsi)
	movq	camlTuturu__anon_fn_1300@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rsi)
	movq	$3, 8(%rsi)
	movq	16(%rbx), %rdx
	movq	%rdx, 16(%rsi)
	movq	24(%rbx), %rdx
	movq	%rdx, 24(%rsi)
	movq	32(%rbx), %rdx
	movq	%rdx, 32(%rsi)
	movq	48(%rbx), %rdx
	movq	%rdx, 40(%rsi)
	movq	56(%rbx), %rbx
	movq	%rbx, 48(%rsi)
	movq	%rax, 56(%rsi)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L201:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L205:
	call	caml_call_gc@PLT
.L206:
	jmp	.L204
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1260,@function
	.size camlTuturu__anon_fn_1260,. - camlTuturu__anon_fn_1260
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1047
camlTuturu__anon_fn_1047:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L209:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L210:
	subq	$80, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L211
	leaq	8(%r15), %rax
	addq	$64, %rax
	movq	$1025, -8(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, (%rax)
	leaq	-24(%rax), %rsi
	movq	$2048, -8(%rsi)
	movq	%rax, (%rsi)
	movq	40(%rbx), %rax
	movq	%rax, 8(%rsi)
	leaq	-40(%rsi), %rax
	movq	%rax, 8(%rsp)
	movq	$4343, -8(%rax)
	movq	camlTuturu__anon_fn_1076@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rax)
	movq	$3, 8(%rax)
	movq	24(%rbx), %rbx
	movq	%rbx, 16(%rax)
	movq	%rsi, 24(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	776(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L207:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L211:
	call	caml_call_gc@PLT
.L212:
	jmp	.L210
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1047,@function
	.size camlTuturu__anon_fn_1047,. - camlTuturu__anon_fn_1047
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1235
camlTuturu__anon_fn_1235:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L216:
	movq	%rax, %r12
	movq	%rbx, (%rsp)
	movq	$217, %rdi
	call	ml_z_of_int@PLT
	movq	%rax, %rdi
.L217:
	subq	$80, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L218
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$9463, -8(%rax)
	movq	camlTuturu__anon_fn_1260@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rbx
	movq	%rbx, 64(%rax)
	movq	%r12, %rax
	movq	%rdi, %rbx
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L214:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L218:
	call	caml_call_gc@PLT
.L219:
	jmp	.L217
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1235,@function
	.size camlTuturu__anon_fn_1235,. - camlTuturu__anon_fn_1235
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1026
camlTuturu__anon_fn_1026:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L222:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L223:
	subq	$56, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L224
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$6391, -8(%rax)
	movq	camlTuturu__anon_fn_1047@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	96(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L220:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L224:
	call	caml_call_gc@PLT
.L225:
	jmp	.L223
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1026,@function
	.size camlTuturu__anon_fn_1026,. - camlTuturu__anon_fn_1026
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1205
camlTuturu__anon_fn_1205:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L228:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L229:
	subq	$80, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L230
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$9463, -8(%rax)
	movq	camlTuturu__anon_fn_1235@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	%rsi, 48(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	56(%rbx), %rbx
	movq	%rbx, 64(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L226:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L230:
	call	caml_call_gc@PLT
.L231:
	jmp	.L229
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1205,@function
	.size camlTuturu__anon_fn_1205,. - camlTuturu__anon_fn_1205
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1003
camlTuturu__anon_fn_1003:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L234:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L235:
	subq	$56, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L236
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$6391, -8(%rax)
	movq	camlTuturu__anon_fn_1026@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L232:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L236:
	call	caml_call_gc@PLT
.L237:
	jmp	.L235
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1003,@function
	.size camlTuturu__anon_fn_1003,. - camlTuturu__anon_fn_1003
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1183
camlTuturu__anon_fn_1183:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L240:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L241:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L242
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1205@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	8(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L238:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L242:
	call	caml_call_gc@PLT
.L243:
	jmp	.L241
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1183,@function
	.size camlTuturu__anon_fn_1183,. - camlTuturu__anon_fn_1183
	.text
	.align	16
	.globl	camlTuturu__anon_fn_982
camlTuturu__anon_fn_982:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L246:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L247:
	subq	$56, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L248
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$6391, -8(%rax)
	movq	camlTuturu__anon_fn_1003@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L244:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L248:
	call	caml_call_gc@PLT
.L249:
	jmp	.L247
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_982,@function
	.size camlTuturu__anon_fn_982,. - camlTuturu__anon_fn_982
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1817
camlTuturu__anon_fn_1817:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L251:
	movq	%rax, %rdi
	movq	16(%rbx), %rsi
.L252:
	subq	$24, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L253
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	24(%rbx), %rbx
	movq	%rbx, (%rax)
	movq	%rdi, 8(%rax)
	movq	(%rsi), %rdi
	movq	%rsi, %rbx
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	jmp	*%rdi
	.cfi_adjust_cfa_offset 8
.L253:
	call	caml_call_gc@PLT
.L254:
	jmp	.L252
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.type camlTuturu__anon_fn_1817,@function
	.size camlTuturu__anon_fn_1817,. - camlTuturu__anon_fn_1817
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1149
camlTuturu__anon_fn_1149:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L257:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L258:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L259
	leaq	8(%r15), %rax
	addq	$72, %rax
	movq	$2048, -8(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, (%rax)
	movq	56(%rbx), %rdx
	movq	%rdx, 8(%rax)
	addq	$-72, %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1183@GOTPCREL(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	24(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	32(%rbx), %rcx
	movq	%rcx, 32(%rax)
	movq	48(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	%rsi, 48(%rax)
	movq	%rdx, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L255:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L259:
	call	caml_call_gc@PLT
.L260:
	jmp	.L258
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1149,@function
	.size camlTuturu__anon_fn_1149,. - camlTuturu__anon_fn_1149
	.text
	.align	16
	.globl	camlTuturu__anon_fn_949
camlTuturu__anon_fn_949:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L263:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L264:
	subq	$56, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L265
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$6391, -8(%rax)
	movq	camlTuturu__anon_fn_982@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L261:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L265:
	call	caml_call_gc@PLT
.L266:
	jmp	.L264
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_949,@function
	.size camlTuturu__anon_fn_949,. - camlTuturu__anon_fn_949
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1830
camlTuturu__anon_fn_1830:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L269:
	movq	%rax, %rsi
	movq	%rdi, (%rsp)
.L270:
	subq	$40, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L271
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$4343, -8(%rax)
	movq	camlTuturu__anon_fn_1817@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rax)
	movq	$3, 8(%rax)
	movq	32(%rdi), %rdi
	movq	%rdi, 16(%rax)
	movq	%rsi, 24(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	776(%rax), %rdi
	movq	%rbx, %rax
	movq	%rdi, %rbx
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L267:
	movq	(%rsp), %rbx
	movq	24(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L271:
	call	caml_call_gc@PLT
.L272:
	jmp	.L270
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1830,@function
	.size camlTuturu__anon_fn_1830,. - camlTuturu__anon_fn_1830
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1127
camlTuturu__anon_fn_1127:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L275:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L276:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L277
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1149@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	16(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L273:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L277:
	call	caml_call_gc@PLT
.L278:
	jmp	.L276
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1127,@function
	.size camlTuturu__anon_fn_1127,. - camlTuturu__anon_fn_1127
	.text
	.align	16
	.globl	camlTuturu__anon_fn_928
camlTuturu__anon_fn_928:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L281:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L282:
	subq	$56, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L283
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$6391, -8(%rax)
	movq	camlTuturu__anon_fn_949@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	16(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L279:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L283:
	call	caml_call_gc@PLT
.L284:
	jmp	.L282
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_928,@function
	.size camlTuturu__anon_fn_928,. - camlTuturu__anon_fn_928
	.text
	.align	16
	.globl	camlTuturu__anon_fn_901
camlTuturu__anon_fn_901:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L292:
	movq	%rax, %rdi
	movq	%rbx, 16(%rsp)
.L293:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L294
	leaq	8(%r15), %rax
	movq	%rax, (%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1796@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	$3321, 16(%rax)
	movq	caml_curry2@GOTPCREL(%rip), %rsi
	movq	%rsi, 24(%rax)
	movq	$5, 32(%rax)
	movq	camlTuturu__anon_fn_1830@GOTPCREL(%rip), %rsi
	movq	%rsi, 40(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	48(%rbx), %rax
	cmpq	$1, %rax
	je	.L291
.L296:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L297
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$8439, -8(%rax)
	movq	camlTuturu__anon_fn_1127@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	72(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L285:
	movq	16(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	call	caml_apply2@PLT
.L286:
	jmp	.L290
	.align	4
.L291:
.L299:
	subq	$56, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L300
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$6391, -8(%rax)
	movq	camlTuturu__anon_fn_928@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	72(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L287:
	movq	16(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	call	caml_apply2@PLT
.L288:
.L290:
	movq	16(%rsp), %rbx
	movq	24(%rbx), %rdi
	movq	(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L300:
	call	caml_call_gc@PLT
.L301:
	jmp	.L299
.L297:
	call	caml_call_gc@PLT
.L298:
	jmp	.L296
.L294:
	call	caml_call_gc@PLT
.L295:
	jmp	.L293
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_901,@function
	.size camlTuturu__anon_fn_901,. - camlTuturu__anon_fn_901
	.text
	.align	16
	.globl	camlTuturu__anon_fn_878
camlTuturu__anon_fn_878:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L304:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L305:
	subq	$88, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L306
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$10487, -8(%rax)
	movq	camlTuturu__anon_fn_901@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rbx
	movq	%rbx, 72(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	552(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L302:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L306:
	call	caml_call_gc@PLT
.L307:
	jmp	.L305
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_878,@function
	.size camlTuturu__anon_fn_878,. - camlTuturu__anon_fn_878
	.text
	.align	16
	.globl	camlTuturu__anon_fn_825
camlTuturu__anon_fn_825:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L311:
	movq	%rax, %r12
	movq	%rbx, (%rsp)
	movq	camlTezos_raw_protocol_alpha__Script_int_repr@GOTPCREL(%rip), %rax
	movq	8(%rax), %rsi
	movq	48(%rbx), %rdi
	call	ml_z_compare@PLT
	cmpq	$1, %rax
	setg	%al
	movzbq	%al, %rdi
	salq	$1, %rdi
	incq	%rdi
.L312:
	subq	$88, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L313
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$10487, -8(%rax)
	movq	camlTuturu__anon_fn_878@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	%rdi, 48(%rax)
	movq	56(%rbx), %rdi
	movq	%rdi, 56(%rax)
	movq	64(%rbx), %rdi
	movq	%rdi, 64(%rax)
	movq	72(%rbx), %rbx
	movq	%rbx, 72(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%r12, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L309:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L313:
	call	caml_call_gc@PLT
.L314:
	jmp	.L312
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_825,@function
	.size camlTuturu__anon_fn_825,. - camlTuturu__anon_fn_825
	.text
	.align	16
	.globl	camlTuturu__anon_fn_802
camlTuturu__anon_fn_802:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L317:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L318:
	subq	$88, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L319
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$10487, -8(%rax)
	movq	camlTuturu__anon_fn_825@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rbx
	movq	%rbx, 72(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	768(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L315:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L319:
	call	caml_call_gc@PLT
.L320:
	jmp	.L318
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_802,@function
	.size camlTuturu__anon_fn_802,. - camlTuturu__anon_fn_802
	.text
	.align	16
	.globl	camlTuturu__anon_fn_756
camlTuturu__anon_fn_756:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L326:
	movq	%rax, (%rsp)
	movq	%rbx, 8(%rsp)
	movq	40(%rbx), %rax
	call	camlTezos_raw_protocol_alpha__Script_ir_translator__compare_comparable_614@PLT
.L321:
	movq	%rax, %rdi
	movq	8(%rsp), %rax
	movq	64(%rax), %rbx
	movq	56(%rax), %rax
	call	caml_apply2@PLT
.L322:
	movq	%rax, %rdi
	call	ml_z_of_int@PLT
	movq	%rax, %rbx
.L327:
	subq	$88, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L328
	leaq	8(%r15), %rax
	movq	%rax, 16(%rsp)
	movq	$10487, -8(%rax)
	movq	camlTuturu__anon_fn_802@GOTPCREL(%rip), %rdi
	movq	%rdi, (%rax)
	movq	$3, 8(%rax)
	movq	8(%rsp), %rsi
	movq	16(%rsi), %rdi
	movq	%rdi, 16(%rax)
	movq	24(%rsi), %rdi
	movq	%rdi, 24(%rax)
	movq	32(%rsi), %rdi
	movq	%rdi, 32(%rax)
	movq	48(%rsi), %rdi
	movq	%rdi, 40(%rax)
	movq	%rbx, 48(%rax)
	movq	72(%rsi), %rbx
	movq	%rbx, 56(%rax)
	movq	80(%rsi), %rbx
	movq	%rbx, 64(%rax)
	movq	88(%rsi), %rbx
	movq	%rbx, 72(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	(%rsp), %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L324:
	movq	8(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	16(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L328:
	call	caml_call_gc@PLT
.L329:
	jmp	.L327
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_756,@function
	.size camlTuturu__anon_fn_756,. - camlTuturu__anon_fn_756
	.text
	.align	16
	.globl	camlTuturu__anon_fn_725
camlTuturu__anon_fn_725:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L333:
	movq	%rax, (%rsp)
	movq	%rbx, 8(%rsp)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	832(%rax), %rsi
	movq	64(%rbx), %rdi
	movq	56(%rbx), %rdx
	movq	40(%rbx), %rax
	movq	%rdx, %rbx
	call	camlTezos_raw_protocol_alpha__Michelson_v1_gas__compare_3782@PLT
.L330:
	movq	%rax, %rbx
.L334:
	subq	$104, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L335
	leaq	8(%r15), %rax
	movq	%rax, 16(%rsp)
	movq	$12535, -8(%rax)
	movq	camlTuturu__anon_fn_756@GOTPCREL(%rip), %rdi
	movq	%rdi, (%rax)
	movq	$3, 8(%rax)
	movq	8(%rsp), %rsi
	movq	16(%rsi), %rdi
	movq	%rdi, 16(%rax)
	movq	24(%rsi), %rdi
	movq	%rdi, 24(%rax)
	movq	32(%rsi), %rdi
	movq	%rdi, 32(%rax)
	movq	40(%rsi), %rdi
	movq	%rdi, 40(%rax)
	movq	48(%rsi), %rdi
	movq	%rdi, 48(%rax)
	movq	56(%rsi), %rdi
	movq	%rdi, 56(%rax)
	movq	64(%rsi), %rdi
	movq	%rdi, 64(%rax)
	movq	72(%rsi), %rdi
	movq	%rdi, 72(%rax)
	movq	80(%rsi), %rdi
	movq	%rdi, 80(%rax)
	movq	88(%rsi), %rdi
	movq	%rdi, 88(%rax)
	movq	(%rsp), %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L331:
	movq	8(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	16(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L335:
	call	caml_call_gc@PLT
.L336:
	jmp	.L334
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_725,@function
	.size camlTuturu__anon_fn_725,. - camlTuturu__anon_fn_725
	.text
	.align	16
	.globl	camlTuturu__anon_fn_677
camlTuturu__anon_fn_677:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L339:
	movq	%rax, %rcx
	movq	%rbx, (%rsp)
.L340:
	subq	$152, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L341
	leaq	8(%r15), %rax
	addq	$128, %rax
	movq	$2048, -8(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, (%rax)
	movq	88(%rbx), %rdx
	movq	%rdx, 8(%rax)
	leaq	-24(%rax), %r8
	movq	$2048, -8(%r8)
	movq	80(%rbx), %rdi
	movq	%rdi, (%r8)
	movq	%rax, 8(%r8)
	leaq	-104(%r8), %rax
	movq	%rax, 8(%rsp)
	movq	$12535, -8(%rax)
	movq	camlTuturu__anon_fn_725@GOTPCREL(%rip), %r8
	movq	%r8, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %r8
	movq	%r8, 16(%rax)
	movq	24(%rbx), %r8
	movq	%r8, 24(%rax)
	movq	32(%rbx), %r8
	movq	%r8, 32(%rax)
	movq	40(%rbx), %r8
	movq	%r8, 40(%rax)
	movq	48(%rbx), %r8
	movq	%r8, 48(%rax)
	movq	64(%rbx), %r8
	movq	%r8, 56(%rax)
	movq	72(%rbx), %rbx
	movq	%rbx, 64(%rax)
	movq	%rdi, 72(%rax)
	movq	%rsi, 80(%rax)
	movq	%rdx, 88(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rcx, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L337:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L341:
	call	caml_call_gc@PLT
.L342:
	jmp	.L340
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_677,@function
	.size camlTuturu__anon_fn_677,. - camlTuturu__anon_fn_677
	.text
	.align	16
	.globl	camlTuturu__anon_fn_650
camlTuturu__anon_fn_650:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L346:
	movq	%rax, %r12
	movq	%rbx, (%rsp)
	movq	$225, %rdi
	call	ml_z_of_int@PLT
	movq	%rax, %rdi
.L347:
	subq	$104, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L348
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$12535, -8(%rax)
	movq	camlTuturu__anon_fn_677@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rsi
	movq	%rsi, 72(%rax)
	movq	80(%rbx), %rsi
	movq	%rsi, 80(%rax)
	movq	88(%rbx), %rbx
	movq	%rbx, 88(%rax)
	movq	%r12, %rax
	movq	%rdi, %rbx
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L344:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L348:
	call	caml_call_gc@PLT
.L349:
	jmp	.L347
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_650,@function
	.size camlTuturu__anon_fn_650,. - camlTuturu__anon_fn_650
	.text
	.align	16
	.globl	camlTuturu__anon_fn_618
camlTuturu__anon_fn_618:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L352:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L353:
	subq	$104, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L354
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$12535, -8(%rax)
	movq	camlTuturu__anon_fn_650@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	%rsi, 64(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 72(%rax)
	movq	72(%rbx), %rsi
	movq	%rsi, 80(%rax)
	movq	80(%rbx), %rbx
	movq	%rbx, 88(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L350:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L354:
	call	caml_call_gc@PLT
.L355:
	jmp	.L353
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_618,@function
	.size camlTuturu__anon_fn_618,. - camlTuturu__anon_fn_618
	.text
	.align	16
	.globl	camlTuturu__anon_fn_594
camlTuturu__anon_fn_594:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L358:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L359:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L360
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$11511, -8(%rax)
	movq	camlTuturu__anon_fn_618@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rsi
	movq	%rsi, 72(%rax)
	movq	80(%rbx), %rbx
	movq	%rbx, 80(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	8(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L356:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L360:
	call	caml_call_gc@PLT
.L361:
	jmp	.L359
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_594,@function
	.size camlTuturu__anon_fn_594,. - camlTuturu__anon_fn_594
	.text
	.align	16
	.globl	camlTuturu__anon_fn_552
camlTuturu__anon_fn_552:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L364:
	movq	%rax, %rcx
	movq	%rbx, (%rsp)
.L365:
	subq	$144, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L366
	leaq	8(%r15), %rax
	addq	$120, %rax
	movq	$2048, -8(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, (%rax)
	movq	80(%rbx), %rdx
	movq	%rdx, 8(%rax)
	leaq	-24(%rax), %r8
	movq	$2048, -8(%r8)
	movq	56(%rbx), %rdi
	movq	%rdi, (%r8)
	movq	%rax, 8(%r8)
	leaq	-96(%r8), %rax
	movq	%rax, 8(%rsp)
	movq	$11511, -8(%rax)
	movq	camlTuturu__anon_fn_594@GOTPCREL(%rip), %r8
	movq	%r8, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %r8
	movq	%r8, 16(%rax)
	movq	24(%rbx), %r8
	movq	%r8, 24(%rax)
	movq	32(%rbx), %r8
	movq	%r8, 32(%rax)
	movq	40(%rbx), %r8
	movq	%r8, 40(%rax)
	movq	48(%rbx), %r8
	movq	%r8, 48(%rax)
	movq	72(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	%rdi, 64(%rax)
	movq	%rsi, 72(%rax)
	movq	%rdx, 80(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rcx, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L362:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L366:
	call	caml_call_gc@PLT
.L367:
	jmp	.L365
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_552,@function
	.size camlTuturu__anon_fn_552,. - camlTuturu__anon_fn_552
	.text
	.align	16
	.globl	camlTuturu__anon_fn_525
camlTuturu__anon_fn_525:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L371:
	movq	%rax, %r12
	movq	%rbx, (%rsp)
	movq	$217, %rdi
	call	ml_z_of_int@PLT
	movq	%rax, %rdi
.L372:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L373
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$11511, -8(%rax)
	movq	camlTuturu__anon_fn_552@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rsi
	movq	%rsi, 72(%rax)
	movq	80(%rbx), %rbx
	movq	%rbx, 80(%rax)
	movq	%r12, %rax
	movq	%rdi, %rbx
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L369:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L373:
	call	caml_call_gc@PLT
.L374:
	jmp	.L372
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_525,@function
	.size camlTuturu__anon_fn_525,. - camlTuturu__anon_fn_525
	.text
	.align	16
	.globl	camlTuturu__anon_fn_498
camlTuturu__anon_fn_498:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L377:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L378:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L379
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$11511, -8(%rax)
	movq	camlTuturu__anon_fn_525@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rsi
	movq	%rsi, 72(%rax)
	movq	80(%rbx), %rbx
	movq	%rbx, 80(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L375:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L379:
	call	caml_call_gc@PLT
.L380:
	jmp	.L378
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_498,@function
	.size camlTuturu__anon_fn_498,. - camlTuturu__anon_fn_498
	.text
	.align	16
	.globl	camlTuturu__anon_fn_473
camlTuturu__anon_fn_473:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L383:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L384:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L385
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$11511, -8(%rax)
	movq	camlTuturu__anon_fn_498@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rsi
	movq	%rsi, 72(%rax)
	movq	80(%rbx), %rbx
	movq	%rbx, 80(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	24(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L381:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L385:
	call	caml_call_gc@PLT
.L386:
	jmp	.L384
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_473,@function
	.size camlTuturu__anon_fn_473,. - camlTuturu__anon_fn_473
	.text
	.align	16
	.globl	camlTuturu__anon_fn_440
camlTuturu__anon_fn_440:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L389:
	movq	%rax, %rdx
	movq	%rbx, (%rsp)
	movq	64(%rbx), %rax
	movq	8(%rax), %rdi
.L390:
	subq	$120, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L391
	leaq	8(%r15), %rcx
	addq	$96, %rcx
	movq	$2048, -8(%rcx)
	movq	%rdi, (%rcx)
	movq	72(%rbx), %rax
	movq	%rax, 8(%rcx)
	movq	8(%rax), %rsi
	movq	(%rax), %rax
	addq	$-96, %rcx
	movq	%rcx, 8(%rsp)
	movq	$11511, -8(%rcx)
	movq	camlTuturu__anon_fn_473@GOTPCREL(%rip), %r8
	movq	%r8, (%rcx)
	movq	$3, 8(%rcx)
	movq	16(%rbx), %r8
	movq	%r8, 16(%rcx)
	movq	24(%rbx), %r8
	movq	%r8, 24(%rcx)
	movq	32(%rbx), %r8
	movq	%r8, 32(%rcx)
	movq	40(%rbx), %r8
	movq	%r8, 40(%rcx)
	movq	48(%rbx), %r8
	movq	%r8, 48(%rcx)
	movq	56(%rbx), %rbx
	movq	%rbx, 56(%rcx)
	movq	%rdi, 64(%rcx)
	movq	%rax, 72(%rcx)
	movq	%rsi, 80(%rcx)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdx, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L387:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L391:
	call	caml_call_gc@PLT
.L392:
	jmp	.L390
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_440,@function
	.size camlTuturu__anon_fn_440,. - camlTuturu__anon_fn_440
	.text
	.align	16
	.globl	camlTuturu__anon_fn_415
camlTuturu__anon_fn_415:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L395:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L396:
	subq	$88, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L397
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$10487, -8(%rax)
	movq	camlTuturu__anon_fn_440@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rbx
	movq	%rbx, 72(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	80(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L393:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L397:
	call	caml_call_gc@PLT
.L398:
	jmp	.L396
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_415,@function
	.size camlTuturu__anon_fn_415,. - camlTuturu__anon_fn_415
	.text
	.align	16
	.globl	camlTuturu__anon_fn_378
camlTuturu__anon_fn_378:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L401:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L402:
	subq	$112, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L403
	leaq	8(%r15), %rax
	addq	$88, %rax
	movq	$2048, -8(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, (%rax)
	movq	80(%rbx), %rsi
	movq	%rsi, 8(%rax)
	leaq	-88(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	$10487, -8(%rsi)
	movq	camlTuturu__anon_fn_415@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rsi)
	movq	$3, 8(%rsi)
	movq	16(%rbx), %rdx
	movq	%rdx, 16(%rsi)
	movq	24(%rbx), %rdx
	movq	%rdx, 24(%rsi)
	movq	32(%rbx), %rdx
	movq	%rdx, 32(%rsi)
	movq	40(%rbx), %rdx
	movq	%rdx, 40(%rsi)
	movq	48(%rbx), %rdx
	movq	%rdx, 48(%rsi)
	movq	56(%rbx), %rdx
	movq	%rdx, 56(%rsi)
	movq	72(%rbx), %rbx
	movq	%rbx, 64(%rsi)
	movq	%rax, 72(%rsi)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L399:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L403:
	call	caml_call_gc@PLT
.L404:
	jmp	.L402
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_378,@function
	.size camlTuturu__anon_fn_378,. - camlTuturu__anon_fn_378
	.text
	.align	16
	.globl	camlTuturu__anon_fn_2057
camlTuturu__anon_fn_2057:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L406:
	movq	%rax, %rdi
	movq	16(%rbx), %rsi
.L407:
	subq	$24, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L408
	leaq	8(%r15), %rax
	movq	$2048, -8(%rax)
	movq	24(%rbx), %rbx
	movq	%rbx, (%rax)
	movq	%rdi, 8(%rax)
	movq	(%rsi), %rdi
	movq	%rsi, %rbx
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	jmp	*%rdi
	.cfi_adjust_cfa_offset 8
.L408:
	call	caml_call_gc@PLT
.L409:
	jmp	.L407
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.type camlTuturu__anon_fn_2057,@function
	.size camlTuturu__anon_fn_2057,. - camlTuturu__anon_fn_2057
	.text
	.align	16
	.globl	camlTuturu__anon_fn_353
camlTuturu__anon_fn_353:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L412:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L413:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L414
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$11511, -8(%rax)
	movq	camlTuturu__anon_fn_378@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rsi
	movq	%rsi, 72(%rax)
	movq	80(%rbx), %rbx
	movq	%rbx, 80(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	16(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L410:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L414:
	call	caml_call_gc@PLT
.L415:
	jmp	.L413
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_353,@function
	.size camlTuturu__anon_fn_353,. - camlTuturu__anon_fn_353
	.text
	.align	16
	.globl	camlTuturu__anon_fn_160
camlTuturu__anon_fn_160:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_adjust_cfa_offset 8
.L417:
	movq	%rax, %rdi
	movq	16(%rbx), %rsi
.L418:
	subq	$64, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L419
	leaq	8(%r15), %rax
	addq	$48, %rax
	movq	$1024, -8(%rax)
	movq	24(%rbx), %rdx
	movq	%rdx, (%rax)
	leaq	-24(%rax), %rdx
	movq	$2048, -8(%rdx)
	movq	%rax, (%rdx)
	movq	32(%rbx), %rax
	movq	%rax, 8(%rdx)
	leaq	-24(%rdx), %rax
	movq	$2048, -8(%rax)
	movq	%rdx, (%rax)
	movq	%rdi, 8(%rax)
	movq	(%rsi), %rdi
	movq	%rsi, %rbx
	addq	$8, %rsp
	.cfi_adjust_cfa_offset -8
	jmp	*%rdi
	.cfi_adjust_cfa_offset 8
.L419:
	call	caml_call_gc@PLT
.L420:
	jmp	.L418
	.cfi_adjust_cfa_offset -8
	.cfi_endproc
	.type camlTuturu__anon_fn_160,@function
	.size camlTuturu__anon_fn_160,. - camlTuturu__anon_fn_160
	.text
	.align	16
	.globl	camlTuturu__anon_fn_2024
camlTuturu__anon_fn_2024:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L423:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
	movq	32(%rbx), %rax
	movq	8(%rax), %rsi
.L424:
	subq	$88, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L425
	leaq	8(%r15), %rax
	addq	$64, %rax
	movq	$2048, -8(%rax)
	movq	camlTuturu__const_block_1985@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rax)
	movq	32(%rbx), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, 8(%rax)
	leaq	-24(%rax), %rdx
	movq	$2048, -8(%rdx)
	movq	%rax, (%rdx)
	movq	%rsi, 8(%rdx)
	leaq	-40(%rdx), %rax
	movq	%rax, 8(%rsp)
	movq	$4343, -8(%rax)
	movq	camlTuturu__anon_fn_2057@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	24(%rbx), %rbx
	movq	%rbx, 16(%rax)
	movq	%rdx, 24(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	776(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L421:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L425:
	call	caml_call_gc@PLT
.L426:
	jmp	.L424
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_2024,@function
	.size camlTuturu__anon_fn_2024,. - camlTuturu__anon_fn_2024
	.text
	.align	16
	.globl	camlTuturu__anon_fn_320
camlTuturu__anon_fn_320:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L429:
	movq	%rax, %rsi
	movq	%rbx, (%rsp)
	movq	64(%rbx), %rax
	movq	(%rax), %rdi
.L430:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L431
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$11511, -8(%rax)
	movq	camlTuturu__anon_fn_353@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rdx
	movq	%rdx, 16(%rax)
	movq	24(%rbx), %rdx
	movq	%rdx, 24(%rax)
	movq	32(%rbx), %rdx
	movq	%rdx, 32(%rax)
	movq	40(%rbx), %rdx
	movq	%rdx, 40(%rax)
	movq	48(%rbx), %rdx
	movq	%rdx, 48(%rax)
	movq	56(%rbx), %rdx
	movq	%rdx, 56(%rax)
	movq	%rdi, 64(%rax)
	movq	72(%rbx), %rdi
	movq	%rdi, 72(%rax)
	movq	80(%rbx), %rbx
	movq	%rbx, 80(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rsi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L427:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L431:
	call	caml_call_gc@PLT
.L432:
	jmp	.L430
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_320,@function
	.size camlTuturu__anon_fn_320,. - camlTuturu__anon_fn_320
	.text
	.align	16
	.globl	camlTuturu__anon_fn_127
camlTuturu__anon_fn_127:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L435:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L436:
	subq	$72, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L437
	leaq	8(%r15), %rax
	addq	$48, %rax
	movq	$2048, -8(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, (%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 8(%rax)
	leaq	-48(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	$5367, -8(%rsi)
	movq	camlTuturu__anon_fn_160@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rsi)
	movq	$3, 8(%rsi)
	movq	24(%rbx), %rdx
	movq	%rdx, 16(%rsi)
	movq	%rax, 24(%rsi)
	movq	48(%rbx), %rax
	movq	%rax, 32(%rsi)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	88(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L433:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L437:
	call	caml_call_gc@PLT
.L438:
	jmp	.L436
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_127,@function
	.size camlTuturu__anon_fn_127,. - camlTuturu__anon_fn_127
	.text
	.align	16
	.globl	camlTuturu__anon_fn_2003
camlTuturu__anon_fn_2003:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L441:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L442:
	subq	$48, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L443
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$5367, -8(%rax)
	movq	camlTuturu__anon_fn_2024@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rbx
	movq	%rbx, 32(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	56(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L439:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L443:
	call	caml_call_gc@PLT
.L444:
	jmp	.L442
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_2003,@function
	.size camlTuturu__anon_fn_2003,. - camlTuturu__anon_fn_2003
	.text
	.align	16
	.globl	camlTuturu__anon_fn_295
camlTuturu__anon_fn_295:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L447:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L448:
	subq	$96, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L449
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$11511, -8(%rax)
	movq	camlTuturu__anon_fn_320@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rsi
	movq	%rsi, 64(%rax)
	movq	72(%rbx), %rsi
	movq	%rsi, 72(%rax)
	movq	80(%rbx), %rbx
	movq	%rbx, 80(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	72(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L445:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L449:
	call	caml_call_gc@PLT
.L450:
	jmp	.L448
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_295,@function
	.size camlTuturu__anon_fn_295,. - camlTuturu__anon_fn_295
	.text
	.align	16
	.globl	camlTuturu__anon_fn_98
camlTuturu__anon_fn_98:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L453:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
	movq	32(%rbx), %rax
	movq	(%rax), %rsi
.L454:
	subq	$64, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L455
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$7415, -8(%rax)
	movq	camlTuturu__anon_fn_127@GOTPCREL(%rip), %rdx
	movq	%rdx, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rdx
	movq	%rdx, 16(%rax)
	movq	24(%rbx), %rdx
	movq	%rdx, 24(%rax)
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rbx
	movq	%rbx, 48(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	56(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L451:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L455:
	call	caml_call_gc@PLT
.L456:
	jmp	.L454
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_98,@function
	.size camlTuturu__anon_fn_98,. - camlTuturu__anon_fn_98
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1977
camlTuturu__anon_fn_1977:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L459:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L460:
	subq	$48, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L461
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$5367, -8(%rax)
	movq	camlTuturu__anon_fn_2003@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rbx
	movq	%rbx, 32(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L457:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L461:
	call	caml_call_gc@PLT
.L462:
	jmp	.L460
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1977,@function
	.size camlTuturu__anon_fn_1977,. - camlTuturu__anon_fn_1977
	.text
	.align	16
	.globl	camlTuturu__anon_fn_262
camlTuturu__anon_fn_262:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L465:
	movq	%rax, %rdx
	movq	%rbx, (%rsp)
	movq	40(%rbx), %rax
	movq	(%rax), %rdi
	movq	8(%rax), %rsi
.L466:
	subq	$120, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L467
	leaq	8(%r15), %rax
	addq	$96, %rax
	movq	$2048, -8(%rax)
	movq	%rdi, (%rax)
	movq	%rsi, 8(%rax)
	addq	$-96, %rax
	movq	%rax, 8(%rsp)
	movq	$11511, -8(%rax)
	movq	camlTuturu__anon_fn_295@GOTPCREL(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	24(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	32(%rbx), %rcx
	movq	%rcx, 32(%rax)
	movq	48(%rbx), %rcx
	movq	%rcx, 40(%rax)
	movq	56(%rbx), %rcx
	movq	%rcx, 48(%rax)
	movq	64(%rbx), %rbx
	movq	%rbx, 56(%rax)
	movq	%rdi, 64(%rax)
	movq	%rdi, 72(%rax)
	movq	%rsi, 80(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	%rdx, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L463:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L467:
	call	caml_call_gc@PLT
.L468:
	jmp	.L466
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_262,@function
	.size camlTuturu__anon_fn_262,. - camlTuturu__anon_fn_262
	.text
	.align	16
	.globl	camlTuturu__anon_fn_65
camlTuturu__anon_fn_65:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L471:
	movq	%rax, %rsi
	movq	%rbx, (%rsp)
	movq	40(%rbx), %rax
	movq	8(%rax), %rdi
.L472:
	subq	$88, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L473
	leaq	8(%r15), %rax
	addq	$64, %rax
	movq	$2048, -8(%rax)
	movq	32(%rbx), %rdx
	movq	%rdx, (%rax)
	movq	%rdi, 8(%rax)
	movq	40(%rbx), %rcx
	movq	(%rcx), %rcx
	addq	$-64, %rax
	movq	%rax, 8(%rsp)
	movq	$7415, -8(%rax)
	movq	camlTuturu__anon_fn_98@GOTPCREL(%rip), %r8
	movq	%r8, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %r8
	movq	%r8, 16(%rax)
	movq	24(%rbx), %rbx
	movq	%rbx, 24(%rax)
	movq	%rcx, 32(%rax)
	movq	%rdx, 40(%rax)
	movq	%rdi, 48(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	72(%rax), %rbx
	movq	%rsi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L469:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L473:
	call	caml_call_gc@PLT
.L474:
	jmp	.L472
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_65,@function
	.size camlTuturu__anon_fn_65,. - camlTuturu__anon_fn_65
	.text
	.align	16
	.globl	camlTuturu__anon_fn_1956
camlTuturu__anon_fn_1956:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L477:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L478:
	subq	$48, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L479
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$5367, -8(%rax)
	movq	camlTuturu__anon_fn_1977@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rbx
	movq	%rbx, 32(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	120(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L475:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L479:
	call	caml_call_gc@PLT
.L480:
	jmp	.L478
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_1956,@function
	.size camlTuturu__anon_fn_1956,. - camlTuturu__anon_fn_1956
	.text
	.align	16
	.globl	camlTuturu__anon_fn_237
camlTuturu__anon_fn_237:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L483:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L484:
	subq	$80, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L485
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$9463, -8(%rax)
	movq	camlTuturu__anon_fn_262@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	32(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 40(%rax)
	movq	48(%rbx), %rsi
	movq	%rsi, 48(%rax)
	movq	56(%rbx), %rsi
	movq	%rsi, 56(%rax)
	movq	64(%rbx), %rbx
	movq	%rbx, 64(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	8(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L481:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L485:
	call	caml_call_gc@PLT
.L486:
	jmp	.L484
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_237,@function
	.size camlTuturu__anon_fn_237,. - camlTuturu__anon_fn_237
	.text
	.align	16
	.globl	camlTuturu__anon_fn_41
camlTuturu__anon_fn_41:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L489:
	movq	%rax, %rdi
	movq	%rbx, (%rsp)
.L490:
	subq	$56, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L491
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)
	movq	$6391, -8(%rax)
	movq	camlTuturu__anon_fn_65@GOTPCREL(%rip), %rsi
	movq	%rsi, (%rax)
	movq	$3, 8(%rax)
	movq	16(%rbx), %rsi
	movq	%rsi, 16(%rax)
	movq	24(%rbx), %rsi
	movq	%rsi, 24(%rax)
	movq	40(%rbx), %rsi
	movq	%rsi, 32(%rax)
	movq	32(%rbx), %rbx
	movq	%rbx, 40(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	16(%rax), %rbx
	movq	%rdi, %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L487:
	movq	(%rsp), %rbx
	movq	16(%rbx), %rdi
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L491:
	call	caml_call_gc@PLT
.L492:
	jmp	.L490
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__anon_fn_41,@function
	.size camlTuturu__anon_fn_41,. - camlTuturu__anon_fn_41
	.text
	.align	16
	.globl	camlTuturu__tz_compiled_2_1928
camlTuturu__tz_compiled_2_1928:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L495:
	movq	(%rax), %rdi
	movq	32(%rdi), %rbx
	movq	16(%rdi), %rsi
	movq	(%rdi), %rdx
	movq	%rdx, 8(%rsp)
.L496:
	subq	$48, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L497
	leaq	8(%r15), %rax
	movq	%rax, (%rsp)
	movq	$5367, -8(%rax)
	movq	camlTuturu__anon_fn_1956@GOTPCREL(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$3, 8(%rax)
	movq	%rdx, 16(%rax)
	movq	%rsi, 24(%rax)
	movq	%rbx, 32(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	24(%rdi), %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L493:
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdi
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L497:
	call	caml_call_gc@PLT
.L498:
	jmp	.L496
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__tz_compiled_2_1928,@function
	.size camlTuturu__tz_compiled_2_1928,. - camlTuturu__tz_compiled_2_1928
	.text
	.align	16
	.globl	camlTuturu__tz_compiled_1_199
camlTuturu__tz_compiled_1_199:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L501:
	movq	(%rax), %rdi
	movq	40(%rdi), %rax
	movq	16(%rax), %rbx
	movq	8(%rax), %rsi
	movq	(%rax), %rdx
	movq	32(%rdi), %rcx
	movq	16(%rdi), %r8
	movq	8(%rdi), %r9
	movq	(%rdi), %r12
	movq	%r12, 8(%rsp)
.L502:
	subq	$80, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L503
	leaq	8(%r15), %rax
	movq	%rax, (%rsp)
	movq	$9463, -8(%rax)
	movq	camlTuturu__anon_fn_237@GOTPCREL(%rip), %r13
	movq	%r13, (%rax)
	movq	$3, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%r9, 24(%rax)
	movq	%r8, 32(%rax)
	movq	%rcx, 40(%rax)
	movq	%rdx, 48(%rax)
	movq	%rsi, 56(%rax)
	movq	%rbx, 64(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	544(%rax), %rbx
	movq	24(%rdi), %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L499:
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdi
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L503:
	call	caml_call_gc@PLT
.L504:
	jmp	.L502
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__tz_compiled_1_199,@function
	.size camlTuturu__tz_compiled_1_199,. - camlTuturu__tz_compiled_1_199
	.text
	.align	16
	.globl	camlTuturu__tz_compiled_0_11
camlTuturu__tz_compiled_0_11:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_adjust_cfa_offset 24
.L507:
	movq	(%rax), %rdi
	movq	40(%rdi), %rbx
	movq	32(%rdi), %rsi
	movq	16(%rdi), %rdx
	movq	(%rdi), %rcx
	movq	%rcx, 8(%rsp)
.L508:
	subq	$56, %r15
	movq	caml_young_limit@GOTPCREL(%rip), %rax
	cmpq	(%rax), %r15
	jb	.L509
	leaq	8(%r15), %rax
	movq	%rax, (%rsp)
	movq	$6391, -8(%rax)
	movq	camlTuturu__anon_fn_41@GOTPCREL(%rip), %r8
	movq	%r8, (%rax)
	movq	$3, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rdx, 24(%rax)
	movq	%rsi, 32(%rax)
	movq	%rbx, 40(%rax)
	movq	camlTezos_raw_protocol_alpha__Michelson_v1_gas__Pmakeblock_6309@GOTPCREL(%rip), %rax
	movq	24(%rax), %rbx
	movq	24(%rdi), %rax
	call	camlTezos_raw_protocol_alpha__Raw_context__consume_gas_1492@PLT
.L505:
	movq	(%rsp), %rbx
	movq	8(%rsp), %rdi
	addq	$24, %rsp
	.cfi_adjust_cfa_offset -24
	jmp	caml_apply2@PLT
	.cfi_adjust_cfa_offset 24
.L509:
	call	caml_call_gc@PLT
.L510:
	jmp	.L508
	.cfi_adjust_cfa_offset -24
	.cfi_endproc
	.type camlTuturu__tz_compiled_0_11,@function
	.size camlTuturu__tz_compiled_0_11,. - camlTuturu__tz_compiled_0_11
	.data
	.align	8
	.quad	3840
	.globl	camlTuturu
camlTuturu:
	.quad	camlTuturu__tz_compiled_0_11_closure
	.quad	camlTuturu__tz_compiled_1_199_closure
	.quad	camlTuturu__tz_compiled_2_1928_closure
	.data
	.align	8
	.data
	.align	8
	.data
	.align	8
	.data
	.align	8
	.quad	2816
	.globl	camlTuturu__const_block_1985
camlTuturu__const_block_1985:
	.quad	1
	.quad	1
	.text
	.align	16
	.globl	camlTuturu__entry
camlTuturu__entry:
	.cfi_startproc
.L511:
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
	.quad	137
	.quad	.L505
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L510
	.word	32
	.word	6
	.word	3
	.word	5
	.word	7
	.word	8
	.word	9
	.word	11
	.align	8
	.quad	.L499
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L504
	.word	32
	.word	9
	.word	3
	.word	5
	.word	7
	.word	8
	.word	9
	.word	11
	.word	13
	.word	15
	.word	17
	.align	8
	.quad	.L493
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L498
	.word	32
	.word	5
	.word	3
	.word	5
	.word	7
	.word	8
	.word	9
	.align	8
	.quad	.L487
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L492
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L481
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L486
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L475
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L480
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L469
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L474
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L463
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L468
	.word	32
	.word	5
	.word	0
	.word	3
	.word	5
	.word	7
	.word	9
	.align	8
	.quad	.L457
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L462
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L451
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L456
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L445
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L450
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L439
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L444
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L433
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L438
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L427
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L432
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L421
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L426
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L420
	.word	16
	.word	3
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L410
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L415
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L409
	.word	16
	.word	3
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L399
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L404
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L393
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L398
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L387
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L392
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	9
	.align	8
	.quad	.L381
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L386
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L375
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L380
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L369
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L374
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	17
	.align	8
	.quad	.L362
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L367
	.word	32
	.word	3
	.word	0
	.word	3
	.word	11
	.align	8
	.quad	.L356
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L361
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L350
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L355
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L344
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L349
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	17
	.align	8
	.quad	.L337
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L342
	.word	32
	.word	3
	.word	0
	.word	3
	.word	11
	.align	8
	.quad	.L331
	.word	33
	.word	2
	.word	8
	.word	16
	.align	8
	.quad	.L512
	.quad	.L336
	.word	32
	.word	3
	.word	0
	.word	3
	.word	8
	.align	8
	.quad	.L330
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L324
	.word	33
	.word	2
	.word	8
	.word	16
	.align	8
	.quad	.L512
	.quad	.L329
	.word	32
	.word	3
	.word	0
	.word	3
	.word	8
	.align	8
	.quad	.L322
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L321
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L315
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L320
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L309
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L314
	.word	32
	.word	3
	.word	0
	.word	3
	.word	17
	.align	8
	.quad	.L302
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L307
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L288
	.word	33
	.word	2
	.word	0
	.word	16
	.align	8
	.quad	.L512
	.quad	.L287
	.word	33
	.word	3
	.word	0
	.word	8
	.word	16
	.align	8
	.quad	.L512
	.quad	.L301
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	16
	.align	8
	.quad	.L286
	.word	33
	.word	2
	.word	0
	.word	16
	.align	8
	.quad	.L512
	.quad	.L285
	.word	33
	.word	3
	.word	0
	.word	8
	.word	16
	.align	8
	.quad	.L512
	.quad	.L298
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	16
	.align	8
	.quad	.L295
	.word	32
	.word	3
	.word	3
	.word	5
	.word	16
	.align	8
	.quad	.L279
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L284
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L273
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L278
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L267
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L272
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L261
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L266
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L255
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L260
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L254
	.word	16
	.word	3
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L244
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L249
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L238
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L243
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L232
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L237
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L226
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L231
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L220
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L225
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L214
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L219
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	17
	.align	8
	.quad	.L207
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L212
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L201
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L206
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L200
	.word	16
	.word	3
	.word	3
	.word	5
	.word	7
	.align	8
	.quad	.L190
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L195
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L184
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L189
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L178
	.word	33
	.word	2
	.word	8
	.word	16
	.align	8
	.quad	.L512
	.quad	.L183
	.word	32
	.word	3
	.word	0
	.word	3
	.word	8
	.align	8
	.quad	.L177
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L171
	.word	33
	.word	2
	.word	8
	.word	16
	.align	8
	.quad	.L512
	.quad	.L176
	.word	32
	.word	3
	.word	0
	.word	3
	.word	8
	.align	8
	.quad	.L170
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L164
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L169
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L158
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L163
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L152
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L157
	.word	32
	.word	4
	.word	0
	.word	3
	.word	5
	.word	17
	.align	8
	.quad	.L145
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L150
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L139
	.word	33
	.word	2
	.word	8
	.word	16
	.align	8
	.quad	.L512
	.quad	.L144
	.word	32
	.word	3
	.word	0
	.word	3
	.word	8
	.align	8
	.quad	.L138
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L132
	.word	33
	.word	2
	.word	8
	.word	16
	.align	8
	.quad	.L512
	.quad	.L137
	.word	32
	.word	3
	.word	0
	.word	3
	.word	8
	.align	8
	.quad	.L131
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L125
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L130
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L119
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L124
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L113
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L118
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L107
	.word	33
	.word	2
	.word	0
	.word	8
	.align	8
	.quad	.L512
	.quad	.L112
	.word	32
	.word	3
	.word	0
	.word	3
	.word	5
	.align	8
	.quad	.L106
	.word	16
	.word	3
	.word	3
	.word	5
	.word	7
	.align	8
	.align	8
.L512:
	.long	(.L513 - .) + -67108864
	.long	-17
	.quad	0
.L513:
	.ascii	"_none_\0"
	.align	8
	.section .note.GNU-stack,"",%progbits
