	.file "inline_call.S"
	.text
	.globl inline_x87
	.type inline_x87, @function
inline_x87:
	mov	%ebx, 0+inline_call_pt_regs
	mov	$inline_call_pt_regs, %ebx
	mov	%eax, 0x18(%ebx)
	mov	%eax, 0x2c(%ebx)
	mov	%ecx, 0x4(%ebx)
	mov	%edx, 0x8(%ebx)
	mov	%esi, 0xc(%ebx)
	mov	%edi, 0x10(%ebx)
	mov	%ebp, 0x14(%ebx)
	pushf
	mov	4(%esp), %esi
	mov	%esi, 0x30(%ebx) /* return address -> saved EIP */
	lea	8(%esp), %eax
	mov	%eax, 0x3c(%ebx) /* ESP before call and pushf -> saved ESP */
	mov	(%esp), %eax
	mov	%eax, 0x38(%ebx) /* saved EFLAGS */
	call	inline_call_math_emulate
	popf
	mov	0x18(%ebx), %eax
	mov	0x14(%ebx), %ebp
	mov	0x10(%ebx), %edi
	mov	0xc(%ebx), %esi
	mov	0x8(%ebx), %edx
	mov	0x4(%ebx), %ecx
	mov	0x3c(%ebx), %esp
	mov	(%ebx), %ebx
	jmp	*(0x30+inline_call_pt_regs)