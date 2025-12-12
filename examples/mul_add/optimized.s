  .globl _main
  .p2align 2
_main:
  .cfi_startproc
  sub sp, sp, #32
  stp x29, x30, [sp, #16]
  add x29, sp, #16
  .cfi_def_cfa x29, 16
  .cfi_offset x30, -8
  .cfi_offset x29, -16
  str x19, [x29, #-8]
Lcfg_node_6:
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=x, type=int, isArgument=false, pos=examples/mul_add/code:13:3), version=0)
  ;   fn=Function(name=getc, args=[] ret=Optional(int)),
  ;   args=[
  ;   ]
  ; )
  bl _getchar
  sxtw x0, w0
  mov x19, x0
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=1)
  ;   fn=Function(name=mul, args=[int, int] ret=Optional(int)),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=x, type=int, isArgument=false, pos=examples/mul_add/code:13:3), version=0)),
  ;     .constInt(2)
  ;   ]
  ; )
  mov x1, #2
  mul x19, x19, x1
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=3)
  ;   fn=Function(name=add, args=[int, int] ret=Optional(int)),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=1)),
  ;     .constInt(3)
  ;   ]
  ; )
  mov x1, #3
  add x19, x19, x1
  ; instruction: .returnValue(.variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=3)))
  mov x0, x19
_main_epilogue:
  ldr x19, [x29, #-8]
  ldp x29, x30, [sp, #16]
  add sp, sp, #32
  ret
  .cfi_endproc
  .globl _str_alloc
  .p2align 2
_str_alloc:
  .cfi_startproc
  sub sp, sp, #32
  stp x29, x30, [sp, #16]
  add x29, sp, #16
  .cfi_def_cfa x29, 16
  .cfi_offset x30, -8
  .cfi_offset x29, -16
  stp x19, x20, [x29, #-16]
  mov x19, x0
  add x0, x19, #8
  bl _malloc
  str x19, [x0]
  ldp x19, x20, [x29, #-16]
  ldp x29, x30, [sp, #16]
  add sp, sp, #32
  ret
  .cfi_endproc