  .globl _mul_add_Tintintint
  .p2align 2
_mul_add_Tintintint:
  .cfi_startproc
  sub sp, sp, #48
  stp x29, x30, [sp, #32]
  add x29, sp, #32
  .cfi_def_cfa x29, 16
  .cfi_offset x30, -8
  .cfi_offset x29, -16
  str x19, [x29, #-8]
  str x20, [x29, #-16]
  str x21, [x29, #-24]
  str x22, [x29, #-32]
Lcfg_node_0:
  ; instruction: .funcArg(
  ;   target=SSAVariable(Variable(name=x, type=int, isArgument=true, pos=examples/mul_add/code:1:12), version=0),
  ;   idx=0
  ; )
  mov x19, x0
  ; instruction: .funcArg(
  ;   target=SSAVariable(Variable(name=y, type=int, isArgument=true, pos=examples/mul_add/code:1:19), version=0),
  ;   idx=1
  ; )
  mov x20, x1
  ; instruction: .funcArg(
  ;   target=SSAVariable(Variable(name=z, type=int, isArgument=true, pos=examples/mul_add/code:1:27), version=0),
  ;   idx=2
  ; )
  mov x21, x2
  ; instruction: .copy(
  ;   target=SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=0),
  ;   source=.variable(SSAVariable(Variable(name=x, type=int, isArgument=true, pos=examples/mul_add/code:1:12), version=0))
  ; )
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=tmp[0], type=int, isArgument=false, pos=examples/mul_add/code:3:12), version=0)
  ;   fn=Function(name=eq, args=[int, int] ret=Optional(int)),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=y, type=int, isArgument=true, pos=examples/mul_add/code:1:19), version=0)),
  ;     .constInt(1)
  ;   ]
  ; )
  mov x1, #1
  cmp x20, x1
  cset x22, eq
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=tmp[1], type=int, isArgument=false, pos=examples/mul_add/code:3:8), version=0)
  ;   fn=Function(name=not, args=[int] ret=Optional(int)),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=tmp[0], type=int, isArgument=false, pos=examples/mul_add/code:3:12), version=0))
  ;   ]
  ; )
  cmp x22, #0
  cset x22, eq
  ; instruction: .call(
  ;   fn=Function(name=checkInt, args=[int] ret=nil),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=tmp[1], type=int, isArgument=false, pos=examples/mul_add/code:3:8), version=0))
  ;   ]
  ; )
  cmp x22, #0
  b.eq Lcfg_node_2
Lcfg_node_3:
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=tmp[2], type=int, isArgument=false, pos=examples/mul_add/code:4:14), version=0)
  ;   fn=Function(name=mul, args=[int, int] ret=Optional(int)),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=0)),
  ;     .variable(SSAVariable(Variable(name=y, type=int, isArgument=true, pos=examples/mul_add/code:1:19), version=0))
  ;   ]
  ; )
  mul x19, x19, x20
  ; instruction: .copy(
  ;   target=SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=1),
  ;   source=.variable(SSAVariable(Variable(name=tmp[2], type=int, isArgument=false, pos=examples/mul_add/code:4:14), version=0))
  ; )
Lcfg_node_2:
  ; instruction: .phi(
  ;   SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=2),
  ;   branches=[
  ;     (key: Node(0), value: .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=0))),
  ;     (key: Node(3), value: .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=1)))
  ;   ]
  ; )
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=tmp[3], type=int, isArgument=false, pos=examples/mul_add/code:6:12), version=0)
  ;   fn=Function(name=eq, args=[int, int] ret=Optional(int)),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=z, type=int, isArgument=true, pos=examples/mul_add/code:1:27), version=0)),
  ;     .constInt(0)
  ;   ]
  ; )
  mov x1, #0
  cmp x21, x1
  cset x20, eq
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=tmp[4], type=int, isArgument=false, pos=examples/mul_add/code:6:8), version=0)
  ;   fn=Function(name=not, args=[int] ret=Optional(int)),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=tmp[3], type=int, isArgument=false, pos=examples/mul_add/code:6:12), version=0))
  ;   ]
  ; )
  cmp x20, #0
  cset x20, eq
  ; instruction: .call(
  ;   fn=Function(name=checkInt, args=[int] ret=nil),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=tmp[4], type=int, isArgument=false, pos=examples/mul_add/code:6:8), version=0))
  ;   ]
  ; )
  cmp x20, #0
  b.eq Lcfg_node_4
Lcfg_node_5:
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=tmp[5], type=int, isArgument=false, pos=examples/mul_add/code:7:14), version=0)
  ;   fn=Function(name=add, args=[int, int] ret=Optional(int)),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=2)),
  ;     .variable(SSAVariable(Variable(name=z, type=int, isArgument=true, pos=examples/mul_add/code:1:27), version=0))
  ;   ]
  ; )
  add x19, x19, x21
  ; instruction: .copy(
  ;   target=SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=3),
  ;   source=.variable(SSAVariable(Variable(name=tmp[5], type=int, isArgument=false, pos=examples/mul_add/code:7:14), version=0))
  ; )
Lcfg_node_4:
  ; instruction: .phi(
  ;   SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=4),
  ;   branches=[
  ;     (key: Node(2), value: .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=2))),
  ;     (key: Node(5), value: .variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=3)))
  ;   ]
  ; )
  ; instruction: .copy(
  ;   target=SSAVariable(Variable(name=<return value>, type=int, isArgument=false, pos=examples/mul_add/code:1:35), version=0),
  ;   source=.variable(SSAVariable(Variable(name=result, type=int, isArgument=false, pos=examples/mul_add/code:2:3), version=4))
  ; )
Lcfg_node_1:
  ; instruction: .returnValue(.variable(SSAVariable(Variable(name=<return value>, type=int, isArgument=false, pos=examples/mul_add/code:1:35), version=0)))
  mov x0, x19
_mul_add_Tintintint_epilogue:
  ldr x19, [x29, #-8]
  ldr x20, [x29, #-16]
  ldr x21, [x29, #-24]
  ldr x22, [x29, #-32]
  ldp x29, x30, [sp, #32]
  add sp, sp, #48
  ret
  .cfi_endproc
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
  ;   target=SSAVariable(Variable(name=tmp[6], type=int, isArgument=false, pos=examples/mul_add/code:13:12), version=0)
  ;   fn=Function(name=getc, args=[] ret=Optional(int)),
  ;   args=[
  ;   ]
  ; )
  bl _getchar
  sxtw x0, w0
  mov x19, x0
  ; instruction: .copy(
  ;   target=SSAVariable(Variable(name=x, type=int, isArgument=false, pos=examples/mul_add/code:13:3), version=0),
  ;   source=.variable(SSAVariable(Variable(name=tmp[6], type=int, isArgument=false, pos=examples/mul_add/code:13:12), version=0))
  ; )
  ; instruction: .callAndStore(
  ;   target=SSAVariable(Variable(name=tmp[7], type=int, isArgument=false, pos=examples/mul_add/code:14:11), version=0)
  ;   fn=Function(name=mul_add, args=[int, int, int] ret=Optional(int)),
  ;   args=[
  ;     .variable(SSAVariable(Variable(name=x, type=int, isArgument=false, pos=examples/mul_add/code:13:3), version=0)),
  ;     .constInt(2),
  ;     .constInt(3)
  ;   ]
  ; )
  mov x0, x19
  mov x1, #2
  mov x2, #3
  bl _mul_add_Tintintint
  mov x19, x0
  ; instruction: .copy(
  ;   target=SSAVariable(Variable(name=<return value>, type=int, isArgument=false, pos=examples/mul_add/code:12:10), version=0),
  ;   source=.variable(SSAVariable(Variable(name=tmp[7], type=int, isArgument=false, pos=examples/mul_add/code:14:11), version=0))
  ; )
Lcfg_node_7:
  ; instruction: .returnValue(.variable(SSAVariable(Variable(name=<return value>, type=int, isArgument=false, pos=examples/mul_add/code:12:10), version=0)))
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