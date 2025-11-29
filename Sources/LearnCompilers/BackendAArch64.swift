public struct BackendAArch64: Backend {

  public enum CompileError: Error {
    case stackOverflow(String)
  }

  public enum Register: CustomStringConvertible, Hashable, Sendable {
    case x(Int)
    case w(Int)
    case sp

    public var description: String {
      switch self {
      case .x(let x): "x\(x)"
      case .w(let w): "w\(w)"
      case .sp: "sp"
      }
    }
  }

  public enum RegOrInt: CustomStringConvertible, Equatable, Sendable {
    case reg(Register)
    case int(UInt16)

    public var description: String {
      switch self {
      case .reg(let r): r.description
      case .int(let x): "#\(x)"
      }
    }
  }

  public typealias StackVarIdx = Int

  public enum VarPlacement: Hashable {
    case register(Register)
    case stack(StackVarIdx)
  }

  public typealias Addr = (base: Register, offset: Int)

  public static func addrStr(_ addr: Addr) -> String {
    if addr.offset == 0 {
      return "[\(addr.base)]"
    } else {
      return "[\(addr.base), #\(addr.offset)]"
    }
  }

  public enum CodeLine: Sendable {
    case globl(String)
    case alignPow2(Int)
    case symbol(String)
    case mov(Register, RegOrInt)
    case movk(Register, UInt16, UInt8)
    case sub(Register, Register, RegOrInt)
    case add(Register, Register, RegOrInt)
    case addSymbol(Register, Register, String)
    case and(Register, Register, RegOrInt)
    case orr(Register, Register, RegOrInt)
    case mul(Register, Register, RegOrInt)
    case sdiv(Register, Register, RegOrInt)
    case msub(Register, Register, Register, Register)
    case ldr(Register, Addr)
    case ldrb(Register, Addr)
    case str(Register, Addr)
    case strb(Register, Addr)
    case ldp(Register, Register, Addr)
    case stp(Register, Register, Addr)
    case adrp(Register, String)
    case sxtw(Register, Register)
    case cmp(Register, RegOrInt)
    case cset(Register, String)
    case b(String)
    case bl(String)
    case bEq(String)
    case ret

    var code: String {
      switch self {
      case .globl(let name):
        return ".globl \(name)"
      case .alignPow2(let n):
        return ".p2align \(n)"
      case .symbol(let name):
        return "\(name):"
      case .mov(let target, let source):
        return "  mov \(target), \(source)"
      case .movk(let target, let value, let shift):
        return "  movk \(target), #\(value), lsl #\(shift)"
      case .sub(let target, let a, let b):
        return "  sub \(target), \(a), \(b)"
      case .add(let target, let a, let b):
        return "  add \(target), \(a), \(b)"
      case .addSymbol(let target, let a, let b):
        return "  add \(target), \(a), \(b)"
      case .and(let target, let a, let b):
        return "  and \(target), \(a), \(b)"
      case .orr(let target, let a, let b):
        return "  orr \(target), \(a), \(b)"
      case .mul(let target, let a, let b):
        return "  mul \(target), \(a), \(b)"
      case .sdiv(let target, let a, let b):
        return "  sdiv \(target), \(a), \(b)"
      case .msub(let target, let a, let b, let c):
        return "  msub \(target), \(a), \(b), \(c)"
      case .ldr(let target, let source):
        return "  ldr \(target), \(addrStr(source))"
      case .ldrb(let target, let source):
        return "  ldrb \(target), \(addrStr(source))"
      case .str(let source, let target):
        return "  str \(source), \(addrStr(target))"
      case .strb(let source, let target):
        return "  strb \(source), \(addrStr(target))"
      case .ldp(let t1, let t2, let source):
        return "  ldp \(t1), \(t2), \(addrStr(source))"
      case .stp(let s1, let s2, let target):
        return "  stp \(s1), \(s2), \(addrStr(target))"
      case .adrp(let reg, let symbol):
        return "  adrp \(reg), \(symbol)"
      case .sxtw(let r1, let r2):
        return "  sxtw \(r1), \(r2)"
      case .cmp(let reg, let value):
        return "  cmp \(reg), \(value)"
      case .cset(let reg, let cond):
        return "  cset \(reg), \(cond)"
      case .b(let label):
        return "  b \(label)"
      case .bl(let label):
        return "  bl \(label)"
      case .bEq(let label):
        return "  b.eq \(label)"
      case .ret:
        return "  ret"
      }
    }
  }

  internal struct Frame {
    let function: Function
    let placement: [CFG.SSAVariable: VarPlacement]
    let stackAllocation: Int  // measured in 64-bit values
    let stackArgCount: Int
    let stackVarCount: Int
    let backupRegisters: [Register]

    func stackVarAddress(_ idx: Int) -> Addr {
      (.x(29), -(idx + 1) * 8)
    }

    func backupRegisterAddresses() -> [(Register, Addr)] {
      backupRegisters.enumerated().map { (i, reg) in
        (reg, (.x(29), -(stackVarCount + i + 1) * 8))
      }
    }
  }

  public struct StringTable {
    public var strToID = [[UInt8]: Int]()

    public mutating func symbol(_ s: [UInt8]) -> String {
      if let id = strToID[s] {
        return "str_const_\(id)"
      }
      let id = strToID.count
      strToID[s] = id
      return "str_const_\(id)"
    }

    public func encode() -> String {
      var result = ""
      for (bytes, id) in strToID.sorted(by: { $0.value < $1.value }) {
        result += ".p2align 4\n"
        result += "str_const_\(id):\n"
        result += "  .quad \(bytes.count)\n"
        for x in bytes {
          result += "  .byte \(x)\n"
        }
      }
      return result
    }
  }

  public static let allocStrCode: [CodeLine] = [
    .globl("_str_alloc"),
    .alignPow2(2),
    .symbol("_str_alloc"),
    // Stack frame
    .sub(.sp, .sp, .int(32)),
    .stp(.x(29), .x(30), (.sp, 16)),
    .add(.x(29), .sp, .int(16)),
    // Backup callee-saved registers
    .stp(.x(19), .x(20), (.sp, 0)),
    // Allocate buffer and store length into it
    .mov(.x(19), .reg(.x(0))),
    .add(.x(0), .x(19), .int(8)),
    .bl("_malloc"),
    .str(.x(19), (Register.x(0), 0)),
    // Restore callee-saved registers
    .ldp(.x(19), .x(20), (.sp, 0)),
    // Exit stack frame
    .ldp(.x(29), .x(30), (.sp, 16)),
    .add(.sp, .sp, .int(32)),
    .ret,
  ]

  public let graphColorAlgorithm: GraphColorAlgorithm

  public init(graphColorAlgorithm: GraphColorAlgorithm = .greedy) {
    self.graphColorAlgorithm = graphColorAlgorithm
  }

  /// Compile the graph as AArch64 assembly code.
  public func compileAssembly(cfg: CFG) throws -> String {
    let liveness = Liveness(cfg: cfg)
    var strTable = StringTable()
    let sortedFuncs = cfg.functions.keys.sorted { (fn1, fn2) in
      let name1 = symbolName(fn: fn1)
      let name2 = symbolName(fn: fn2)
      return name1 < name2
    }
    var results = try sortedFuncs.flatMap {
      try compileFunction(cfg: cfg, stringTable: &strTable, liveness: liveness, fn: $0)
    }
    results.append(contentsOf: Self.allocStrCode)
    var out = results.map { $0.code }.joined(separator: "\n")
    let constStr = strTable.encode()
    if !constStr.isEmpty {
      out += "\n\n\(constStr)"
    }
    return out
  }

  public func compileFunction(
    cfg: CFG,
    stringTable: inout StringTable,
    liveness: Liveness,
    fn: Function
  ) throws -> [CodeLine] {
    let frame = try calculateFrame(cfg: cfg, liveness: liveness, fn: fn)
    let prologue = encodePrologue(cfg: cfg, frame: frame)
    let epilogue = encodeEpilogue(cfg: cfg, frame: frame)

    let entrypoint = cfg.functions[fn]!
    let allNodeCode = cfg.dfsFrom(node: entrypoint).flatMap { node in
      var bodyCode = [CodeLine]()
      for inst in cfg.nodeCode[node]!.instructions {
        bodyCode.append(
          contentsOf: encodeInstruction(
            cfg: cfg,
            stringTable: &stringTable,
            frame: frame,
            inst: inst
          )
        )
      }
      let header = [CodeLine.symbol("cfg_node_\(node.id)")]
      var tail = [CodeLine]()
      switch cfg.successors[node] {
      case .single(let nextNode):
        let phiCode = encodeParallelPhi(
          cfg: cfg, stringTable: &stringTable, frame: frame, from: node, to: nextNode
        )
        if !phiCode.isEmpty {
          tail.append(contentsOf: phiCode)
        }
        tail.append(.b("cfg_node_\(nextNode.id)"))
      case .branch(let ifFalse, let ifTrue):
        let falsePhiCode = encodeParallelPhi(
          cfg: cfg, stringTable: &stringTable, frame: frame, from: node, to: ifFalse
        )
        let truePhiCode = encodeParallelPhi(
          cfg: cfg, stringTable: &stringTable, frame: frame, from: node, to: ifTrue
        )
        var falseSymbol = "cfg_node_\(ifFalse.id)"
        var trueSymbol = "cfg_node_\(ifTrue.id)"
        var postTail = [CodeLine]()
        if !falsePhiCode.isEmpty {
          falseSymbol = "cfg_node_\(node.id)_false"
          postTail.append(.symbol(falseSymbol))
          postTail.append(contentsOf: falsePhiCode)
          postTail.append(.b("cfg_node_\(ifFalse.id)"))
        }
        if !truePhiCode.isEmpty {
          trueSymbol = "cfg_node_\(node.id)_true"
          postTail.append(.symbol(trueSymbol))
          postTail.append(contentsOf: truePhiCode)
          postTail.append(.b("cfg_node_\(ifTrue.id)"))
        }
        tail.append(.bEq(falseSymbol))
        tail.append(.b(trueSymbol))
        tail.append(contentsOf: postTail)
      case .none: ()
      }
      return header + bodyCode + tail
    }

    return optimizeInstructions(prologue + allNodeCode + epilogue)
  }

  internal func calculateFrame(cfg: CFG, liveness: Liveness, fn: Function) throws -> Frame {
    let fnNodes = cfg.dfsFrom(node: cfg.functions[fn]!)
    var interference = Liveness.VariableGraph()
    var affinity = Liveness.VariableGraph()
    for node in fnNodes {
      interference.insert(graph: liveness.interferenceFor(node: node))
      affinity.insert(graph: liveness.affinityFor(node: node))
    }
    let varColors = color(
      graph: interference,
      affinity: affinity,
      order: cfg.orderedVariables,
      algorithm: graphColorAlgorithm
    )
    let colorCount = 1 + (varColors.values.max() ?? -1)

    let maybeMaxCallArgs = callMaxArgCount(cfg: cfg, fn: fn)

    let availRegs: [VarPlacement] =
      if maybeMaxCallArgs != nil {
        // There's a function call; don't use caller-saved registers (for now).
        (19...28).map { VarPlacement.register(.x($0)) }
      } else {
        (9...28).map { VarPlacement.register(.x($0)) }
      }

    var slots = Array(availRegs[..<min(availRegs.count, colorCount)])
    var stackVarCount = 0
    while slots.count < colorCount {
      slots.append(.stack(stackVarCount))
      stackVarCount += 1
    }

    let backupRegs: [Register] = slots.compactMap { x in
      if case .register(.x(let r)) = x, (19...28).contains(r) {
        .x(r)
      } else {
        nil
      }
    }

    let stackArgs = max(0, (maybeMaxCallArgs ?? 0) - 8)
    var stackAllocation = stackVarCount + stackArgs + backupRegs.count
    // Keep stack 16-byte aligned
    if stackAllocation % 2 != 0 {
      stackAllocation += 1
    }

    if stackAllocation * 8 + 16 >= 0x10000 {
      throw CompileError.stackOverflow("stack allocations for function \(fn) exceed 64k")
    }

    return Frame(
      function: fn,
      placement: varColors.mapValues { slots[$0] },
      stackAllocation: stackAllocation,
      stackArgCount: stackArgs,
      stackVarCount: stackVarCount,
      backupRegisters: backupRegs
    )
  }

  /// Compute the maximum arg count that will be
  internal func callMaxArgCount(cfg: CFG, fn: Function) -> Int? {
    var result: Int? = nil
    for node in cfg.dfsFrom(node: cfg.functions[fn]!) {
      for inst in cfg.nodeCode[node]!.instructions {
        let thisCount: Int? =
          switch inst.op {
          case .call(let fn, let args):
            if requiresActualCall(fn: fn) {
              args.count
            } else {
              nil
            }
          case .callAndStore(_, let fn, let args):
            if requiresActualCall(fn: fn) {
              args.count
            } else {
              nil
            }
          default: nil
          }
        if let c = thisCount {
          if let r = result {
            result = max(r, c)
          } else {
            result = c
          }
        }
      }
    }
    return result
  }

  internal func requiresActualCall(fn: Function) -> Bool {
    guard let builtIn = fn.builtIn else {
      return true
    }
    switch builtIn {
    case .strAlloc: return true
    case .strFree: return true
    case .putc: return true
    case .getc: return true
    default: return false
    }
  }

  internal func symbolName(frame: Frame) -> String {
    symbolName(fn: frame.function)
  }

  internal func symbolName(fn function: Function) -> String {
    if function.name == "main" && function.signature.args.isEmpty {
      "_main"
    } else {
      "_\(function.name)_T\(function.signature.args.map { $0.description }.joined())"
    }
  }

  internal func encodePrologue(cfg: CFG, frame: Frame) -> [CodeLine] {
    let symbolName = symbolName(frame: frame)
    var result: [CodeLine] = [
      .globl(symbolName),
      .alignPow2(2),
      .symbol(symbolName),
      .sub(.sp, .sp, .int(UInt16(frame.stackAllocation * 8 + 16))),
      .stp(.x(29), .x(30), (.sp, frame.stackAllocation * 8)),
      .add(.x(29), .sp, .int(UInt16(frame.stackAllocation * 8))),
    ]
    for (reg, addr) in frame.backupRegisterAddresses() {
      result.append(.str(reg, addr))
    }
    return result
  }

  internal func encodeEpilogue(cfg: CFG, frame: Frame) -> [CodeLine] {
    let symbolName = symbolName(frame: frame)
    var result = [CodeLine.symbol("\(symbolName)_epilogue")]
    for (reg, addr) in frame.backupRegisterAddresses() {
      result.append(.ldr(reg, addr))
    }
    result.append(.ldp(.x(29), .x(30), (.sp, frame.stackAllocation * 8)))
    result.append(.add(.sp, .sp, .int(UInt16(frame.stackAllocation * 8 + 16))))
    result.append(.ret)
    return result
  }

  internal func encodeInstruction(
    cfg: CFG,
    stringTable: inout StringTable,
    frame: Frame,
    inst: CFG.Inst
  ) -> [CodeLine] {
    switch inst.op {
    case .phi:
      return []
    case .copy(let target, let source):
      let (targetCode, targetReg) = writableVariableRegister(
        frame: frame,
        target: target,
        defaultReg: .x(8)
      )
      let (sourceCode, sourceReg) = argumentToRegister(
        stringTable: &stringTable,
        frame: frame,
        argument: source,
        defaultReg: targetReg
      )
      if targetReg != sourceReg {
        return sourceCode + [.mov(targetReg, .reg(sourceReg))] + targetCode
      } else {
        return sourceCode + targetCode
      }
    case .funcArg(let target, let argIdx):
      switch frame.placement[target]! {
      case .register(let targetReg):
        if argIdx < 8 {
          return [.mov(targetReg, .reg(.x(argIdx)))]
        } else {
          return [.ldr(targetReg, (.x(29), 16 + 8 * (argIdx - 8)))]
        }
      case .stack(let targetIdx):
        if argIdx < 8 {
          return [.str(.x(argIdx), frame.stackVarAddress(targetIdx))]
        } else {
          return [
            .ldr(.x(8), (.x(29), 16 + 8 * (argIdx - 8))),
            .str(.x(8), frame.stackVarAddress(targetIdx)),
          ]
        }
      }
    case .check(let value):
      let (insts, reg) = argumentToRegister(
        stringTable: &stringTable,
        frame: frame,
        argument: value,
        defaultReg: .x(0)
      )
      switch value.dataType {
      case .integer:
        return insts + [.cmp(reg, .int(0))]
      case .string:
        return insts + [.ldr(.x(8), (reg, 0)), .cmp(.x(8), .int(0))]
      }
    case .call(let fn, let args):
      if let builtIn = fn.builtIn {
        switch builtIn {
        case .putc:
          return encodeFunctionCall(
            stringTable: &stringTable, frame: frame, symbol: "_putchar", args: args
          )
        case .strFree:
          return encodeFunctionCall(
            stringTable: &stringTable, frame: frame, symbol: "_free", args: args
          )
        case .getc:
          return encodeFunctionCall(
            stringTable: &stringTable, frame: frame, symbol: "_getchar", args: args
          )
        case .strSet:
          let (instsIn1, regStr) = argumentToRegister(
            stringTable: &stringTable,
            frame: frame,
            argument: args[0],
            defaultReg: .x(0)
          )
          let (instsIn2, regOff) = argumentToRegister(
            stringTable: &stringTable,
            frame: frame,
            argument: args[1],
            defaultReg: .x(1)
          )
          let (instsIn3, regValue) = argumentToRegister(
            stringTable: &stringTable,
            frame: frame,
            argument: args[2],
            defaultReg: .x(2)
          )
          guard case .x(let regValueIdx) = regValue else {
            fatalError("unexpected out register \(regValue)")
          }
          return instsIn1 + instsIn2 + instsIn3 + [
            .add(.x(0), regStr, .reg(regOff)),
            .strb(.w(regValueIdx), (.x(0), 8)),
          ]
        default: return []
        }
      }
      return encodeFunctionCall(
        stringTable: &stringTable, frame: frame, symbol: symbolName(fn: fn), args: args
      )
    case .callAndStore(let target, let fn, let args):
      var symbol: String
      switch fn.builtIn {
      case .add:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.add(dst, a, .reg(b))]
        }
      case .sub:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.sub(dst, a, .reg(b))]
        }
      case .and:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.and(dst, a, .reg(b))]
        }
      case .or:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.orr(dst, a, .reg(b))]
        }
      case .mul:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.mul(dst, a, .reg(b))]
        }
      case .div:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.sdiv(dst, a, .reg(b))]
        }
      case .mod:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.sdiv(.x(2), a, .reg(b)), .msub(dst, .x(2), b, a)]
        }
      case .lt:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.cmp(a, .reg(b)), .cset(dst, "lt")]
        }
      case .gt:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.cmp(a, .reg(b)), .cset(dst, "gt")]
        }
      case .eqInt:
        return encodeBinaryOp(frame: frame, args: args, target: target) { dst, a, b in
          [.cmp(a, .reg(b)), .cset(dst, "eq")]
        }
      case .notInt:
        let (instsIn, source) = argumentToRegister(
          stringTable: &stringTable, frame: frame, argument: args[0], defaultReg: .x(0)
        )
        let (instsOut, dst) = writableVariableRegister(frame: frame, target: target)
        return instsIn + [.cmp(source, .int(0)), .cset(dst, "eq")] + instsOut
      case .len:
        let (instsIn, reg) = argumentToRegister(
          stringTable: &stringTable,
          frame: frame,
          argument: args[0],
          defaultReg: .x(8)
        )
        let (instsOut, outReg) = writableVariableRegister(frame: frame, target: target)
        return instsIn + [.ldr(outReg, (reg, 0))] + instsOut
      case .getc:
        symbol = "_getchar"
      case .strAlloc:
        symbol = "_str_alloc"
      case .strGet:
        let (instsIn1, regStr) = argumentToRegister(
          stringTable: &stringTable,
          frame: frame,
          argument: args[0],
          defaultReg: .x(0)
        )
        let (instsIn2, regOff) = argumentToRegister(
          stringTable: &stringTable,
          frame: frame,
          argument: args[1],
          defaultReg: .x(1)
        )
        let (instsOut, regOut) = writableVariableRegister(
          frame: frame,
          target: target,
          defaultReg: .x(2)
        )
        guard case .x(let regOutIdx) = regOut else {
          fatalError("unexpected out register \(regOut)")
        }
        return instsIn1 + instsIn2 + [
          .add(.x(0), regStr, .reg(regOff)),
          .ldrb(.w(regOutIdx), (.x(0), 8)),
        ] + instsOut
      case .strFree, .strSet, .putc:
        fatalError("cannot assign return of call")
      case .none:
        symbol = symbolName(fn: fn)
      }
      let callCode = encodeFunctionCall(
        stringTable: &stringTable,
        frame: frame,
        symbol: symbol,
        args: args,
        signExtend: fn.builtIn == .getc
      )
      let finalCodeLines = registerToVariable(frame: frame, target: target, source: .x(0))
      return callCode + finalCodeLines
    case .returnValue(let arg):
      let (insts, r) = argumentToRegister(
        stringTable: &stringTable,
        frame: frame,
        argument: arg,
        defaultReg: .x(0)
      )
      return insts + (r == .x(0) ? [] : [.mov(.x(0), .reg(r))]) + [
        .b("\(symbolName(frame: frame))_epilogue")
      ]
    case .returnVoid:
      return [.b("\(symbolName(frame: frame))_epilogue")]
    }
  }

  internal func encodeBinaryOp(
    frame: Frame,
    args: [CFG.Argument],
    target: CFG.SSAVariable,
    builder: (Register, Register, Register) -> [CodeLine]
  ) -> [CodeLine] {
    let (inst1, r1) = intArgumentToRegister(frame: frame, argument: args[0], defaultReg: .x(0))
    let (inst2, r2) = intArgumentToRegister(frame: frame, argument: args[1], defaultReg: .x(1))
    let (inst3, regOut) = writableVariableRegister(frame: frame, target: target)
    return inst1 + inst2 + builder(regOut, r1, r2) + inst3
  }

  internal func encodeFunctionCall(
    stringTable: inout StringTable,
    frame: Frame,
    symbol: String,
    args: [CFG.Argument],
    signExtend: Bool = false
  ) -> [CodeLine] {
    var result = [CodeLine]()
    for (i, arg) in args.enumerated() {
      if i < 8 {
        let targetReg = Register.x(i)
        let (insts, reg) = argumentToRegister(
          stringTable: &stringTable, frame: frame, argument: arg, defaultReg: targetReg
        )
        result.append(contentsOf: insts)
        if reg != targetReg {
          result.append(.mov(targetReg, .reg(reg)))
        }
      } else {
        let stackOffset = i - 8
        let (insts, reg) = argumentToRegister(
          stringTable: &stringTable, frame: frame, argument: arg, defaultReg: .x(8)
        )
        result.append(contentsOf: insts)
        result.append(.str(reg, (.sp, stackOffset * 8)))
      }
    }
    result.append(.bl(symbol))
    if signExtend {
      result.append(.sxtw(.x(0), .w(0)))
    }
    return result
  }

  internal func encodeParallelPhi(
    cfg: CFG,
    stringTable: inout StringTable,
    frame: Frame,
    from: CFG.Node,
    to: CFG.Node
  ) -> [CodeLine] {
    var phiMoves = [(VarPlacement, VarPlacement)]()
    var phiConsts = [(VarPlacement, Int64)]()
    var phiStrs = [(VarPlacement, [UInt8])]()

    for inst in cfg.nodeCode[to]!.instructions {
      guard case .phi(let target, let sources) = inst.op else {
        continue
      }
      let t = frame.placement[target]!
      switch sources[from]! {
      case .constInt(let x):
        phiConsts.append((t, x))
      case .constStr(let x):
        phiStrs.append((t, x))
      case .variable(let v):
        let s = frame.placement[v]!
        if t != s {
          phiMoves.append((t, s))
        }
      }
    }

    var result = [CodeLine]()

    if !phiMoves.isEmpty {
      let encodedMoves = MoveOp.encodeParallelMove(phiMoves)
      for op in encodedMoves {
        switch op {
        case .saveTmp(let src):
          let (insts, reg) = variableToRegister(frame: frame, placement: src, defaultReg: .x(8))
          result.append(contentsOf: insts)
          if reg != .x(8) {
            result.append(.mov(.x(8), .reg(reg)))
          }
        case .loadTmp(let dst):
          result.append(contentsOf: registerToVariable(frame: frame, placement: dst, source: .x(8)))
        case .move(let dst, let src):
          let (insts, reg) = variableToRegister(frame: frame, placement: src, defaultReg: .x(0))
          result.append(contentsOf: insts)
          result.append(contentsOf: registerToVariable(frame: frame, placement: dst, source: reg))
        }
      }
    }

    for (target, sourceValue) in phiConsts {
      let (code, reg) = writableVariableRegister(frame: frame, placement: target)
      result.append(contentsOf: encodeConstantMov(target: reg, value: sourceValue))
      result.append(contentsOf: code)
    }
    for (target, sourceData) in phiStrs {
      let strSymbol = stringTable.symbol(sourceData)
      let (code, reg) = writableVariableRegister(frame: frame, placement: target)
      result.append(.adrp(reg, "\(strSymbol)@PAGE"))
      result.append(.addSymbol(reg, reg, "\(strSymbol)@PAGEOFF"))
      result.append(contentsOf: code)
    }

    return result
  }

  internal func argumentToRegister(
    stringTable: inout StringTable,
    frame: Frame,
    argument: CFG.Argument,
    defaultReg: Register
  ) -> ([CodeLine], Register) {
    switch argument {
    case .constStr(let x):
      let strSymbol = stringTable.symbol(x)
      return (
        [
          .adrp(defaultReg, "\(strSymbol)@PAGE"),
          .addSymbol(defaultReg, defaultReg, "\(strSymbol)@PAGEOFF"),
        ], defaultReg
      )
    default:
      return intArgumentToRegister(frame: frame, argument: argument, defaultReg: defaultReg)
    }
  }

  internal func intArgumentToRegister(
    frame: Frame,
    argument: CFG.Argument,
    defaultReg: Register
  ) -> ([CodeLine], Register) {
    switch argument {
    case .constInt(let x):
      return (encodeConstantMov(target: defaultReg, value: x), defaultReg)
    case .constStr:
      fatalError("string not supported here")
    case .variable(let v):
      return variableToRegister(
        frame: frame, placement: frame.placement[v]!, defaultReg: defaultReg
      )
    }
  }

  internal func variableToRegister(
    frame: Frame,
    placement: VarPlacement,
    defaultReg: Register
  ) -> ([CodeLine], Register) {
    switch placement {
    case .register(let r):
      ([], r)
    case .stack(let idx):
      ([.ldr(defaultReg, frame.stackVarAddress(idx))], defaultReg)
    }
  }

  internal func writableVariableRegister(
    frame: Frame,
    target: CFG.SSAVariable,
    defaultReg: Register = .x(8)
  ) -> ([CodeLine], Register) {
    writableVariableRegister(
      frame: frame,
      placement: frame.placement[target]!,
      defaultReg: defaultReg
    )
  }

  internal func writableVariableRegister(
    frame: Frame,
    placement: VarPlacement,
    defaultReg: Register = .x(8)
  ) -> ([CodeLine], Register) {
    switch placement {
    case .register(let r):
      ([], r)
    case .stack(let idx):
      ([.str(defaultReg, frame.stackVarAddress(idx))], defaultReg)
    }
  }

  internal func registerToVariable(
    frame: Frame,
    target: CFG.SSAVariable,
    source: Register
  ) -> [CodeLine] {
    registerToVariable(frame: frame, placement: frame.placement[target]!, source: source)
  }

  internal func registerToVariable(
    frame: Frame,
    placement: VarPlacement,
    source: Register
  ) -> [CodeLine] {
    switch placement {
    case .register(let r):
      source == r ? [] : [.mov(r, .reg(source))]
    case .stack(let idx):
      [.str(source, frame.stackVarAddress(idx))]
    }
  }

  internal func encodeConstantMov(target: Register, value: Int64) -> [CodeLine] {
    let v64 = UInt64(bitPattern: value)
    var result = [CodeLine.mov(target, .int(UInt16(v64 & 0xffff)))]
    for i in stride(from: 16, through: 48, by: 16) {
      let word = UInt16((v64 >> i) & 0xffff)
      if word != 0 {
        result.append(.movk(target, word, UInt8(i)))
      }
    }
    return result
  }

  internal func optimizeInstructions(_ insts: [CodeLine]) -> [CodeLine] {
    var insts = insts

    var i = 0
    while i < insts.count {
      let inst = insts[i]
      // Remove `b` instructions when the label comes right after.
      if case .b(let symbol) = inst, i + 1 < insts.count {
        if case .symbol(let name) = insts[i + 1], name == symbol {
          insts.remove(at: i)
          continue
        }
      }
      i += 1
    }

    return insts
  }

}
