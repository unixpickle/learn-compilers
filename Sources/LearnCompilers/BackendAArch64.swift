public struct BackendAArch64: Backend {

  internal enum VarPlacement {
    case register(Int)
    case stack(Int)
  }

  internal struct Frame {
    let function: Function
    let placement: [CFG.SSAVariable: VarPlacement]
    let stackAllocation: Int  // measured in 64-bit values
    let stackVarCount: Int
    let backupRegisters: [Int]

    func stackVarAddress(_ idx: Int) -> String {
      "[x29, #-\(idx*8)]"
    }

    func backupRegisterAddresses() -> [(Int, String)] {
      backupRegisters.enumerated().map { (i, reg) in
        (reg, "[x29, #-\(stackVarCount*8+i)]")
      }
    }
  }

  public let graphColorAlgorithm = GraphColorAlgorithm.greedy

  // TODO: write built-in functions for string operations and printing.

  /// Compile the graph as AArch64 assembly code.
  public func compileAssembly(cfg: CFG) throws -> String {
    let liveness = Liveness(cfg: cfg)
    let results = cfg.functions.keys.map { compileFunction(cfg: cfg, liveness: liveness, fn: $0) }
    return results.joined(separator: "\n\n")
  }

  public func compileFunction(cfg: CFG, liveness: Liveness, fn: Function) -> String {
    let frame = calculateFrame(cfg: cfg, liveness: liveness, fn: fn)
    let prologue = encodePrologue(cfg: cfg, frame: frame)
    let epilogue = encodeEpilogue(cfg: cfg, frame: frame)

    let entrypoint = cfg.functions[fn]!
    let blockCodes = cfg.dfsFrom(node: entrypoint).map { node in
      var insts = [String]()
      for inst in cfg.nodeCode[node]!.instructions {
        insts.append(contentsOf: encodeInstruction(cfg: cfg, frame: frame, inst: inst))
      }
      let bodyCode = insts.map { "  " + $0 }.joined(separator: "\n")
      let header = "cfg_node_\(node.id):"
      var tail = ""
      switch cfg.successors[node] {
      case .single(let nextNode):
        let phiCode = encodeParallelPhi(cfg: cfg, frame: frame, from: node, to: nextNode)
        if !phiCode.isEmpty {
          tail = phiCode.map { "  " + $0 }.joined(separator: "\n") + "\n"
        }
        tail += "  b cfg_node_\(nextNode.id)"
      case .branch(let ifFalse, let ifTrue):
        let falsePhiCode = encodeParallelPhi(cfg: cfg, frame: frame, from: node, to: ifFalse)
        let truePhiCode = encodeParallelPhi(cfg: cfg, frame: frame, from: node, to: ifTrue)
        var falseSymbol = "cfg_node_\(ifFalse.id)"
        var trueSymbol = "cfg_node_\(ifTrue.id)"
        var postTail = ""
        if !falsePhiCode.isEmpty {
          falseSymbol = "cfg_node_\(node.id)_false"
          postTail += "\n\(falseSymbol):\n"
          postTail += falsePhiCode.map { "  " + $0 }.joined(separator: "\n")
          postTail += "  b cfg_node_\(ifFalse.id)"
        }
        if !truePhiCode.isEmpty {
          trueSymbol = "cfg_node_\(node.id)_true"
          postTail += "\n\(trueSymbol):\n"
          postTail += truePhiCode.map { "  " + $0 }.joined(separator: "\n")
          postTail += "  b cfg_node_\(ifTrue.id)"
        }
        tail += "  b.eq \(falseSymbol)\n  b \(trueSymbol)"
        tail += postTail
      case .none:
        tail = "  b \(symbolName(frame: frame))_epilogue"
      }

      return "\(header)\n\(bodyCode)\n\(tail)"
    }.joined()

    return "\(prologue)\n\(blockCodes)\n\(epilogue)"
  }

  internal func calculateFrame(cfg: CFG, liveness: Liveness, fn: Function) -> Frame {
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
      algorithm: graphColorAlgorithm
    )
    let colorCount = 1 + (varColors.values.max() ?? -1)

    let maybeMaxCallArgs = callMaxArgCount(cfg: cfg, fn: fn)

    let availRegs: [VarPlacement] =
      if maybeMaxCallArgs != nil {
        // There's a function call; don't use caller-saved registers (for now).
        (19...28).map { VarPlacement.register($0) }
      } else {
        (9...28).map { VarPlacement.register($0) }
      }

    var slots = Array(availRegs[..<min(availRegs.count, colorCount)])
    var stackVarCount = 0
    while slots.count < colorCount {
      slots.append(.stack(stackVarCount))
      stackVarCount += 1
    }

    let backupRegs: [Int] = slots.compactMap { x in
      if case .register(let r) = x, (19...28).contains(r) {
        r
      } else {
        nil
      }
    }

    var stackAllocation = stackVarCount + max(0, (maybeMaxCallArgs ?? 0) - 8) + backupRegs.count
    // Keep stack 16-byte aligned
    if stackAllocation % 2 != 0 {
      stackAllocation += 1
    }

    return Frame(
      function: fn,
      placement: varColors.mapValues { slots[$0] },
      stackAllocation: stackAllocation,
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
    case .concat: return true
    case .str: return true
    case .eqStr: return true
    case .print: return true
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

  internal func encodePrologue(cfg: CFG, frame: Frame) -> String {
    let symbolName = symbolName(frame: frame)
    let alloc = "sub sp, sp, #\(frame.stackAllocation*8+16)"
    let backup = "stp x29, x30, [sp, #\(frame.stackAllocation*8)]"
    let fpCode = "add x29, sp, #\(frame.stackAllocation*8)"
    var result =
      ".globl \(symbolName)\n.p2align 2\n\(symbolName):\n  \(alloc)\n  \(backup)\n  \(fpCode)"
    for (regIdx, addr) in frame.backupRegisterAddresses() {
      result += "\n  str x\(regIdx), \(addr)"
    }
    return result
  }

  internal func encodeEpilogue(cfg: CFG, frame: Frame) -> String {
    let symbolName = symbolName(frame: frame)
    var result = "\(symbolName)_epilogue:\n"
    for (regIdx, addr) in frame.backupRegisterAddresses() {
      result += "\n  ldr x\(regIdx), \(addr)"
    }
    result +=
      "  ldp x29, x30, [sp, #\(frame.stackAllocation*8)]\n"
      + "  add sp, sp, #\(frame.stackAllocation*8+16)\n"
      + "  ret"
    return result
  }

  internal func encodeInstruction(cfg: CFG, frame: Frame, inst: CFG.Inst) -> [String] {
    switch inst.op {
    case .phi:
      return []
    case .copy(let target, let source):
      switch source {
      case .constInt(let x):
        switch frame.placement[target]! {
        case .register(let id):
          return ["mov x\(id), #\(x)"]
        case .stack(let stackIdx):
          return ["mov x8, #\(x)", "str x8, \(frame.stackVarAddress(stackIdx))"]
        }
      case .constStr:
        fatalError("string constants are not yet supported")
      case .variable(let sourceVar):
        switch frame.placement[target]! {
        case .register(let targetReg):
          switch frame.placement[sourceVar]! {
          case .register(let sourceReg):
            return ["mov x\(targetReg), x\(sourceReg)"]
          case .stack(let stackIdx):
            return ["ldr x\(targetReg), \(frame.stackVarAddress(stackIdx))"]
          }
        case .stack(let targetIdx):
          switch frame.placement[sourceVar]! {
          case .register(let sourceReg):
            return ["str x\(sourceReg), \(frame.stackVarAddress(targetIdx))"]
          case .stack(let sourceIdx):
            return [
              "ldr x8, \(frame.stackVarAddress(sourceIdx))",
              "str x8, \(frame.stackVarAddress(targetIdx))",
            ]
          }
        }
      }
    case .funcArg(let target, let argIdx):
      switch frame.placement[target]! {
      case .register(let targetReg):
        if argIdx < 8 {
          return ["mov x\(targetReg), x\(argIdx)"]
        } else {
          return ["ldr x\(targetReg), [x29, #\(16+8*argIdx)]"]
        }
      case .stack(let targetIdx):
        if argIdx < 8 {
          return ["str x\(argIdx), [x29, #-\(targetIdx*16)]"]
        } else {
          return ["ldr x8, [x29, #\(16+8*argIdx)]", "str x8, [x29, #-\(targetIdx*16)]"]
        }
      }
    case .check(let value):
      let (insts, reg) = intoRegister(frame: frame, argument: value, defaultReg: "x0")
      switch value.dataType {
      case .integer:
        return insts + ["cmp \(reg), #0"]
      case .string:
        return insts + ["ldr x0, [x0, #8]", "cmp \(reg), #0"]
      }
    case .call(let fn, let args):
      if let builtIn = fn.builtIn {
        if case .print = builtIn {
          return encodeFunctionCall(frame: frame, symbol: "_print", args: args)
        }
        return []
      }
      return encodeFunctionCall(frame: frame, symbol: symbolName(fn: fn), args: args)
    case .callAndStore(let target, let fn, let args):
      switch fn.builtIn {
      case .add:
        let (inst1, r1) = intoRegister(frame: frame, argument: args[0], defaultReg: "x0")
        let (inst2, r2) = intoRegister(frame: frame, argument: args[0], defaultReg: "x1")
        let (inst3, sumOut) = fromRegister(frame: frame, target: target)
        return (inst1 + inst2 + ["add \(sumOut), \(r1), \(r2)"] + inst3)
      case .sub:
        let (inst1, r1) = intoRegister(frame: frame, argument: args[0], defaultReg: "x0")
        let (inst2, r2) = intoRegister(frame: frame, argument: args[0], defaultReg: "x1")
        let (inst3, sumOut) = fromRegister(frame: frame, target: target)
        return (inst1 + inst2 + ["sub \(sumOut), \(r1), \(r2)"] + inst3)
      case .none: ()
      default: fatalError("not implemented")
      }
      let callCode = encodeFunctionCall(frame: frame, symbol: symbolName(fn: fn), args: args)
      let finalInsts = writeRegister(frame: frame, target: target, source: "x0")
      return callCode + finalInsts
    case .returnValue(let arg):
      let (insts, r) = intoRegister(frame: frame, argument: arg, defaultReg: "x0")
      return insts + (r == "x0" ? [] : ["mov x0, \(r)"]) + [
        "b \(symbolName(frame: frame))_epilogue"
      ]
    case .returnVoid:
      return ["b \(symbolName(frame: frame))_epilogue"]
    }
  }

  internal func encodeFunctionCall(frame: Frame, symbol: String, args: [CFG.Argument]) -> [String] {
    var result = [String]()

    for (i, arg) in args.enumerated() {
      if i < 8 {
        let targetReg = "x\(i)"
        let (insts, reg) = intoRegister(frame: frame, argument: arg, defaultReg: targetReg)
        result.append(contentsOf: insts)
        if reg != targetReg {
          result.append("mov \(targetReg), \(reg)")
        }
      } else {
        let stackOffset = i - 8
        let (insts, reg) = intoRegister(frame: frame, argument: arg, defaultReg: "x8")
        result.append(contentsOf: insts)
        result.append("str \(reg), [sp, #\(stackOffset*8)]")
      }
    }

    result.append("bl \(symbol)")
    return result
  }

  internal func encodeParallelPhi(
    cfg: CFG,
    frame: Frame,
    from: CFG.Node,
    to: CFG.Node
  ) -> [String] {
    var phiMoves = [(VarPlacement, VarPlacement)]()
    var phiConsts = [(VarPlacement, Int64)]()

    for inst in cfg.nodeCode[to]!.instructions {
      guard case .phi(let target, let sources) = inst.op else {
        continue
      }
      let t = frame.placement[target]!
      switch sources[from]! {
      case .constInt(let x):
        phiConsts.append((t, x))
      case .constStr:
        fatalError("string constants not yet implemented")
      case .variable(let v):
        phiMoves.append((t, frame.placement[v]!))
      }
    }

    // Instead of doing an efficient parallel move, for now we will simply push
    // all the used variables to the stack and then pop them again.
    if phiMoves.count % 2 == 1 {
      phiMoves.append((.register(0), .register(0)))
    }

    var result = [String]()
    if !phiMoves.isEmpty {
      result.append("sub sp, sp, #\(phiMoves.count*8)")
      for i in stride(from: 0, to: phiMoves.count, by: 2) {
        let (source1, _) = phiMoves[i]
        let (source2, _) = phiMoves[i + 1]
        result.append("stp x\(source1), x\(source2), [sp, #\(i*8)]")
      }
      for i in stride(from: 0, to: phiMoves.count, by: 2) {
        let (_, target1) = phiMoves[i]
        let (_, target2) = phiMoves[i + 1]
        result.append("ldp x\(target1), x\(target2), [sp, #\(i*8)]")
      }
      result.append("add sp, sp, #\(phiMoves.count*8)")
    }
    for (target, sourceValue) in phiConsts {
      switch target {
      case .register(let x):
        result.append("mov x\(x), #\(sourceValue)")
      case .stack(let x):
        result.append("mov x8, #\(sourceValue)")
        result.append("str x8, \(frame.stackVarAddress(x))")
      }
    }

    return result
  }

  internal func intoRegister(frame: Frame, argument: CFG.Argument, defaultReg: String) -> (
    [String], String
  ) {
    switch argument {
    case .constInt(let x):
      (["mov \(defaultReg), #\(x)"], defaultReg)
    case .constStr:
      fatalError("constStr not yet supported")
    case .variable(let v):
      switch frame.placement[v]! {
      case .register(let r):
        ([], "x\(r)")
      case .stack(let idx):
        (["ldr \(defaultReg), [x29, #-\(idx*8)]"], defaultReg)
      }
    }
  }

  internal func fromRegister(frame: Frame, target: CFG.SSAVariable) -> ([String], String) {
    switch frame.placement[target]! {
    case .register(let r):
      ([], "x\(r)")
    case .stack(let idx):
      (["str x8, \(frame.stackVarAddress(idx))"], "x8")
    }
  }

  internal func writeRegister(frame: Frame, target: CFG.SSAVariable, source: String) -> [String] {
    switch frame.placement[target]! {
    case .register(let r):
      source == "x\(r)" ? [] : ["mov x\(r), \(source)"]
    case .stack(let idx):
      ["str \(source) \(frame.stackVarAddress(idx))"]
    }
  }

}
