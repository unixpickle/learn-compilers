public struct BackendAArch64: Backend {

  internal enum VarPlacement {
    case register(Int)
    case stack(Int)
  }

  internal struct Frame {
    let function: Function
    let placement: [CFG.SSAVariable: VarPlacement]
    let stackAllocation: Int  // measured in 64-bit values
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
      // TODO: phi moves for each outgoing branch
      // TODO: implement trailing check instruction
      return "\(header)\n\(bodyCode)"
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

    // If there's any function calls, don't use caller-saved registers (for now).
    let availRegs: [VarPlacement] =
      if maybeMaxCallArgs != nil {
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

    // We may have to store arguments and/or spilled registers
    var stackAllocation = stackVarCount + max(0, (maybeMaxCallArgs ?? 0) - 8)
    // Keep stack 16-byte aligned
    if stackAllocation % 2 != 0 {
      stackAllocation += 1
    }

    return Frame(
      function: fn,
      placement: varColors.mapValues { slots[$0] },
      stackAllocation: stackAllocation
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
    if frame.function.name == "main" && frame.function.signature.args.isEmpty {
      "_main"
    } else {
      "_\(frame.function.name)_T\(frame.function.signature.args.map { $0.description }.joined())"
    }
  }

  internal func encodePrologue(cfg: CFG, frame: Frame) -> String {
    let symbolName = symbolName(frame: frame)
    let alloc = "sub sp, sp, #\(frame.stackAllocation*8+16)"
    let backup = "stp x29, x30, [sp, #\(frame.stackAllocation*8)]"
    let fpCode = "add x29, sp, #\(frame.stackAllocation*8)"
    // TODO: backup used callee-saved registers
    return ".globl \(symbolName)\n.p2align 2\n\(symbolName):\n  \(alloc)\n  \(backup)\n  \(fpCode)"
  }

  internal func encodeEpilogue(cfg: CFG, frame: Frame) -> String {
    // TODO: restore used callee-saved registers
    let symbolName = symbolName(frame: frame)
    return "\(symbolName)_epilogue:\n"
      + "  ldp x29, x30, [sp, #\(frame.stackAllocation*8)]\n"
      + "  add sp, sp, #\(frame.stackAllocation*8+16)\n"
      + "  ret"
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
        case .stack(let offset):
          return ["mov x8, #\(x)", "str x8, [x29, #-\(offset*8)]"]
        }
      case .constStr:
        fatalError("string constants are not yet supported")
      case .variable(let sourceVar):
        switch frame.placement[target]! {
        case .register(let targetReg):
          switch frame.placement[sourceVar]! {
          case .register(let sourceReg):
            return ["mov x\(targetReg), x\(sourceReg)"]
          case .stack(let offset):
            return ["ldr x\(targetReg), [x29, #-\(offset*8)]"]
          }
        case .stack(let targetOffset):
          switch frame.placement[sourceVar]! {
          case .register(let sourceReg):
            return ["str x\(sourceReg), [x29, #-\(targetOffset*8)]"]
          case .stack(let sourceOffset):
            return ["ldr x8, [x29, #-\(sourceOffset*8)]", "str x8, [x29, #-\(targetOffset*8)]"]
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
    case .check:
      return []
    case .call(let fn, let args):
      if let builtIn = fn.builtIn {
        if case .print = builtIn {
          fatalError("print not implemented")
        }
        return []
      }
      // TODO: do call here
      fatalError("func calls not implemented")
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
      // TODO: do function call here.
      fatalError("func calls not implemented")
    case .returnValue(let arg):
      let (insts, r) = intoRegister(frame: frame, argument: arg, defaultReg: "x0")
      return insts + (r == "x0" ? [] : ["mov x0, \(r)"]) + [
        "br \(symbolName(frame: frame))_epilogue"
      ]
    case .returnVoid:
      return ["br \(symbolName(frame: frame))_epilogue"]
    }
    fatalError()
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
      (["str x8, [x29, #-\(idx*8)]"], "x8")
    }
  }

}
