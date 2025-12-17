/// Helpers for implementing comparisons efficiently in backends.
public struct Comparisons {

  // Pseudo-functions for various comparison operations and unary checks.
  public enum Op {
    case checkStr
    case checkInt
    case eqInt
    case notEqInt
    case lt
    case gt
  }

  public let opToFn: [Op: Function]
  public let fnToOp: [Function: Op]

  public init() {
    var ops = [Op: Function]()
    for op in [Op.eqInt, .notEqInt, .lt, .gt] {
      ops[op] = Function(
        declarationPosition: Position(fileID: ""),
        name: "\(op)",
        signature: .init(args: [.integer, .integer], ret: nil)
      )
    }
    ops[.checkInt] = Function(
      declarationPosition: Position(fileID: ""),
      name: "checkInt",
      signature: .init(args: [.integer], ret: nil)
    )
    ops[.checkStr] = Function(
      declarationPosition: Position(fileID: ""),
      name: "checkStr",
      signature: .init(args: [.string], ret: nil)
    )

    opToFn = ops
    fnToOp = Dictionary(uniqueKeysWithValues: ops.map { ($0.value, $0.key) })
  }

  internal func countUsage(cfg: CFG) -> [CFG.SSAVariable: Int] {
    var useCount = [CFG.SSAVariable: Int]()
    for code in cfg.nodeCode.values {
      for inst in code.instructions {
        for use in inst.op.uses {
          useCount[use, default: 0] += 1
        }
      }
    }
    return useCount
  }

  /// Convert .check ops (and possibly preceding function calls) into comparisons.
  public func translateOps(cfg: CFG) -> CFG {
    var cfg = cfg
    let useCount = countUsage(cfg: cfg)

    for (node, code) in Array(cfg.nodeCode) {
      guard case .check(let arg) = code.instructions.last?.op else {
        continue
      }
      let naiveOp: Op =
        switch arg.dataType {
        case .string: .checkStr
        case .integer: .checkInt
        }
      var newCode = code
      newCode.instructions.removeLast()
      var callOp = CFG.Inst.Op.call(opToFn[naiveOp]!, [arg])

      if case .variable(let v) = arg, useCount[v]! == 1, let prevInst = newCode.instructions.last {
        if case .callAndStore(let target, let fn, let args) = prevInst.op, target == v {
          switch fn.builtIn {
          case .eqInt:
            callOp = .call(opToFn[.eqInt]!, args)
            newCode.instructions.removeLast()
          case .lt:
            callOp = .call(opToFn[.lt]!, args)
            newCode.instructions.removeLast()
          case .gt:
            callOp = .call(opToFn[.gt]!, args)
            newCode.instructions.removeLast()
          case .notInt:
            callOp = .call(opToFn[.eqInt]!, [args[0], .constInt(0)])
            newCode.instructions.removeLast()
          default: ()
          }
        }
      }
      newCode.instructions.append(
        CFG.Inst(
          position: code.instructions.last!.position,
          op: callOp
        ))
      cfg.nodeCode[node] = newCode
    }
    return cfg
  }

  /// Invert any branch predicate equating something to a constant.
  /// This should be called after translateOps().
  public func invertConstEqBranches(cfg: CFG) -> CFG {
    var cfg = cfg
    for (node, code) in Array(cfg.nodeCode) {
      guard case .branch(let ifFalse, let ifTrue) = cfg.successors[node] else {
        continue
      }
      guard case .call(let fn, let args) = code.instructions.last?.op else {
        continue
      }
      guard case .eqInt = fnToOp[fn] else {
        continue
      }
      if case .constInt = args[0] {
      } else if case .constInt = args[1] {
      } else {
        continue
      }

      cfg.successors[node] = .branch(ifFalse: ifTrue, ifTrue: ifFalse)
      var newCode = code
      newCode.instructions[newCode.instructions.count - 1].op = .call(opToFn[.notEqInt]!, args)
      cfg.nodeCode[node] = newCode
    }
    return cfg
  }

}
