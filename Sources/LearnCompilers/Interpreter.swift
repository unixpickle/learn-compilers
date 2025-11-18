open class Interpreter {
  public class MutableString {
    public var data: [UInt8]
    public var count: Int { data.count }
    public var isEmpty: Bool { data.isEmpty }

    public init(count: Int) {
      self.data = [UInt8](repeating: 0, count: count)
    }

    public init(_ data: [UInt8]) {
      self.data = data
    }

    public init(_ str: String) {
      self.data = Array(str.utf8)
    }

    public subscript(_ idx: Int) -> UInt8 {
      get {
        data[idx]
      }
      set {
        data[idx] = newValue
      }
    }
  }

  public enum VarValue {
    case integer(Int64)
    case string(MutableString)

    public var dataType: Variable.DataType {
      switch self {
      case .integer: .integer
      case .string: .string
      }
    }

    public var integer: Int64? {
      if case .integer(let x) = self {
        x
      } else {
        nil
      }
    }

    public var string: MutableString? {
      if case .string(let x) = self {
        x
      } else {
        nil
      }
    }
  }

  public enum InterpreterError: Error {
    case mismatchedArgumentCount(String)
    case mismatchedArgumentTypes(String)
  }

  public typealias StackEntry = (
    node: CFG.Node,
    nextInst: Int,
    vars: [CFG.SSAVariable: VarValue],
    args: [VarValue],
    prevNode: CFG.Node?
  )

  public let cfg: CFG
  public var stack = [StackEntry]()
  public var returnValue: VarValue? = nil

  public init(cfg: CFG, entrypoint: Function, arguments: [VarValue]) throws {
    self.cfg = cfg
    if arguments.count != entrypoint.signature.args.count {
      throw InterpreterError.mismatchedArgumentCount(
        "expected \(entrypoint.signature.args.count) arguments but got \(arguments.count)"
      )
    }
    if arguments.map({ $0.dataType }) != entrypoint.signature.args {
      throw InterpreterError.mismatchedArgumentTypes(
        "expected types \(entrypoint.signature.args) but got \(arguments.map { $0.dataType })"
      )
    }
    stack = [
      (
        node: cfg.functions[entrypoint]!,
        nextInst: 0,
        vars: [:],
        args: arguments,
        prevNode: nil
      )
    ]
  }

  public func run() -> VarValue? {
    while step() {}
    return returnValue
  }

  public func step() -> Bool {
    guard let state = stack.popLast() else {
      return false
    }
    let insts = cfg.nodeCode[state.node]!.instructions
    assert(state.nextInst <= insts.count)

    // We might need to advance to the next node.
    if state.nextInst == insts.count {
      guard let succ = cfg.successors[state.node], case .single(let nextNode) = succ else {
        fatalError(
          "hit end of block when successors are \(String(describing: cfg.successors[state.node]))"
        )
      }
      stack.append(
        (node: nextNode, nextInst: 0, vars: state.vars, args: state.args, prevNode: state.node)
      )
      return true
    }

    func argToVarValue(_ arg: CFG.Argument) -> VarValue {
      switch arg {
      case .constInt(let x):
        .integer(x)
      case .constStr(let x):
        .string(MutableString(x))
      case .variable(let v):
        state.vars[v]!
      }
    }

    let op = insts[state.nextInst].op

    switch op {
    case .check(let argument):
      // Determine a branch and push the new node to the stack.
      assert(state.nextInst == insts.count - 1, "check instruction must come at end of block")
      guard
        let succ = cfg.successors[state.node],
        case .branch(let ifFalse, let ifTrue) = succ
      else {
        fatalError("expected branch successors after check instruction")
      }
      let nextNode =
        if check(value: argument, vars: state.vars) {
          ifTrue
        } else {
          ifFalse
        }
      stack.append(
        (node: nextNode, nextInst: 0, vars: state.vars, args: state.args, prevNode: state.node)
      )
    case .phi(let target, let sources):
      guard let prevNode = state.prevNode else {
        fatalError("entrypoint node has phi function")
      }
      guard let sourceVar = sources[prevNode] else {
        fatalError("phi argument not defined for source node")
      }
      var nextState = state
      nextState.vars[target] = argToVarValue(sourceVar)
      nextState.nextInst += 1
      stack.append(nextState)
    case .returnValue(let valueArg):
      let value = argToVarValue(valueArg)

      if var prevState = stack.popLast() {
        // This was a function call and we should write a return.
        let prevCode = cfg.nodeCode[prevState.node]!
        let prevOp = prevCode.instructions[prevState.nextInst - 1].op
        if case .callAndStore(let target, _, _) = prevOp {
          prevState.vars[target] = value
        }
        stack.append(prevState)
      } else {
        // This must have been the entrypoint.
        returnValue = value
        return false
      }
    case .returnVoid:
      if stack.isEmpty {
        return false
      }
    case .funcArg(let v, let argIdx):
      var newState = state
      newState.nextInst += 1
      newState.vars[v] = state.args[argIdx]
      stack.append(newState)
    case .call(let fn, let args):
      var returnState = state
      returnState.nextInst += 1
      stack.append(returnState)

      let argValues = args.map(argToVarValue)

      if fn.builtIn != nil {
        runBuiltIn(fn: fn, args: argValues)
      } else {
        let entryNode = cfg.functions[fn]!
        stack.append(
          (
            node: entryNode,
            nextInst: 0,
            vars: [:],
            args: argValues,
            prevNode: nil
          )
        )
      }
    case .callAndStore(let target, let fn, let args):
      var returnState = state
      returnState.nextInst += 1

      let argValues = args.map(argToVarValue)

      if let builtIn = fn.builtIn {
        guard let result = runBuiltIn(fn: fn, args: argValues) else {
          fatalError("built-in for \(builtIn) returned nil unexpectedly")
        }
        returnState.vars[target] = result
        stack.append(returnState)
      } else {
        stack.append(returnState)
        let entryNode = cfg.functions[fn]!
        stack.append(
          (
            node: entryNode,
            nextInst: 0,
            vars: [:],
            args: argValues,
            prevNode: nil
          )
        )
      }
    case .copy(let target, let source):
      var newState = state
      newState.nextInst += 1
      newState.vars[target] = argToVarValue(source)
      stack.append(newState)
    }
    return true
  }

  /// Convert an argument into a boolean for a branch condition.
  public func check(value: CFG.Argument, vars: [CFG.SSAVariable: VarValue]) -> Bool {
    switch value {
    case .variable(let v):
      switch vars[v]! {
      case .integer(let x):
        x != 0
      case .string(let x):
        !x.isEmpty
      }
    case .constInt(let x):
      x != 0
    case .constStr(let x):
      !x.isEmpty
    }
  }

  @discardableResult
  public func runBuiltIn(fn: Function, args: [VarValue]) -> VarValue? {
    guard let builtIn = fn.builtIn else {
      fatalError("runBuiltIn called with non-built-in function")
    }
    switch builtIn {
    case .add:
      return .integer(args[0].integer! + args[1].integer!)
    case .sub:
      return .integer(args[0].integer! - args[1].integer!)
    case .mul:
      return .integer(args[0].integer! * args[1].integer!)
    case .div:
      return .integer(args[0].integer! / args[1].integer!)
    case .mod:
      return .integer(args[0].integer! % args[1].integer!)
    case .notInt:
      return .integer(args[0].integer! == 0 ? 1 : 0)
    case .eqInt:
      return .integer(args[0].integer! == args[1].integer! ? 1 : 0)
    case .lt:
      return .integer(args[0].integer! < args[1].integer! ? 1 : 0)
    case .gt:
      return .integer(args[0].integer! > args[1].integer! ? 1 : 0)
    case .len:
      return .integer(Int64(args[0].string!.count))
    case .or:
      return .integer(args[0].integer! | args[1].integer!)
    case .and:
      return .integer(args[0].integer! & args[1].integer!)
    case .putc:
      print(Character(UnicodeScalar(UInt8(args[0].integer!))), terminator: "")
      return nil
    case .strAlloc:
      return .string(.init(count: Int(args[0].integer!)))
    case .strFree:
      return nil
    case .strGet:
      return .integer(Int64(args[0].string![Int(args[1].integer!)]))
    case .strSet:
      args[0].string![Int(args[1].integer!)] = UInt8(args[2].integer!)
      return nil
    }
  }
}
