public struct BackendLLVM: Backend {
  public enum CodeLine: Sendable {
    case inst(String)
    case comment(String)
    case symbol(String)
    case raw(String)

    var code: String {
      switch self {
      case .inst(let x): "  \(x)"
      case .comment(let x): x.split(separator: "\n").map { "  ; \($0)" }.joined(separator: "\n")
      case .symbol(let x): "\(x):"
      case .raw(let x): x
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

    public func encode() -> [CodeLine] {
      var result = [CodeLine]()
      for (bytes, id) in strToID.sorted(by: { $0.value < $1.value }) {
        let payload = bytes.map { "i8 \($0)" }.joined(separator: ", ")
        let len = bytes.count
        result.append(
          .raw(
            "@str_const_\(id) = private constant { i64, [\(len) x i8] } { i64 \(len), [\(len) x i8] [\(payload)] }, align 8"
          )
        )
      }
      return result
    }
  }

  public static let libCDeclarations: [CodeLine] = [
    .raw("declare i64 @putchar(i64)"),
    .raw("declare i32 @getchar()"),
    .raw("declare ptr @malloc(i64)"),
    .raw("declare void @free(ptr)"),
  ]

  public static let getcCode: [CodeLine] = [
    .raw("define i64 @getc() {"),
    .inst("%result32 = call i32 @getchar()"),
    .inst("%result = sext i32 %result32 to i64"),
    .inst("ret i64 %result"),
    .raw("}"),
  ]

  public static let strAllocCode: [CodeLine] = [
    .raw("define ptr @str_alloc(i64 %len) {"),
    .inst("%size = add i64 %len, 8"),
    .inst("%ptr = call ptr @malloc(i64 %size)"),
    .inst("store i64 %len, ptr %ptr, align 8"),
    .inst("ret ptr %ptr"),
    .raw("}"),
  ]

  public static let strSetCode: [CodeLine] = [
    .raw("define void @str_set(ptr %p, i64 %idx, i64 %value) {"),
    .inst("%data = getelementptr i8, ptr %p, i64 8"),
    .inst("%bytePtr = getelementptr i8, ptr %data, i64 %idx"),
    .inst("%value8 = trunc i64 %value to i8"),
    .inst("store i8 %value8, ptr %bytePtr, align 1"),
    .inst("ret void"),
    .raw("}"),
  ]

  public static let strGetCode: [CodeLine] = [
    .raw("define i64 @str_get(ptr %p, i64 %idx) {"),
    .inst("%data = getelementptr i8, ptr %p, i64 8"),
    .inst("%bytePtr = getelementptr i8, ptr %data, i64 %idx"),
    .inst("%byte = load i8, ptr %bytePtr, align 1"),
    .inst("%value = zext i8 %byte to i64"),
    .inst("ret i64 %value"),
    .raw("}"),
  ]

  public static let strLenCode: [CodeLine] = [
    .raw("define i64 @str_len(ptr %p) {"),
    .inst("%result = load i64, ptr %p, align 8"),
    .inst("ret i64 %result"),
    .raw("}"),
  ]

  public static let ltCode: [CodeLine] = [
    .raw("define i64 @lt(i64 %x, i64 %y) {"),
    .inst("%tmp = icmp slt i64 %x, %y"),
    .inst("%res = zext i1 %tmp to i64"),
    .inst("ret i64 %res"),
    .raw("}"),
  ]

  public static let gtCode: [CodeLine] = [
    .raw("define i64 @gt(i64 %x, i64 %y) {"),
    .inst("%tmp = icmp sgt i64 %x, %y"),
    .inst("%res = zext i1 %tmp to i64"),
    .inst("ret i64 %res"),
    .raw("}"),
  ]

  public static let eqCode: [CodeLine] = [
    .raw("define i64 @eq(i64 %x, i64 %y) {"),
    .inst("%tmp = icmp eq i64 %x, %y"),
    .inst("%res = zext i1 %tmp to i64"),
    .inst("ret i64 %res"),
    .raw("}"),
  ]

  public let includeComments: Bool

  public init(includeComments: Bool = true) {
    self.includeComments = includeComments
  }

  /// Compile the graph as LLVM IR code.
  public func compileAssembly(cfg: CFG) throws -> String {
    let cfg = renumberSharedNames(cfg: cfg)
    var strTable = StringTable()
    let sortedFuncs = cfg.functions.keys.sorted { (fn1, fn2) in
      cfg.functions[fn1]!.id < cfg.functions[fn2]!.id
    }
    var results = try sortedFuncs.flatMap {
      try compileFunction(cfg: cfg, stringTable: &strTable, fn: $0)
    }
    results.insert(contentsOf: strTable.encode(), at: 0)
    results.insert(
      contentsOf: (Self.getcCode + Self.strGetCode + Self.strSetCode + Self.strLenCode
        + Self.strAllocCode + Self.ltCode + Self.gtCode + Self.eqCode),
      at: 0
    )
    results.insert(contentsOf: Self.libCDeclarations, at: 0)
    return results.map { $0.code }.joined(separator: "\n")
  }

  public func compileFunction(
    cfg: CFG,
    stringTable: inout StringTable,
    fn: Function
  ) throws -> [CodeLine] {
    let entrypoint = cfg.functions[fn]!
    let allNodeCode = cfg.dfsFrom(node: entrypoint).flatMap { node in
      var bodyCode = [CodeLine]()
      for (i, inst) in cfg.nodeCode[node]!.instructions.enumerated() {
        if includeComments {
          bodyCode.append(.comment("instruction: \(inst)"))
        }
        bodyCode.append(
          contentsOf: encodeInstruction(
            cfg: cfg,
            node: node,
            stringTable: &stringTable,
            inst: inst,
            idx: i
          )
        )
      }
      let header = [CodeLine.symbol("cfg_node_\(node.id)")]
      var tail = [CodeLine]()
      switch cfg.successors[node] {
      case .single(let nextNode):
        tail.append(.inst("br label %cfg_node_\(nextNode.id)"))
      case .branch(let ifFalse, let ifTrue):
        let falseSymbol = "cfg_node_\(ifFalse.id)"
        let trueSymbol = "cfg_node_\(ifTrue.id)"
        // The special _cond variable is created by the .check instruction.
        tail.append(
          .inst("br i1 %cfg_node_\(node.id)_cond, label %\(trueSymbol), label %\(falseSymbol)")
        )
      case .none: ()
      }
      return header + bodyCode + tail
    }

    return [.raw(encodeFunctionDefinition(fn: fn))] + allNodeCode + [.raw("}")]
  }

  internal func encodeFunctionDefinition(fn: Function) -> String {
    let returnType = nameFor(retType: fn.signature.ret)
    let argTypes = fn.signature.args.enumerated().map { (i, arg) in
      return "\(nameFor(dataType: arg)) %arg\(i)"
    }
    return "define \(returnType) @\(symbolName(fn: fn))(\(argTypes.joined(separator: ", "))) {"
  }

  internal func symbolName(fn function: Function) -> String {
    if function.name == "main" && function.signature.args.isEmpty {
      "main"
    } else {
      "\(function.name)_T\(function.signature.args.map { $0.description }.joined())"
    }
  }

  internal func encodeInstruction(
    cfg: CFG,
    node: CFG.Node,
    stringTable: inout StringTable,
    inst: CFG.Inst,
    idx: Int
  ) -> [CodeLine] {
    switch inst.op {
    case .check(let arg):
      // The end-of-node branch instruction will check this specific symbol.
      let resultSymbol = "cfg_node_\(node.id)_cond"
      switch arg {
      case .constInt(let x):
        return [.inst("%\(resultSymbol) = add i1 0, \(x == 0 ? 0 : 1)")]
      case .constStr(let x):
        return [.inst("%\(resultSymbol) = add i1 0, \(x.count == 0 ? 0 : 1)")]
      case .variable(let v):
        switch v.variable.type {
        case .integer:
          return [.inst("%\(resultSymbol) = icmp ne i64 %\(nameFor(variable: v)), 0")]
        case .string:
          return [
            .inst("%\(resultSymbol)_len = load i64, ptr %\(nameFor(variable: v)), align 8"),
            .inst("%\(resultSymbol) = icmp ne i64 %\(resultSymbol)_len, 0"),
          ]
        }
      }
    case .funcArg(let v, let idx):
      return [createAlias(from: "arg\(idx)", into: v)]
    case .copy(let v, let value):
      let name = nameFor(variable: v)
      switch value {
      case .constInt(let x):
        return [.inst("%\(name) = add i64 0, \(x)")]
      case .constStr(let x):
        return [.inst("%\(name) = getelementptr i64, ptr @\(stringTable.symbol(x)), i32 0")]
      case .variable(let source):
        return [createAlias(from: source, into: v)]
      }
    case .returnVoid:
      return [.inst("ret void")]
    case .returnValue(let arg):
      return [.inst("ret \(argumentToExpression(arg: arg, stringTable: &stringTable))")]
    case .phi(let v, let sources):
      let joinedSources = sources.map { (node, value) in
        "[\(argumentToExpression(arg: value, stringTable: &stringTable, includeType: false)), %cfg_node_\(node.id)]"
      }.joined(separator: ", ")
      return [
        .inst(
          "%\(nameFor(variable: v)) = phi \(nameFor(dataType: v.variable.type)) \(joinedSources)"
        )
      ]
    case .call(let fn, let args):
      switch fn.builtIn {
      case .putc:
        return [
          encodeFunctionCall(
            stringTable: &stringTable, symbol: "putchar", retType: nil, args: args
          )
        ]
      case .strFree:
        return [
          encodeFunctionCall(
            stringTable: &stringTable, symbol: "free", retType: nil, args: args
          )
        ]
      case .getc:
        return [
          encodeFunctionCall(
            stringTable: &stringTable, symbol: "getc", retType: .integer, args: args
          )
        ]
      case .strSet:
        return [
          encodeFunctionCall(
            stringTable: &stringTable, symbol: "str_set", retType: .integer, args: args
          )
        ]
      case .none:
        return [encodeFunctionCall(stringTable: &stringTable, fn: fn, args: args)]
      default: return []
      }
    case .callAndStore(let target, let fn, let args):
      func singleInst(_ code: String) -> [CodeLine] {
        let joinedArgs = args.enumerated().map { (i, arg) in
          argumentToExpression(arg: arg, stringTable: &stringTable, includeType: i == 0)
        }.joined(separator: ", ")
        return [.inst("%\(nameFor(variable: target)) = \(code) \(joinedArgs)")]
      }
      func intCall(_ name: String) -> [CodeLine] {
        return [
          encodeFunctionCall(
            stringTable: &stringTable,
            symbol: name,
            retType: .integer,
            args: args,
            target: target
          )
        ]
      }
      switch fn.builtIn {
      case .add: return singleInst("add")
      case .sub: return singleInst("sub")
      case .and: return singleInst("and")
      case .or: return singleInst("or")
      case .xor: return singleInst("xor")
      case .shl: return singleInst("shl")
      case .shr: return singleInst("ashr")
      case .mul: return singleInst("mul")
      case .div: return singleInst("sdiv")
      case .mod: return singleInst("srem")
      case .lt: return intCall("lt")
      case .gt: return intCall("gt")
      case .eqInt: return intCall("eq")
      case .len: return intCall("str_len")
      case .getc: return intCall("getc")
      case .strGet: return intCall("str_get")
      case .notInt:
        return [
          encodeFunctionCall(
            stringTable: &stringTable,
            symbol: "eq",
            retType: .integer,
            args: [args[0], .constInt(0)],
            target: target
          )
        ]
      case .strAlloc:
        return [
          encodeFunctionCall(
            stringTable: &stringTable,
            symbol: "str_alloc",
            retType: .string,
            args: args,
            target: target
          )
        ]
      case .strFree, .strSet, .putc:
        fatalError("cannot assign return of call")
      case .none:
        return [
          encodeFunctionCall(
            stringTable: &stringTable, fn: fn, args: args, target: target
          )
        ]
      }
    }
  }

  internal func encodeFunctionCall(
    stringTable: inout StringTable,
    fn: Function,
    args: [CFG.Argument],
    target: CFG.SSAVariable? = nil
  ) -> CodeLine {
    encodeFunctionCall(
      stringTable: &stringTable,
      symbol: symbolName(fn: fn),
      retType: fn.signature.ret,
      args: args,
      target: target
    )
  }

  internal func encodeFunctionCall(
    stringTable: inout StringTable,
    symbol: String,
    retType: Variable.DataType?,
    args: [CFG.Argument],
    target: CFG.SSAVariable? = nil
  ) -> CodeLine {
    let joinedArgs = args.map { arg in
      argumentToExpression(arg: arg, stringTable: &stringTable)
    }.joined(separator: ", ")
    var result = "call \(nameFor(retType: retType)) @\(symbol)(\(joinedArgs))"
    if let t = target {
      result = "%\(nameFor(variable: t)) = \(result)"
    }
    return .inst(result)
  }

  internal func argumentToExpression(
    arg: CFG.Argument,
    stringTable: inout StringTable,
    includeType: Bool = true
  ) -> String {
    switch arg {
    case .constInt(let x):
      includeType ? "i64 \(x)" : "\(x)"
    case .constStr(let x):
      (includeType ? "ptr " : "") + "@\(stringTable.symbol(x))"
    case .variable(let v):
      (includeType ? "\(nameFor(dataType: v.variable.type)) " : "") + "%\(nameFor(variable: v))"
    }
  }

  internal func createAlias(from: String, into: CFG.SSAVariable) -> CodeLine {
    createAlias(from: from, into: nameFor(variable: into), type: into.variable.type)
  }

  internal func createAlias(from: CFG.SSAVariable, into: CFG.SSAVariable) -> CodeLine {
    createAlias(
      from: nameFor(variable: from),
      into: nameFor(variable: into),
      type: into.variable.type
    )
  }

  internal func createAlias(from: String, into: String, type: Variable.DataType) -> CodeLine {
    switch type {
    case .integer:
      .inst("%\(into) = add i64 %\(from), 0")
    case .string:
      .inst(
        "%\(into) = getelementptr i64, ptr %\(from), i32 0"
      )
    }
  }

  internal func nameFor(dataType: Variable.DataType) -> String {
    switch dataType {
    case .integer: "i64"
    case .string: "ptr"
    }
  }

  internal func nameFor(retType: Variable.DataType?) -> String {
    switch retType {
    case .integer: "i64"
    case .string: "ptr"
    case .none: "void"
    }
  }

  internal func nameFor(variable: CFG.SSAVariable) -> String {
    if variable.variable.isTemporary {
      return variable.variable.name.replacing("[", with: "_").replacing("]", with: "_")
    } else if variable.variable.name == CFG.ReturnValueName {
      return "returnValue\(variable.version!)"
    } else {
      return "user_\(variable.variable.name)_\(variable.version!)"
    }
  }

  struct NameAndVersion: Hashable {
    let name: String
    let version: Int
  }

  /// Some variables might have identical names and version numbers,
  /// but point to logically different variables in the SSA.
  /// This procedure creates new unique version numbers for these variables
  /// so that the variable naming procedure doesn't create collisions.
  internal func renumberSharedNames(cfg: CFG) -> CFG {
    var cfg = cfg
    for root in cfg.functions.values {
      let nodes = cfg.dfsFrom(node: root)
      var seenVersions = [String: Set<Int>]()
      for node in nodes {
        for inst in cfg.nodeCode[node]!.instructions {
          for def in inst.op.defs {
            seenVersions[def.variable.name, default: []].insert(def.version!)
          }
        }
      }

      var rewriteMap = [CFG.SSAVariable: CFG.SSAVariable]()
      var covered = Set<NameAndVersion>()
      for node in nodes {
        for inst in cfg.nodeCode[node]!.instructions {
          for def in inst.op.defs {
            let naiveKey = NameAndVersion(name: def.variable.name, version: def.version!)
            if !covered.contains(naiveKey) {
              covered.insert(naiveKey)
            } else {
              // This version is duplicated with another variable.
              let taken = seenVersions[def.variable.name]!
              let nextVersion = (0...Int.max).first { !taken.contains($0) }!
              seenVersions[def.variable.name]!.insert(nextVersion)  // Never pick this one again
              let newVar = CFG.SSAVariable(variable: def.variable, version: nextVersion)
              rewriteMap[def] = newVar
              seenVersions[def.variable.name, default: []].insert(def.version!)
            }
          }
        }
      }

      for node in nodes {
        var code = cfg.nodeCode[node]!
        for (old, new) in rewriteMap {
          code = code.replacing(old, with: new)
        }
        cfg.nodeCode[node] = code
      }
    }

    return cfg
  }

}
