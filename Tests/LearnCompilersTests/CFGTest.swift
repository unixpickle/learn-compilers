import LearnParsers
import Testing

@testable import LearnCompilers

@Test func testInitialCFG() throws {
  let code = """
      fn main(x: int, y: str) -> int {
        print(x)
        print(y)

        y: int = x
        print(y)

        while? (lt(x, add(5,5))) {
          if? (eq(x, 5)) {
            return!(x)
          }
          if? (eq(x, 20)) {
            break!()
          }
          x = add(x, 1)
        }
        return!(sub(y, 10))
      }

      fn print(x: int) {
      }

      fn print(x: str) {
        while? (1) {
          return!()
        }
      }

      fn lt(x: int, y: int) -> int {
        return!(x)
      }

      fn eq(x: int, y: int) -> int {
        return!(sub(x, y))
      }

      fn sub(x: int, y: int) -> int {
        return!(x)
      }

      fn add(x: int, y: int) -> int {
        return!(x)
      }
    """

  let match = try Parser.parse(code)
  var ast = AST(match: match)
  #expect(ast.codeString == code)

  var table = ScopeTable()
  var errors: [ASTDecorationError]
  (ast, errors) = ast.decorated(table: &table, fileID: "stdin")

  #expect(errors.isEmpty)
  if !errors.isEmpty {
    return
  }

  let cfg = CFG(ast: ast)
  let mainFunction = cfg.functions.keys.compactMap { key in key.name == "main" ? key : nil }.first!
  let mainEntrypoint = cfg.functions[mainFunction]!
  let mainRetVar = CFG.SSAVariable(variable: cfg.returnVars[mainFunction]!)

  let mainFunctionDecl = ast.functions[0]
  let arg0 = CFG.SSAVariable(variable: mainFunctionDecl.args[0].identifier.variable!)
  let arg1 = CFG.SSAVariable(variable: mainFunctionDecl.args[1].identifier.variable!)

  let printIntDecl = ast.functions[1]
  let printStrDecl = ast.functions[2]

  let redecY = CFG.SSAVariable(
    variable: mainFunctionDecl.block.statements[2].statement.varDecl!.identifier.variable!)

  let mainCode = cfg.nodeCode[mainEntrypoint]!
  #expect(mainCode.instructions.count == 6)
  if mainCode.instructions.count != 6 {
    return
  }
  #expect(mainCode.instructions[0].op == .funcArg(arg0, 0))
  #expect(mainCode.instructions[1].op == .funcArg(arg1, 1))
  #expect(mainCode.instructions[2].op == .call(printIntDecl.function!, [.variable(arg0)]))
  #expect(mainCode.instructions[3].op == .call(printStrDecl.function!, [.variable(arg1)]))
  #expect(mainCode.instructions[4].op == .copy(redecY, .variable(arg0)))
  #expect(mainCode.instructions[5].op == .call(printIntDecl.function!, [.variable(redecY)]))

  guard case .single(let whileCondNode) = cfg.successors[mainEntrypoint] else {
    #expect(Bool(false), "missing while cond block")
    return
  }

  let whileCondCode = cfg.nodeCode[whileCondNode]!
  #expect(whileCondCode.instructions.count == 3)
  if whileCondCode.instructions.count != 3 {
    return
  }
  guard case .callAndStore(let tmp1, let addFn, let fiveFive) = whileCondCode.instructions[0].op
  else {
    #expect(Bool(false), "unexpected op \(whileCondCode.instructions[0])")
    return
  }
  #expect(fiveFive == [.constInt(5), .constInt(5)])
  #expect(addFn.name == "add")
  guard case .callAndStore(let tmp2, let ltFn, let ltArgs) = whileCondCode.instructions[1].op else {
    #expect(Bool(false), "unexpected op \(whileCondCode.instructions[1])")
    return
  }
  #expect(ltFn.name == "lt")
  #expect(ltArgs == [.variable(arg0), .variable(tmp1)])
  #expect(whileCondCode.instructions[2].op == .check(.variable(tmp2)))

  guard case .branch(let whileFinishNode, let whileBodyNode) = cfg.successors[whileCondNode] else {
    #expect(Bool(false), "unexpected branch")
    return
  }

  let whileFinishCode = cfg.nodeCode[whileFinishNode]!
  #expect(whileFinishCode.instructions.count == 2)
  guard
    case .callAndStore(let returnTmp, let subFn, let subArgs) = whileFinishCode.instructions[0].op
  else {
    #expect(Bool(false))
    return
  }
  #expect(subFn.name == "sub")
  #expect(subArgs == [.variable(redecY), .constInt(10)])
  #expect(whileFinishCode.instructions[1].op == .copy(mainRetVar, .variable(returnTmp)))
  guard case .single(let mainTail) = cfg.successors[whileFinishNode] else {
    #expect(Bool(false))
    return
  }
  let mainTailCode = cfg.nodeCode[mainTail]!
  #expect(mainTailCode.instructions.count == 1)
  #expect(mainTailCode.instructions[0].op == .returnValue(.variable(mainRetVar)))

  let whileBodyCode = cfg.nodeCode[whileBodyNode]!
  #expect(whileBodyCode.instructions.count == 2)

  // Processing the if statement starts with a temporary variable for the eq.
  //
  //     if? (eq(x, 5)) {
  //       return!(x)
  //     }
  //
  guard case .callAndStore(let tmp3, let eqFn, let eqArgs) = whileBodyCode.instructions[0].op else {
    #expect(Bool(false))
    return
  }
  #expect(eqFn.name == "eq")
  #expect(eqArgs == [.variable(arg0), .constInt(5)])
  #expect(whileBodyCode.instructions[1].op == .check(.variable(tmp3)))

  guard case .branch(let ifFalseBranch, let ifTrueBranch) = cfg.successors[whileBodyNode] else {
    #expect(Bool(false), "unexpected successors for if statement")
    return
  }

  let ifTrueBranchCode = cfg.nodeCode[ifTrueBranch]!
  #expect(ifTrueBranchCode.instructions.count == 1)
  #expect(ifTrueBranchCode.instructions[0].op == .copy(mainRetVar, .variable(arg0)))
  #expect(cfg.successors[ifTrueBranch] == .single(mainTail))

  // False branch handles
  //
  //     if? (eq(x, 20)) {
  //       break!()
  //     }
  //     x = add(x, 1)
  //
  let secondIfCode = cfg.nodeCode[ifFalseBranch]!
  #expect(secondIfCode.instructions.count == 2)
  guard case .callAndStore(let tmp4, let eqFn1, let eqArgs1) = secondIfCode.instructions[0].op
  else {
    #expect(Bool(false))
    return
  }
  #expect(eqFn1 == eqFn)
  #expect(eqArgs1 == [.variable(arg0), .constInt(20)])
  #expect(secondIfCode.instructions[1].op == .check(.variable(tmp4)))
  guard case .branch(let ifFalseBranch1, let ifTrueBranch1) = cfg.successors[ifFalseBranch] else {
    #expect(Bool(false), "unexpected successors for if statement")
    return
  }

  let ifTrueBranch1Code = cfg.nodeCode[ifTrueBranch1]!
  #expect(ifTrueBranch1Code.instructions.isEmpty)
  #expect(cfg.successors[ifTrueBranch1]! == .single(whileFinishNode))

  let ifFalseBranch1Code = cfg.nodeCode[ifFalseBranch1]!
  #expect(ifFalseBranch1Code.instructions.count == 2)
  guard case .callAndStore(let dst, let addFn, let addArgs) = ifFalseBranch1Code.instructions[0].op
  else {
    #expect(Bool(false))
    return
  }
  #expect(addFn.name == "add")
  #expect(addArgs == [.variable(arg0), .constInt(1)])
  #expect(ifFalseBranch1Code.instructions[1].op == .copy(arg0, .variable(dst)))
  #expect(cfg.successors[ifFalseBranch1]! == .single(whileCondNode))
}

@Test func testCFGIntoSSA() throws {
  let code = """
      fn main(x: int, y: str) -> int {
        print(x)
        print(y)

        y: int = x
        print(y)

        while? (lt(x, add(5,5))) {
          if? (eq(x, 5)) {
            return!(x)
          }
          if? (eq(x, 20)) {
            break!()
          }
          x = add(x, 1)
        }
        return!(sub(y, 10))
      }

      fn print(x: int) {
      }

      fn print(x: str) {
        while? (1) {
          return!()
        }
      }

      fn lt(x: int, y: int) -> int {
        return!(x)
      }

      fn eq(x: int, y: int) -> int {
        return!(sub(x, y))
      }

      fn sub(x: int, y: int) -> int {
        return!(x)
      }

      fn add(x: int, y: int) -> int {
        return!(x)
      }
    """

  let match = try Parser.parse(code)
  var ast = AST(match: match)
  #expect(ast.codeString == code)

  var table = ScopeTable()
  var errors: [ASTDecorationError]
  (ast, errors) = ast.decorated(table: &table, fileID: "stdin")

  #expect(errors.isEmpty)
  if !errors.isEmpty {
    return
  }

  var cfg = CFG(ast: ast)
  try cfg.insertPhiAndNumberVars()
  checkSSA(cfg: cfg)
  checkPhi(cfg: cfg)
}

@Test func testCFGIntoSSAWithMissingReturn() throws {
  let code = """
      fn main(x: int, y: str) -> str {
        if? (x) {
          return!(y)
        }
      }
    """

  let match = try Parser.parse(code)
  var ast = AST(match: match)
  #expect(ast.codeString == code)

  var table = ScopeTable()
  var errors: [ASTDecorationError]
  (ast, errors) = ast.decorated(table: &table, fileID: "stdin")

  #expect(errors.isEmpty)
  if !errors.isEmpty {
    return
  }

  var cfg = CFG(ast: ast)

  #expect(throws: SSAError.self) {
    var c1 = cfg
    try c1.insertPhiAndNumberVars(allowMissingReturn: false)
  }
  try cfg.insertPhiAndNumberVars(allowMissingReturn: true)
  checkSSA(cfg: cfg)
}

@Test func testCFGIntoSSASelfAssign() throws {
  let code = """
      fn main(x: int, y: str) {
        x = x
      }
    """

  let match = try Parser.parse(code)
  var ast = AST(match: match)
  #expect(ast.codeString == code)

  var table = ScopeTable()
  var errors: [ASTDecorationError]
  (ast, errors) = ast.decorated(table: &table, fileID: "stdin")

  #expect(errors.isEmpty)
  if !errors.isEmpty {
    return
  }

  var cfg = CFG(ast: ast)

  try cfg.insertPhiAndNumberVars()
  checkSSA(cfg: cfg)
  checkPhi(cfg: cfg)
}

/// Check SSA condition that variable versions are only assigned once.
func checkSSA(cfg: CFG) {
  var assignmentCount = [CFG.SSAVariable: Int]()
  for node in cfg.nodes {
    for inst in cfg.nodeCode[node]!.instructions {
      for def in inst.op.defs {
        assignmentCount[def, default: 0] += 1
      }
    }
  }
  for (v, count) in assignmentCount {
    #expect(count == 1, "variable \(v) assigned \(count) times")
  }

  // Ensure no instruction uses a variable it also defines.
  for node in cfg.nodes {
    for inst in cfg.nodeCode[node]!.instructions {
      let uses = Set(inst.op.uses)
      #expect(inst.op.defs.filter(uses.contains).isEmpty)
    }
  }
}

/// Check that all phi functions are filled in
func checkPhi(cfg: CFG) {
  for node in cfg.nodes {
    let preds = cfg.predecessors[node, default: []]
    let phis = cfg.nodeCode[node]!.instructions.filter {
      if case .phi = $0.op {
        true
      } else {
        false
      }
    }
    if preds.count == 0 {
      #expect(phis.count == 0, "root node should not have phis")
      continue
    }

    for inst in cfg.nodeCode[node]!.instructions {
      if case .phi(_, let branches) = inst.op {
        #expect(
          Set(branches.keys) == Set(preds),
          "phi function is missing or has extra source(s)"
        )
      }
    }
  }
}
