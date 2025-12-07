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

  let cfg2 = CFG(ast: ast)
  checkCFGEqual(cfg1: cfg, cfg2: cfg2)

  let mainFunction = cfg.functions.keys.compactMap { key in key.name == "main" ? key : nil }.first!
  let mainEntrypoint = cfg.functions[mainFunction]!
  let mainRetVar = CFG.SSAVariable(variable: cfg.returnVariableMap()[mainFunction]!)

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
  cfg.insertPhiAndNumberVars()
  checkReturns(cfg: cfg)
  checkSSA(cfg: cfg)
  checkPhi(cfg: cfg)

  var cfg2 = CFG(ast: ast)
  cfg2.insertPhiAndNumberVars()
  checkCFGEqual(cfg1: cfg, cfg2: cfg2)
}

@Test func testCFGIntoSSANoPhiForArg() throws {
  let code = """
      fn main(x: int) -> int {
        while? (x) {
          main(x)
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
  cfg.insertPhiAndNumberVars()
  checkReturns(cfg: cfg)
  checkSSA(cfg: cfg)
  checkPhi(cfg: cfg)

  var cfg2 = CFG(ast: ast)
  cfg2.insertPhiAndNumberVars()
  checkCFGEqual(cfg1: cfg, cfg2: cfg2)

  for (_, code) in cfg.nodeCode {
    for inst in code.instructions {
      if case .phi = inst.op {
        #expect(Bool(false), "unexpected phi function \(inst.op)")
      }
    }
  }
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
    c1.insertPhiAndNumberVars()
    try c1.checkMissingReturns()
  }
  cfg.insertPhiAndNumberVars()
  checkSSA(cfg: cfg)
  checkReturns(cfg: cfg)

  var cfg2 = CFG(ast: ast)
  cfg2.insertPhiAndNumberVars()
  checkCFGEqual(cfg1: cfg, cfg2: cfg2)
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

  cfg.insertPhiAndNumberVars()
  checkSSA(cfg: cfg)
  checkPhi(cfg: cfg)
  checkReturns(cfg: cfg)

  var cfg2 = CFG(ast: ast)
  cfg2.insertPhiAndNumberVars()
  checkCFGEqual(cfg1: cfg, cfg2: cfg2)
}

@Test func testCFGIntoSSALoopWithoutAssignment() throws {
  let code = """
      fn main(x: int, y: str) {
        while? (x) {
          if? (y) {
            x = 0
          }
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

  cfg.insertPhiAndNumberVars()
  checkSSA(cfg: cfg)
  checkPhi(cfg: cfg)
  checkReturns(cfg: cfg)

  var cfg2 = CFG(ast: ast)
  cfg2.insertPhiAndNumberVars()
  checkCFGEqual(cfg1: cfg, cfg2: cfg2)
}

@Test func testCFGIntoSSADeclareAfterBranch() throws {
  let code = """
    fn eq(x: str, y: str) -> int {
      x_len: int = len(x)
      y_len: int = len(y)
      if? (not(eq(x_len, y_len))) {
        return!(0)
      }
      i: int = 0
      while? (lt(i, x_len)) {
        if? (not(eq(str_get(x, i), str_get(y, i)))) {
          return!(0)
        }
        i = add(i, 1)
      }
      return!(1)
    }

    fn len(x: str) -> int {
      return!(0)
    }

    fn str_get(x: str, y: int) -> int {
      return!(0)
    }

    fn add(x: int, y: int) -> int {
      return!(0)
    }

    fn not(x: int) -> int {
      return!(0)
    }

    fn eq(x: int, y: int) -> int {
      return!(0)
    }

    fn lt(x: int, y: int) -> int {
      return!(0)
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

  cfg.insertPhiAndNumberVars()
  checkSSA(cfg: cfg)
  checkPhi(cfg: cfg)
  checkReturns(cfg: cfg)

  var cfg2 = CFG(ast: ast)
  cfg2.insertPhiAndNumberVars()
  checkCFGEqual(cfg1: cfg, cfg2: cfg2)
}

/// Check that each function has at most one exit node with a return
/// instruction in it.
func checkReturns(cfg: CFG) {
  for (fn, root) in cfg.functions {
    let nodes = cfg.dfsFrom(node: root)

    var returnCount = 0
    for node in nodes {
      for inst in cfg.nodeCode[node]!.instructions {
        switch inst.op {
        case .returnVoid: returnCount += 1
        case .returnValue: returnCount += 1
        default: ()
        }
      }
    }
    #expect(returnCount < 2, "expected at most one return but found \(returnCount)")

    let children = nodes.filter { cfg.successors(of: $0).isEmpty }
    #expect(children.count < 2, "expect at most one leaf node but got \(children) for \(fn)")
    if children.count != 1 {
      continue
    }
    for child in children {
      let code = cfg.nodeCode[child]!
      switch code.instructions.last?.op {
      case .returnValue: ()
      case .returnVoid: ()
      default:
        #expect(Bool(false), "unexpected final instruction from leaf node for function \(fn)")
      }
    }
  }
}

/// Check SSA condition that variable versions are only assigned once.
func checkSSA(cfg: CFG) {
  let domTree = DominatorTree(cfg: cfg)

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
    let dominating = Set(domTree.dominated(by: node)).subtracting([node])
    let domSelf = !Set(cfg.predecessors[node, default: []]).intersection(dominating).isEmpty

    for inst in cfg.nodeCode[node]!.instructions {
      if domSelf, case .phi = inst.op {
        // If a node dominates a predecessor, then phi functions can use variables
        // that they also define.
        continue
      }
      let uses = Set(inst.op.uses)
      #expect(inst.op.defs.filter(uses.contains).isEmpty, "\(inst.op)")
    }
  }
}

/// Check that all phi functions are filled in
func checkPhi(cfg: CFG) {
  func phis(node: CFG.Node) -> [CFG.Inst] {
    cfg.nodeCode[node]!.instructions.filter {
      if case .phi = $0.op {
        true
      } else {
        false
      }
    }
  }
  for node in cfg.functions.values {
    #expect(phis(node: node).isEmpty, "entrypoint should not contain phi")
  }
  for node in cfg.nodes {
    let preds = cfg.predecessors[node, default: []]
    let phis = phis(node: node)
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

struct HashableVariable: Hashable {
  let position: Position
  let name: String
}

func checkCFGEqual(cfg1: CFG, cfg2: CFG) {
  #expect(cfg1.functions.count == cfg2.functions.count)
  #expect(cfg1.nodes.count == cfg2.nodes.count)
  if cfg1.nodes.count != cfg2.nodes.count {
    return
  }

  for (fn, node1) in cfg1.functions {
    let node2 = cfg2.functions[fn]!
    #expect(node1.id == node2.id)
  }

  func computeVarMapping(cfg: CFG) -> [HashableVariable: Variable] {
    var result = [HashableVariable: Variable]()
    for code in cfg.nodeCode.values {
      for inst in code.instructions {
        for v in inst.op.uses + inst.op.defs {
          result[.init(position: v.variable.declarationPosition, name: v.variable.name)] =
            v.variable
        }
      }
    }
    return result
  }

  let cfg1Mapping = computeVarMapping(cfg: cfg1)

  let idToNode1 = Dictionary(uniqueKeysWithValues: cfg1.nodes.map { ($0.id, $0) })
  let idToNode2 = Dictionary(uniqueKeysWithValues: cfg2.nodes.map { ($0.id, $0) })

  for (id, node1) in idToNode1 {
    let node2 = idToNode2[id]!
    let code1 = cfg1.nodeCode[node1]!
    let code2 = cfg2.nodeCode[node2]!
    #expect(code1.instructions.count == code2.instructions.count)
    for (i, inst1) in code1.instructions.enumerated() {
      var inst2 = code2.instructions[i]
      for v in inst2.op.defs + inst2.op.uses {
        let v1 = cfg1Mapping[
          .init(position: v.variable.declarationPosition, name: v.variable.name)
        ]!
        assert(Set((inst1.op.defs + inst1.op.uses).map { $0.variable }).contains(v1))
        inst2.op = inst2.op.replacing(
          v, with: CFG.SSAVariable.init(variable: v1, version: v.version)
        )
      }

      if case .phi(let target1, let sources1) = inst1.op,
        case .phi(let target2, let sources2) = inst2.op
      {
        #expect(target1 == target2)
        #expect(
          Dictionary(uniqueKeysWithValues: sources1.map { (key, value) in (key.id, value) })
            == Dictionary(uniqueKeysWithValues: sources2.map { (key, value) in (key.id, value) })
        )
      } else {
        #expect(inst1 == inst2)
      }
    }
  }
}
