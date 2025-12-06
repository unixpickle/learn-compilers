import LearnParsers
import Testing

@testable import LearnCompilers

@Test func testBasicOptimizationAllowsNoReturn() throws {
  var code = """
    fn main() -> int {
      if? (0) {
        return!(0)
      }
      while? (1) {
        print(str(0))
      }
    }
    """
  #expect(throws: SSAError.self) {
    try codeToCFG(code, opt: .none)
  }
  verifyCFGInvariants(cfg: try codeToCFG(code, opt: .basic))

  code = """
    fn main() -> int {
      while? (1) {
        print(str(0))
      }
    }
    """
  #expect(throws: SSAError.self) {
    try codeToCFG(code, opt: .none)
  }
  verifyCFGInvariants(cfg: try codeToCFG(code, opt: .basic))
  verifyCFGInvariants(cfg: try codeToCFG(code, opt: .basicWithInlining))

  code = """
    fn main(x: int) -> int {
      a: int = add(x, 0)
      b: int = add(0, x)
      while? (and(eq(x, b), eq(a, x))) {
        print(str(0))
      }
    }
    """
  #expect(throws: SSAError.self) {
    try codeToCFG(code, opt: .none)
  }
  verifyCFGInvariants(cfg: try codeToCFG(code, opt: .basic))
  verifyCFGInvariants(cfg: try codeToCFG(code, opt: .basicWithInlining))
}

@Test func testBasicOptimizationCascadingConstants() throws {
  // The first branch should never be taken, so b should be constant
  // 0, so print is never called.
  let code = """
    fn main(x: int) {
      a: int = add(x, 0)
      b: int = 0
      if? (not(eq(a, x))) {
        b = 1
      }
      if? (b) {
        print(str(b))
      }
    }
    """

  func containsPrint(cfg: CFG) -> Bool {
    let nodes = cfg.dfsFrom(
      node: cfg.functions.compactMap { $0.key.name == "main" ? $0.value : nil }.first!
    )
    for node in nodes {
      let code = cfg.nodeCode[node]!
      for inst in code.instructions {
        if case .call(let fn, _) = inst.op {
          if fn.name == "print" {
            return true
          }
        }
      }
    }
    return false
  }

  let naiveCFG = try codeToCFG(code, opt: .none)
  verifyCFGInvariants(cfg: naiveCFG)
  #expect(containsPrint(cfg: naiveCFG), "naive CFG shouldn't optimize away print")
  let optCFGs = try codeToCFGs(code, opt: .basic, count: 2)
  let optCFG = optCFGs[0]
  verifyCFGInvariants(cfg: optCFG)
  #expect(!containsPrint(cfg: optCFG), "optimized CFG should optimize away print")
  checkCFGEqual(cfg1: optCFG, cfg2: optCFGs[1])
}

@Test func testBasicOptimizationAssignmentReduction() throws {
  let code = """
    fn main(x: int, y: int) {
      b: int = eq(add(x, 0), x)
      TESTING_unused: int = add(y, 1)
      if? (not(b)) {
        print(str(TESTING_unused))
      }
      if? (TESTING_unused) {
        if? (not(b)) {
          print(str(TESTING_unused))
        }
      }
    }
    """

  func containsUnused(cfg: CFG) -> Bool {
    for (_, code) in cfg.nodeCode {
      for inst in code.instructions {
        for v in inst.op.defs {
          if v.variable.name == "TESTING_unused" {
            return true
          }
        }
      }
    }
    return false
  }

  let naiveCFG = try codeToCFG(code, opt: .none)
  verifyCFGInvariants(cfg: naiveCFG)
  #expect(containsUnused(cfg: naiveCFG), "naive CFG shouldn't optimize away unused var")
  let optCFGs = try codeToCFGs(code, opt: .basic, count: 2)
  let optCFG = optCFGs[0]
  verifyCFGInvariants(cfg: optCFG)
  #expect(!containsUnused(cfg: optCFG), "optimized CFG should optimize away unused var")

  checkCFGEqual(cfg1: optCFG, cfg2: optCFGs[1])
}

@Test func testInlineBasicOptimizationRemovesNestedFunctions() throws {
  let code = """
    fn foo_0(x: int) -> int {
      return!(foo_2(x))
    }
    fn foo_1(x: int) -> int {
      return!(foo_3(x))
    }
    fn foo_2(x: int) -> int {
      return!(foo_1(x))
    }
    fn foo_3(x: int) -> int {
      return!(add(x, 3))
    }

    fn bar_0(x: int) -> int {
      return!(bar_3(x))
    }
    fn bar_1(x: int) -> int {
      return!(bar_2(x))
    }
    fn bar_2(x: int) -> int {
      return!(mul(x, 3))
    }
    fn bar_3(x: int) -> int {
      return!(bar_1(x))
    }

    fn main(x: int, y: int) -> int {
      return!(mul(foo_0(x), bar_0(y)))
    }
    """

  func containsInlined(cfg: CFG) -> Bool {
    for (fn, _) in cfg.functions {
      if fn.name.starts(with: "foo_") || fn.name.starts(with: "bar_") {
        return true
      }
    }
    return false
  }

  let naiveCFG = try codeToCFG(code, opt: .none)
  verifyCFGInvariants(cfg: naiveCFG)
  #expect(containsInlined(cfg: naiveCFG), "naive CFG shouldn't optimize away inlined funcs")
  let optCFGs = try codeToCFGs(code, opt: .basicWithInlining, count: 5)
  let optCFG = optCFGs[0]
  verifyCFGInvariants(cfg: optCFG)
  #expect(!containsInlined(cfg: optCFG), "optimized CFG should optimize away inlined funcs")

  for i in 1..<optCFGs.count {
    checkCFGEqual(cfg1: optCFG, cfg2: optCFGs[i])
  }
}

@Test func testBasicOptimizationFibonacci() throws {
  for opt in [OptLevel.basic, .basicWithInlining] {
    for code in FibonacciImplementations {
      let cfgs = try codeToCFGs(code, opt: opt, count: 2)
      let cfg = cfgs[0]
      verifyCFGInvariants(cfg: cfg)
      try checkFibonacciImplementation(cfg: cfg)
      checkCFGEqual(cfg1: cfg, cfg2: cfgs[1])
    }
  }
}

@Test func testBasicOptimizationTableFormat() throws {
  for opt in [OptLevel.basic, .basicWithInlining] {
    let cfgs = try codeToCFGs(TableFormatImplementation, opt: opt, count: 2)
    let cfg = cfgs[0]
    verifyCFGInvariants(cfg: cfg)
    try checkTableFormatImplementation(cfg: cfg)
    checkCFGEqual(cfg1: cfg, cfg2: cfgs[1])
  }
}

func verifyCFGInvariants(cfg: CFG) {
  checkSSA(cfg: cfg)
  checkPhi(cfg: cfg)

  #expect(cfg.nodes == Set(cfg.nodeCode.keys))
  let reachable = Set(cfg.functions.values.flatMap { cfg.dfsFrom(node: $0) })
  #expect(cfg.nodes == reachable)
}
