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
}

@Test func testBasicOptimizationFibonacci() throws {
  for code in FibonacciImplementations {
    let cfg = try codeToCFG(code, opt: .basic)
    verifyCFGInvariants(cfg: cfg)
    try checkFibonacciImplementation(cfg: cfg)
  }
}

@Test func testBasicOptimizationTableFormat() throws {
  let cfg = try codeToCFG(TableFormatImplementation, opt: .basic)
  verifyCFGInvariants(cfg: cfg)
  try checkTableFormatImplementation(cfg: cfg)
}

func verifyCFGInvariants(cfg: CFG) {
  checkSSA(cfg: cfg)
  checkPhi(cfg: cfg)

  #expect(cfg.nodes == Set(cfg.nodeCode.keys))
  let reachable = Set(cfg.functions.values.flatMap { cfg.dfsFrom(node: $0) })
  #expect(cfg.nodes == reachable)
}
