import LearnParsers
import Testing

@testable import LearnCompilers

@Test func testInterpreterFibonacci() throws {
  for code in FibonacciImplementations {
    let cfg = try codeToCFG(code, opt: .none)
    try checkFibonacciImplementation(cfg: cfg)
  }
}

@Test func testInterpreterTableFormat() throws {
  let cfg = try codeToCFG(TableFormatImplementation, opt: .none)
  try checkTableFormatImplementation(cfg: cfg)
}

@Test func testInterpreterFactor() throws {
  let cfg = try codeToCFG(FactorImplementation, opt: .none)
  try checkFactorImplementation(cfg: cfg)
}

@Test func testInterpreterDoublePhi() throws {
  let code = """
    fn main() -> int {
      x: int = 0
      y: int = 0
      while? (lt(x, 10)) {
        if? (lt(x, 20)) {
          y = add(y, 10)
        }
        x = add(x, 1)
      }
      return!(y)
    }
    """
  let cfg = try codeToCFG(code, opt: .none)
  let mainFunction = cfg.functions.keys.compactMap { key in key.name == "main" ? key : nil }.first!
  let interp = try Interpreter(
    cfg: cfg,
    entrypoint: mainFunction,
    arguments: []
  )
  guard let returnVal = interp.run() else {
    #expect(Bool(false), "got nil return value")
    return
  }
  if case .integer(let x) = returnVal {
    #expect(x == 100, "expected 100 got \(x)")
  } else {
    #expect(
      Bool(false),
      "expected return value of a number but got \(String(describing: returnVal))"
    )
  }
}

@Test func testInterpreterStrOnly() throws {
  let code = """
    fn main(x: int) -> str {
      return!(str(x))
    }
    """
  let cfg = try codeToCFG(code, opt: .none)
  let mainFunction = cfg.functions.keys.compactMap { key in key.name == "main" ? key : nil }.first!
  let interp = try Interpreter(
    cfg: cfg,
    entrypoint: mainFunction,
    arguments: [.integer(1337)]
  )
  guard let returnVal = interp.run() else {
    #expect(Bool(false), "got nil return value")
    return
  }
  if case .string(let x) = returnVal {
    #expect(String(bytes: x.data, encoding: .utf8) == "1337")
  } else {
    #expect(
      Bool(false),
      "expected return value of a string but got \(String(describing: returnVal))"
    )
  }
}
