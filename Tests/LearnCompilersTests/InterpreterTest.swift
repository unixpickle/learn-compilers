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
