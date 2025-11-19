import Testing

@testable import LearnCompilers

let FibonacciImplementations = [
  """
    fn nthFib(n: int) -> int {
      if? (lt(n, 2)) {
        return!(1)
      }
      return!(add(nthFib(sub(n, 2)), nthFib(sub(n, 1))))
    }
    fn main(n: int) -> int {
      return!(nthFib(n))
    }
  """,
  """
    fn main(n: int) -> int {
      a: int = 0
      b: int = 1
      i: int = 0
      while? (lt(i, n)) {
        tmp: int = b
        b = add(a, b)
        a = tmp
        i = add(i, 1)
      }
      return!(b)
    }
  """,
]

func checkFibonacciImplementation(cfg: CFG) throws {
  let mainFunction = cfg.functions.keys.compactMap { key in key.name == "main" ? key : nil }.first!

  let fib = [1, 1, 2, 3, 5, 8]
  for (i, expected) in fib.enumerated() {
    let interp = try Interpreter(
      cfg: cfg,
      entrypoint: mainFunction,
      arguments: [.integer(Int64(i))]
    )
    guard let returnVal = interp.run() else {
      #expect(Bool(false), "got nil return value")
      continue
    }
    if case .integer(let x) = returnVal {
      #expect(x == expected, "expected fibonacci_\(i) = \(expected) but got \(x)")
    } else {
      #expect(
        Bool(false),
        "expected return value of an integer but got \(String(describing: returnVal))"
      )
    }
  }
}

let TableFormatImplementation = """
  fn max(a: int, b: int) -> int {
    if? (gt(a, b)) {
      return!(a)
    }
    return!(b)
  }

  fn max(a: int, b: int, c: int) -> int {
    return!(max(a, max(b, c)))
  }

  fn pad(s: str, size: int, padding: str) -> str {
    result: str = s
    while? (lt(len(result), size)) {
      result = concat(result, padding)
    }
    return!(result)
  }

  fn main(padding: str, aa: str, ab: str, ba: str, bb: str, ca: str, cb: str) -> str {
    colSizeA: int = add(1, max(len(aa), len(ba), len(ca)))
    colSizeB: int = add(1, max(len(ab), len(bb), len(cb)))

    rowA: str = concat(pad(aa, colSizeA, padding), pad(ab, colSizeB, padding))
    rowB: str = concat(pad(ba, colSizeA, padding), pad(bb, colSizeB, padding))
    rowC: str = concat(pad(ca, colSizeA, padding), pad(cb, colSizeB, padding))
    result: str = concat(rowA, concat(rowB, rowC))
    return!(result)
  }
  """

func checkTableFormatImplementation(cfg: CFG) throws {
  let strs = [
    ["aoeu", "aor", "bcdo", "oaeuu", "acorer", "absaorec"],
    ["r,c.roeu", "aor", "bcdo", "oaeuu", "acorer", "ab"],
  ]
  let mainFunction = cfg.functions.keys.compactMap { key in key.name == "main" ? key : nil }.first!
  for inputs in strs {
    let col0Size = [inputs[0], inputs[2], inputs[4]].map { $0.count }.max()!
    let col1Size = [inputs[1], inputs[3], inputs[5]].map { $0.count }.max()!

    var expected = ""
    for (i, var x) in inputs.enumerated() {
      let padSize = 1 + (i % 2 == 0 ? col0Size : col1Size)
      while x.count < padSize {
        x += "*"
      }
      expected += x
    }

    let interp = try Interpreter(
      cfg: cfg,
      entrypoint: mainFunction,
      arguments: [.string(.init("*"))] + inputs.map { .string(.init($0)) }
    )
    guard let returnVal = interp.run() else {
      #expect(Bool(false), "got nil return value")
      continue
    }
    if case .string(let x) = returnVal {
      #expect(x.data == Array(expected.utf8), "expected output = \(expected) but got \(x)")
    } else {
      #expect(
        Bool(false),
        "expected return value of a string but got \(String(describing: returnVal))"
      )
    }
  }
}

let FactorImplementation = """
  fn smallest_factor(number: int) -> int {
    i: int = 2
    while? (lt(i, number)) {
      if? (eq(mod(number, i), 0)) {
        return!(i)
      }
      i = add(i, 1)
    }
    return!(number)
  }

  fn concat_free(x: str, y: str) -> str {
    result: str = concat(x, y)
    str_free(x)
    str_free(y)
    return!(result)
  }

  fn main(number: int) -> str {
    result: str = str_alloc(0)
    while? (gt(number, 1)) {
      factor: int = smallest_factor(number)
      factor_str: str = str(factor)
      if? (result) {
        times: str = str_alloc(3)
        str_set(times, 0, 32)
        str_set(times, 1, 42)
        str_set(times, 2, 32)
        result = concat_free(result, times)
      }
      result = concat_free(result, factor_str)
      number = div(number, factor)
    }
    return!(result)
  }
  """

func checkFactorImplementation(cfg: CFG) throws {
  let insAndOuts = [
    (123, "3 * 41"),
    (710073, "3 * 3 * 3 * 7 * 13 * 17 * 17"),
  ]
  let mainFunction = cfg.functions.keys.compactMap { key in key.name == "main" ? key : nil }.first!
  for (inNum, outStr) in insAndOuts {
    let interp = try Interpreter(
      cfg: cfg,
      entrypoint: mainFunction,
      arguments: [.integer(Int64(inNum))]
    )
    guard let returnVal = interp.run() else {
      #expect(Bool(false), "got nil return value")
      continue
    }
    if case .string(let x) = returnVal {
      #expect(x.data == Array(outStr.utf8), "expected output = \(outStr) but got \(x)")
    } else {
      #expect(
        Bool(false),
        "expected return value of a string but got \(String(describing: returnVal))"
      )
    }
  }
}

enum OptLevel {
  case none
  case basic
}

internal func codeToCFG(_ code: String, opt: OptLevel) throws -> CFG {
  try codeToCFGs(code, opt: opt, count: 1)[0]
}

internal func codeToCFGs(_ code: String, opt: OptLevel, count: Int) throws -> [CFG] {
  let match = try Parser.parse(code)
  var ast = AST(match: match)
  #expect(ast.codeString == code)

  var table = ScopeTable()
  table.addStandardLibrary()
  var errors: [ASTDecorationError]
  (ast, errors) = ast.decorated(table: &table, fileID: "stdin")

  #expect(errors.isEmpty)
  if !errors.isEmpty {
    throw errors.first!
  }

  return try (0..<count).map { _ in
    var cfg = CFG(ast: ast)
    cfg.add(ast: StandardLibrary.ast)
    cfg.insertPhiAndNumberVars()
    if opt != .none {
      cfg.performBasicOptimizations(fnReduction: BuiltInFunction.reduce)
    }
    try cfg.checkMissingReturns()
    return cfg
  }
}
