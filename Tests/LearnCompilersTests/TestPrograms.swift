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
