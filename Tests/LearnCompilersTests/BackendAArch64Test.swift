import Foundation
import Testing

@testable import LearnCompilers

@Test func testBackendAArch64BasicPrint() throws {
  let code = """
    fn main() -> int {
      print("hello, world!\\n")
      return!(0)
    }
    """
  let output = try runCode(code: code, stdin: Data(), opt: .basic)
  #expect(output == Data("hello, world!\n".utf8))
}

@Test func testBackendAArch64Factorize() throws {
  let code = """
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

    fn main() -> int {
      println("enter a positive integer:")
      input_str: str = readline()
      number: int = parse_int(input_str)
      if? (lt(number, 0)) {
        println("string is not a number")
        return!(1)
      }

      result: str = str_alloc(0)
      while? (gt(number, 1)) {
        factor: int = smallest_factor(number)
        factor_str: str = str(factor)
        if? (result) {
          result = concat_free(result, str_copy(" * "))
        }
        result = concat_free(result, factor_str)
        number = div(number, factor)
      }

      print(input_str)
      str_free(input_str)
      print(" = ")
      println(result)

      return!(0)
    }
    """
  for opt in [OptLevel.none, .basic, .basicWithInlining] {
    let output = try runCode(code: code, stdin: Data("103928\n".utf8), opt: opt)
    #expect(output == Data("enter a positive integer:\n103928 = 2 * 2 * 2 * 11 * 1181\n".utf8))
  }
}

@Test func testBackendAArch64ManyArgsAndVars() throws {
  let code = """
    fn print_char_codes(a: int, b: int, c: int, d: int, e: int, f: int, g: int, h: int, i: int, j: int, k: int, after: str) {
      putc(a)
      putc(b)
      putc(c)
      putc(d)
      putc(e)
      putc(f)
      putc(g)
      putc(h)
      putc(i)
      putc(j)
      putc(k)
      print(after)
    }

    fn main() -> int {
      a: int = getc()
      b: int = getc()
      c: int = getc()
      d: int = getc()
      e: int = getc()
      f: int = getc()
      g: int = getc()
      h: int = getc()
      i: int = getc()
      j: int = getc()
      k: int = getc()
      print_char_codes(a,b,c,d,e,f,g,h,i,j,k," are some letters\\n")
      return!(0)
    }
    """
  for opt in [OptLevel.none, .basic, .basicWithInlining] {
    let output = try runCode(code: code, stdin: Data("abcdefghijk".utf8), opt: opt)
    #expect(output == Data("abcdefghijk are some letters\n".utf8))
  }
}

@Test func testBackendAArch64LargeConstants() throws {
  let code = """
    fn main() -> int {
      x: int = 12345678901
      print_int(x)
      println("")
      x = 16776978
      print_int(x)
      println("")
      x = 65535
      print_int(x)
      println("")
      return!(0)
    }
    """
  for opt in [OptLevel.none, .basic, .basicWithInlining] {
    let output = try runCode(code: code, stdin: Data(), opt: opt)
    #expect(output == Data("12345678901\n16776978\n65535\n".utf8))
  }
}

@Test func testBackendAArch64Shifts() throws {
  let code = """
    fn main() -> int {
      while? (1) {
        x: str = readline()
        if? (not(x)) {
          break!()
        }
        y: int = parse_int(x)
        str_free(x)
        x = readline()
        shift: int = parse_int(x)
        str_free(x)
        print_int(shl(y, shift))
        println("")
        print_int(shr(y, shift))
        println("")
      }
      return!(0)
    }
    """
  var expectedOut = ""
  var inputs = ""
  for _ in 0..<20 {
    let x = Int64(bitPattern: UInt64.random(in: UInt64.min...UInt64.max))
    let shift = Int.random(in: 0...63)
    inputs += "\(x)\n"
    inputs += "\(shift)\n"
    expectedOut += "\(x << shift)\n"
    expectedOut += "\(x >> shift)\n"
  }
  for opt in [OptLevel.none, .basic, .basicWithInlining] {
    let output = try runCode(code: code, stdin: Data(inputs.utf8), opt: opt)
    #expect(output == Data(expectedOut.utf8))
  }
}

@Test func testBackendAArch64Brainfck() throws {
  let code = """
    fn comment(x: str) {}

    fn read_all() -> str {
      result: str = str_alloc(0)
      while? (1) {
        x: int = getc()
        if? (lt(x, 0)) {
          break!()
        }
        one: str = str_alloc(1)
        str_set(one, 0, x)
        new_result: str = concat(result, one)
        str_free(result)
        str_free(one)
        result = new_result
      }
      return!(result)
    }

    fn grow_zeroed(s: str, new_len: int) -> str {
      old_len: int = len(s)
      if? (not(lt(new_len, old_len))) {
        comment("allocate new, copy old, zero the rest")
        t: str = str_alloc(new_len)
        i: int = 0
        while? (lt(i, old_len)) {
          str_set(t, i, str_get(s, i))
          i = add(i, 1)
        }
        while? (lt(i, new_len)) {
          str_set(t, i, 0)
          i = add(i, 1)
        }
        str_free(s)
        return!(t)
      }
      return!(s)
    }

    fn match_forward(prog: str, pos: int) -> int {
      n: int = len(prog)
      depth: int = 1
      i: int = add(pos, 1)
      while? (lt(i, n)) {
        ch: int = str_get(prog, i)
        if? (eq(ch, 91)) {
          comment("is_command '['")
          depth = add(depth, 1)
        }
        if? (eq(ch, 93)) {
          comment("is_command ']'")
          depth = sub(depth, 1)
          if? (eq(depth, 0)) {
            return!(i)
          }
        }
        i = add(i, 1)
      }
      return!(-1)
    }

    fn match_backward(prog: str, pos: int) -> int {
      depth: int = 1
      i: int = sub(pos, 1)
      while? (not(lt(i, 0))) {
        ch: int = str_get(prog, i)
        if? (eq(ch, 93)) {
          comment("is_command ']' while scanning backward")
          depth = add(depth, 1)
        }
        if? (eq(ch, 91)) {
          comment("is_command '[' while scanning backward")
          depth = sub(depth, 1)
          if? (eq(depth, 0)) {
            return!(i)
          }
        }
        i = sub(i, 1)
      }
      return!(-1)
    }

    fn ensure_tape_capacity(tape: str, needed_index: int) -> str {
      needed_len: int = add(needed_index, 1)
      if? (lt(len(tape), needed_len)) {
        comment("grow to needed_len")
        return!(grow_zeroed(tape, needed_len))
      }
      return!(tape)
    }

    fn cell_inc(v: int) -> int {
      nv: int = add(v, 1)
      if? (gt(nv, 255)) {
        return!(0)
      }
      return!(nv)
    }

    fn cell_dec(v: int) -> int {
      nv: int = sub(v, 1)
      if? (lt(nv, 0)) {
        return!(255)
      }
      return!(nv)
    }

    fn run_bf(program: str) {
      pc: int = 0
      plen: int = len(program)
      dp: int = 0

      comment("start with 1 zeroed cell")
      tape: str = str_alloc(1)
      str_set(tape, 0, 0)

      while? (lt(pc, plen)) {
        op: int = str_get(program, pc)
        is_command: int = 0

        if? (eq(op, 62)) {
          comment("'>'")
          is_command = 1
          dp = add(dp, 1)
          tape = ensure_tape_capacity(tape, dp)
          pc = add(pc, 1)
        }

        if? (eq(op, 60)) {
          comment("'<'")
          is_command = 1
          dp = sub(dp, 1)
          if? (lt(dp, 0)) {
            dp = 0
          }
          pc = add(pc, 1)
        }

        if? (eq(op, 43)) {
          comment("'+'")
          is_command = 1
          cur: int = str_get(tape, dp)
          cur = cell_inc(cur)
          str_set(tape, dp, cur)
          pc = add(pc, 1)
        }

        if? (eq(op, 45)) {
          comment("'-'")
          is_command = 1
          cur: int = str_get(tape, dp)
          cur = cell_dec(cur)
          str_set(tape, dp, cur)
          pc = add(pc, 1)
        }

        if? (eq(op, 46)) {
          comment("'.'")
          is_command = 1
          putc(str_get(tape, dp))
          pc = add(pc, 1)
        }

        if? (eq(op, 91)) {
          comment("'['")
          is_command = 1
          cur: int = str_get(tape, dp)
          if? (eq(cur, 0)) {
            j: int = match_forward(program, pc)
            if? (lt(j, 0)) {
              comment("unmatched '['")
              pc = add(pc, 1)
            }
            if? (not(lt(j, 0))) {
              pc = add(j, 1)
            }
          }
          if? (not(eq(cur, 0))) {
            pc = add(pc, 1)
          }
        }

        if? (eq(op, 93)) {
          comment("']'")
          is_command = 1
          cur2: int = str_get(tape, dp)
          if? (eq(cur2, 0)) {
            pc = add(pc, 1)
          }
          if? (not(eq(cur2, 0))) {
            j2: int = match_backward(program, pc)
            if? (lt(j2, 0)) {
              comment("unmatched ']'")
              pc = add(pc, 1)
            }
            if? (not(lt(j2, 0))) {
              pc = add(j2, 1)
            }
          }
        }

        if? (not(is_command)) {
          comment("ignore non-BF chars")
          pc = add(pc, 1)
        }
      }
    }

    fn main() -> int {
      program: str = read_all()
      run_bf(program)
      return!(0)
    }

    """
  let input = """
      >++++++++[<+++++++++>-]<.
      >++++[<+++++++>-]<+.
      +++++++..
      +++.
      >>++++++[<+++++++>-]<++.
      ------------.
      >++++++[<+++++++++>-]<+.
      <.
      +++.
      ------.
      --------.
      >>>++++[<++++++++>-]<+.
    """
  for opt in [OptLevel.none, .basic, .basicWithInlining] {
    let output = try runCode(code: code, stdin: Data(input.utf8), opt: opt)
    #expect(output == Data("Hello, World!".utf8))
  }
}

@Test func testBackendAArch64VariablePermutations() throws {
  let code = """
    fn permutation(x: str) {
      a: int = str_get(x, 0)
      b: int = str_get(x, 1)
      c: int = str_get(x, 2)
      d: int = str_get(x, 3)
      e: int = str_get(x, 4)
      f: int = str_get(x, 5)
      g: int = str_get(x, 6)
      h: int = str_get(x, 7)
      i: int = str_get(x, 8)
      j: int = str_get(x, 9)
      k: int = str_get(x, 10)
      l: int = str_get(x, 11)
      m: int = str_get(x, 12)
      n: int = str_get(x, 13)
      o: int = str_get(x, 14)
      p: int = str_get(x, 15)
      q: int = str_get(x, 16)
      r: int = str_get(x, 17)
      s: int = str_get(x, 18)
      t: int = str_get(x, 19)
      u: int = str_get(x, 20)
      v: int = str_get(x, 21)
      w: int = str_get(x, 22)

      while? (1) {
        perm: int = getc()
        if? (lt(perm, 0)) {
          break!()
        }
        perm = sub(perm, 48)
        if? (eq(perm, 0)) {
          tmp: int = a
          a = b
          b = tmp

          tmp = c
          c = d
          d = tmp

          tmp = e
          e = f
          f = tmp

          tmp = g
          g = h
          h = tmp

          tmp = i
          i = j
          j = tmp

          tmp = k
          k = l
          l = tmp

          tmp = m
          m = n
          n = tmp

          tmp = o
          o = p
          p = tmp

          tmp = q
          q = r
          r = tmp

          tmp = s
          s = t
          t = tmp

          tmp = u
          u = v
          v = tmp
        }
        if? (eq(perm, 1)) {
          tmp: int = a
          a = b
          b = c
          c = tmp

          tmp = d
          d = e
          e = f
          f = tmp

          tmp = g
          g = h
          h = i
          i = tmp

          tmp = j
          j = k
          k = l
          l = tmp

          tmp = m
          m = n
          n = o
          o = tmp

          tmp = p
          p = q
          q = r
          r = tmp

          tmp = s
          s = t
          t = u
          u = tmp
        }
        if? (eq(perm, 2)) {
          tmp: int = a
          a = b
          b = c
          c = d
          d = e
          e = f
          f = g
          g = h
          h = i
          i = j
          j = k
          k = l
          l = m
          m = n
          n = o
          o = p
          p = q
          q = r
          r = s
          s = t
          t = u
          u = v
          v = w
          w = tmp
        }
      }

      str_set(x, 0, a)
      str_set(x, 1, b)
      str_set(x, 2, c)
      str_set(x, 3, d)
      str_set(x, 4, e)
      str_set(x, 5, f)
      str_set(x, 6, g)
      str_set(x, 7, h)
      str_set(x, 8, i)
      str_set(x, 9, j)
      str_set(x, 10, k)
      str_set(x, 11, l)
      str_set(x, 12, m)
      str_set(x, 13, n)
      str_set(x, 14, o)
      str_set(x, 15, p)
      str_set(x, 16, q)
      str_set(x, 17, r)
      str_set(x, 18, s)
      str_set(x, 19, t)
      str_set(x, 20, u)
      str_set(x, 21, v)
      str_set(x, 22, w)
    }

    fn main() -> int {
      result: str = str_copy("abcdefghijklmnopqrstuvw\\n")
      permutation(result)
      print(result)
      str_free(result)
      return!(0)
    }
    """

  func applyPerm(data: Data, op: Character) -> Data {
    var newData = data
    switch op {
    case Character("0"):
      for x in 0..<(newData.count / 2) {
        let i = x * 2
        (newData[i], newData[i + 1]) = (data[i + 1], data[i])
      }
    case Character("1"):
      for x in 0..<(newData.count / 3) {
        let i = x * 3
        (newData[i], newData[i + 1], newData[i + 2]) = (data[i + 1], data[i + 2], data[i])
      }
    case Character("2"):
      for i in 0..<data.count {
        newData[i] = data[(i + 1) % data.count]
      }
    default: fatalError()
    }
    return newData
  }

  let input = "012020222"
  var expectedOutput = Data("abcdefghijklmnopqrstuvw".utf8)
  for op in input {
    expectedOutput = applyPerm(data: expectedOutput, op: op)
  }
  expectedOutput += Data("\n".utf8)

  for opt in [OptLevel.none, .basic, .basicWithInlining] {
    let output = try runCode(code: code, stdin: Data(input.utf8), opt: opt)
    #expect(
      output == expectedOutput, "output is \(Array(output)), expected \(Array(expectedOutput))")
  }
}

@Test func testBackendAArch64Comparisons() throws {
  let code = """
    fn main() -> int {
      while? (1) {
        ch1: int = getc()
        ch2: int = getc()
        if? (lt(ch1, 0)) {
          break!()
        }

        if? (lt(ch1, ch2)) {
          println("lt")
        }
        if? (gt(ch1, ch2)) {
          println("gt")
        }
        if? (eq(ch1, ch2)) {
          println("eq")
        }

        x: int = lt(ch1, ch2)
        y: int = gt(ch1, ch2)
        z: int = eq(ch1, ch2)
        print_int(x)
        print_int(y)
        print_int(z)
        println("")

        ch2 = add(48, 5)
        if? (lt(ch1, ch2)) {
          println("lt 5")
        }
        if? (gt(ch1, ch2)) {
          println("gt 5")
        }
        if? (eq(ch1, ch2)) {
          println("eq 5")
        }

        x = lt(ch1, ch2)
        y = gt(ch1, ch2)
        z = eq(ch1, ch2)
        s: str = ""
        if? (x) {
          s = "hi"
        }
        if? (s) {
          println("lt 5")
        }
        if? (y) {
          println("gt 5")
        }
        if? (z) {
          println("eq 5")
        }
        print_int(x)
        print_int(y)
        print_int(z)
        println("")
      }

      return!(0)
    }
    """

  let input = "01770550"
  let expectedOutput = Data(
    "lt\n100\nlt 5\nlt 5\n100\neq\n001\ngt 5\ngt 5\n010\nlt\n100\nlt 5\nlt 5\n100\ngt\n010\neq 5\neq 5\n001\n"
      .utf8
  )

  for opt in [OptLevel.none, .basic, .basicWithInlining] {
    let output = try runCode(code: code, stdin: Data(input.utf8), opt: opt)
    #expect(
      output == expectedOutput,
      "output is \(Array(output)), expected \(Array(expectedOutput))"
    )
  }
}

enum CompileError: Error {
  case clangError(String)
}

func runCode(code: String, stdin: Data, opt: OptLevel) throws -> Data {
  let baseURL = FileManager.default.temporaryDirectory
  let uniqueName = UUID().uuidString
  let dirURL = baseURL.appendingPathComponent("lc-\(uniqueName)", isDirectory: true)
  try FileManager.default.createDirectory(at: dirURL, withIntermediateDirectories: true)
  defer { try? FileManager.default.removeItem(at: dirURL) }

  try compileCode(code: code, tmpDir: dirURL, opt: opt)

  let exeURL = dirURL.appending(component: "a.out")

  // === Run the executable with stdin/stdout pipes
  let process = Process()
  process.executableURL = exeURL

  // Set up stdin
  let stdinPipe = Pipe()
  process.standardInput = stdinPipe
  stdinPipe.fileHandleForWriting.writeabilityHandler = { handle in
    handle.writeabilityHandler = nil
    try? handle.write(contentsOf: stdin)
    try? handle.close()
  }
  defer {
    try! stdinPipe.fileHandleForWriting.close()
  }

  let stdoutPipe = Pipe()
  process.standardOutput = stdoutPipe
  defer {
    try! stdoutPipe.fileHandleForReading.close()
  }

  try process.run()
  process.waitUntilExit()
  let finalData = stdoutPipe.fileHandleForReading.readDataToEndOfFile()

  if process.terminationStatus != 0 {
    throw CompileError.clangError("Program exited with status \(process.terminationStatus)")
  }

  return finalData
}

func compileCode(code: String, tmpDir: URL, opt: OptLevel) throws {
  let match = try Parser.parse(code)
  var ast = AST(match: match)

  var table = ScopeTable()
  table.addStandardLibrary()
  var errors: [ASTDecorationError]
  (ast, errors) = ast.decorated(table: &table, fileID: "stdin")

  if let f = errors.first {
    throw f
  }

  var cfg = CFG(ast: ast)
  cfg.add(ast: StandardLibrary.ast)
  cfg.insertPhiAndNumberVars()
  if opt != .none {
    cfg.performBasicOptimizations(fnReduction: BuiltInFunction.reduce)
  }
  if opt == .basicWithInlining {
    cfg.inlineSingleCalls()
    cfg.performBasicOptimizations(fnReduction: BuiltInFunction.reduce)
  }
  try cfg.checkMissingReturns()

  let asmCode = try BackendAArch64().compileAssembly(cfg: cfg)
  let asmCode2 = try BackendAArch64().compileAssembly(cfg: cfg)
  #expect(asmCode == asmCode2, "non-deterministic output")

  let asmPath = tmpDir.appending(component: "program.s").path()
  let binPath = tmpDir.appending(component: "a.out").path()

  try asmCode.write(toFile: asmPath, atomically: true, encoding: .utf8)

  let process = Process()
  process.executableURL = URL(fileURLWithPath: "/usr/bin/clang")
  process.arguments = [asmPath, "-o", binPath]

  let pipe = Pipe()
  defer {
    try! pipe.fileHandleForReading.close()
  }
  process.standardError = pipe
  process.standardOutput = pipe
  let outputData = CatData()
  pipe.fileHandleForReading.readabilityHandler = { handle in
    outputData.add(handle.availableData)
  }

  try process.run()
  process.waitUntilExit()

  if process.terminationStatus != 0 {
    let msg = String(data: outputData.finalData, encoding: .utf8) ?? "Unknown clang error"
    throw CompileError.clangError(msg)
  }
}

final class CatData: @unchecked Sendable {
  private var data = Data()
  private let lock = NSLock()

  public init() {}

  public func add(_ d: Data) {
    lock.withLock {
      data.append(d)
    }
  }

  public var finalData: Data {
    lock.withLock { data }
  }
}
