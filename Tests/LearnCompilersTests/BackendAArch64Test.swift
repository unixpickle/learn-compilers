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
  let output = try runCode(code: code, stdin: Data())
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
  let output = try runCode(code: code, stdin: Data("103928\n".utf8))
  #expect(output == Data("enter a positive integer:\n103928 = 2 * 2 * 2 * 11 * 1181\n".utf8))
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
  let output = try runCode(code: code, stdin: Data("abcdefghijk".utf8))
  #expect(output == Data("abcdefghijk are some letters\n".utf8))
}

enum CompileError: Error {
  case clangError(String)
}

func runCode(code: String, stdin: Data) throws -> Data {
  let baseURL = FileManager.default.temporaryDirectory
  let uniqueName = UUID().uuidString
  let dirURL = baseURL.appendingPathComponent("lc-\(uniqueName)", isDirectory: true)
  try FileManager.default.createDirectory(at: dirURL, withIntermediateDirectories: true)
  defer { try? FileManager.default.removeItem(at: dirURL) }

  try compileCode(code: code, tmpDir: dirURL)

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
  let outputData = CatData()
  stdoutPipe.fileHandleForReading.readabilityHandler = { handle in
    outputData.add(handle.availableData)
  }
  defer {
    try! stdoutPipe.fileHandleForReading.close()
  }

  try process.run()
  process.waitUntilExit()

  if process.terminationStatus != 0 {
    throw CompileError.clangError("Program exited with status \(process.terminationStatus)")
  }

  return outputData.finalData
}

func compileCode(code: String, tmpDir: URL) throws {
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
  cfg.performBasicOptimizations(fnReduction: BuiltInFunction.reduce)
  try cfg.checkMissingReturns()

  let asmCode = try BackendAArch64().compileAssembly(cfg: cfg)

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
