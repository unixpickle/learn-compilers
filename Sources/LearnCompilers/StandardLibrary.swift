import Foundation

public final class StandardLibrary {
  private static let lock = NSLock()
  nonisolated(unsafe) private static var _ast: AST? = nil
  nonisolated(unsafe) private static var _table: ScopeTable? = nil

  public static var ast: AST {
    let (ast, _) = standard
    return ast
  }

  public static var table: ScopeTable {
    let (_, table) = standard
    return table
  }

  private static var standard: (AST, ScopeTable) {
    lock.withLock {
      if let ast = _ast, let table = _table {
        return (ast, table)
      }
      do {
        let match = try Parser.parse(stdlibCode)
        let rawAST = AST(match: match)
        _table = ScopeTable()
        for fn in BuiltInFunction.functions {
          _table!.functions[fn.name, default: []].append(fn)
        }
        let (newAST, errors) = rawAST.decorated(table: &_table!, fileID: "<stdlib>")
        if let first = errors.first {
          throw first
        }
        _ast = newAST
      } catch {
        fatalError("ERROR initializing standard library: \(error)")
      }
      return (_ast!, _table!)
    }
  }

  private static let stdlibCode: String = """
    fn print(x: str) {
      i: int = 0
      size: int = len(x)
      while? (lt(i, size)) {
        putc(str_get(x, i))
        i = add(i, 1)
      }
    }

    fn println(x: str) {
      print(x)
      putc(10)
    }

    fn str(x: int) -> str {
      if? (lt(x, 0)) {
        pos_str: str = str(sub(0, x))
        minus: str = str_alloc(1)
        str_set(minus, 0, 45)
        result: str = concat(minus, pos_str)
        str_free(pos_str)
        str_free(minus)
        return!(result)
      }
      if? (eq(x, 0)) {
        result: str = str_alloc(1)
        str_set(result, 0, 48)
        return!(result)
      }
      digit_str: str = str_alloc(1)
      str_set(digit_str, 0, add(mod(x, 10), 48))
      remaining: int = div(x, 10)
      if? (eq(remaining, 0)) {
        return!(digit_str)
      }
      remaining_str: str = str(remaining)
      result: str = concat(remaining_str, digit_str)
      str_free(digit_str)
      str_free(remaining_str)
      return!(result)
    }

    fn concat(x: str, y: str) -> str {
      x_len: int = len(x)
      y_len: int = len(y)
      result: str = str_alloc(add(len(x), len(y)))
      i: int = 0
      while? (lt(i, x_len)) {
        str_set(result, i, str_get(x, i))
        i = add(i, 1)
      }
      i = 0
      while? (lt(i, y_len)) {
        str_set(result, add(x_len, i), str_get(y, i))
        i = add(i, 1)
      }
      return!(result)
    }

    fn not(x: str) -> int {
      return!(not(len(x)))
    }

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
    """
}
