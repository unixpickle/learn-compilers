public enum BuiltInFunction: Hashable, Sendable {
  case add
  case sub
  case concat
  case str
  case notInt
  case notStr
  case eqInt
  case eqStr
  case lt
  case gt
  case len
  case or
  case and
  case print

  public var name: String {
    switch self {
    case .add: "add"
    case .sub: "sub"
    case .concat: "concat"
    case .str: "str"
    case .notInt: "not"
    case .notStr: "not"
    case .eqInt: "eq"
    case .eqStr: "eq"
    case .lt: "lt"
    case .gt: "gt"
    case .len: "len"
    case .or: "or"
    case .and: "and"
    case .print: "print"
    }
  }

  public var signature: Function.Signature {
    switch self {
    case .add: .init(args: [.integer, .integer], ret: .integer)
    case .sub: .init(args: [.integer, .integer], ret: .integer)
    case .concat: .init(args: [.string, .string], ret: .string)
    case .str: .init(args: [.integer], ret: .string)
    case .notInt: .init(args: [.integer], ret: .integer)
    case .notStr: .init(args: [.string], ret: .integer)
    case .eqInt: .init(args: [.integer, .integer], ret: .integer)
    case .eqStr: .init(args: [.string, .string], ret: .string)
    case .lt: .init(args: [.integer, .integer], ret: .integer)
    case .gt: .init(args: [.integer, .integer], ret: .integer)
    case .len: .init(args: [.string], ret: .integer)
    case .or: .init(args: [.integer, .integer], ret: .integer)
    case .and: .init(args: [.integer, .integer], ret: .integer)
    case .print: .init(args: [.string], ret: nil)
    }
  }

  public static let functions: [Function] = {
    [
      BuiltInFunction.add,
      .sub,
      .concat,
      .str,
      .notInt,
      .notStr,
      .eqInt,
      .eqStr,
      .lt,
      .gt,
      .len,
      .or,
      .and,
      .print,
    ].map {
      .init(
        declarationPosition: Position(fileID: "<built-in>"),
        name: $0.name,
        signature: $0.signature,
        builtIn: $0
      )
    }
  }()

  /// Reduce a function to a constant value or to one of its arguments.
  ///
  /// Returns nil if no such reduction is available.
  public static func reduce(fn: Function, args: [CFG.Argument]) -> CFG.Argument? {
    guard let builtIn = fn.builtIn else {
      return nil
    }
    switch builtIn {
    case .add:
      if case .constInt(let x) = args[0], case .constInt(let y) = args[1] {
        return .constInt(x + y)
      } else if case .constInt(0) = args[0] {
        return args[1]
      } else if case .constInt(0) = args[1] {
        return args[0]
      }
    case .sub:
      if case .constInt(let x) = args[0], case .constInt(let y) = args[1] {
        return .constInt(x - y)
      } else if case .constInt(0) = args[1] {
        return args[0]
      }
    case .lt:
      if case .constInt(let x) = args[0], case .constInt(let y) = args[1] {
        return .constInt(x < y ? 1 : 0)
      }
    case .gt:
      if case .constInt(let x) = args[0], case .constInt(let y) = args[1] {
        return .constInt(x > y ? 1 : 0)
      }
    case .and:
      if case .constInt(let x) = args[0], case .constInt(let y) = args[1] {
        return .constInt(x & y)
      }
    case .or:
      if case .constInt(let x) = args[0], case .constInt(let y) = args[1] {
        return .constInt(x | y)
      }
    case .eqInt:
      if case .constInt(let x) = args[0], case .constInt(let y) = args[1] {
        return .constInt(x == y ? 1 : 0)
      } else if args[0] == args[1] {
        // Comparing a var to itself is always true (for now)
        return .constInt(1)
      }
    case .notInt:
      if case .constInt(let x) = args[0] {
        return .constInt(x == 0 ? 1 : 0)
      }
    default: ()
    }
    return nil
  }
}
