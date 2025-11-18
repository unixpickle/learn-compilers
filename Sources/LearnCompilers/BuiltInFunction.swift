public enum BuiltInFunction: Hashable, Sendable {
  case add
  case sub
  case mul
  case div
  case mod
  case notInt
  case eqInt
  case lt
  case gt
  case len
  case or
  case and
  case putc
  case strAlloc
  case strFree
  case strGet
  case strSet

  public var name: String {
    switch self {
    case .add: "add"
    case .sub: "sub"
    case .mul: "mul"
    case .div: "div"
    case .mod: "mod"
    case .notInt: "not"
    case .eqInt: "eq"
    case .lt: "lt"
    case .gt: "gt"
    case .len: "len"
    case .or: "or"
    case .and: "and"
    case .putc: "putc"
    case .strAlloc: "str_alloc"
    case .strFree: "str_free"
    case .strGet: "str_get"
    case .strSet: "str_set"
    }
  }

  public var signature: Function.Signature {
    switch self {
    case .add: .init(args: [.integer, .integer], ret: .integer)
    case .sub: .init(args: [.integer, .integer], ret: .integer)
    case .mul: .init(args: [.integer, .integer], ret: .integer)
    case .div: .init(args: [.integer, .integer], ret: .integer)
    case .mod: .init(args: [.integer, .integer], ret: .integer)
    case .notInt: .init(args: [.integer], ret: .integer)
    case .eqInt: .init(args: [.integer, .integer], ret: .integer)
    case .lt: .init(args: [.integer, .integer], ret: .integer)
    case .gt: .init(args: [.integer, .integer], ret: .integer)
    case .len: .init(args: [.string], ret: .integer)
    case .or: .init(args: [.integer, .integer], ret: .integer)
    case .and: .init(args: [.integer, .integer], ret: .integer)
    case .putc: .init(args: [.integer], ret: nil)
    case .strAlloc: .init(args: [.integer], ret: .string)
    case .strFree: .init(args: [.string], ret: nil)
    case .strGet: .init(args: [.string, .integer], ret: .integer)
    case .strSet: .init(args: [.string, .integer, .integer], ret: nil)
    }
  }

  public static let functions: [Function] = {
    [
      BuiltInFunction.add,
      .sub,
      .mul,
      .div,
      .mod,
      .notInt,
      .eqInt,
      .lt,
      .gt,
      .len,
      .or,
      .and,
      .putc,
      .strAlloc,
      .strFree,
      .strGet,
      .strSet,
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
