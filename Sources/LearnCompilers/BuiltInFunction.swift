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
}
