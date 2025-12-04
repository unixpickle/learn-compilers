import LearnParsers

/// Format an error from the context-free grammar parser.
public func formatParseError(filename: String, _ error: Error) -> String {
  if let readError = error as? ParserReadError {
    let errorMsg =
      if case .unexpectedTerminal(let term, _) = readError.error as? Parser.ParserType.ParseError {
        switch term {
        case .terminal(let t): "unexpected byte: \(t.debugDescription)"
        case .end: "unexpected EOF"
        }
      } else {
        "\(readError.error)"
      }
    let md = readError.metadata
    return "\(filename):\((md.line ?? 0) + 1):\((md.column ?? 0) + 1): \(errorMsg)"
  }
  return "\(error)"
}

/// An error which occurs during AST decoration.
public protocol ASTDecorationError: Error {
  var position: Position { get }
}

public enum VariableResolutionError: ASTDecorationError, CustomStringConvertible {
  case notDefined(Position, String)
  case redefined(original: Position, newPosition: Position, name: String)

  public var position: Position {
    switch self {
    case .notDefined(let p, _): p
    case .redefined(_, let p, _): p
    }
  }

  public var description: String {
    let body =
      switch self {
      case .notDefined(_, let name): "name \(name.debugDescription) is not defined"
      case .redefined(let origPos, _, let name):
        "name \(name.debugDescription) was already defined at \(origPos)"
      }
    return "\(position): \(body)"
  }
}

public enum FuncResolutionError: ASTDecorationError, CustomStringConvertible {
  case redefined(
    original: Position,
    newPosition: Position,
    name: String,
    oldSignature: Function.Signature,
    newSignature: Function.Signature
  )

  public var position: Position {
    switch self {
    case .redefined(_, let p, _, _, _): p
    }
  }

  public var description: String {
    let body =
      switch self {
      case .redefined(let origPos, _, let name, _, _):
        "function \(name.debugDescription) was already defined at \(origPos)"
      }
    return "\(position): \(body)"
  }
}

public enum TypeAndControlFlowError: ASTDecorationError, CustomStringConvertible {
  case breakOutsideOfLoop(Position)
  case continueOutsideOfLoop(Position)
  case incorrectType(
    position: Position, expectedType: Variable.DataType, actualType: Variable.DataType
  )
  case unknownFunction(position: Position, name: String, argTypes: [Variable.DataType])
  case missingValue(position: Position)
  case returnWithTypeFromVoidFunction(position: Position)
  case returnWithVoidFromTypedFunction(position: Position, expectedType: Variable.DataType)

  public var position: Position {
    switch self {
    case .breakOutsideOfLoop(let p): p
    case .continueOutsideOfLoop(let p): p
    case .incorrectType(let p, _, _): p
    case .unknownFunction(let p, _, _): p
    case .missingValue(let p): p
    case .returnWithTypeFromVoidFunction(let p): p
    case .returnWithVoidFromTypedFunction(let p, _): p
    }
  }

  public var description: String {
    let body =
      switch self {
      case .breakOutsideOfLoop: "break statement outside of loop"
      case .continueOutsideOfLoop: "continue statement outside of loop"
      case .incorrectType(_, let expectedType, let actualType):
        "expected type \(expectedType) but got \(actualType)"
      case .unknownFunction(_, let name, let argTypes):
        "unknown function \(name.debugDescription) with signature \(argTypes)"
      case .missingValue: "missing value"
      case .returnWithTypeFromVoidFunction: "cannot return from this function with a value"
      case .returnWithVoidFromTypedFunction(_, let expectedType):
        "cannot return from this function without a value of type \(expectedType)"
      }
    return "\(position): \(body)"
  }
}

public enum SSAError: Error, CustomStringConvertible {
  case missingReturn(Position)

  public var description: String {
    switch self {
    case .missingReturn(let pos): "\(pos): function is missing a return"
    }
  }
}
