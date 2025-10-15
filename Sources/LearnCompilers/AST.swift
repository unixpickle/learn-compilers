import LearnParsers

public typealias ASTMatch = ParserMatch<String, SimpleGrammarKeyword>

public enum ChildrenOrCode {
  case children([ASTNode])
  case code(String)
}

public protocol ASTNode {
  var contents: ChildrenOrCode { get }
}

extension ASTNode {
  public var codeString: String {
    switch self.contents {
    case .code(let x): x
    case .children(let c): c.map { $0.codeString }.joined()
    }
  }
}

public struct AST: ASTNode {
  public struct Identifier: ASTNode {
    public var name: String

    public init(name: String) {
      self.name = name
    }

    public var contents: ChildrenOrCode { .code(name) }
  }

  public struct TypeIdentifier: ASTNode {
    public var name: String

    public init(name: String) {
      self.name = name
    }

    public var contents: ChildrenOrCode { .code(name) }
  }

  public struct Raw: ASTNode {
    public var text: String

    public init(text: String) {
      self.text = text
    }

    public var contents: ChildrenOrCode { .code(text) }
  }

  public struct FuncDecl: ASTNode {
    public struct ArgDecl: ASTNode {
      public var leadingWhitespace: Raw
      public var identifier: Identifier
      public var whitespace1: Raw
      public var colon: Raw
      public var whitespace2: Raw
      public var typeID: TypeIdentifier
      public var trailingWhitespace: Raw
      public var comma: Raw?

      public var contents: ChildrenOrCode {
        .children(
          [
            leadingWhitespace, identifier, whitespace1, colon, whitespace2, typeID,
            trailingWhitespace,
          ] + maybeList(comma)
        )
      }

      internal init(leadingWhitespace: Raw, trailingWhitespace: Raw, comma: Raw?, rhs: [ASTMatch]) {
        self.leadingWhitespace = leadingWhitespace
        self.trailingWhitespace = trailingWhitespace
        self.comma = comma
        self.identifier = Identifier(name: toText(match: rhs[0]))
        self.whitespace1 = Raw(text: toText(match: rhs[1]))
        self.colon = Raw(text: toText(match: rhs[2]))
        self.whitespace2 = Raw(text: toText(match: rhs[3]))
        self.typeID = TypeIdentifier(name: toText(match: rhs[4]))

      }
    }

    public struct RetDecl: ASTNode {
      public var whitespace1: ASTNode
      public var arrow: Raw
      public var whitespace2: ASTNode
      public var retType: TypeIdentifier

      public var contents: ChildrenOrCode {
        .children([whitespace1, arrow, whitespace2, retType])
      }
    }

    public var leadingWhitespace: Raw
    public var fn: Raw
    public var whitespace1: Raw
    public var name: Identifier
    public var whitespace2: Raw
    public var openParenthesis: Raw
    public var arguments: [ArgDecl]
    public var closeParenthesis: Raw
    public var retType: RetDecl?
    public var trailingWhitespace: Raw
    public var block: Block

    public var contents: ChildrenOrCode {
      .children(
        [leadingWhitespace, fn, whitespace1, name, whitespace2, openParenthesis] + arguments + [
          closeParenthesis
        ] + maybeList(retType) + [trailingWhitespace, block]
      )
    }

    internal init(leadingWhitespace: Raw, rhs: [ASTMatch]) {
      self.leadingWhitespace = leadingWhitespace
      fn = Raw(text: rhs[..<2].map(toText).joined())
      whitespace1 = Raw(text: toText(match: rhs[2]))
      name = Identifier(name: toText(match: rhs[3]))
      whitespace2 = Raw(text: toText(match: rhs[4]))
      openParenthesis = Raw(text: toText(match: rhs[5]))

      guard case .nonTerminal(.funcArgDeclList, let argDeclRHS) = rhs[6] else {
        fatalError()
      }

      arguments = []
      var argDecl = argDeclRHS
      while argDecl.count == 5 {
        guard case .nonTerminal(.funcArgDecl, let argRHS) = argDecl[1] else {
          fatalError()
        }
        arguments.append(
          ArgDecl(
            leadingWhitespace: Raw(text: toText(match: argDecl[0])),
            trailingWhitespace: Raw(text: toText(match: argDecl[2])),
            comma: Raw(text: toText(match: argDecl[3])),
            rhs: argRHS
          )
        )
        guard case .nonTerminal(.funcArgDeclList, let a) = argDecl[4] else {
          fatalError()
        }
        argDecl = a
      }
      if argDecl.count == 3 {
        guard case .nonTerminal(.funcArgDecl, let argRHS) = argDecl[1] else {
          fatalError()
        }
        arguments.append(
          ArgDecl(
            leadingWhitespace: Raw(text: toText(match: argDecl[0])),
            trailingWhitespace: Raw(text: toText(match: argDecl[2])),
            comma: nil,
            rhs: argRHS
          )
        )
      }

      closeParenthesis = Raw(text: toText(match: rhs[7]))

      guard case .nonTerminal(.funcRetDecl, let retRHS) = rhs[8] else {
        fatalError()
      }
      if retRHS.count == 1 {
        trailingWhitespace = Raw(text: toText(match: retRHS[0]))
        retType = nil
      } else {
        retType = RetDecl(
          whitespace1: Raw(text: toText(match: retRHS[0])),
          arrow: Raw(text: retRHS[1...2].map(toText).joined()),
          whitespace2: Raw(text: toText(match: retRHS[3])),
          retType: TypeIdentifier(name: toText(match: retRHS[4]))
        )
        trailingWhitespace = Raw(text: toText(match: retRHS[5]))
      }

      guard case .nonTerminal(.codeBlock, let blockRHS) = rhs[9] else {
        fatalError()
      }
      self.block = Block(rhs: blockRHS)
    }
  }

  public struct Block: ASTNode {
    // TODO: make this actually contain something.
    public var textContent: String

    internal init(rhs: [ASTMatch]) {
      self.textContent = rhs.map(toText).joined()
    }

    public var contents: ChildrenOrCode { .code(textContent) }
  }

  public struct IfStatement: ASTNode {
    public var ifText: Raw
    public var whitespace1: Raw
    public var openParenthesis: Raw
    public var expression: Expression
    public var closeParenthesis: Raw
    public var whitespace2: Raw
    public var block: Block

    public var contents: ChildrenOrCode {
      .children([
        ifText, whitespace1, openParenthesis, expression, closeParenthesis, whitespace2, block,
      ])
    }
  }

  public struct IntLiteral: ASTNode {
    public var text: String

    public var contents: ChildrenOrCode { .code(text) }

    public init(text: String) {
      self.text = text
    }
  }

  public struct FuncCall: ASTNode {
    public struct FuncArg: ASTNode {
      public var whitespace1: Raw
      public var expression: Expression
      public var whitespace2: Raw
      public var comma: Raw?

      public var contents: ChildrenOrCode {
        .children([whitespace1, expression, whitespace2] + maybeList(comma))
      }
    }

    public var identifier: Identifier
    public var openParenthesis: Raw
    public var args: [FuncArg]
    public var closeParenthesis: Raw

    public var contents: ChildrenOrCode {
      .children([identifier, openParenthesis] + args + [closeParenthesis])
    }
  }

  public enum Expression: ASTNode {
    indirect case funcCall(FuncCall)
    case identifier(Identifier)
    case intLiteral(IntLiteral)

    public var contents: ChildrenOrCode {
      switch self {
      case .funcCall(let c): .children([c])
      case .identifier(let c): .children([c])
      case .intLiteral(let c): .children([c])
      }
    }
  }

  public var functions: [FuncDecl]
  public var trailingWhitespace: Raw

  public var contents: ChildrenOrCode {
    .children(functions + [trailingWhitespace])
  }

  public init(functions: [FuncDecl], trailingWhitespace: Raw) {
    self.functions = functions
    self.trailingWhitespace = trailingWhitespace
  }

  /// Create an ASTNode given a match on an entire code file with SimpleGrammar.
  /// If this is not a full match for the correct grammar, behavior is undefined.
  public init(match: ASTMatch) {
    guard case .nonTerminal(.codeFile, let rawRHS) = match else {
      fatalError("invalid root match")
    }

    functions = []

    var rhs = rawRHS
    while rhs.count > 1 {
      guard case .nonTerminal(_, let funcRHS) = rhs[1] else {
        fatalError()
      }
      functions.append(
        FuncDecl(leadingWhitespace: Raw(text: toText(match: rhs[0])), rhs: funcRHS)
      )
      guard case .nonTerminal(.codeFile, let nextRHS) = rhs[2] else {
        fatalError()
      }
      rhs = nextRHS
    }

    trailingWhitespace = Raw(text: toText(match: rhs[0]))
  }
}

public enum ASTNodeError: Error {
  case unexpectedTerminal(terminal: String)
}

internal func toText(match: ASTMatch) -> String {
  switch match {
  case .terminal(let x): x
  case .nonTerminal(_, let rhs): rhs.map(toText).joined()
  }
}

private func maybeList<T>(_ x: T?) -> [T] {
  if let x = x {
    [x]
  } else {
    []
  }
}
