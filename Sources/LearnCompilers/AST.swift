import LearnParsers

public typealias ASTMatch = ParserMatch<String, SimpleGrammarKeyword>

public protocol ASTNode {
  var codeString: String { get }
}

public struct AST: ASTNode {
  public struct Identifier: ASTNode {
    public var name: String

    public init(name: String) {
      self.name = name
    }

    public var codeString: String { name }
  }

  public struct TypeIdentifier: ASTNode {
    public var name: String

    public init(name: String) {
      self.name = name
    }

    public var codeString: String { name }
  }

  public struct Raw: ASTNode {
    public var text: String

    public init(text: String) {
      self.text = text
    }

    public var codeString: String { text }
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

      public var codeString: String {
        "\(leadingWhitespace.codeString)\(identifier.codeString)\(whitespace1.codeString)\(colon.codeString)\(whitespace2.codeString)\(typeID.codeString)\(trailingWhitespace.codeString)\(comma?.codeString ?? "")"
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

      public var codeString: String {
        "\(whitespace1.codeString)\(arrow.codeString)\(whitespace2.codeString)\(retType.codeString)"
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
    public var trailingSpace: Raw
    public var block: Block

    public var codeString: String {
      let blocks: [ASTNode] =
        [leadingWhitespace, fn, whitespace1, name, whitespace2, openParenthesis] + arguments + [
          closeParenthesis
        ] + (retType == nil ? [] : [retType!]) + [trailingSpace, block]
      return blocks.map { $0.codeString }.joined()
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
        trailingSpace = Raw(text: toText(match: retRHS[0]))
        retType = nil
      } else {
        retType = RetDecl(
          whitespace1: Raw(text: toText(match: retRHS[0])),
          arrow: Raw(text: retRHS[1...2].map(toText).joined()),
          whitespace2: Raw(text: toText(match: retRHS[3])),
          retType: TypeIdentifier(name: toText(match: retRHS[4]))
        )
        trailingSpace = Raw(text: toText(match: retRHS[5]))
      }

      self.block = Block(contents: toText(match: rhs[9]))
    }
  }

  public struct Block: ASTNode {
    // TODO: make this actually contain something.
    public var contents: String

    public init(contents: String) {
      self.contents = contents
    }

    public var codeString: String { contents }
  }

  public var functions: [FuncDecl]
  public var trailingWhitespace: Raw

  public var codeString: String {
    var ret = ""
    for x in functions {
      ret += x.codeString
    }
    ret += trailingWhitespace.codeString
    return ret
  }

  public init(functions: [FuncDecl], trailingWhitespace: Raw) {
    self.functions = functions
    self.trailingWhitespace = trailingWhitespace
  }

  internal init(rhs: [ASTMatch]) {
    functions = []

    var rhs = rhs
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
    self.trailingWhitespace = Raw(text: toText(match: rhs[0]))
  }
}

public enum ASTNodeError: Error {
  case unexpectedTerminal(terminal: String)
}

/// Create an ASTNode given a match on an entire code file with SimpleGrammar.
/// If this is not a full match for the correct grammar, behavior is undefined.
public func astNodeFor(
  match: ASTMatch
) -> ASTNode {
  switch match {
  case .nonTerminal(let lhs, let rhs):
    switch lhs {
    case .codeFile:
      return AST(rhs: rhs)
    case .typeIdentifier:
      fatalError()
    case .varDecl:
      fatalError()
    case .varAssign:
      fatalError()
    case .expression:
      fatalError()
    case .intLiteral:
      fatalError()
    case .strLiteral:
      fatalError()
    case .maybeIntLiteral:
      fatalError()
    case .identifier:
      fatalError()
    case .maybeIdentifier:
      fatalError()
    case .funcDecl:
      fatalError()
    case .funcArgDeclList:
      fatalError()
    case .funcArgDecl:
      fatalError()
    case .funcRetDecl:
      fatalError()
    case .funcCall:
      fatalError()
    case .funcCallArgList:
      fatalError()
    case .maybeWhitespace:
      fatalError()
    case .whitespace:
      fatalError()
    case .ifStatement:
      fatalError()
    case .codeBlock:
      fatalError()
    case .codeBlockStatements:
      fatalError()
    case .codeBlockStatement:
      fatalError()
    }
  case .terminal(let text):
    fatalError("unexpected terminal: \"\(text)\"")
  }
}

internal func toText(match: ASTMatch) -> String {
  switch match {
  case .terminal(let x): x
  case .nonTerminal(_, let rhs): rhs.map(toText).joined()
  }
}
