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

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.identifier, _) = match else {
        fatalError()
      }
      self.name = toText(match: match)
    }

    public var contents: ChildrenOrCode { .code(name) }
  }

  public struct TypeIdentifier: ASTNode {
    public var name: String

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.typeIdentifier, _) = match else {
        fatalError()
      }
      self.name = toText(match: match)
    }

    public var contents: ChildrenOrCode { .code(name) }
  }

  public struct Raw: ASTNode {
    public var text: String

    internal init(match: ASTMatch) {
      self.text = toText(match: match)
    }

    internal init(matches: some Sequence<ASTMatch>) {
      self.text = matches.map { toText(match: $0) }.joined()
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
        self.identifier = Identifier(match: rhs[0])
        self.whitespace1 = Raw(match: rhs[1])
        self.colon = Raw(match: rhs[2])
        self.whitespace2 = Raw(match: rhs[3])
        self.typeID = TypeIdentifier(match: rhs[4])
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
      fn = Raw(matches: rhs[..<2])
      whitespace1 = Raw(match: rhs[2])
      name = Identifier(match: rhs[3])
      whitespace2 = Raw(match: rhs[4])
      openParenthesis = Raw(match: rhs[5])

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
            leadingWhitespace: Raw(match: argDecl[0]),
            trailingWhitespace: Raw(match: argDecl[2]),
            comma: Raw(match: argDecl[3]),
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
            leadingWhitespace: Raw(match: argDecl[0]),
            trailingWhitespace: Raw(match: argDecl[2]),
            comma: nil,
            rhs: argRHS
          )
        )
      }

      closeParenthesis = Raw(match: rhs[7])

      guard case .nonTerminal(.funcRetDecl, let retRHS) = rhs[8] else {
        fatalError()
      }
      if retRHS.count == 1 {
        trailingWhitespace = Raw(match: retRHS[0])
        retType = nil
      } else {
        retType = RetDecl(
          whitespace1: Raw(match: retRHS[0]),
          arrow: Raw(matches: retRHS[1...2]),
          whitespace2: Raw(match: retRHS[3]),
          retType: TypeIdentifier(match: retRHS[4])
        )
        trailingWhitespace = Raw(match: retRHS[5])
      }

      self.block = Block(match: rhs[9])
    }
  }

  public struct Block: ASTNode {
    public struct BlockStatement: ASTNode {
      public var statement: Statement
      public var trailingWhitespace: Raw?

      public var contents: ChildrenOrCode {
        .children([statement] + maybeList(trailingWhitespace))
      }
    }

    public var openBrace: Raw
    public var whitespace: Raw
    public var statements: [BlockStatement]
    public var closeBrace: Raw

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.codeBlock, let rhs) = match else {
        fatalError()
      }
      openBrace = Raw(match: rhs[0])
      whitespace = Raw(match: rhs[1])
      closeBrace = Raw(match: rhs[3])

      guard case .nonTerminal(.codeBlockStatements, let startStatements) = rhs[2] else {
        fatalError()
      }
      statements = []
      var statementsRHS = startStatements
      while statementsRHS.count == 3 {
        statements.append(
          .init(
            statement: Statement.from(match: statementsRHS[0]),
            trailingWhitespace: Raw(match: statementsRHS[1])
          )
        )
        guard case .nonTerminal(.codeBlockStatements, let n) = statementsRHS[2] else {
          fatalError()
        }
        statementsRHS = n
      }
      if statementsRHS.count == 1 {
        statements.append(
          .init(statement: Statement.from(match: statementsRHS[0]), trailingWhitespace: nil))
      }
    }

    public var contents: ChildrenOrCode {
      .children([openBrace, whitespace] + statements + [closeBrace])
    }
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

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.ifStatement, let rhs) = match else {
        fatalError()
      }
      ifText = Raw(matches: rhs[0..<3])
      whitespace1 = Raw(match: rhs[3])
      openParenthesis = Raw(match: rhs[4])
      expression = Expression.from(match: rhs[5])
      closeParenthesis = Raw(match: rhs[6])
      whitespace2 = Raw(match: rhs[7])
      block = Block(match: rhs[8])
    }
  }

  public struct WhileLoop: ASTNode {
    public var whileText: Raw
    public var whitespace1: Raw
    public var openParenthesis: Raw
    public var expression: Expression
    public var closeParenthesis: Raw
    public var whitespace2: Raw
    public var block: Block

    public var contents: ChildrenOrCode {
      .children([
        whileText, whitespace1, openParenthesis, expression, closeParenthesis, whitespace2, block,
      ])
    }

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.whileLoop, let rhs) = match else {
        fatalError()
      }
      whileText = Raw(matches: rhs[0..<6])
      whitespace1 = Raw(match: rhs[6])
      openParenthesis = Raw(match: rhs[7])
      expression = Expression.from(match: rhs[8])
      closeParenthesis = Raw(match: rhs[9])
      whitespace2 = Raw(match: rhs[10])
      block = Block(match: rhs[11])
    }
  }

  public struct ReturnStatement: ASTNode {
    public var returnText: Raw
    public var openParenthesis: Raw
    public var expression: Expression?
    public var closeParenthesis: Raw

    public var contents: ChildrenOrCode {
      .children([returnText, openParenthesis] + maybeList(expression) + [closeParenthesis])
    }

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.returnStatement, let rhs) = match else {
        fatalError()
      }
      returnText = Raw(matches: rhs[0..<7])
      openParenthesis = Raw(match: rhs[7])
      guard case .nonTerminal(.maybeExpression, let maybeExpr) = rhs[8] else {
        fatalError()
      }
      if maybeExpr.isEmpty {
        expression = nil
      } else {
        expression = Expression.from(match: maybeExpr[0])
      }
      closeParenthesis = Raw(match: rhs[9])
    }
  }

  public struct BreakStatement: ASTNode {
    public var breakText: Raw

    public var contents: ChildrenOrCode {
      .children([breakText])
    }

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.breakStatement, let rhs) = match else {
        fatalError()
      }
      breakText = Raw(matches: rhs)
    }
  }

  public struct VarDecl: ASTNode {
    public var identifier: Identifier
    public var whitespace1: Raw
    public var colon: Raw
    public var whitespace2: Raw
    public var typeID: TypeIdentifier
    public var whitespace3: Raw
    public var equals: Raw
    public var whitespace4: Raw
    public var expression: Expression

    public var contents: ChildrenOrCode {
      .children([
        identifier, whitespace1, colon, whitespace2, typeID, whitespace3, equals, whitespace4,
        expression,
      ])
    }

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.varDecl, let rhs) = match else {
        fatalError()
      }
      identifier = Identifier(match: rhs[0])
      whitespace1 = Raw(match: rhs[1])
      colon = Raw(match: rhs[2])
      whitespace2 = Raw(match: rhs[3])
      typeID = TypeIdentifier(match: rhs[4])
      whitespace3 = Raw(match: rhs[5])
      equals = Raw(match: rhs[6])
      whitespace4 = Raw(match: rhs[7])
      expression = Expression.from(match: rhs[8])
    }
  }

  public struct VarAssign: ASTNode {
    public var identifier: Identifier
    public var whitespace1: Raw
    public var equals: Raw
    public var whitespace2: Raw
    public var expression: Expression

    public var contents: ChildrenOrCode {
      .children([identifier, whitespace1, equals, whitespace2, expression])
    }

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.varAssign, let rhs) = match else {
        fatalError()
      }
      identifier = Identifier(match: rhs[0])
      whitespace1 = Raw(match: rhs[1])
      equals = Raw(match: rhs[2])
      whitespace2 = Raw(match: rhs[3])
      expression = Expression.from(match: rhs[4])
    }
  }

  public struct IntLiteral: ASTNode {
    public var text: String

    public var contents: ChildrenOrCode { .code(text) }

    internal init(match: ASTMatch) {
      self.text = toText(match: match)
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

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.funcCall, let rhs) = match else {
        fatalError()
      }
      identifier = Identifier(match: rhs[0])
      openParenthesis = Raw(match: rhs[1])
      closeParenthesis = Raw(match: rhs[3])
      guard case .nonTerminal(.funcCallArgList, let outerArgList) = rhs[2] else {
        fatalError()
      }
      args = []
      var curArgList = outerArgList
      while curArgList.count == 5 {
        args.append(
          .init(
            whitespace1: Raw(match: curArgList[0]),
            expression: Expression.from(match: curArgList[1]),
            whitespace2: Raw(match: curArgList[2]),
            comma: Raw(match: curArgList[3])
          )
        )
        guard case .nonTerminal(.funcCallArgList, let r) = curArgList[4] else {
          fatalError()
        }
        curArgList = r
      }
      if curArgList.count == 3 {
        args.append(
          .init(
            whitespace1: Raw(match: curArgList[0]),
            expression: Expression.from(match: curArgList[1]),
            whitespace2: Raw(match: curArgList[2])
          )
        )
      }
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

    internal static func from(match: ASTMatch) -> Self {
      guard case .nonTerminal(.expression, let rhs) = match else {
        fatalError()
      }
      assert(rhs.count == 1)
      switch rhs[0] {
      case .nonTerminal(.funcCall, _): return .funcCall(FuncCall(match: rhs[0]))
      case .nonTerminal(.identifier, _): return .identifier(Identifier(match: rhs[0]))
      case .nonTerminal(.intLiteral, _): return .intLiteral(IntLiteral(match: rhs[0]))
      default: fatalError()
      }
    }
  }

  public enum Statement: ASTNode {
    indirect case funcCall(FuncCall)
    case varDecl(VarDecl)
    case varAssign(VarAssign)
    case ifStatement(IfStatement)
    case whileLoop(WhileLoop)
    case breakStatement(BreakStatement)
    case returnStatement(ReturnStatement)

    public var contents: ChildrenOrCode {
      switch self {
      case .funcCall(let c): .children([c])
      case .varDecl(let c): .children([c])
      case .varAssign(let c): .children([c])
      case .ifStatement(let c): .children([c])
      case .whileLoop(let c): .children([c])
      case .breakStatement(let c): .children([c])
      case .returnStatement(let c): .children([c])
      }
    }

    internal static func from(match: ASTMatch) -> Self {
      guard case .nonTerminal(.codeBlockStatement, let rhs) = match else {
        fatalError()
      }
      assert(rhs.count == 1)
      switch rhs[0] {
      case .nonTerminal(.funcCall, _): return .funcCall(FuncCall(match: rhs[0]))
      case .nonTerminal(.varDecl, _): return .varDecl(VarDecl(match: rhs[0]))
      case .nonTerminal(.varAssign, _): return .varAssign(VarAssign(match: rhs[0]))
      case .nonTerminal(.ifStatement, _): return .ifStatement(IfStatement(match: rhs[0]))
      case .nonTerminal(.whileLoop, _): return .whileLoop(WhileLoop(match: rhs[0]))
      case .nonTerminal(.breakStatement, _): return .breakStatement(BreakStatement(match: rhs[0]))
      case .nonTerminal(.returnStatement, _):
        return .returnStatement(ReturnStatement(match: rhs[0]))
      default: fatalError()
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
        FuncDecl(leadingWhitespace: Raw(match: rhs[0]), rhs: funcRHS)
      )
      guard case .nonTerminal(.codeFile, let nextRHS) = rhs[2] else {
        fatalError()
      }
      rhs = nextRHS
    }

    trailingWhitespace = Raw(match: rhs[0])
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
