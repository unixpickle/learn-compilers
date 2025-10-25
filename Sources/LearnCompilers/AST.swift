import LearnParsers

public typealias ASTMatch = ParserMatch<String, SimpleGrammarKeyword>

public enum ChildrenOrCode {
  case children([ASTNode])
  case code(String)

  internal var children: [ASTNode]? {
    guard case .children(let x) = self else {
      return nil
    }
    return x
  }

  internal var code: String? {
    guard case .code(let x) = self else {
      return nil
    }
    return x
  }
}

public protocol ASTNode {
  /// Starts as nil during a parse, but gets filled in during decoration.
  var position: Position? { get set }

  /// Determine the children or text of this node.
  /// When setting contents, the structure (count and type) must be identical.
  var contents: ChildrenOrCode { get set }
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
    public var position: Position? = nil

    // One of these two should be set.
    public var variable: Variable? = nil
    public var function: Function? = nil

    public var name: String

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.identifier, _) = match else {
        fatalError()
      }
      self.name = toText(match: match)
    }

    public var contents: ChildrenOrCode {
      get { .code(name) }
      set { name = newValue.code! }
    }
  }

  public struct TypeIdentifier: ASTNode {
    public var position: Position? = nil
    public var name: String

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.typeIdentifier, _) = match else {
        fatalError()
      }
      self.name = toText(match: match)
    }

    public var contents: ChildrenOrCode {
      get { .code(name) }
      set { name = newValue.code! }
    }
  }

  public struct Raw: ASTNode {
    public var position: Position? = nil
    public var text: String

    internal init(match: ASTMatch) {
      self.text = toText(match: match)
    }

    internal init(matches: some Sequence<ASTMatch>) {
      self.text = matches.map { toText(match: $0) }.joined()
    }

    public var contents: ChildrenOrCode {
      get { .code(text) }
      set { text = newValue.code! }
    }
  }

  public struct FuncDecl: ASTNode {
    public struct ArgDecl: ASTNode {
      public var position: Position? = nil
      public var leadingWhitespace: Raw
      public var identifier: Identifier
      public var whitespace1: Raw
      public var colon: Raw
      public var whitespace2: Raw
      public var typeID: TypeIdentifier
      public var trailingWhitespace: Raw
      public var comma: Raw?

      public var contents: ChildrenOrCode {
        get {
          .children(
            [
              leadingWhitespace, identifier, whitespace1, colon, whitespace2, typeID,
              trailingWhitespace,
            ] + maybeList(comma)
          )
        }
        set {
          let ch = newValue.children!
          leadingWhitespace = ch[0] as! Raw
          identifier = ch[1] as! Identifier
          whitespace1 = ch[2] as! Raw
          colon = ch[3] as! Raw
          whitespace2 = ch[4] as! Raw
          typeID = ch[5] as! TypeIdentifier
          trailingWhitespace = ch[6] as! Raw
          assert((ch.count == 8) == (comma != nil))
          if comma != nil {
            comma = (ch[7] as! Raw)
          }
        }
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
      public var position: Position? = nil
      public var whitespace1: Raw
      public var arrow: Raw
      public var whitespace2: Raw
      public var retType: TypeIdentifier

      public var contents: ChildrenOrCode {
        get {
          .children([whitespace1, arrow, whitespace2, retType])
        }
        set {
          let ch = newValue.children!
          whitespace1 = ch[0] as! Raw
          arrow = ch[1] as! Raw
          whitespace2 = ch[2] as! Raw
          retType = ch[3] as! TypeIdentifier
        }
      }
    }

    public var position: Position? = nil
    public var scope: Scope? = nil
    public var function: Function? = nil

    public var leadingWhitespace: Raw
    public var fn: Raw
    public var whitespace1: Raw
    public var name: Identifier
    public var whitespace2: Raw
    public var openParenthesis: Raw
    public var args: [ArgDecl]
    public var closeParenthesis: Raw
    public var retType: RetDecl?
    public var trailingWhitespace: Raw
    public var block: Block

    public var contents: ChildrenOrCode {
      get {
        .children(
          [leadingWhitespace, fn, whitespace1, name, whitespace2, openParenthesis] + args + [
            closeParenthesis
          ] + maybeList(retType) + [trailingWhitespace, block]
        )
      }
      set {
        let ch = newValue.children!
        leadingWhitespace = ch[0] as! Raw
        fn = ch[1] as! Raw
        whitespace1 = ch[2] as! Raw
        name = ch[3] as! Identifier
        whitespace2 = ch[4] as! Raw
        openParenthesis = ch[5] as! Raw
        args = ch[6..<(6 + args.count)].map { $0 as! ArgDecl }
        closeParenthesis = ch[6 + args.count] as! Raw
        let offset = args.count + 7 + (retType == nil ? 0 : 1)
        if retType != nil {
          retType = (ch[7 + args.count] as! RetDecl)
        }
        trailingWhitespace = ch[offset] as! Raw
        block = ch[offset + 1] as! Block
      }
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

      args = []
      var argDecl = argDeclRHS
      while argDecl.count == 5 {
        guard case .nonTerminal(.funcArgDecl, let argRHS) = argDecl[1] else {
          fatalError()
        }
        args.append(
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
        args.append(
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
      public var position: Position? = nil
      public var statement: Statement
      public var trailingWhitespace: Raw?

      public var contents: ChildrenOrCode {
        get {
          .children([statement] + maybeList(trailingWhitespace))
        }
        set {
          let ch = newValue.children!
          statement = ch[0] as! Statement
          if trailingWhitespace != nil {
            trailingWhitespace = (ch[1] as! Raw)
          }
        }
      }
    }

    public var position: Position? = nil
    public var scope: Scope? = nil

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
      get {
        .children([openBrace, whitespace] + statements + [closeBrace])
      }
      set {
        let ch = newValue.children!
        openBrace = ch[0] as! Raw
        whitespace = ch[1] as! Raw
        statements = ch[2..<(2 + statements.count)].map { $0 as! BlockStatement }
        closeBrace = ch[2 + statements.count] as! Raw
      }
    }
  }

  public struct IfStatement: ASTNode {
    public var position: Position? = nil
    public var ifText: Raw
    public var whitespace1: Raw
    public var openParenthesis: Raw
    public var expression: Expression
    public var closeParenthesis: Raw
    public var whitespace2: Raw
    public var block: Block

    public var contents: ChildrenOrCode {
      get {
        .children([
          ifText, whitespace1, openParenthesis, expression, closeParenthesis, whitespace2, block,
        ])
      }
      set {
        let ch = newValue.children!
        ifText = ch[0] as! Raw
        whitespace1 = ch[1] as! Raw
        openParenthesis = ch[2] as! Raw
        expression = ch[3] as! Expression
        closeParenthesis = ch[4] as! Raw
        whitespace2 = ch[5] as! Raw
        block = ch[6] as! Block
      }
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
    public var position: Position? = nil
    public var whileText: Raw
    public var whitespace1: Raw
    public var openParenthesis: Raw
    public var expression: Expression
    public var closeParenthesis: Raw
    public var whitespace2: Raw
    public var block: Block

    public var contents: ChildrenOrCode {
      get {
        .children([
          whileText, whitespace1, openParenthesis, expression, closeParenthesis, whitespace2, block,
        ])
      }
      set {
        let ch = newValue.children!
        whileText = ch[0] as! Raw
        whitespace1 = ch[1] as! Raw
        openParenthesis = ch[2] as! Raw
        expression = ch[3] as! Expression
        closeParenthesis = ch[4] as! Raw
        whitespace2 = ch[5] as! Raw
        block = ch[6] as! Block
      }
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
    public var position: Position? = nil
    public var returnText: Raw
    public var openParenthesis: Raw
    public var expression: Expression?
    public var closeParenthesis: Raw

    public var contents: ChildrenOrCode {
      get {
        .children([returnText, openParenthesis] + maybeList(expression) + [closeParenthesis])
      }
      set {
        let ch = newValue.children!
        returnText = ch[0] as! Raw
        openParenthesis = ch[1] as! Raw
        if expression != nil {
          expression = (ch[2] as! Expression)
          closeParenthesis = ch[3] as! Raw
        } else {
          closeParenthesis = ch[2] as! Raw
        }
      }
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
    public var position: Position? = nil
    public var breakText: Raw

    public var contents: ChildrenOrCode {
      get { .children([breakText]) }
      set { breakText = newValue.children![0] as! Raw }
    }

    internal init(match: ASTMatch) {
      guard case .nonTerminal(.breakStatement, let rhs) = match else {
        fatalError()
      }
      breakText = Raw(matches: rhs)
    }
  }

  public struct VarDecl: ASTNode {
    public var position: Position? = nil
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
      get {
        .children([
          identifier, whitespace1, colon, whitespace2, typeID, whitespace3, equals, whitespace4,
          expression,
        ])
      }
      set {
        let ch = newValue.children!
        identifier = ch[0] as! Identifier
        whitespace1 = ch[1] as! Raw
        colon = ch[2] as! Raw
        whitespace2 = ch[3] as! Raw
        typeID = ch[4] as! TypeIdentifier
        whitespace3 = ch[5] as! Raw
        equals = ch[6] as! Raw
        whitespace4 = ch[7] as! Raw
        expression = ch[8] as! Expression
      }
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
    public var position: Position? = nil
    public var identifier: Identifier
    public var whitespace1: Raw
    public var equals: Raw
    public var whitespace2: Raw
    public var expression: Expression

    public var contents: ChildrenOrCode {
      get {
        .children([identifier, whitespace1, equals, whitespace2, expression])
      }
      set {
        let ch = newValue.children!
        identifier = ch[0] as! Identifier
        whitespace1 = ch[1] as! Raw
        equals = ch[2] as! Raw
        whitespace2 = ch[3] as! Raw
        expression = ch[4] as! Expression
      }
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
    public var position: Position? = nil
    public var text: String

    public var integer: Int64 {
      Int64(text)!
    }

    public var contents: ChildrenOrCode {
      get { .code(text) }
      set { text = newValue.code! }
    }

    internal init(match: ASTMatch) {
      self.text = toText(match: match)
    }
  }

  public struct FuncCall: ASTNode {
    public struct FuncArg: ASTNode {
      public var position: Position? = nil
      public var whitespace1: Raw
      public var expression: Expression
      public var whitespace2: Raw
      public var comma: Raw?

      public var contents: ChildrenOrCode {
        get {
          .children([whitespace1, expression, whitespace2] + maybeList(comma))
        }
        set {
          let ch = newValue.children!
          whitespace1 = ch[0] as! Raw
          expression = ch[1] as! Expression
          whitespace2 = ch[2] as! Raw
          if comma != nil {
            comma = (ch[3] as! Raw)
          }
        }
      }
    }

    public var position: Position? = nil
    public var identifier: Identifier
    public var openParenthesis: Raw
    public var args: [FuncArg]
    public var closeParenthesis: Raw

    public var contents: ChildrenOrCode {
      get {
        .children([identifier, openParenthesis] + args + [closeParenthesis])
      }
      set {
        let ch = newValue.children!
        identifier = ch[0] as! Identifier
        openParenthesis = ch[1] as! Raw
        args = ch[2..<(args.count + 2)].map { $0 as! FuncArg }
        closeParenthesis = ch[2 + args.count] as! Raw
      }
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
    indirect case funcCall(FuncCall, Position?)
    case identifier(Identifier, Position?)
    case intLiteral(IntLiteral, Position?)

    public var funcCall: FuncCall? {
      if case .funcCall(let c, _) = self { c } else { nil }
    }

    public var identifier: Identifier? {
      if case .identifier(let c, _) = self { c } else { nil }
    }

    public var intLiteral: IntLiteral? {
      if case .intLiteral(let c, _) = self { c } else { nil }
    }

    public var contents: ChildrenOrCode {
      get {
        switch self {
        case .funcCall(let c, _): .children([c])
        case .identifier(let c, _): .children([c])
        case .intLiteral(let c, _): .children([c])
        }
      }
      set {
        let c = newValue.children![0]
        self =
          switch self {
          case .funcCall(_, let p): .funcCall(c as! FuncCall, p)
          case .identifier(_, let p): .identifier(c as! Identifier, p)
          case .intLiteral(_, let p): .intLiteral(c as! IntLiteral, p)
          }
      }
    }

    public var position: Position? {
      get {
        switch self {
        case .funcCall(_, let p): p
        case .identifier(_, let p): p
        case .intLiteral(_, let p): p
        }
      }
      set {
        self =
          switch self {
          case .funcCall(let c, _): .funcCall(c, newValue)
          case .identifier(let c, _): .identifier(c, newValue)
          case .intLiteral(let c, _): .intLiteral(c, newValue)
          }
      }
    }

    internal static func from(match: ASTMatch) -> Self {
      guard case .nonTerminal(.expression, let rhs) = match else {
        fatalError()
      }
      assert(rhs.count == 1)
      switch rhs[0] {
      case .nonTerminal(.funcCall, _): return .funcCall(FuncCall(match: rhs[0]), nil)
      case .nonTerminal(.identifier, _): return .identifier(Identifier(match: rhs[0]), nil)
      case .nonTerminal(.intLiteral, _): return .intLiteral(IntLiteral(match: rhs[0]), nil)
      default: fatalError()
      }
    }
  }

  public enum Statement: ASTNode {
    indirect case funcCall(FuncCall, Position?)
    case varDecl(VarDecl, Position?)
    case varAssign(VarAssign, Position?)
    case ifStatement(IfStatement, Position?)
    case whileLoop(WhileLoop, Position?)
    case breakStatement(BreakStatement, Position?)
    case returnStatement(ReturnStatement, Position?)

    public var funcCall: FuncCall? {
      if case .funcCall(let c, _) = self { c } else { nil }
    }

    public var varDecl: VarDecl? {
      if case .varDecl(let c, _) = self { c } else { nil }
    }

    public var varAssign: VarAssign? {
      if case .varAssign(let c, _) = self { c } else { nil }
    }

    public var ifStatement: IfStatement? {
      if case .ifStatement(let c, _) = self { c } else { nil }
    }

    public var whileLoop: WhileLoop? {
      if case .whileLoop(let c, _) = self { c } else { nil }
    }

    public var breakStatement: BreakStatement? {
      if case .breakStatement(let c, _) = self { c } else { nil }
    }

    public var returnStatement: ReturnStatement? {
      if case .returnStatement(let c, _) = self { c } else { nil }
    }

    public var contents: ChildrenOrCode {
      get {
        switch self {
        case .funcCall(let c, _): .children([c])
        case .varDecl(let c, _): .children([c])
        case .varAssign(let c, _): .children([c])
        case .ifStatement(let c, _): .children([c])
        case .whileLoop(let c, _): .children([c])
        case .breakStatement(let c, _): .children([c])
        case .returnStatement(let c, _): .children([c])
        }
      }
      set {
        let c = newValue.children![0]
        self =
          switch self {
          case .funcCall(_, let p): .funcCall(c as! FuncCall, p)
          case .varDecl(_, let p): .varDecl(c as! VarDecl, p)
          case .varAssign(_, let p): .varAssign(c as! VarAssign, p)
          case .ifStatement(_, let p): .ifStatement(c as! IfStatement, p)
          case .whileLoop(_, let p): .whileLoop(c as! WhileLoop, p)
          case .breakStatement(_, let p): .breakStatement(c as! BreakStatement, p)
          case .returnStatement(_, let p): .returnStatement(c as! ReturnStatement, p)
          }
      }
    }

    public var position: Position? {
      get {
        switch self {
        case .funcCall(_, let p): p
        case .varDecl(_, let p): p
        case .varAssign(_, let p): p
        case .ifStatement(_, let p): p
        case .whileLoop(_, let p): p
        case .breakStatement(_, let p): p
        case .returnStatement(_, let p): p
        }
      }
      set {
        self =
          switch self {
          case .funcCall(let c, _): .funcCall(c, newValue)
          case .varDecl(let c, _): .varDecl(c, newValue)
          case .varAssign(let c, _): .varAssign(c, newValue)
          case .ifStatement(let c, _): .ifStatement(c, newValue)
          case .whileLoop(let c, _): .whileLoop(c, newValue)
          case .breakStatement(let c, _): .breakStatement(c, newValue)
          case .returnStatement(let c, _): .returnStatement(c, newValue)
          }
      }
    }

    internal static func from(match: ASTMatch) -> Self {
      guard case .nonTerminal(.codeBlockStatement, let rhs) = match else {
        fatalError()
      }
      assert(rhs.count == 1)
      switch rhs[0] {
      case .nonTerminal(.funcCall, _): return .funcCall(FuncCall(match: rhs[0]), nil)
      case .nonTerminal(.varDecl, _): return .varDecl(VarDecl(match: rhs[0]), nil)
      case .nonTerminal(.varAssign, _): return .varAssign(VarAssign(match: rhs[0]), nil)
      case .nonTerminal(.ifStatement, _): return .ifStatement(IfStatement(match: rhs[0]), nil)
      case .nonTerminal(.whileLoop, _): return .whileLoop(WhileLoop(match: rhs[0]), nil)
      case .nonTerminal(.breakStatement, _):
        return .breakStatement(BreakStatement(match: rhs[0]), nil)
      case .nonTerminal(.returnStatement, _):
        return .returnStatement(ReturnStatement(match: rhs[0]), nil)
      default: fatalError()
      }
    }
  }

  public var position: Position? = nil
  public var functions: [FuncDecl]
  public var trailingWhitespace: Raw

  public var contents: ChildrenOrCode {
    get {
      .children(functions + [trailingWhitespace])
    }
    set {
      let ch = newValue.children!
      functions = ch[..<functions.count].map { $0 as! FuncDecl }
      trailingWhitespace = ch[functions.count] as! Raw
    }
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
