import LearnParsers

public enum SimpleGrammarKeyword: SymbolProto {
  case codeFile
  case typeIdentifier
  case varDecl
  case varAssign
  case expression
  case intLiteral
  case strLiteral
  case maybeIntLiteral
  case identifier
  case maybeIdentifier
  case funcDecl
  case funcArgDeclList
  case funcArgDecl
  case funcRetDecl
  case funcCall
  case funcCallArgList
  case maybeWhitespace
  case whitespace
  case ifStatement
  case codeBlock
  case codeBlockStatements
  case codeBlockStatement
}

public class SimpleGrammar: Grammar<String, SimpleGrammarKeyword> {
  public init() {
    var rules: [Rule] = [
      .init(lhs: .codeFile, rhs: [.nonTerminal(.maybeWhitespace)]),
      .init(
        lhs: .codeFile,
        rhs: [.nonTerminal(.maybeWhitespace), .nonTerminal(.funcDecl), .nonTerminal(.codeFile)]
      ),
      // Whitespace
      .init(lhs: .maybeWhitespace, rhs: []),
      .init(lhs: .maybeWhitespace, rhs: [.nonTerminal(.whitespace)]),
      .init(lhs: .whitespace, rhs: [.terminal(" "), .nonTerminal(.maybeWhitespace)]),
      .init(lhs: .whitespace, rhs: [.terminal("\n"), .nonTerminal(.maybeWhitespace)]),
      .init(lhs: .whitespace, rhs: [.terminal("\t"), .nonTerminal(.maybeWhitespace)]),

      // Types
      .init(lhs: .typeIdentifier, rhs: [.terminal("s"), .terminal("t"), .terminal("r")]),
      .init(lhs: .typeIdentifier, rhs: [.terminal("i"), .terminal("n"), .terminal("t")]),

      // Function declarations.
      .init(
        lhs: .funcDecl,
        rhs: [
          .terminal("f"),
          .terminal("n"),
          .nonTerminal(.whitespace),
          .nonTerminal(.identifier),
          .nonTerminal(.maybeWhitespace),
          .terminal("("),
          .nonTerminal(.funcArgDeclList),
          .terminal(")"),
          .nonTerminal(.funcRetDecl),
          .nonTerminal(.codeBlock),
        ]
      ),
      .init(lhs: .funcRetDecl, rhs: [.nonTerminal(.maybeWhitespace)]),
      .init(
        lhs: .funcRetDecl,
        rhs: [
          .nonTerminal(.maybeWhitespace),
          .terminal("-"),
          .terminal(">"),
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.typeIdentifier),
          .nonTerminal(.maybeWhitespace),
        ]
      ),
      .init(lhs: .funcArgDeclList, rhs: []),
      .init(
        lhs: .funcArgDeclList,
        rhs: [
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.funcArgDecl),
          .nonTerminal(.maybeWhitespace),
          .terminal(","),
          .nonTerminal(.funcArgDeclList),
        ]
      ),
      .init(
        lhs: .funcArgDeclList,
        rhs: [
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.funcArgDecl),
          .nonTerminal(.maybeWhitespace),
        ]
      ),
      .init(
        lhs: .funcArgDecl,
        rhs: [
          .nonTerminal(.identifier),
          .nonTerminal(.maybeWhitespace),
          .terminal(":"),
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.typeIdentifier),
        ]
      ),

      // Code blocks.
      .init(
        lhs: .codeBlock,
        rhs: [
          .terminal("{"),
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.codeBlockStatements),
          .terminal("}"),
        ]
      ),
      .init(
        lhs: .codeBlockStatements,
        rhs: []
      ),
      .init(
        lhs: .codeBlockStatements,
        rhs: [
          .nonTerminal(.codeBlockStatement)
        ]
      ),
      .init(
        lhs: .codeBlockStatements,
        rhs: [
          .nonTerminal(.codeBlockStatement),
          .nonTerminal(.whitespace),
          .nonTerminal(.codeBlockStatements),
        ]
      ),
      .init(lhs: .codeBlockStatement, rhs: [.nonTerminal(.ifStatement)]),
      .init(lhs: .codeBlockStatement, rhs: [.nonTerminal(.varDecl)]),
      .init(lhs: .codeBlockStatement, rhs: [.nonTerminal(.varAssign)]),
      .init(lhs: .codeBlockStatement, rhs: [.nonTerminal(.funcCall)]),

      // Statements
      .init(
        lhs: .ifStatement,
        rhs: [
          .terminal("?"),
          .terminal("i"),
          .terminal("f"),
          .nonTerminal(.whitespace),
          .terminal("("),
          .nonTerminal(.expression),
          .terminal(")"),
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.codeBlock),
        ]
      ),
      .init(
        lhs: .varDecl,
        rhs: [
          .nonTerminal(.identifier),
          .nonTerminal(.maybeWhitespace),
          .terminal(":"),
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.typeIdentifier),
          .nonTerminal(.maybeWhitespace),
          .terminal("="),
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.expression),
        ]
      ),
      .init(
        lhs: .varAssign,
        rhs: [
          .nonTerminal(.identifier),
          .nonTerminal(.maybeWhitespace),
          .terminal("="),
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.expression),
        ]
      ),
      .init(
        lhs: .funcCall,
        rhs: [
          .nonTerminal(.identifier),
          .terminal("("),
          .nonTerminal(.funcCallArgList),
          .terminal(")"),
        ]
      ),
      .init(
        lhs: .funcCallArgList,
        rhs: []
      ),
      .init(
        lhs: .funcCallArgList,
        rhs: [
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.expression),
          .nonTerminal(.maybeWhitespace),
        ]
      ),
      .init(
        lhs: .funcCallArgList,
        rhs: [
          .nonTerminal(.maybeWhitespace),
          .nonTerminal(.expression),
          .nonTerminal(.maybeWhitespace),
          .terminal(","),
          .nonTerminal(.funcCallArgList),
        ]
      ),

      // Expressions
      .init(lhs: .expression, rhs: [.nonTerminal(.funcCall)]),
      .init(lhs: .expression, rhs: [.nonTerminal(.identifier)]),
      .init(lhs: .expression, rhs: [.nonTerminal(.intLiteral)]),

      // Helper used by identifier to enforce non-emptiness
      .init(lhs: .maybeIdentifier, rhs: [.nonTerminal(.identifier)]),
      .init(lhs: .maybeIdentifier, rhs: []),

      // Literal helpers.
      .init(lhs: .intLiteral, rhs: [.terminal("0")]),
      .init(lhs: .maybeIntLiteral, rhs: []),
    ]

    for ch in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_" {
      rules.append(
        .init(lhs: .identifier, rhs: [.terminal(String(ch)), .nonTerminal(.maybeIdentifier)])
      )
    }

    for ch in "0123456789" {
      if ch != "0".first! {
        rules.append(
          .init(lhs: .intLiteral, rhs: [.terminal(String(ch)), .nonTerminal(.maybeIntLiteral)])
        )
      }
      rules.append(
        .init(lhs: .maybeIntLiteral, rhs: [.terminal(String(ch)), .nonTerminal(.maybeIntLiteral)])
      )
    }
    super.init(
      start: .codeFile,
      rules: rules
    )
  }
}
