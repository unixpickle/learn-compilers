import Foundation
import LearnParsers

public final class Parser {
  public typealias ParserType = LR1Parser<String, SimpleGrammarKeyword, SimpleGrammar>

  private static let lock = NSLock()
  nonisolated(unsafe) private static var p: ParserType? = nil

  public static var standard: ParserType {
    lock.withLock {
      if let p = p {
        return p
      }
      do {
        p = try LR1Parser(grammar: SimpleGrammar())
      } catch {
        fatalError("ERROR initializing shared parser: \(error)")
      }
      return p!
    }
  }

  public static func parse(_ text: String) throws -> ASTMatch {
    var offset = text.startIndex
    var parser = standard
    var line = 0
    var column = 0
    while true {
      let metadata = TokenMetadata(line: line, column: column)
      do {
        if offset >= text.endIndex {
          return try parser.end()
        } else {
          let nextChar = String(text[offset])

          offset = text.index(after: offset)

          try parser.put(terminal: nextChar)

          if nextChar == "\n" {
            line += 1
            column = 0
          } else {
            column += 1
          }
        }
      } catch {
        throw ParserReadError(metadata: metadata, error: error)
      }
    }
  }

  private init() {}
}

public enum SimpleGrammarKeyword: SymbolProto {
  case codeFile
  case typeIdentifier
  case varDecl
  case varAssign
  case expression
  case maybeExpression
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
  case whileLoop
  case breakStatement
  case returnStatement
  case codeBlock
  case codeBlockStatements
  case codeBlockStatement
}

private protocol TerminalOrNonTerminal {
  var symbol: GrammarSymbol<String, SimpleGrammarKeyword> { get }
}

extension String: TerminalOrNonTerminal {
  var symbol: GrammarSymbol<String, SimpleGrammarKeyword> { .terminal(self) }
}

extension SimpleGrammarKeyword: TerminalOrNonTerminal {
  var symbol: GrammarSymbol<String, SimpleGrammarKeyword> { .nonTerminal(self) }
}

public class SimpleGrammar: Grammar<String, SimpleGrammarKeyword> {
  public init() {
    func rule(_ lhs: SimpleGrammarKeyword, _ rhs: [TerminalOrNonTerminal]) -> Rule {
      .init(lhs: lhs, rhs: rhs.map { $0.symbol })
    }

    typealias S = SimpleGrammarKeyword

    var rules: [Rule] = [
      rule(.codeFile, [S.maybeWhitespace]),
      rule(.codeFile, [S.maybeWhitespace, S.funcDecl, S.codeFile]),

      // Whitespace
      rule(.maybeWhitespace, []),
      rule(.maybeWhitespace, [S.whitespace]),
      rule(.whitespace, [" ", S.maybeWhitespace]),
      rule(.whitespace, ["\n", S.maybeWhitespace]),
      rule(.whitespace, ["\t", S.maybeWhitespace]),

      // Types
      rule(.typeIdentifier, ["s", "t", "r"]),
      rule(.typeIdentifier, ["i", "n", "t"]),

      // Function declarations.
      rule(
        .funcDecl,
        [
          "f",
          "n",
          S.whitespace,
          S.identifier,
          S.maybeWhitespace,
          "(",
          S.funcArgDeclList,
          ")",
          S.funcRetDecl,
          S.codeBlock,
        ]
      ),
      rule(.funcRetDecl, [S.maybeWhitespace]),
      rule(
        .funcRetDecl,
        [S.maybeWhitespace, "-", ">", S.maybeWhitespace, S.typeIdentifier, S.maybeWhitespace]
      ),
      rule(.funcArgDeclList, []),
      rule(
        .funcArgDeclList,
        [S.maybeWhitespace, S.funcArgDecl, S.maybeWhitespace, ",", S.funcArgDeclList]
      ),
      rule(.funcArgDeclList, [S.maybeWhitespace, S.funcArgDecl, S.maybeWhitespace]),
      rule(
        .funcArgDecl,
        [S.identifier, S.maybeWhitespace, ":", S.maybeWhitespace, S.typeIdentifier]
      ),

      // Code blocks.
      rule(.codeBlock, ["{", S.maybeWhitespace, S.codeBlockStatements, "}"]),
      rule(.codeBlockStatements, []),
      rule(.codeBlockStatements, [S.codeBlockStatement]),
      rule(.codeBlockStatements, [S.codeBlockStatement, S.whitespace, S.codeBlockStatements]),
      rule(.codeBlockStatement, [S.ifStatement]),
      rule(.codeBlockStatement, [S.whileLoop]),
      rule(.codeBlockStatement, [S.varDecl]),
      rule(.codeBlockStatement, [S.varAssign]),
      rule(.codeBlockStatement, [S.funcCall]),
      rule(.codeBlockStatement, [S.breakStatement]),
      rule(.codeBlockStatement, [S.returnStatement]),

      // Statements
      rule(
        .ifStatement,
        ["i", "f", "?", S.whitespace, "(", S.expression, ")", S.maybeWhitespace, S.codeBlock]
      ),
      rule(
        .whileLoop,
        [
          "w", "h", "i", "l", "e", "?",
          S.whitespace, "(", S.expression, ")", S.maybeWhitespace, S.codeBlock,
        ]
      ),
      rule(.breakStatement, ["b", "r", "e", "a", "k", "!", "(", ")"]),
      rule(.returnStatement, ["r", "e", "t", "u", "r", "n", "!", "(", S.maybeExpression, ")"]),
      rule(
        .varDecl,
        [
          S.identifier,
          S.maybeWhitespace,
          ":",
          S.maybeWhitespace,
          S.typeIdentifier,
          S.maybeWhitespace,
          "=",
          S.maybeWhitespace,
          S.expression,
        ]
      ),
      rule(
        .varAssign,
        [S.identifier, S.maybeWhitespace, "=", S.maybeWhitespace, S.expression]
      ),
      rule(.funcCall, [S.identifier, "(", S.funcCallArgList, ")"]),
      rule(.funcCallArgList, []),
      rule(.funcCallArgList, [S.maybeWhitespace, S.expression, S.maybeWhitespace]),
      rule(
        .funcCallArgList,
        [S.maybeWhitespace, S.expression, S.maybeWhitespace, ",", S.funcCallArgList]
      ),

      // Expressions
      rule(.expression, [S.funcCall]),
      rule(.expression, [S.identifier]),
      rule(.expression, [S.intLiteral]),
      rule(.maybeExpression, [S.expression]),
      rule(.maybeExpression, []),

      // Helper used by identifier to enforce non-emptiness
      rule(.maybeIdentifier, [S.identifier]),
      rule(.maybeIdentifier, []),

      // Literal helpers.
      rule(.intLiteral, ["0"]),
      rule(.maybeIntLiteral, []),
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
