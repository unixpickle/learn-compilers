import LearnParsers
import Testing

@testable import LearnCompilers

@Test func testSimpleGrammarIdentity() async throws {
  let parser = try LR1Parser(grammar: SimpleGrammar())

  // Declarations, expressions, calls.
  try await testSimpleGrammarForText(
    parser: parser,
    text: "fn main() -> int {\n  x: int = 3\n  y: int = add(x, 4)\n  print(y)\n}"
  )

  // Multiple functions
  try await testSimpleGrammarForText(
    parser: parser,
    text: "fn main() -> int {\n  x: int = 3\n}\nfn foo() {\n  print(3)\n}"
  )

  // Function arguments
  try await testSimpleGrammarForText(
    parser: parser,
    text:
      "fn main(x: int) -> int {\n  x: int = 3\n}\nfn foo(x: int, y: str, z: int) {\n  print(3)\n}"
  )

  // Multiple call arguments.
  try await testSimpleGrammarForText(
    parser: parser,
    text:
      "fn main(x: int) -> int {\n  x: int = 3\n  foo(x, 3, y)}"
  )

  // If statements
  try await testSimpleGrammarForText(
    parser: parser,
    text:
      "fn main(x: int) -> int {\n  x: int = 3\n  ?if (x) {\n    print(0)\n  }\n  print(1)\n}"
  )
}

func testSimpleGrammarForText(
  parser: LR1Parser<String, SimpleGrammarKeyword, SimpleGrammar>, text: String
) async throws {
  var parser = parser
  let result = try await parser.read(StringParserReader(text))
  #expect(reconstructTextOfTree(result) == text)
}

func reconstructTextOfTree(_ match: ParserMatch<String, SimpleGrammarKeyword>) -> String {
  switch match {
  case .terminal(let x): x
  case .nonTerminal(_, let rhs): rhs.map(reconstructTextOfTree).joined()
  }
}
