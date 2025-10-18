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
      "fn main(x: int) -> int {\n  x: int = 3\n  if? (x) {\n    print(0)\n  }\n  print(1)\n}"
  )

  // While loop
  try await testSimpleGrammarForText(
    parser: parser,
    text:
      "fn main(x: int) -> int {\n  x: int = 3\n  while? (lt(x, 5)) {\n    print(x)\n  x = add(x, 1)\n  }\n  print(x)\n}"
  )

  // While loop with break
  try await testSimpleGrammarForText(
    parser: parser,
    text:
      "fn main(x: int) -> int {\n  x: int = 3\n  while? (lt(x, 5)) {\n    print(x)\n  x = add(x, 1)\n  break!()\n  }\n  print(x)\n}"
  )

  // Return
  try await testSimpleGrammarForText(
    parser: parser,
    text: "fn main(x: int) -> int {\n  x: int = 3\n  return!(add(x, 3))\n}"
  )

  // Return (empty)
  try await testSimpleGrammarForText(
    parser: parser,
    text: "fn main(x: int) {\n  x: int = 3\n  return!()\n}"
  )
}

func testSimpleGrammarForText(
  parser: LR1Parser<String, SimpleGrammarKeyword, SimpleGrammar>, text: String
) async throws {
  var parser = parser
  let result = try await parser.read(StringParserReader(text))
  #expect(toText(match: result) == text)
}
