import LearnParsers
import Testing

@testable import LearnCompilers

@Test func testASTVariableResolution() async throws {
  let parser = try LR1Parser(grammar: SimpleGrammar())

  func parseCode(_ code: String) async throws -> ASTMatch {
    var p = parser
    return try await p.read(StringParserReader(code))
  }

  let code = """
      fn main(x: int, w: int, z: int) -> int {
        print(x)
        a: int = 7
        x: int = 3
        y: int = x
        x = 2
        print(y)
        while? (lt(x, 10)) {
          z: int = y

          y: int = y

          y = sub(x, 1)

          x: int = z
          if? (eq(x, 5)) {
            a = add(x, 2)
          }
        }
      }
    """

  let match = try await parseCode(code)
  var ast = AST(match: match)
  #expect(ast.codeString == code)

  var table = ScopeTable()
  (ast, _) = ast.insertingPositions(start: Position(fileID: "myFile"))
  ast = ast.insertingScopes(table: &table)
  var errors: [VariableResolutionError]
  (ast, errors) = ast.resolvingVariables(table: &table)

  #expect(errors.isEmpty)
  if !errors.isEmpty {
    return
  }

  // Now we'll find various symbols and make sure they are correct.
  let argX = ast.functions[0].args[0].identifier.variable
  let argW = ast.functions[0].args[1].identifier.variable
  let argZ = ast.functions[0].args[2].identifier.variable
  #expect(argX != argW)
  #expect(argX != argZ)
  #expect(argW != argZ)

  let funcBlock = ast.functions[0].block
  #expect(funcBlock.scope != nil)

  let printArg = funcBlock.statements[0].statement.funcCall!.args[0].expression.identifier!.variable
  #expect(printArg == argX)

  let outerA = funcBlock.statements[1].statement.varDecl!.identifier.variable
  let outerX = funcBlock.statements[2].statement.varDecl!.identifier.variable
  let outerY = funcBlock.statements[3].statement.varDecl!.identifier.variable
  let assignX1 = funcBlock.statements[4].statement.varAssign!.identifier.variable
  #expect(outerX != argX)
  #expect(assignX1 == outerX)
  #expect(assignX1 != outerA)
  #expect(assignX1 != outerY)
  let printY = funcBlock.statements[5].statement.funcCall!.args[0].expression.identifier!.variable
  #expect(printY == outerY)

  let whileLoop = funcBlock.statements[6].statement.whileLoop!
  let conditionVar = whileLoop.expression.funcCall!.args[0].expression.identifier!.variable
  #expect(conditionVar == outerX)

  let whileBlock = whileLoop.block
  let loopZ = whileBlock.statements[0].statement.varDecl!.identifier.variable
  let loopY = whileBlock.statements[1].statement.varDecl!.identifier.variable
  #expect(loopZ != loopY)
  let loopYInit = whileBlock.statements[1].statement.varDecl!.expression.identifier!.variable
  #expect(loopYInit == outerY)
  #expect(loopY != loopYInit)
  let yAssignVar = whileBlock.statements[2].statement.varAssign!.identifier.variable
  let yAssignX = whileBlock.statements[2].statement.varAssign!.expression.funcCall!.args[0]
    .expression.identifier!.variable
  #expect(yAssignVar == loopY)
  #expect(yAssignX == outerX)

  let loopX = whileBlock.statements[3].statement.varDecl!.identifier.variable
  #expect(loopX != outerX)

  let ifStatement = whileBlock.statements[4].statement.ifStatement!
  let ifCondX = ifStatement.expression.funcCall!.args[0].expression.identifier!.variable
  #expect(ifCondX == loopX)
  let ifAssignment = ifStatement.block.statements[0].statement.varAssign!.identifier.variable
  let ifAssignmentX = ifStatement.block.statements[0].statement.varAssign!.expression.funcCall!
    .args[0].expression.identifier!.variable
  #expect(ifAssignment == outerA)
  #expect(ifAssignmentX == loopX)
}
