import LearnParsers
import Testing

@testable import LearnCompilers

@Test func testASTVariableResolution() async throws {
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

  let match = try Parser.parse(code)
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

@Test func testASTVariableResolutionErrors() async throws {
  let badCode = [
    """
    fn main() {
      x: int = x
    }
    """,
    """
    fn main(x: int) {
      if? (x) {
        print(y)
      }
    }
    """,
    """
    fn main(x: int) {
      print(y)
    }
    """,
    """
    fn main() {
      x: int = 3
      x: int = 4
    }
    """,
  ]

  for code in badCode {
    let match = try Parser.parse(code)
    var ast = AST(match: match)
    #expect(ast.codeString == code)

    var table = ScopeTable()
    (ast, _) = ast.insertingPositions(start: Position(fileID: "myFile"))
    ast = ast.insertingScopes(table: &table)
    var errors: [VariableResolutionError]
    (ast, errors) = ast.resolvingVariables(table: &table)
    #expect(!errors.isEmpty, "should not have succeeded for code: \(code)")
  }
}

@Test func testASTFullDecoration() async throws {
  let code = """
      fn main(x: int, y: str) -> int {
        print(x)
        print(y)

        y: int = x
        print(y)

        while? (lt(x, add(5,5))) {
          if? (eq(x, 5)) {
            return!(x)
          }
          if? (eq(x, 20)) {
            break!()
          }
          x = add(x, 1)
        }
        return!(sub(y, 10))
      }

      fn print(x: int) {
      }

      fn print(x: str) {
        while? (1) {
          return!()
        }
      }

      fn lt(x: int, y: int) -> int {
        return!(x)
      }

      fn eq(x: int, y: int) -> int {
        return!(sub(x, y))
      }

      fn sub(x: int, y: int) -> int {
        return!(x)
      }

      fn add(x: int, y: int) -> int {
        return!(x)
      }
    """

  let match = try Parser.parse(code)
  var ast = AST(match: match)
  #expect(ast.codeString == code)

  var table = ScopeTable()
  var errors: [ASTDecorationError]
  (ast, errors) = ast.decorated(table: &table, fileID: "stdin")

  #expect(errors.isEmpty)
  if !errors.isEmpty {
    return
  }

  // Now we'll find various symbols and make sure they are correct.
  let argX = ast.functions[0].args[0].identifier.variable!
  let argY = ast.functions[0].args[1].identifier.variable!
  #expect(argX != argY)
  #expect(argX.type == .integer)
  #expect(argY.type == .string)

  let funcBlock = ast.functions[0].block
  #expect(funcBlock.scope != nil)

  let print1 = funcBlock.statements[0].statement.funcCall!
  #expect(print1.identifier.function!.name == "print")
  #expect(print1.identifier.function!.signature.args == [.integer])
  let print1Arg = print1.args[0].expression.identifier!.variable
  #expect(print1Arg == argX)

  let print2 = funcBlock.statements[1].statement.funcCall!
  #expect(print2.identifier.function!.name == "print")
  #expect(print2.identifier.function!.signature.args == [.string])
  let print2Arg = print2.args[0].expression.identifier!.variable
  #expect(print2Arg == argY)

  let redecY = funcBlock.statements[2].statement.varDecl!.identifier.variable!
  #expect(redecY.type == .integer)

  let print3 = funcBlock.statements[3].statement.funcCall!
  #expect(print3.identifier.function!.name == "print")
  #expect(print3.identifier.function!.signature.args == [.integer])
  let print3Arg = print3.args[0].expression.identifier!.variable
  #expect(print3Arg == redecY)

  let whileLoop = funcBlock.statements[4].statement.whileLoop!
  let conditionFuncCall = whileLoop.expression.funcCall!
  #expect(conditionFuncCall.identifier.function!.name == "lt")

  // Make sure nested function calls get populated.
  let conditionFuncCallArgCall = conditionFuncCall.args[1].expression.funcCall!
  #expect(conditionFuncCallArgCall.identifier.function!.name == "add")

  let whileBlock = whileLoop.block
  let ifStatement = whileBlock.statements[0].statement.ifStatement!
  let ifCondCall = ifStatement.expression.funcCall!
  #expect(ifCondCall.identifier.function!.name == "eq")
  let retStatement = ifStatement.block.statements[0].statement.returnStatement!
  #expect(retStatement.expression != nil)
}

@Test func testASTFullDecorationErrors() async throws {
  let badCode = [
    """
    fn main() {
      return!(3)
    }
    """,
    """
    fn main() -> int {
      return!()
    }
    """,
    """
    fn main(x: int) {
      foo(x)
    }
    """,
    """
    fn foo(x: str) {}
    fn main() {
      x: int = 3
      foo(x)
    }
    """,
    """
    fn foo(x: int) {}
    fn main() {
      x: int = 3
      foo(foo(x))
    }
    """,
    """
    fn foo(x: str) -> str {
      return!(x)
    }
    fn main(x: str) {
      y: int = foo(x)
    }
    """,
    """
    fn main() {
      while? (1) {
        break!()
      }
      if? (1) {
        break!()
      }
    }
    """,
  ]

  for code in badCode {
    let match = try Parser.parse(code)
    let ast = AST(match: match)
    #expect(ast.codeString == code)

    var table = ScopeTable()
    let (_, errors) = ast.decorated(table: &table, fileID: "stdin")
    #expect(!errors.isEmpty, "should not have succeeded for code: \(code)")
  }
}
