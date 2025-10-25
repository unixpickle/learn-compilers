public struct CFG {

  public struct SSAVariable {
    public let variable: Variable
    public var version: Int?
  }

  public enum Argument {
    case constInt(Int64)
    case variable(SSAVariable)
  }

  public struct Inst {
    public enum Op {
      case ret
      case retValue(Argument)
      case brk
      case check(Argument)
      case call(Function, [Argument])
      case copy(SSAVariable, Argument)
      case callAndStore(SSAVariable, Function, [Argument])
    }

    public let position: Position
    public var op: Op
  }

  public struct NodeCode {
    public var instructions: [Inst]
  }

  public class Node: PointerHashable {
  }

  public var nodes = Set<Node>()
  public var nodeCode = [Node: NodeCode]()
  public var successors = [Node: [Node]]()
  public var functions = [Function: Node]()
  private var tmpCounter: Int = 0

  public init() {
  }

  private mutating func add(fn: AST.FuncDecl) {
    let head = addNode()
    let end = addNode()
    addBlock(block: fn.block, node: head, next: end)
    functions[fn.function!] = head
  }

  private enum AddBlockOp {
    case build(node: Node, statements: [AST.Statement], next: Node)
  }

  private mutating func addBlock(block: AST.Block, node: Node, next: Node) {
    var queue: [AddBlockOp] = [.build(node: node, statements: block.statements.map { $0.statement }, next: next)]

    while case .build(let node, let statements, let next) = queue.popLast() {
      var hitControlFlow = false
      for (i, statement) in statements.enumerated() {
        switch statement {
        case .ifStatement(let ifStatement, let pos):
          let checkArg = lowerExpression(node: node, expr: ifStatement.expression)
          nodeCode[node]!.instructions.append(
            Inst(position: ifStatement.expression.position!, op: .check(checkArg))
          )
          let falseNode = addNode()
          let trueNode = addNode()
          addEdge(from: node, to: falseNode)
          addEdge(from: node, to: trueNode)
          let nextStatements = Array(statements[(i+1)...])
          queue.append(.build(node: falseNode, statements: nextStatements, next: next))
          queue.append(.build(node: trueNode, statements: ifStatement.block.statements.map { $0.statement }, next: falseNode))
          hitControlFlow = true
        case .whileLoop(let whileLoop, let pos):
          let checkNode = addNode()
          addEdge(from: node, to: checkNode)

          let checkArg = lowerExpression(node: checkNode, expr: whileLoop.expression)
          nodeCode[checkNode]!.instructions.append(
            Inst(position: whileLoop.expression.position!, op: .check(checkArg))
          )
          let falseNode = addNode()
          let trueNode = addNode()
          addEdge(from: checkNode, to: falseNode)
          addEdge(from: checkNode, to: trueNode)
          let nextStatements = Array(statements[(i+1)...])
          queue.append(.build(node: falseNode, statements: nextStatements, next: next))
          queue.append(.build(node: trueNode, statements: whileLoop.block.statements.map { $0.statement }, next: checkNode))
          hitControlFlow = true
        default: fatalError("TODO: handle simple statements")
        }
        if hitControlFlow {
          break
        }
      }
      if !hitControlFlow {
        addEdge(from: node, to: next)
      }
    }
  }

  private mutating func lowerExpression(node: Node, expr: AST.Expression) -> Argument {
    switch expr {
    case .identifier(let v, _): .variable(SSAVariable(variable: v.variable!))
    case .intLiteral(let v, _): .constInt(v.integer)
    case .funcCall(let fCall, _):
      // TODO: lower the function call to a temp variable recursively.
      fatalError()
    }
  }

  private mutating func addNode() -> Node {
    let node = Node()
    nodes.insert(node)
    nodeCode[node] = NodeCode(instructions: [])
    return node
  }

  private mutating func addEdge(from: Node, to: Node) {
    successors[from, default: []].append(to)
  }

}
