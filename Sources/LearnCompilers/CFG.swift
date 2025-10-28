public struct CFG {

  public struct SSAVariable: Hashable {
    public let variable: Variable
    public var version: Int?
  }

  public enum Argument: Hashable {
    case constInt(Int64)
    case variable(SSAVariable)

    internal var variables: [SSAVariable] {
      switch self {
      case .constInt(_): []
      case .variable(let v): [v]
      }
    }

    public func replacing(_ v: SSAVariable, with r: SSAVariable) -> Self {
      switch self {
      case .constInt(_): self
      case .variable(let v1): .variable(v1 == v ? r : v1)
      }
    }
  }

  public struct Inst: Hashable {
    public enum Op: Hashable {
      case funcArg(SSAVariable, Int)

      /// Only to be used at the bottom of a node and passed a constant
      /// or integer variable.
      case check(Argument)

      case copy(SSAVariable, Argument)
      case call(Function, [Argument])
      case callAndStore(SSAVariable, Function, [Argument])

      case consumeReturnVar(Argument)

      public var defs: [SSAVariable] {
        switch self {
        case .funcArg(let v, _): [v]
        case .copy(let v, _): [v]
        case .callAndStore(let v, _, _): [v]
        default: []
        }
      }

      public var uses: [SSAVariable] {
        switch self {
        case .funcArg(_, _): []
        case .check(let a): a.variables
        case .copy(_, let a): a.variables
        case .call(_, let args): args.flatMap { $0.variables }
        case .callAndStore(_, _, let args): args.flatMap { $0.variables }
        case .consumeReturnVar(let v): v.variables
        }
      }

      public func replacing(_ v: SSAVariable, with r: SSAVariable) -> Self {
        switch self {
        case .funcArg(let target, let idx): .funcArg(target == v ? r : target, idx)
        case .check(let a): .check(a.replacing(v, with: r))
        case .copy(let target, let source):
          .copy(target == v ? r : target, source.replacing(v, with: r))
        case .call(let fn, let args): .call(fn, args.map { $0.replacing(v, with: r) })
        case .callAndStore(let target, let fn, let args):
          .callAndStore(target == v ? r : target, fn, args.map { $0.replacing(v, with: r) })
        case .consumeReturnVar(let retVar): .consumeReturnVar(retVar.replacing(v, with: r))
        }
      }
    }

    public let position: Position
    public var op: Op
  }

  public struct NodeCode {
    public var instructions: [Inst]
  }

  public class Node: PointerHashable, CustomStringConvertible {
    public let id: Int

    public init(id: Int) {
      self.id = id
    }

    public var description: String {
      "Node(\(id))"
    }
  }

  public enum Successors: Hashable {
    case single(Node)
    case branch(ifFalse: Node, ifTrue: Node)

    var nodes: [Node] {
      switch self {
      case .single(let n): [n]
      case .branch(let a, let b): [a, b]
      }
    }
  }

  public var nodes = Set<Node>()
  public var nodeCode = [Node: NodeCode]()
  public var successors = [Node: Successors]()
  public var predecessors = [Node: Set<Node>]()
  public var functions = [Function: Node]()
  public var returnVars = [Function: Variable]()
  private var tmpCounter: Int = 0
  private var nodeIDCounter: Int = 0

  public init() {
  }

  public init(ast: AST) {
    add(ast: ast)
  }

  // Add an entire AST to the graph.
  public mutating func add(ast: AST) {
    for fn in ast.functions {
      add(fn: fn)
    }
  }

  // Add a function to the graph, given its declaration.
  public mutating func add(fn: AST.FuncDecl) {
    let head = addNode()
    let end = addNode()
    for (i, arg) in fn.args.enumerated() {
      nodeCode[head]!.instructions.append(
        Inst(
          position: arg.position!,
          op: .funcArg(SSAVariable(variable: arg.identifier.variable!), i)
        )
      )
    }

    var returnVar: Variable?
    if let retType = fn.retType {
      returnVar = Variable(
        declarationPosition: retType.position!,
        name: "<return value>",
        type: retType.retType.dataType
      )
      returnVars[fn.function!] = returnVar!
      nodeCode[end]!.instructions.append(
        Inst(
          position: retType.position!,
          op: .consumeReturnVar(.variable(SSAVariable(variable: returnVar!)))
        )
      )
    }

    addBlock(block: fn.block, node: head, next: end, returnBlock: end, returnVar: returnVar)
    functions[fn.function!] = head
  }

  private enum AddBlockOp {
    case build(
      node: Node, statements: [AST.Statement], next: Node, loopExit: Node?
    )
  }

  private mutating func addBlock(
    block: AST.Block,
    node: Node,
    next: Node,
    returnBlock: Node,
    returnVar: Variable?
  ) {
    var queue: [AddBlockOp] = [
      .build(
        node: node, statements: block.statements.map { $0.statement }, next: next, loopExit: nil
      )
    ]

    while case .build(let node, let statements, let next, let loopExit) = queue.popLast() {
      var hitControlFlow = false
      for (i, statement) in statements.enumerated() {
        switch statement {
        case .ifStatement(let ifStatement, _):
          let checkArg = lowerExpression(node: node, expr: ifStatement.expression)
          nodeCode[node]!.instructions.append(
            Inst(position: ifStatement.expression.position!, op: .check(checkArg))
          )
          let nextStatements = Array(statements[(i + 1)...])
          let falseNode = nextStatements.isEmpty ? next : addNode()
          let trueNode = addNode()
          addEdge(from: node, to: .branch(ifFalse: falseNode, ifTrue: trueNode))
          if !nextStatements.isEmpty {
            queue.append(
              .build(node: falseNode, statements: nextStatements, next: next, loopExit: loopExit)
            )
          }
          queue.append(
            .build(
              node: trueNode,
              statements: ifStatement.block.statements.map { $0.statement },
              next: falseNode,
              loopExit: loopExit
            )
          )
          hitControlFlow = true
        case .whileLoop(let whileLoop, _):
          let checkNode = addNode()
          addEdge(from: node, to: .single(checkNode))

          let checkArg = lowerExpression(node: checkNode, expr: whileLoop.expression)
          nodeCode[checkNode]!.instructions.append(
            Inst(position: whileLoop.expression.position!, op: .check(checkArg))
          )
          let nextStatements = Array(statements[(i + 1)...])
          let falseNode = nextStatements.isEmpty ? next : addNode()
          let trueNode = addNode()
          addEdge(from: checkNode, to: .branch(ifFalse: falseNode, ifTrue: trueNode))
          if !nextStatements.isEmpty {
            queue.append(
              .build(node: falseNode, statements: nextStatements, next: next, loopExit: loopExit)
            )
          }
          queue.append(
            .build(
              node: trueNode,
              statements: whileLoop.block.statements.map { $0.statement },
              next: checkNode,
              loopExit: falseNode
            )
          )
          hitControlFlow = true
        case .returnStatement(let statement, let position):
          if let retVal = statement.expression {
            assert(returnVar != nil, "return with value in block without return variable")
            let retArg = lowerExpression(node: node, expr: retVal)
            nodeCode[node]!.instructions.append(
              Inst(
                position: position!,
                op: .copy(SSAVariable(variable: returnVar!), retArg)
              )
            )
          } else {
            assert(returnVar == nil, "return without value in block with return variable")
          }
          addEdge(from: node, to: .single(returnBlock))
          hitControlFlow = true
        case .breakStatement(_, _):
          assert(loopExit != nil, "hit break outside of loop")
          addEdge(from: node, to: .single(loopExit!))
          hitControlFlow = true
        case .varDecl(let statement, let position):
          let exprArg = lowerExpression(node: node, expr: statement.expression)
          nodeCode[node]!.instructions.append(
            Inst(
              position: position!,
              op: .copy(SSAVariable(variable: statement.identifier.variable!), exprArg)
            )
          )
        case .varAssign(let statement, let position):
          let exprArg = lowerExpression(node: node, expr: statement.expression)
          nodeCode[node]!.instructions.append(
            Inst(
              position: position!,
              op: .copy(SSAVariable(variable: statement.identifier.variable!), exprArg)
            )
          )
        case .funcCall(let fCall, let position):
          let args = fCall.args.map { lowerExpression(node: node, expr: $0.expression) }
          nodeCode[node]!.instructions.append(
            Inst(
              position: position!,
              op: .call(fCall.identifier.function!, args)
            )
          )
        }
        if hitControlFlow {
          break
        }
      }
      if !hitControlFlow {
        addEdge(from: node, to: .single(next))
      }
    }
  }

  private enum LowerExpressionOp {
    case expand(AST.Expression)
    case doFuncCall(AST.FuncCall)
  }

  private mutating func lowerExpression(node: Node, expr: AST.Expression) -> Argument {
    var opStack: [LowerExpressionOp] = [.expand(expr)]
    var resultStack = [Argument]()

    while let op = opStack.popLast() {
      switch op {
      case .expand(let expr):
        switch expr {
        case .identifier(let v, _):
          resultStack.append(.variable(SSAVariable(variable: v.variable!)))
        case .intLiteral(let v, _):
          resultStack.append(.constInt(v.integer))
        case .funcCall(let fCall, _):
          opStack.append(.doFuncCall(fCall))
          for arg in fCall.args.reversed() {
            opStack.append(.expand(arg.expression))
          }
        }
      case .doFuncCall(let fCall):
        let args = Array(resultStack[(resultStack.count - fCall.args.count)...])
        resultStack.removeLast(fCall.args.count)
        let tmp = createTmp(
          position: fCall.position!,
          type: fCall.identifier.function!.signature.ret!
        )
        let ssaVar = SSAVariable(variable: tmp)
        nodeCode[node]!.instructions.append(
          Inst(
            position: fCall.position!,
            op: .callAndStore(ssaVar, fCall.identifier.function!, args)
          )
        )
        resultStack.append(.variable(ssaVar))
      }
    }

    assert(resultStack.count == 1)
    return resultStack[0]
  }

  private mutating func createTmp(position: Position, type: Variable.DataType) -> Variable {
    let id = tmpCounter
    tmpCounter += 1
    return Variable(declarationPosition: position, name: "tmp[\(id)]", type: type)
  }

  public mutating func addNode() -> Node {
    let node = Node(id: nodeIDCounter)
    nodeIDCounter += 1
    nodes.insert(node)
    nodeCode[node] = NodeCode(instructions: [])
    return node
  }

  public mutating func addEdge(from: Node, to: Successors) {
    if successors[from] != nil {
      fatalError("adding edge from node when edge already exists")
    }
    successors[from] = to
    for n in to.nodes {
      predecessors[n, default: []].insert(from)
    }
  }

  public func successors(of: Node) -> [Node] {
    if let succ = successors[of] {
      succ.nodes
    } else {
      []
    }
  }

}
