public enum SSAError: Error {
  case missingReturn(Position)
}

public struct CFG {

  public struct SSAVariable: Hashable {
    public let variable: Variable
    public var version: Int?
  }

  public enum Argument: Hashable {
    case constInt(Int64)
    case constStr([UInt8])
    case variable(SSAVariable)

    public var dataType: Variable.DataType {
      switch self {
      case .constInt: .integer
      case .constStr: .string
      case .variable(let v): v.variable.type
      }
    }

    internal var variables: [SSAVariable] {
      switch self {
      case .constInt(_): []
      case .constStr(_): []
      case .variable(let v): [v]
      }
    }

    public func replacing(_ v: SSAVariable, with r: SSAVariable) -> Self {
      switch self {
      case .constInt(_): self
      case .constStr(_): self
      case .variable(let v1): .variable(v1 == v ? r : v1)
      }
    }
  }

  public struct Inst: Hashable {
    public enum Op: Hashable {
      /// Only to be used at the top of an entrypoint node.
      case funcArg(SSAVariable, Int)

      /// Only to be used at the bottom of a node.
      case check(Argument)

      case copy(SSAVariable, Argument)
      case call(Function, [Argument])
      case callAndStore(SSAVariable, Function, [Argument])

      case returnValue(Argument)
      case returnVoid
      case phi(SSAVariable, [Node: Argument])

      public var defs: [SSAVariable] {
        switch self {
        case .funcArg(let v, _): [v]
        case .copy(let v, _): [v]
        case .callAndStore(let v, _, _): [v]
        case .phi(let v, _): [v]
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
        case .returnValue(let v): v.variables
        case .returnVoid: []
        case .phi(_, let args): args.flatMap { (_, v) in v.variables }
        }
      }

      public func replacing(
        _ v: SSAVariable,
        with r: SSAVariable,
        replaceDefs: Bool = true
      ) -> Self {
        switch self {
        case .funcArg(let target, let idx): .funcArg(replaceDefs && target == v ? r : target, idx)
        case .check(let a): .check(a.replacing(v, with: r))
        case .copy(let target, let source):
          .copy(replaceDefs && target == v ? r : target, source.replacing(v, with: r))
        case .call(let fn, let args): .call(fn, args.map { $0.replacing(v, with: r) })
        case .callAndStore(let target, let fn, let args):
          .callAndStore(
            replaceDefs && target == v ? r : target, fn, args.map { $0.replacing(v, with: r) }
          )
        case .returnValue(let retVar): .returnValue(retVar.replacing(v, with: r))
        case .returnVoid: self
        case .phi(let target, let mapping):
          .phi(
            replaceDefs && target == v ? r : target, mapping.mapValues { $0.replacing(v, with: r) }
          )
        }
      }

      public func replacing(
        arg: Argument,
        with r: Argument
      ) -> Self {
        switch self {
        case .check(let a): .check(a == arg ? r : a)
        case .copy(let target, let source):
          .copy(target, source == arg ? r : source)
        case .call(let fn, let args): .call(fn, args.map { $0 == arg ? r : $0 })
        case .callAndStore(let target, let fn, let args):
          .callAndStore(target, fn, args.map { $0 == arg ? r : $0 })
        case .returnValue(let retVar): .returnValue(retVar == arg ? r : retVar)
        case .phi(let target, let mapping): .phi(target, mapping.mapValues { $0 == arg ? r : $0 })
        default: self
        }
      }
    }

    public let position: Position
    public var op: Op
  }

  public struct NodeCode {
    public var instructions: [Inst]

    public func replacing(
      arg: Argument,
      with r: Argument
    ) -> NodeCode {
      NodeCode(
        instructions: instructions.map { inst in
          .init(position: inst.position, op: inst.op.replacing(arg: arg, with: r))
        }
      )
    }
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
  private var tmpCounter: Int = 0
  private var nodeIDCounter: Int = 0

  public init() {
  }

  public init(ast: AST) {
    add(ast: ast)
  }

  /// Insert phi functions along dominance frontiers and assign versions to
  /// variables to create a proper SSA.
  ///
  /// This uses semi-pruned SSA, only inserting phi functions for variables
  /// that are used in more than one block.
  public mutating func insertPhiAndNumberVars() {
    let domTree = DominatorTree(cfg: self)
    insertPhi(domTree: domTree)
    numberVariables(domTree: domTree)
    eliminateIncompletePhi()
  }

  private mutating func insertPhi(domTree: DominatorTree) {
    var varToNode = [Variable: Set<Node>]()
    var varToDef = [Variable: Set<Node>]()
    var nodeToFrontier = [Node: Set<Node>]()

    var orderedVars = [Variable]()

    for node in nodes.sorted(by: { $0.id < $1.id }) {
      nodeToFrontier[node] = domTree.iteratedDominanceFrontier(of: node)
      for inst in nodeCode[node]!.instructions {
        for v in inst.op.defs + inst.op.uses {
          if varToNode[v.variable] == nil {
            orderedVars.append(v.variable)
          }
          varToNode[v.variable, default: []].insert(node)
        }
        for v in inst.op.defs {
          varToDef[v.variable, default: []].insert(node)
        }
      }
    }

    // Mark phi functions for insertion in a deterministic order.
    var nodeToPhis = [Node: Set<Variable>]()
    var nodeToPhisOrdered = [Node: [Variable]]()
    for v in orderedVars {
      if varToNode[v]!.count == 1 {
        continue
      }
      let allFrontier = varToDef[v]!.map { nodeToFrontier[$0]! }.reduce(Set(), { $0.union($1) })
      for frontierNode in allFrontier {
        if !nodeToPhis[frontierNode, default: []].contains(v) {
          nodeToPhis[frontierNode, default: []].insert(v)
          nodeToPhisOrdered[frontierNode, default: []].append(v)
        }
      }
    }

    // Insert phi functions into the top of each node's instruction stream.
    for (node, vars) in nodeToPhisOrdered {
      var code = nodeCode[node]!
      for (i, v) in vars.enumerated() {
        // The position of this instruction is basically wrong, as it points to the
        // definition of the variable, but we shouldn't end up using it.
        code.instructions.insert(
          Inst(position: v.declarationPosition, op: .phi(SSAVariable(variable: v), [:])),
          at: i
        )
      }
      nodeCode[node] = code
    }
  }

  private mutating func numberVariables(domTree: DominatorTree) {
    var queue: [(Node, [Variable: Int])] = domTree.roots.map { ($0, [:]) }

    var versionCounter = [Variable: Int]()
    func makeVersion(_ v: Variable) -> Int {
      if let nextV = versionCounter[v] {
        versionCounter[v] = nextV + 1
        return nextV
      } else {
        versionCounter[v] = 1
        return 0
      }
    }

    let allReturnVars = Set(returnVariables())

    while let (node, versionsIn) = queue.popLast() {
      var curVersions = versionsIn
      let code = nodeCode[node]!
      var newCode = code
      for (i, var inst) in code.instructions.enumerated() {
        if case .phi = inst.op {
          // For phi functions, versions are filled in indirectly by predecessors.
        } else {
          for v in inst.op.uses {
            guard let curV = curVersions[v.variable] else {
              if allReturnVars.contains(v.variable) {
                continue
              } else {
                fatalError("variable \(v.variable) used before assignment in instruction \(inst)")
              }
            }
            inst.op = inst.op.replacing(
              v,
              with: SSAVariable(variable: v.variable, version: curV),
              replaceDefs: false
            )
          }
        }
        for v in inst.op.defs {
          let newVersion = makeVersion(v.variable)
          inst.op = inst.op.replacing(
            v, with: SSAVariable(variable: v.variable, version: newVersion)
          )
          curVersions[v.variable] = newVersion
        }
        newCode.instructions[i] = inst
      }
      nodeCode[node] = newCode

      // Fill in successor phi functions now that we have versions.
      for successor in successors(of: node) {
        let code = nodeCode[successor]!
        var newCode = code
        for (i, var inst) in code.instructions.enumerated() {
          if case .phi(let v, var branches) = inst.op {
            guard let curV = curVersions[v.variable] else {
              // This may be a return that is truly missing, or a phi function that will
              // be eliminated later because the variable isn't defined for every branch
              // and therefore the phi must never be used.
              continue
            }
            branches[node] = .variable(SSAVariable(variable: v.variable, version: curV))
            inst.op = .phi(v, branches)
            newCode.instructions[i] = inst
          }
        }
        nodeCode[successor] = newCode
      }

      // DFS on children.
      for child in domTree.children[node, default: []] {
        queue.append((child, curVersions))
      }
    }
  }

  private mutating func eliminateIncompletePhi() {
    let retVars = Set(returnVariables())
    while true {
      var deletedVariables = Set<SSAVariable>()
      for node in nodes {
        var newCode = nodeCode[node]!
        var i = 0
        while i < newCode.instructions.count {
          let inst = newCode.instructions[i]
          if case .phi(let v, let branches) = inst.op {
            if Set(branches.keys) != Set(predecessors[node, default: []]) {
              if !retVars.contains(v.variable) {
                deletedVariables.insert(v)
                newCode.instructions.remove(at: i)
                continue
              }
            }
          }
          i += 1
        }
        nodeCode[node] = newCode
      }
      if deletedVariables.isEmpty {
        break
      }
      for node in nodes {
        var newCode = nodeCode[node]!
        for i in 0..<newCode.instructions.count {
          let inst = newCode.instructions[i]
          if case .phi(let v, let branches) = inst.op {
            newCode.instructions[i].op = .phi(
              v,
              Dictionary(
                uniqueKeysWithValues: branches.filter {
                  if case .variable(let v) = $0.value {
                    return !deletedVariables.contains(v)
                  }
                  return true
                }
              )
            )
          } else {
            assert(
              inst.op.uses.allSatisfy { !deletedVariables.contains($0) },
              "deleted phi variable used in non-phi instruction"
            )
          }
        }
        nodeCode[node] = newCode
      }
    }
  }

  /// Raise an error if any functions are missing returns, or crash if
  /// other phi functions are missing branches (should never happen).
  ///
  /// This can happen late in the game because `insertPhiAndNumberVars()`
  /// doesn't check for this, to allow dead code elimination to remove the
  /// need for returns in some cases.
  public func checkMissingReturns() throws {
    let retVars = Set(returnVariables())

    // This detects when NO branches return, i.e. a return var is never defined.
    let allDefs = Set(
      nodes.flatMap { node in
        nodeCode[node]!.instructions.flatMap { $0.op.defs.map { $0.variable } }
      }
    )
    for retVar in retVars {
      if !allDefs.contains(retVar) {
        throw SSAError.missingReturn(retVar.declarationPosition)
      }
    }

    // This detects when some branches don't return but others do
    for node in nodes {
      let code = nodeCode[node]!
      for inst in code.instructions {
        if case .phi(let v, let branches) = inst.op {
          if Set(branches.keys) != Set(predecessors[node, default: []]) {
            if retVars.contains(v.variable) {
              // The phi block takes its position from consumption of the variable,
              // which is in turn exactly where the return type is declared.
              throw SSAError.missingReturn(inst.position)
            } else {
              fatalError("phi is missing an argument")
            }
          }
        }
      }
    }
  }

  /// Get all variables passed to a return.
  public func returnVariables() -> [Variable] {
    nodes.flatMap { node in
      nodeCode[node]!.instructions.compactMap { inst in
        if case .returnValue(.variable(let ssaVar)) = inst.op {
          ssaVar.variable
        } else {
          nil
        }
      }
    }
  }

  /// Get a mapping of functions to return variables.
  public func returnVariableMap() -> [Function: Variable] {
    var result = [Function: Variable]()
    for (fn, entrypoint) in functions {
      let nodes = dfsFrom(node: entrypoint)
      for node in nodes {
        for inst in nodeCode[node]!.instructions {
          if case .returnValue(.variable(let ssaVar)) = inst.op {
            assert(result[fn] == nil, "multiple return instructions should not exist")
            result[fn] = ssaVar.variable
          }
        }
      }
    }
    return result
  }

  /// Perform a depth-first search from a given node and return the DFS-ordered
  /// list of discovered nodes (including the node itself).
  public func dfsFrom(node: Node) -> [Node] {
    var nodeSet: Set<Node> = [node]
    var nodeArr = [node]
    var queue = [node]
    while let n = queue.popLast() {
      for succ in successors(of: n).reversed() {
        if !nodeSet.contains(succ) {
          nodeSet.insert(succ)
          nodeArr.append(succ)
          queue.append(succ)
        }
      }
    }
    return nodeArr
  }

  /// Add an entire AST to the graph.
  public mutating func add(ast: AST) {
    for fn in ast.functions {
      add(fn: fn)
    }
  }

  /// Add a function to the graph, given its declaration.
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
      nodeCode[end]!.instructions.append(
        Inst(
          position: retType.position!,
          op: .returnValue(.variable(SSAVariable(variable: returnVar!)))
        )
      )
    } else {
      nodeCode[end]!.instructions.append(
        Inst(
          position: fn.position!,  // TODO: not particularly correct or useful
          op: .returnVoid
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
        case .strLiteral(let v, _):
          resultStack.append(.constStr(v.bytes))
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
