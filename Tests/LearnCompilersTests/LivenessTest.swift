import LearnParsers
import Testing

@testable import LearnCompilers

private typealias G = Liveness.VariableGraph

@Test func testLivenessSimple() throws {
  var cfg = CFG()

  let nullPos = Position(fileID: "foo")

  let v1 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v1", type: .integer)
  )
  let v2 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v2", type: .integer)
  )
  let v3v1 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v3", type: .integer), version: 0
  )
  let v3v2 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v3", type: .integer), version: 1
  )
  let v3v3 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v3", type: .integer), version: 2
  )
  let v4 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v4", type: .integer)
  )

  let addFunc = Function(
    declarationPosition: nullPos, name: "add",
    signature: .init(args: [.integer, .integer], ret: .integer)
  )
  let printFunc = Function(
    declarationPosition: nullPos, name: "print",
    signature: .init(args: [.integer], ret: nil)
  )
  let main = Function(
    declarationPosition: nullPos, name: "main",
    signature: .init(args: [.integer], ret: nil)
  )

  let entrypoint = cfg.addNode()
  cfg.nodeCode[entrypoint] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .funcArg(v1, 0)),
    .init(position: nullPos, op: .callAndStore(v2, addFunc, [.variable(v1), .constInt(5)])),
    .init(position: nullPos, op: .callAndStore(v4, addFunc, [.variable(v2), .constInt(10)])),
    .init(position: nullPos, op: .check(.variable(v2))),
  ])

  let branch1 = cfg.addNode()
  cfg.nodeCode[branch1] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .callAndStore(v3v1, addFunc, [.variable(v1), .constInt(6)]))
  ])

  let branch2 = cfg.addNode()
  cfg.nodeCode[branch2] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .callAndStore(v3v2, addFunc, [.variable(v2), .constInt(6)]))
  ])

  let afterIf = cfg.addNode()
  cfg.nodeCode[afterIf] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .phi(v3v3, [branch1: .variable(v3v1), branch2: .variable(v3v2)])),
    .init(position: nullPos, op: .call(printFunc, [.variable(v3v3)])),
  ])

  let finalNode = cfg.addNode()
  cfg.nodeCode[finalNode] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .call(printFunc, [.variable(v4)]))
  ])

  cfg.addEdge(from: entrypoint, to: .branch(ifFalse: branch1, ifTrue: branch2))
  cfg.addEdge(from: branch1, to: .single(afterIf))
  cfg.addEdge(from: branch2, to: .single(afterIf))
  cfg.addEdge(from: afterIf, to: .single(finalNode))

  cfg.functions[main] = entrypoint

  let liveness = Liveness(cfg: cfg)
  #expect(liveness.liveIn(node: entrypoint).isEmpty)
  #expect(liveness.liveOut(node: entrypoint) == Set([v1, v2, v4]))
  #expect(liveness.liveIn(node: branch1) == Set([v1, v4]))
  #expect(liveness.liveOut(node: branch1) == Set([v3v1, v4]))
  #expect(liveness.liveIn(node: branch2) == Set([v2, v4]))
  #expect(liveness.liveOut(node: branch2) == Set([v3v2, v4]))
  #expect(liveness.liveIn(node: afterIf) == Set([v3v1, v3v2, v4]))
  #expect(liveness.liveOut(node: afterIf) == Set([v4]))
  #expect(liveness.liveIn(node: finalNode) == Set([v4]))
  #expect(liveness.liveOut(node: finalNode) == [])

  #expect(
    liveness.interferenceFor(node: entrypoint).neighbors
      == G.init(edges: [(v1, v2), (v1, v4), (v2, v4)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: branch1).neighbors
      == G.init(edges: [(v3v1, v4), (v1, v4)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: branch2).neighbors
      == G.init(edges: [(v3v2, v4), (v2, v4)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: afterIf).neighbors
      == G.init(edges: [(v3v3, v4), (v3v1, v4), (v3v2, v4)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: finalNode).neighbors == G.init().neighbors
  )
}

@Test func testLivenessPhiSelf() throws {
  var cfg = CFG()

  let nullPos = Position(fileID: "foo")

  let v1 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v1", type: .integer)
  )
  let v2 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v2", type: .integer)
  )

  let printFunc = Function(
    declarationPosition: nullPos, name: "print",
    signature: .init(args: [.integer], ret: nil)
  )
  let main = Function(
    declarationPosition: nullPos, name: "main",
    signature: .init(args: [.integer], ret: nil)
  )

  let entrypoint = cfg.addNode()
  cfg.nodeCode[entrypoint] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .funcArg(v1, 0))
  ])

  let loopBody = cfg.addNode()
  cfg.nodeCode[loopBody] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .phi(v2, [loopBody: .variable(v2), entrypoint: .variable(v1)])),
    .init(position: nullPos, op: .call(printFunc, [.variable(v2)])),
  ])

  cfg.addEdge(from: entrypoint, to: .single(loopBody))
  cfg.addEdge(from: loopBody, to: .single(loopBody))
  cfg.functions[main] = entrypoint

  let liveness = Liveness(cfg: cfg)
  #expect(liveness.liveIn(node: entrypoint).isEmpty)
  #expect(liveness.liveOut(node: entrypoint) == Set([v1]))
  #expect(liveness.liveIn(node: loopBody) == Set([v1, v2]))
  #expect(liveness.liveOut(node: loopBody) == Set([v2]))
  #expect(liveness.liveOnEdge[.init(from: loopBody, to: loopBody)] == Set([v2]))

  #expect(liveness.interferenceFor(node: entrypoint).neighbors == [:])
  #expect(liveness.interferenceFor(node: loopBody).neighbors == [:])
}

@Test func testLivenessPhiMultiple() throws {
  var cfg = CFG()

  let nullPos = Position(fileID: "foo")

  let v1 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v1", type: .integer)
  )
  let v2 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v2", type: .integer)
  )
  let v3 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v3", type: .integer)
  )
  let v4 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v4", type: .integer)
  )
  let v5 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v5", type: .integer)
  )
  let v6 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v6", type: .integer)
  )
  let v7 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v7", type: .integer)
  )
  let v8 = CFG.SSAVariable(
    variable: Variable(declarationPosition: nullPos, name: "v8", type: .integer)
  )

  let printFunc = Function(
    declarationPosition: nullPos, name: "print",
    signature: .init(args: [.integer], ret: nil)
  )
  let addFunc = Function(
    declarationPosition: nullPos, name: "add",
    signature: .init(args: [.integer, .integer], ret: .integer)
  )
  let subFunc = Function(
    declarationPosition: nullPos, name: "sub",
    signature: .init(args: [.integer, .integer], ret: .integer)
  )
  let main = Function(
    declarationPosition: nullPos, name: "main",
    signature: .init(args: [.integer], ret: nil)
  )

  let entrypoint = cfg.addNode()
  cfg.nodeCode[entrypoint] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .funcArg(v1, 0)),
    .init(position: nullPos, op: .funcArg(v2, 1)),
    .init(position: nullPos, op: .check(.variable(v1))),
  ])

  let branch1 = cfg.addNode()
  cfg.nodeCode[branch1] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .callAndStore(v3, addFunc, [.variable(v1), .variable(v2)])),
    .init(position: nullPos, op: .callAndStore(v4, subFunc, [.variable(v1), .variable(v3)])),
  ])
  let branch2 = cfg.addNode()
  cfg.nodeCode[branch2] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .callAndStore(v5, addFunc, [.variable(v2), .variable(v1)]))
  ])

  let joinPoint = cfg.addNode()
  cfg.nodeCode[joinPoint] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .phi(v6, [branch1: .variable(v3), branch2: .variable(v5)])),
    .init(position: nullPos, op: .phi(v7, [branch1: .variable(v4), branch2: .variable(v1)])),
    .init(position: nullPos, op: .callAndStore(v8, addFunc, [.variable(v6), .variable(v7)])),
    .init(position: nullPos, op: .call(printFunc, [.variable(v8)])),
    .init(position: nullPos, op: .call(printFunc, [.variable(v6)])),
  ])

  cfg.addEdge(from: entrypoint, to: .branch(ifFalse: branch1, ifTrue: branch2))
  cfg.addEdge(from: branch1, to: .single(joinPoint))
  cfg.addEdge(from: branch2, to: .single(joinPoint))
  cfg.functions[main] = entrypoint

  var liveness = Liveness(cfg: cfg)
  #expect(liveness.liveIn(node: entrypoint).isEmpty)
  #expect(liveness.liveOut(node: entrypoint) == Set([v1, v2]))
  #expect(liveness.liveIn(node: branch1) == Set([v1, v2]))
  #expect(liveness.liveOut(node: branch1) == Set([v3, v4]))
  #expect(liveness.liveIn(node: branch2) == Set([v1, v2]))
  #expect(liveness.liveOut(node: branch2) == Set([v1, v5]))
  #expect(liveness.liveIn(node: joinPoint) == Set([v1, v3, v4, v5]))
  #expect(liveness.liveOut(node: joinPoint) == [])

  #expect(
    liveness.interferenceFor(node: entrypoint).neighbors == G.init(edges: [(v1, v2)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: branch1).neighbors
      == G.init(edges: [(v1, v3), (v3, v4), (v1, v2)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: branch2).neighbors
      == G.init(edges: [(v1, v5), (v1, v2)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: joinPoint).neighbors
      == G.init(edges: [(v6, v7), (v8, v6), (v1, v5), (v3, v4)]).neighbors
  )

  cfg.nodeCode[joinPoint] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .phi(v6, [branch1: .variable(v3), branch2: .variable(v5)])),
    .init(position: nullPos, op: .phi(v7, [branch1: .variable(v4), branch2: .variable(v1)])),
    .init(position: nullPos, op: .callAndStore(v8, addFunc, [.variable(v6), .variable(v1)])),
    .init(position: nullPos, op: .call(printFunc, [.variable(v8)])),
    .init(position: nullPos, op: .call(printFunc, [.variable(v6)])),
  ])

  liveness = Liveness(cfg: cfg)
  #expect(liveness.liveIn(node: entrypoint).isEmpty)
  #expect(liveness.liveOut(node: entrypoint) == Set([v1, v2]))
  #expect(liveness.liveIn(node: branch1) == Set([v1, v2]))
  #expect(liveness.liveOut(node: branch1) == Set([v3, v4, v1]))
  #expect(liveness.liveIn(node: branch2) == Set([v1, v2]))
  #expect(liveness.liveOut(node: branch2) == Set([v1, v5]))
  #expect(liveness.liveIn(node: joinPoint) == Set([v1, v3, v4, v5]))
  #expect(liveness.liveOut(node: joinPoint) == [])

  #expect(
    liveness.interferenceFor(node: entrypoint).neighbors == G.init(edges: [(v1, v2)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: branch1).neighbors
      == G.init(edges: [(v1, v3), (v1, v4), (v3, v4), (v1, v2)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: branch2).neighbors
      == G.init(edges: [(v1, v5), (v1, v2)]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: joinPoint).neighbors
      == G.init(edges: [
        (v6, v7), (v6, v1), (v7, v1), (v8, v6), (v3, v4), (v3, v1), (v5, v1), (v4, v1),
      ]).neighbors
  )

  // Try separate phi and continuation
  let finalPoint = cfg.addNode()

  cfg.nodeCode[joinPoint] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .phi(v6, [branch1: .variable(v3), branch2: .variable(v5)])),
    .init(position: nullPos, op: .phi(v7, [branch1: .variable(v4), branch2: .variable(v1)])),
  ])
  cfg.nodeCode[finalPoint] = CFG.NodeCode(instructions: [
    .init(position: nullPos, op: .callAndStore(v8, addFunc, [.variable(v6), .variable(v1)])),
    .init(position: nullPos, op: .call(printFunc, [.variable(v8)])),
    .init(position: nullPos, op: .call(printFunc, [.variable(v6)])),
  ])
  cfg.addEdge(from: joinPoint, to: .single(finalPoint))

  liveness = Liveness(cfg: cfg)
  #expect(liveness.liveIn(node: joinPoint) == Set([v1, v3, v4, v5]))
  #expect(liveness.liveOut(node: joinPoint) == [v1, v6])
  #expect(liveness.liveIn(node: finalPoint) == Set([v1, v6]))
  #expect(liveness.liveOut(node: finalPoint) == [])

  #expect(
    liveness.interferenceFor(node: joinPoint).neighbors
      == G.init(edges: [
        (v6, v1), (v7, v1), (v7, v6), (v5, v1), (v4, v1), (v3, v1), (v3, v4),
      ]).neighbors
  )
  #expect(
    liveness.interferenceFor(node: finalPoint).neighbors
      == G.init(edges: [(v8, v6), (v6, v1)]).neighbors
  )
}
