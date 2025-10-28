import Testing

@testable import LearnCompilers

@Test func testDominanceFrontier() throws {
  var cfg = CFG()
  let node1 = cfg.addNode()
  let node2 = cfg.addNode()
  let node3 = cfg.addNode()
  let node4 = cfg.addNode()
  let node5 = cfg.addNode()
  let node6 = cfg.addNode()
  let node7 = cfg.addNode()
  cfg.addEdge(from: node1, to: .single(node2))
  cfg.addEdge(from: node2, to: .branch(ifFalse: node7, ifTrue: node6))
  cfg.addEdge(from: node7, to: .branch(ifFalse: node3, ifTrue: node4))
  cfg.addEdge(from: node3, to: .single(node5))
  cfg.addEdge(from: node4, to: .single(node5))
  cfg.addEdge(from: node5, to: .single(node2))

  let tree = DominatorTree(cfg: cfg)
  #expect(tree.children == [node1: [node2], node2: [node6, node7], node7: [node3, node4, node5]])
}
