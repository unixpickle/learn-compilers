/// A structure for operations involving dominators in a control flow graph.
public struct DominatorTree {
  public let cfg: CFG
  public let roots: [CFG.Node]
  public let children: [CFG.Node: [CFG.Node]]

  public init(cfg: CFG) {
    self.cfg = cfg
    var roots = [CFG.Node]()
    var children = [CFG.Node: [CFG.Node]]()

    let doms = dominators(cfg: cfg)
    for node in cfg.nodes.sorted(by: { $0.id < $1.id }) {
      guard let nodeDoms = doms[node], nodeDoms.count > 1 else {
        roots.append(node)
        continue
      }
      let strictDoms = nodeDoms.subtracting([node])
      let strictDomUnion = strictDoms.map { doms[$0]!.subtracting([$0]) }.reduce(
        Set<CFG.Node>()
      ) { return $0.union($1) }
      let remaining = strictDoms.subtracting(strictDomUnion)
      assert(remaining.count == 1, "\(remaining.count)")
      let idom = remaining.first!
      children[idom, default: []].append(node)
    }
    self.roots = roots
    self.children = children
  }

  public func dominanceFrontier(of node: CFG.Node) -> Set<CFG.Node> {
    let dom = dominated(by: node)
    var result = Set<CFG.Node>()
    for node in dom {
      for succ in cfg.successors(of: node) {
        if !dom.contains(succ) {
          result.insert(succ)
        }
      }
    }
    return result
  }

  public func dominated(by node: CFG.Node) -> [CFG.Node] {
    var result = [CFG.Node]()
    var stack = [node]
    while let n = stack.popLast() {
      stack.append(contentsOf: children[n, default: []])
      result.append(n)
    }
    return result
  }
}

internal func dominators(cfg: CFG) -> [CFG.Node: Set<CFG.Node>] {
  // O(N^2) solution based on data-flow equation.
  var result = [CFG.Node: Set<CFG.Node>]()
  for node in cfg.nodes {
    result[node] = cfg.nodes
  }

  var changed = true
  while changed {
    changed = false
    for node in cfg.nodes {
      guard let preds = cfg.predecessors[node], !preds.isEmpty else {
        if result[node] != [node] {
          changed = true
          result[node] = [node]
        }
        continue
      }
      let predDoms = preds.map { result[$0]! }
      var intersection = predDoms.first!
      for d in predDoms {
        intersection.formIntersection(d)
      }
      intersection.insert(node)
      if result[node] != intersection {
        result[node] = intersection
        changed = true
      }
    }
  }
  return result
}
