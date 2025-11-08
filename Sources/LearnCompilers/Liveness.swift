public struct Liveness {

  public struct Edge: Hashable {
    let from: CFG.Node
    let to: CFG.Node
  }

  internal let cfg: CFG
  internal var liveOnEdge = [Edge: Set<CFG.SSAVariable>]()
  internal var defs = [CFG.SSAVariable: (CFG.Node, Int)]()
  internal var lastUses = [CFG.SSAVariable: [CFG.Node: Int]]()

  /// For each node, keep track of all the variables which are defined
  /// but not also consumed in the defining op (which can happen for phi
  /// functions in a loop).
  internal var nodeToDefs = [CFG.Node: Set<CFG.SSAVariable>]()

  /// Compute a liveness graph for the CFG.
  ///
  /// The CFG should be in SSA form.
  public init(cfg: CFG) {
    self.cfg = cfg
    for node in cfg.nodes {
      let code = cfg.nodeCode[node]!
      for (i, inst) in code.instructions.enumerated() {
        for def in inst.op.defs {
          if defs[def] != nil {
            fatalError("multiple definitions of \(def) is not supported in SSA")
          }
          defs[def] = (node, i)
          nodeToDefs[node, default: []].insert(def)
        }
        for use in inst.op.uses {
          lastUses[use, default: [:]][node] = i
        }
      }
    }

    dataFlowUpdateIncoming()
  }

  private mutating func dataFlowUpdateIncoming() {
    for fn in cfg.functions.values {
      let enumerationOrder = cfg.dfsFrom(node: fn).reversed()
      var changed = true
      while changed {
        changed = false
        for node in enumerationOrder {
          guard let preds = cfg.predecessors[node], !preds.isEmpty else {
            continue
          }

          let outEdges = cfg.successors(of: node).map { Edge(from: node, to: $0) }
          let outEdgeUses = Set(outEdges.flatMap { liveOnEdge[$0, default: []] })

          var usedOnAllIncoming = outEdgeUses.subtracting(nodeToDefs[node, default: []])
          var usedInPhi = [CFG.Node: Set<CFG.SSAVariable>]()

          var definedSoFar = Set<CFG.SSAVariable>()
          for inst in cfg.nodeCode[node]!.instructions {
            if case .phi(let v, let sources) = inst.op {
              for (source, arg) in sources {
                if case .variable(let v) = arg {
                  usedInPhi[source, default: []].insert(v)
                }
              }
              definedSoFar.insert(v)
            } else {
              for use in inst.op.uses {
                if !definedSoFar.contains(use) {
                  usedOnAllIncoming.insert(use)
                }
              }
            }
            for def in inst.op.defs {
              definedSoFar.insert(def)
            }
          }
          for source in preds {
            let e = Edge(from: source, to: node)
            let newLiveIn = usedOnAllIncoming.union(usedInPhi[source, default: []])
            if liveOnEdge[e] != newLiveIn {
              liveOnEdge[e] = newLiveIn
              changed = true
            }
          }
        }
      }
    }
  }

  public func liveIn(node: CFG.Node) -> Set<CFG.SSAVariable> {
    Set(
      cfg.predecessors[node, default: []].flatMap {
        liveOnEdge[Edge(from: $0, to: node), default: []]
      }
    )
  }

  public func liveOut(node: CFG.Node) -> Set<CFG.SSAVariable> {
    Set(
      cfg.successors(of: node).flatMap {
        liveOnEdge[Edge(from: node, to: $0), default: []]
      }
    )
  }

}
