public struct Liveness {

  public struct VariableGraph: Hashable {
    public typealias V = CFG.SSAVariable
    public var neighbors = [V: Set<V>]()

    public init() {}

    public init<S: Sequence<(V, V)>>(edges: S) {
      for (x, y) in edges {
        insertEdge(x, y)
      }
    }

    public mutating func insertEdge(_ from: V, _ to: V) {
      if from == to {
        return
      }
      neighbors[from, default: []].insert(to)
      neighbors[to, default: []].insert(from)
    }
  }

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

  /// Compute an interference graph for variables in a node.
  ///
  /// This assumes that all phi functions can be applied in parallel, and
  /// arguments to phi functions do not interfere with their targets.
  public func interferenceFor(node: CFG.Node) -> VariableGraph {
    var result = VariableGraph()
    var live = liveIn(node: node)
    let liveOut = liveOut(node: node)
    var onlySeenPhis = true
    var phiDeclarations = Set<CFG.SSAVariable>()

    for parent in cfg.predecessors[node, default: []] {
      let liveChunk = liveOnEdge[.init(from: parent, to: node), default: []]
      for x in liveChunk {
        for y in liveChunk {
          result.insertEdge(x, y)
        }
      }
    }

    for (i, inst) in cfg.nodeCode[node]!.instructions.enumerated() {
      if case .phi = inst.op {
        precondition(onlySeenPhis, "phi functions must all appear at the top of a node")
        phiDeclarations.formUnion(inst.op.defs)
      } else {
        if onlySeenPhis {
          // After all the phis run in parallel, we are left with the current
          // live set, which interferes with all the phi definitions.
          for decl in phiDeclarations {
            for l in live {
              result.insertEdge(decl, l)
            }
          }
        }
        onlySeenPhis = false
      }

      // Remove live variables that were used for the last time.
      for arg in inst.op.uses {
        if lastUses[arg]![node]! == i && !liveOut.contains(arg) {
          live.remove(arg)
        }
      }

      if !onlySeenPhis {
        // Insert interference for defs and remaining live variables.
        for arg in inst.op.defs {
          for l in live {
            result.insertEdge(arg, l)
          }
        }
      }

      // Record newly created variables as live if they are used again.
      for arg in inst.op.defs {
        if liveOut.contains(arg) || (lastUses[arg]?[node] ?? 0) > i {
          live.insert(arg)
        }
      }
    }

    assert(live == liveOut)

    // If this block only contained phi functions, we never recorded
    // interferences between the live out set and the phi declarations.
    if onlySeenPhis {
      for decl in phiDeclarations {
        for l in live {
          result.insertEdge(decl, l)
        }
      }
    }

    return result
  }

  /// Create an affinity graph, where moves might be eliminated by coloring
  /// connected vertices the same way.
  public func affinityFor(node: CFG.Node) -> VariableGraph {
    var result = VariableGraph()
    for inst in cfg.nodeCode[node]!.instructions {
      if case .phi = inst.op {
        for dst in inst.op.defs {
          for src in inst.op.uses {
            result.insertEdge(dst, src)
          }
        }
      }
    }
    return result
  }

}
