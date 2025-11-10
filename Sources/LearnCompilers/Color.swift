public enum GraphColorAlgorithm {
  case greedy
}

/// Color the liveness graph
public func color(
  graph: Liveness.VariableGraph,
  affinity: Liveness.VariableGraph,
  algorithm: GraphColorAlgorithm
) -> [CFG.SSAVariable: Int] {
  switch algorithm {
  case .greedy:
    colorGreedy(graph: graph, affinity: affinity)
  }
}

private func colorGreedy(
  graph: Liveness.VariableGraph,
  affinity: Liveness.VariableGraph
) -> [CFG.SSAVariable: Int] {
  var result = [CFG.SSAVariable: Int]()
  for v in graph.nodes {
    let notAllowed = Set(graph.neighbors[v, default: []].compactMap { result[$0] })
    var color = 0
    while notAllowed.contains(color) {
      color += 1
    }
    result[v] = color
  }
  return result
}
