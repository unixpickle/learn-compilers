public enum GraphColorAlgorithm {
  case greedy
}

/// Color the variables in a liveness graph.
public func color(
  graph: Liveness.VariableGraph,
  affinity: Liveness.VariableGraph,
  order: [CFG.SSAVariable],
  algorithm: GraphColorAlgorithm
) -> [CFG.SSAVariable: Int] {
  switch algorithm {
  case .greedy:
    colorGreedy(graph: graph, affinity: affinity, order: order)
  }
}

private func colorGreedy(
  graph: Liveness.VariableGraph,
  affinity: Liveness.VariableGraph,
  order: [CFG.SSAVariable]
) -> [CFG.SSAVariable: Int] {
  let sortFn = variableSortFn(order)
  var result = [CFG.SSAVariable: Int]()
  for v in graph.nodes.sorted(by: sortFn) {
    let notAllowed = Set(graph.neighbors[v, default: []].compactMap { result[$0] })
    let affinityColors = affinity.neighbors[v, default: []].sorted(by: sortFn).compactMap {
      result[$0]
    }.filter {
      !notAllowed.contains($0)
    }
    if let affinityColor = affinityColors.first {
      result[v] = affinityColor
    } else {
      var color = 0
      while notAllowed.contains(color) {
        color += 1
      }
      result[v] = color
    }
  }
  return result
}

func variableSortFn(_ order: [CFG.SSAVariable]) -> ((CFG.SSAVariable, CFG.SSAVariable) -> Bool) {
  let varToID = Dictionary(uniqueKeysWithValues: order.enumerated().map { ($0.1, $0.0) })
  return { x, y in varToID[x]! < varToID[y]! }
}
