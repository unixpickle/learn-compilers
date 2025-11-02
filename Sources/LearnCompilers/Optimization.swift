extension CFG {

  /// Eliminate branches on known constants, possibly removing
  /// nodes as possible.
  ///
  /// Returns false if no branches were eliminated.
  public mutating func eliminateConstantBranches() -> Bool {
    var succeeded = false
    for node in nodes {
      var code = nodeCode[node]!
      guard let lastInst = code.instructions.last else {
        continue
      }
      guard case .check(.constInt(let x)) = lastInst.op else {
        continue
      }
      guard case .branch(let ifFalse, let ifTrue) = successors[node] else {
        fatalError("check without branch")
      }
      let (keepBranch, removeBranch) =
        if x == 0 {
          (ifFalse, ifTrue)
        } else {
          (ifTrue, ifFalse)
        }
      code.instructions.removeLast()
      nodeCode[node] = code

      predecessors[removeBranch]!.remove(node)
      successors[node] = .single(keepBranch)

      pruneIfUnreachable(removeBranch)
      succeeded = true
    }
    return succeeded
  }

  private mutating func pruneIfUnreachable(_ node: Node) {
    if !predecessors[node, default: []].isEmpty {
      return
    }

    var queue = [node]
    while let next = queue.popLast() {
      predecessors.removeValue(forKey: next)
      nodes.remove(next)
      for successor in successors(of: next) {
        predecessors[successor]!.remove(next)
        if predecessors[successor]!.isEmpty {
          queue.append(successor)
        }
      }
      successors.removeValue(forKey: next)
    }
  }

}
