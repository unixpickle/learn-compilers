extension CFG {

  /// Perform constant propagation, optionally using a helper to
  /// reduce function calls with constant arguments to constants.
  /// Also propagates copies by eliminating duplicated variables.
  ///
  /// Returns false if no values were changed.
  @discardableResult
  public mutating func propagateConstantsAndCopies(
    fnReduction: ((Function, [Argument]) -> Argument?)? = nil
  ) -> Bool {
    var succeeded = false
    var converged = false
    while !converged {
      converged = true
      for node in nodes {
        var newCode = nodeCode[node]!
        var i = 0
        while i < newCode.instructions.count {
          let inst = newCode.instructions[i]

          var variable: SSAVariable? = nil
          var newArgument: Argument? = nil

          switch inst.op {
          case .copy(let target, let source):
            variable = target
            newArgument = source
          case .callAndStore(let target, let fn, let args):
            let allConstants = args.allSatisfy {
              if case .variable = $0 {
                false
              } else {
                true
              }
            }
            if allConstants, let red = fnReduction, let newValue = red(fn, args) {
              variable = target
              newArgument = newValue
            }
          case .phi(let target, let sourceMap):
            // Avoid reducing incomplete phis (which may exist for return blocks)
            if Set(sourceMap.keys) == Set(predecessors[node, default: []]) {
              let unique = Set(sourceMap.values)
              if unique.count == 1, let v = unique.first {
                // Phi function with one unique argument is removed.
                variable = target
                newArgument = v
              }
            }
          default: ()
          }

          guard let variable = variable, let newArgument = newArgument else {
            // No substitution was found.
            i += 1
            continue
          }

          newCode.instructions.remove(at: i)
          nodeCode[node] = newCode

          for n in nodes {
            nodeCode[n] = nodeCode[n]!.replacing(arg: .variable(variable), with: newArgument)
          }
          newCode = nodeCode[node]!

          converged = false
        }
      }
      if !converged {
        succeeded = true
      }
    }
    return succeeded
  }

  /// Eliminate branches on known constants, possibly removing
  /// nodes as possible.
  ///
  /// Returns false if no branches were eliminated.
  @discardableResult
  public mutating func eliminateConstantBranches() -> Bool {
    var succeeded = false
    let oldNodes = nodes
    for node in oldNodes {
      if !nodes.contains(node) {
        // This node was already eliminated
        continue
      }

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
      nodeCode.removeValue(forKey: next)
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
