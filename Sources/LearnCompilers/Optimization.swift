extension CFG {

  /// Perform basic optimizations on the SSA representation.
  @discardableResult
  public mutating func performBasicOptimizations(
    fnReduction: ((Function, [Argument]) -> Argument?)? = nil
  ) -> Bool {
    var converged = false
    var changed = false
    while !converged {
      converged = true
      if propagateConstantsAndCopies(fnReduction: fnReduction) {
        converged = false
      }
      if eliminateConstantBranches() {
        converged = false
      }
      if eliminateUnusedVariables() {
        converged = false
      }
      if !converged {
        changed = true
      }
    }
    return changed
  }

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
            if let red = fnReduction, let newValue = red(fn, args) {
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
        } else {
          // Remove relevant arguments to successor phi functions
          var succCode = nodeCode[successor]!
          for (i, var inst) in succCode.instructions.enumerated() {
            if case .phi(let target, var sources) = inst.op, sources[next] != nil {
              sources.removeValue(forKey: next)
              inst.op = .phi(target, sources)
              succCode.instructions[i] = inst
            }
          }
          nodeCode[successor] = succCode
        }
      }
      successors.removeValue(forKey: next)
    }
  }

  /// Remove variable definitions that have no uses.
  @discardableResult
  public mutating func eliminateUnusedVariables() -> Bool {
    var succeeded = false
    var converged = false
    while !converged {
      converged = true

      var usageCount = [SSAVariable: Int]()
      for node in nodes {
        for inst in nodeCode[node]!.instructions {
          // Exclude phi's with itself as an argument
          let defs = Set(inst.op.defs)
          let uses = Set(inst.op.uses).subtracting(defs)

          for use in uses {
            usageCount[use, default: 0] += 1
          }
        }
      }

      for node in nodes {
        var newCode = nodeCode[node]!
        var i = 0
        while i < newCode.instructions.count {
          let inst = newCode.instructions[i]
          let defs = inst.op.defs
          if let def = defs.first, defs.count == 1, usageCount[def, default: 0] == 0 {
            if case .callAndStore(_, let fn, let args) = inst.op {
              // The function call might have side-effects that we want to keep.
              newCode.instructions[i].op = .call(fn, args)
              i += 1
            } else {
              newCode.instructions.remove(at: i)
            }
            converged = false
          } else {
            i += 1
          }
        }
        nodeCode[node] = newCode
      }
      if !converged {
        succeeded = true
      }
    }
    return succeeded
  }

}
