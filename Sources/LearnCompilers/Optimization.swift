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

          for (n, code) in nodeCode {
            nodeCode[n] = code.replacing(arg: .variable(variable), with: newArgument)
          }

          // We might have replaced a user-defined variable with a temporary
          // variable, in which case we prefer to retain the user-defined one.
          if case .variable(let tmpVar) = newArgument,
            tmpVar.variable.isTemporary && !variable.variable.isTemporary
          {
            for (n, code) in nodeCode {
              nodeCode[n] = code.replacing(tmpVar, with: variable)
            }
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

  /// Inline all functions which are only called once, removing the
  /// function definitions that were inlined.
  public mutating func inlineSingleCalls() {
    while let (fn, node, idx) = functionUsage().compactMap({ (fn, uses) -> (Function, Node, Int)? in
      if uses.count != 1 {
        return nil
      }
      if functions[fn] == nil {
        // This must be a built-in function, so we can't inline it.
        return nil
      }
      let (node, idx) = uses.first!
      let fnNodes = Set(dfsFrom(node: functions[fn]!))
      if fnNodes.contains(node) {
        // The function is called once, but it's recursive.
        return nil
      }
      return (fn, node, idx)
    }).sorted(by: { $0.1.id < $1.1.id || ($0.1.id == $1.1.id && $0.2 < $1.2) }).first {
      inlineCall(node: node, instruction: idx)
      remove(function: fn)
    }
  }

  /// Inline the function call at a given instruction position.
  public mutating func inlineCall(node: Node, instruction: Int) {
    let code = nodeCode[node]!
    let (targetVar, fn, arguments): (SSAVariable?, Function, [Argument]) =
      switch code.instructions[instruction].op {
      case .call(let f, let a): (nil, f, a)
      case .callAndStore(let t, let f, let a): (t, f, a)
      default: fatalError("not a function call")
      }

    let (newNodes, entry, exit) = duplicate(function: fn)
    for node in newNodes {
      var newCode = nodeCode[node]!
      var newInsts = [CFG.Inst]()
      for var inst in newCode.instructions {
        switch inst.op {
        case .funcArg(let target, let idx):
          inst.op = .copy(target, arguments[idx])
        case .returnValue(let value):
          if let t = targetVar {
            inst.op = .copy(t, value)
          } else {
            continue
          }
        case .returnVoid:
          continue
        default: ()
        }
        newInsts.append(inst)
      }
      newCode.instructions = newInsts
      nodeCode[node] = newCode
    }

    // Stitch together the new graph.
    predecessors[entry] = [node]
    var newTail = exit
    if instruction + 1 < code.instructions.count {
      // Split out the remaining instructions of the node.
      newTail = addNode()
      nodeCode[newTail]!.instructions = Array(code.instructions[(instruction + 1)...])
      successors[exit] = .single(newTail)
      predecessors[newTail] = [exit]
    }
    if let succ = successors[node] {
      successors[newTail] = succ
      for n in succ.nodes {
        predecessors[n] = Set(predecessors[n]!.map { $0 == node ? newTail : $0 })

        // Replace the source node in phi functions of this successor.
        var newCode = nodeCode[n]!
        for (i, var inst) in newCode.instructions.enumerated() {
          if case .phi(let target, var branches) = inst.op,
            let v = branches.removeValue(forKey: node)
          {
            branches[newTail] = v
            inst.op = .phi(target, branches)
            newCode.instructions[i] = inst
          }
        }
        nodeCode[n] = newCode
      }
    }
    successors[node] = .single(entry)
    nodeCode[node]!.instructions = Array(code.instructions[..<instruction])
  }

  internal mutating func duplicate(function fn: Function) -> (
    nodes: [Node], entry: Node, exit: Node
  ) {
    // Duplicate the inlined function with new nodes and variables.
    var newVariables = [Variable: Variable]()
    let origNodes = dfsFrom(node: functions[fn]!)
    let copiedNodes: Array = origNodes.map { oldNode in
      let newNode = addNode()
      var newCode = nodeCode[oldNode]!
      for (i, var inst) in newCode.instructions.enumerated() {
        for v in inst.op.defs + inst.op.uses {
          if let newV = newVariables[v.variable] {
            inst.op = inst.op.replacing(v, with: SSAVariable(variable: newV, version: v.version))
          } else {
            let newV = v.variable.duplicate()
            newVariables[v.variable] = newV
            inst.op = inst.op.replacing(v, with: SSAVariable(variable: newV, version: v.version))
          }
        }
        newCode.instructions[i] = inst
      }
      nodeCode[newNode] = newCode
      return newNode
    }

    // Replicate the graph connectivity into the new nodes.
    let origToCopy = Dictionary(uniqueKeysWithValues: zip(origNodes, copiedNodes))
    for (orig, copy) in zip(origNodes, copiedNodes) {
      if let p = predecessors[orig] {
        predecessors[copy] = Set(p.map { origToCopy[$0]! })
      }
      switch successors[orig] {
      case .single(let x): successors[copy] = .single(origToCopy[x]!)
      case .branch(let x, let y):
        successors[copy] = .branch(ifFalse: origToCopy[x]!, ifTrue: origToCopy[y]!)
      case .none: ()
      }
    }

    // Replace phi function arguments
    for node in copiedNodes {
      var newCode = nodeCode[node]!
      for (i, var inst) in newCode.instructions.enumerated() {
        if case .phi(let target, let branches) = inst.op {
          inst.op = .phi(
            target,
            Dictionary(
              uniqueKeysWithValues: branches.map { (origToCopy[$0.key]!, $0.value) }
            )
          )
          newCode.instructions[i] = inst
        }
      }
      nodeCode[node] = newCode
    }

    let entryNodes = copiedNodes.filter { predecessors[$0, default: []].isEmpty }
    assert(
      entryNodes.count == 1, "expected exactly one root node in a function, got \(entryNodes.count)"
    )
    let entryNode = entryNodes.first!
    let exitNodes = copiedNodes.filter { successors(of: $0).isEmpty }
    assert(
      exitNodes.count == 1,
      "expected exactly one leaf node in a function"
    )
    let exitNode = exitNodes.first!

    return (nodes: copiedNodes, entry: entryNode, exit: exitNode)
  }

}
