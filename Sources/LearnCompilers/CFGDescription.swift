extension CFG: CustomStringConvertible {

  public var description: String {
    var fnDescs = [String]()
    let sortedFunctions = functions.sorted { a, b in
      let (f1, _) = a
      let (f2, _) = b
      if f1.name < f2.name {
        return true
      } else if f2.name < f1.name {
        return false
      }
      if f1.signature.args.count < f2.signature.args.count {
        return true
      } else if f2.signature.args.count < f1.signature.args.count {
        return false
      }
      let sig1 = f1.signature.args.map { $0.description }.joined()
      let sig2 = f2.signature.args.map { $0.description }.joined()
      return sig1 < sig2
    }
    for (fn, node) in sortedFunctions {
      fnDescs.append("  fn \(fn.name) {\n" + describeFunction(node: node, indent: "    ") + "\n  }")
    }
    return "CFG(\n\(fnDescs.joined(separator: "\n"))\n)"
  }

  private func describeFunction(node: Node, indent: String) -> String {
    var blocks = [String]()
    for node in dfsFrom(node: node) {
      let blockCode = describeNode(node: node, indent: indent + "  ")
      blocks.append(indent + "node\(node.id):\n" + blockCode)
    }
    return blocks.joined(separator: "\n\n")
  }

  private func describeNode(node: Node, indent: String) -> String {
    var insts = [String]()
    for inst in nodeCode[node]!.instructions {
      insts.append("\(inst)")
    }
    switch successors[node] {
    case .branch(let ifFalse, let ifTrue):
      insts.append("branch true=node\(ifFalse.id) true=node\(ifTrue.id)")
    case .single(let other):
      insts.append("goto node\(other.id)")
    case .none: ()
    }
    return insts.map { indent + $0 }.joined(separator: "\n")
  }

}

extension CFG.Inst.Op: CustomStringConvertible {
  public var description: String {
    switch self {
    case .funcArg(let v, let idx): ".funcArg(\(v), \(idx))"
    case .check(let arg): ".check(\(arg))"
    case .copy(let target, let source): ".copy(\(target), \(source))"
    case .call(let fn, let args): ".call(\(fn), \(args))"
    case .callAndStore(let target, let fn, let args): ".callAndStore(\(target), \(fn), \(args))"
    case .returnValue(let arg): ".returnValue\(arg)"
    case .returnVoid: ".returnVoid"
    case .phi(let target, let branches):
      ".phi(\(target), \(branches.sorted(by: { $0.key.id < $1.key.id })))"
    }
  }
}
