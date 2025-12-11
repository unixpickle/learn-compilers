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
      insts.append(contentsOf: inst.description.split(separator: "\n").map(String.init))
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

extension CFG.Inst: CustomStringConvertible {
  public var description: String {
    op.description
  }
}

extension CFG.Inst.Op: CustomStringConvertible {
  public var description: String {
    switch self {
    case .funcArg(let v, let idx): ".funcArg(\n  target=\(v),\n  idx=\(idx)\n)"
    case .check(let arg): ".check(\(arg))"
    case .copy(let target, let source): ".copy(\n  target=\(target),\n  source=\(source)\n)"
    case .call(let fn, let args):
      ".call(\n  fn=\(fn),\n  args=[\n\(args.map { "    \($0)" }.joined(separator: ",\n"))\n  ]\n)"
    case .callAndStore(let target, let fn, let args):
      ".callAndStore(\n  target=\(target)\n  fn=\(fn),\n  args=[\n\(args.map { "    \($0)" }.joined(separator: ",\n"))\n  ]\n)"
    case .returnValue(let arg): ".returnValue(\(arg))"
    case .returnVoid: ".returnVoid"
    case .phi(let target, let branches):
      ".phi(\n  \(target),\n  branches=[\n\(branches.sorted(by: { $0.key.id < $1.key.id }).map { "    \($0)" }.joined(separator: ",\n") )\n  ]\n)"
    }
  }
}

extension CFG.Argument: CustomStringConvertible {
  public var description: String {
    switch self {
    case .constInt(let x): ".constInt(\(x))"
    case .constStr(let x): ".constStr(\(x))"
    case .variable(let v): ".variable(\(v))"
    }
  }
}

extension CFG.SSAVariable: CustomStringConvertible {
  public var description: String {
    if let v = version {
      "SSAVariable(\(variable), version=\(v))"
    } else {
      "SSAVariable(\(variable), version=nil)"
    }
  }
}
