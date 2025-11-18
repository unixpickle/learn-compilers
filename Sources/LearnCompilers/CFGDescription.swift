extension CFG: CustomStringConvertible {

  public var description: String {
    var fnDescs = [String]()
    for (fn, node) in functions {
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
