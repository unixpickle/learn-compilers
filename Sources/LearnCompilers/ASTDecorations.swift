public protocol FileID {}

extension String: FileID {}

public struct Position {
  public var fileID: FileID
  public var line: Int
  public var column: Int

  public func advanced(text: String) -> Position {
    var res = self
    for x in text {
      if x == "\n" {
        res.line += 1
        res.column = 0
      } else {
        res.column += 1
      }
    }
    return res
  }
}

public class PointerHashable: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }

  public static func == (lhs: PointerHashable, rhs: PointerHashable) -> Bool {
    ObjectIdentifier(lhs) == ObjectIdentifier(rhs)
  }
}

public final class Scope: PointerHashable {
  public let startPosition: Position

  public init(startPosition p: Position) {
    self.startPosition = p
  }
}

public final class Variable: PointerHashable {
  public enum DataType {
    case string
    case integer
  }

  public let declarationPosition: Position
  public let name: String
  public let type: DataType

  public init(declarationPosition p: Position, name n: String, type t: DataType) {
    declarationPosition = p
    name = n
    type = t
  }
}

public struct VariableTable {
  public var nameToVariable: [String: Variable]
}

public struct ScopeTable {
  public var scopeParent: [Scope: Scope]
  public var scopeVariables: [Scope: VariableTable]
}

extension ASTNode {
  public mutating func calculatePosition(start: Position) -> Position {
    self.position = start
    switch self.contents {
    case .code(let text):
      self.position = start
      return start.advanced(text: text)
    case .children(let ch):
      var p = start
      var newChildren = ch
      for (i, child) in ch.enumerated() {
        var newChild = child
        p = newChild.calculatePosition(start: p)
        newChildren[i] = newChild
      }
      contents = .children(newChildren)
      return p
    }
  }
}
