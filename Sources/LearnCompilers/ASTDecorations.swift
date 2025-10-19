public struct Position: Hashable, Sendable {
  public var fileID: String
  public var line: Int
  public var column: Int

  public init(fileID: String, line: Int = 0, column: Int = 0) {
    self.fileID = fileID
    self.line = line
    self.column = column
  }

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

extension AST.TypeIdentifier {
  public var dataType: Variable.DataType {
    switch self.name {
    case "str": .string
    case "int": .integer
    default: fatalError()
    }
  }
}

public struct ScopeTable {
  public var scopeParent: [Scope: Scope]
  public var scopeVariables: [Scope: [String: Variable]]

  public init() {
    scopeParent = [:]
    scopeVariables = [:]
  }

  public mutating func addScope(startPosition: Position, parent: Scope?) -> Scope {
    let scope = Scope(startPosition: startPosition)
    if let p = parent {
      scopeParent[scope] = p
    }
    scopeVariables[scope] = [:]
    return scope
  }

  public func resolve(scope: Scope, name: String) -> Variable? {
    var scope: Scope? = scope
    while let s = scope {
      if let out = scopeVariables[s]![name] {
        return out
      }
      scope = scopeParent[s]
    }
    return nil
  }
}

public enum VariableResolutionError: Error {
  case notDefined(Position, String)
  case redefined(original: Position, newPosition: Position, name: String)
}

extension ASTNode {
  /// Insert self.position and the position of every child, given the starting
  /// position of this node.
  ///
  /// Returns the starting position of the node after this one.
  public func insertingPositions(start: Position) -> (Self, Position) {
    var result = self
    result.position = start
    switch self.contents {
    case .code(let text):
      return (result, start.advanced(text: text))
    case .children(let ch):
      var p = start
      var newChildren = ch
      for (i, var child) in ch.enumerated() {
        (child, p) = child.insertingPositions(start: p)
        newChildren[i] = child
      }
      result.contents = .children(newChildren)
      return (result, p)
    }
  }

  /// Insert scopes on every code block to setup the scope hierarchy.
  public func insertingScopes(table: inout ScopeTable, parent: Scope? = nil) -> Self {
    var parent = parent
    var result = self
    if var block = (self as? AST.Block) {
      let scope = table.addScope(startPosition: position!, parent: parent)
      block.scope = scope
      parent = scope
      result = block as! Self
    }
    if case .children(let children) = result.contents {
      let newChildren = children.map { $0.insertingScopes(table: &table, parent: parent) }
      result.contents = .children(newChildren)
    }
    return result
  }

  /// Resolve variable references and definitions in the table, possibly returning errors.
  public func resolvingVariables(table: inout ScopeTable, scope: Scope? = nil) -> (
    Self, [VariableResolutionError]
  ) {
    var scope = scope
    var result = self
    var errors = [VariableResolutionError]()

    if let block = (self as? AST.Block) {
      scope = block.scope
    }
    if var decl = (self as? AST.VarDecl) {
      guard let scope = scope else {
        fatalError("statement appears outside block scope")
      }
      if let existing = table.scopeVariables[scope]![decl.identifier.name] {
        errors.append(
          VariableResolutionError.redefined(
            original: existing.declarationPosition,
            newPosition: decl.position!,
            name: decl.identifier.name
          )
        )
      } else {
        // Make sure to resolve symbols on the RHS before defining this one, to avoid
        // allowing circular cases like "a: int = a" when "a" isn't already defined.
        let (newExpr, newErrors) = decl.expression.resolvingVariables(table: &table, scope: scope)
        errors.append(contentsOf: newErrors)
        decl.expression = newExpr
        let v = Variable(
          declarationPosition: decl.position!,
          name: decl.identifier.name,
          type: decl.typeID.dataType
        )
        table.scopeVariables[scope]![decl.identifier.name] = v
        decl.identifier.variable = v
        result = decl as! Self
      }
    } else if var assign = (self as? AST.VarAssign) {
      guard let scope = scope else {
        fatalError("statement appears outside block scope")
      }
      if let v = table.resolve(scope: scope, name: assign.identifier.name) {
        assign.identifier.variable = v
        let (newExpr, newErrors) = assign.expression.resolvingVariables(table: &table, scope: scope)
        errors.append(contentsOf: newErrors)
        assign.expression = newExpr
        result = assign as! Self
      } else {
        errors.append(VariableResolutionError.notDefined(assign.position!, assign.identifier.name))
      }
    } else if let expr = (self as? AST.Expression), case .identifier(var id, let pos) = expr {
      guard let scope = scope else {
        fatalError("statement appears outside block scope")
      }
      if let v = table.resolve(scope: scope, name: id.name) {
        id.variable = v
        result = AST.Expression.identifier(id, pos) as! Self
      } else {
        errors.append(VariableResolutionError.notDefined(pos!, id.name))
      }
    } else {
      if case .children(let children) = result.contents {
        var newChildren = [ASTNode]()
        for child in children {
          let (newChild, e) = child.resolvingVariables(table: &table, scope: scope)
          newChildren.append(newChild)
          errors.append(contentsOf: e)
        }
        result.contents = .children(newChildren)
      }
    }

    return (result, errors)
  }
}
