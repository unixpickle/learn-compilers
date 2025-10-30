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

public final class Variable: PointerHashable, CustomStringConvertible {
  public enum DataType: Hashable, Sendable, CustomStringConvertible {
    case string
    case integer

    public var description: String {
      switch self {
      case .string: "str"
      case .integer: "int"
      }
    }
  }

  public let declarationPosition: Position
  public let name: String
  public let type: DataType
  public let isArgument: Bool

  public var description: String {
    "Variable(name=\(name), type=\(type), isArgument=\(isArgument), pos=\(declarationPosition))"
  }

  public init(
    declarationPosition p: Position,
    name n: String,
    type t: DataType,
    isArgument i: Bool = false
  ) {
    declarationPosition = p
    name = n
    type = t
    isArgument = i
  }
}

public final class Function: Hashable, Sendable {
  public struct Signature: Hashable, Sendable {
    let args: [Variable.DataType]
    let ret: Variable.DataType?
  }

  public let declarationPosition: Position
  public let name: String
  public let signature: Signature
  public let builtIn: BuiltInFunction?

  public init(
    declarationPosition p: Position,
    name n: String,
    signature s: Signature,
    builtIn b: BuiltInFunction? = nil
  ) {
    declarationPosition = p
    name = n
    signature = s
    builtIn = b
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }

  public static func == (lhs: Function, rhs: Function) -> Bool {
    ObjectIdentifier(lhs) == ObjectIdentifier(rhs)
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
  public var functions: [String: [Function]]

  public init() {
    scopeParent = [:]
    scopeVariables = [:]
    functions = [:]
  }

  public mutating func addBuiltIns() {
    for fn in BuiltInFunction.functions {
      functions[fn.name, default: []].append(fn)
    }
  }

  public func resolveFunc(name: String, args: [Variable.DataType]) -> Function? {
    for f in functions[name, default: []] {
      if f.signature.args == args {
        return f
      }
    }
    return nil
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

public protocol ASTDecorationError: Error {
  var position: Position { get }
}

public enum VariableResolutionError: ASTDecorationError {
  case notDefined(Position, String)
  case redefined(original: Position, newPosition: Position, name: String)

  public var position: Position {
    switch self {
    case .notDefined(let p, _): p
    case .redefined(_, let p, _): p
    }
  }
}

public enum FuncResolutionError: ASTDecorationError {
  case redefined(
    original: Position,
    newPosition: Position,
    name: String,
    oldSignature: Function.Signature,
    newSignature: Function.Signature
  )

  public var position: Position {
    switch self {
    case .redefined(_, let p, _, _, _): p
    }
  }
}

public enum TypeAndControlFlowError: ASTDecorationError {
  case breakOutsideOfLoop(Position)
  case incorrectType(
    position: Position, expectedType: Variable.DataType, actualType: Variable.DataType
  )
  case unknownFunction(position: Position, name: String, argTypes: [Variable.DataType])
  case missingValue(position: Position)
  case returnWithTypeFromVoidFunction(position: Position)
  case returnWithVoidFromTypedFunction(position: Position, expectedType: Variable.DataType)

  public var position: Position {
    switch self {
    case .breakOutsideOfLoop(let p): p
    case .incorrectType(let p, _, _): p
    case .unknownFunction(let p, _, _): p
    case .missingValue(let p): p
    case .returnWithTypeFromVoidFunction(let p): p
    case .returnWithVoidFromTypedFunction(let p, _): p
    }
  }
}

private enum InsertingScopesOp {
  case expand(Scope?, ASTNode)
  case finish(Scope?, ASTNode, Int)
}

private enum ResolvingVariablesOp {
  case expand(Scope?, ASTNode)
  case finish(Scope?, ASTNode, Int)
}

private enum ResolvingTypesOp {
  case expand(ASTNode, Function?, Bool)
  case finish(ASTNode, Function?, Bool, Int)
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
    var opStack: [InsertingScopesOp] = [.expand(parent, self)]
    var outputStack = [ASTNode]()

    while let op = opStack.popLast() {
      switch op {
      case .expand(var scope, let node):
        if (node as? AST.Block) != nil || (node as? AST.FuncDecl) != nil {
          scope = table.addScope(startPosition: position!, parent: scope)
        }
        if case .children(let children) = node.contents {
          opStack.append(.finish(scope, node, children.count))
          for child in children.reversed() {
            opStack.append(.expand(scope, child))
          }
        } else {
          outputStack.append(node)
        }
      case .finish(let scope, var node, let childCount):
        node.contents = .children(Array(outputStack[(outputStack.count - childCount)...]))
        outputStack.removeLast(childCount)
        if var block = (node as? AST.Block) {
          block.scope = scope
          node = block
        } else if var block = (node as? AST.FuncDecl) {
          block.scope = scope
          node = block
        }
        outputStack.append(node)
      }
    }

    assert(outputStack.count == 1)
    return outputStack[0] as! Self
  }

  /// Insert function pointers at every function declaration.
  public func recordingFunctions(table: inout ScopeTable) -> (Self, [FuncResolutionError]) {
    if (self as? AST.Block) != nil {
      // No functions are defined within blocks.
      return (self, [])
    }

    var result = self
    var errors = [FuncResolutionError]()
    if var fun = (self as? AST.FuncDecl) {
      let fn = Function(
        declarationPosition: fun.position!,
        name: fun.name.name,
        signature: .init(
          args: fun.args.map { $0.typeID.dataType },
          ret: fun.retType?.retType.dataType
        )
      )
      if let existing = table.resolveFunc(name: fn.name, args: fn.signature.args) {
        errors.append(
          .redefined(
            original: existing.declarationPosition,
            newPosition: fn.declarationPosition,
            name: fn.name,
            oldSignature: existing.signature,
            newSignature: fn.signature
          )
        )
      } else {
        table.functions[fn.name, default: []].append(fn)
        fun.function = fn
        result = fun as! Self
      }
    }
    if case .children(let children) = result.contents {
      var newChildren = [ASTNode]()
      for child in children {
        let (newChild, subErrs) = child.recordingFunctions(table: &table)
        errors.append(contentsOf: subErrs)
        newChildren.append(newChild)
      }
      result.contents = .children(newChildren)
    }
    return (result, errors)
  }

  /// Resolve variable references and definitions in the table, possibly returning errors.
  public func resolvingVariables(table: inout ScopeTable, scope: Scope? = nil) -> (
    Self, [VariableResolutionError]
  ) {
    var opStack: [ResolvingVariablesOp] = [.expand(scope, self)]
    var outputStack = [ASTNode]()
    var errors = [VariableResolutionError]()

    while let op = opStack.popLast() {
      switch op {
      case .expand(var scope, let node):
        if let block = (node as? AST.Block) {
          scope = block.scope
        } else if let fun = (node as? AST.FuncDecl) {
          scope = fun.scope
        }

        if case .children(let children) = node.contents {
          opStack.append(.finish(scope, node, children.count))
          for child in children.reversed() {
            opStack.append(.expand(scope, child))
          }
        } else {
          outputStack.append(node)
        }
      case .finish(let scope, var node, let childCount):
        node.contents = .children(Array(outputStack[(outputStack.count - childCount)...]))
        outputStack.removeLast(childCount)

        if var arg = (node as? AST.FuncDecl.ArgDecl) {
          guard let scope = scope else {
            fatalError("function argument should appear inside function scope")
          }
          let name = arg.identifier.name
          if let existing = table.scopeVariables[scope]![name] {
            errors.append(
              VariableResolutionError.redefined(
                original: existing.declarationPosition,
                newPosition: arg.position!,
                name: name
              )
            )
          } else {
            let v = Variable(
              declarationPosition: arg.position!,
              name: name,
              type: arg.typeID.dataType,
              isArgument: true
            )
            table.scopeVariables[scope]![name] = v
            arg.identifier.variable = v
            node = arg
          }
        } else if var decl = (node as? AST.VarDecl) {
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
            let v = Variable(
              declarationPosition: decl.position!,
              name: decl.identifier.name,
              type: decl.typeID.dataType
            )
            table.scopeVariables[scope]![decl.identifier.name] = v
            decl.identifier.variable = v
            node = decl
          }
        } else if var assign = (node as? AST.VarAssign) {
          guard let scope = scope else {
            fatalError("statement appears outside block scope")
          }
          if let v = table.resolve(scope: scope, name: assign.identifier.name) {
            assign.identifier.variable = v
            node = assign
          } else {
            errors.append(
              VariableResolutionError.notDefined(assign.position!, assign.identifier.name)
            )
          }
        } else if let expr = (node as? AST.Expression), case .identifier(var id, let pos) = expr {
          guard let scope = scope else {
            fatalError("statement appears outside block scope")
          }
          if let v = table.resolve(scope: scope, name: id.name) {
            id.variable = v
            node = AST.Expression.identifier(id, pos)
          } else {
            errors.append(VariableResolutionError.notDefined(pos!, id.name))
          }
        }
        outputStack.append(node)
      }
    }

    assert(outputStack.count == 1)

    return (outputStack[0] as! Self, errors)
  }

  /// Map function calls to specific functions based on types, verify break
  /// and return statements, and check variable assignment types.
  public func resolvingTypesAndControlFlow(
    table: inout ScopeTable,
    inFunction: Function? = nil,
    inLoop: Bool = false
  ) -> (Self, [TypeAndControlFlowError]) {
    var opStack: [ResolvingTypesOp] = [.expand(self, inFunction, inLoop)]
    var outputStack = [ASTNode]()
    var errors = [TypeAndControlFlowError]()

    func typeFor(expr: AST.Expression) -> Variable.DataType? {
      switch expr {
      case .intLiteral: .integer
      case .identifier(let id, _): id.variable!.type
      case .funcCall(let call, _): call.identifier.function!.signature.ret
      }
    }

    while let op = opStack.popLast() {
      switch op {
      case .expand(let node, var inFunction, var inLoop):
        if let fun = (node as? AST.FuncDecl) {
          inFunction = fun.function!
        } else if (node as? AST.WhileLoop) != nil {
          inLoop = true
        }
        if case .children(let children) = node.contents {
          opStack.append(.finish(node, inFunction, inLoop, children.count))
          for child in children.reversed() {
            opStack.append(.expand(child, inFunction, inLoop))
          }
        } else {
          outputStack.append(node)
        }
      case .finish(var node, let inFunction, let inLoop, let childCount):
        node.contents = .children(Array(outputStack[(outputStack.count - childCount)...]))
        outputStack.removeLast(childCount)

        if var call = (node as? AST.FuncCall) {
          // Resolve function call based on name and argument types.
          var argTypes = [Variable.DataType]()
          var badTypes = false
          for arg in call.args {
            if let maybeType = typeFor(expr: arg.expression) {
              argTypes.append(maybeType)
            } else {
              // An error should already have been flagged when handling
              // our children.
              badTypes = true
              break
            }
          }
          if !badTypes {
            if let fn = table.resolveFunc(name: call.identifier.name, args: argTypes) {
              call.identifier.function = fn
              node = call
            } else {
              errors.append(
                .unknownFunction(
                  position: call.identifier.position!,
                  name: call.identifier.name,
                  argTypes: argTypes
                )
              )
            }
          }
        } else if let expr = (node as? AST.Expression) {
          // Raise an error if any expression doesn't have a resolvable type.
          if typeFor(expr: expr) == nil {
            errors.append(.missingValue(position: expr.position!))
          }
        } else if let assign = (node as? AST.VarAssign), let t = typeFor(expr: assign.expression) {
          if t != assign.identifier.variable!.type {
            errors.append(
              .incorrectType(
                position: assign.expression.position!,
                expectedType: assign.identifier.variable!.type,
                actualType: t
              )
            )
          }
        } else if let decl = (node as? AST.VarDecl), let t = typeFor(expr: decl.expression) {
          if t != decl.identifier.variable!.type {
            errors.append(
              .incorrectType(
                position: decl.expression.position!,
                expectedType: decl.identifier.variable!.type,
                actualType: t
              )
            )
          }
        } else if let br = (node as? AST.BreakStatement) {
          if !inLoop {
            errors.append(.breakOutsideOfLoop(br.position!))
          }
        } else if let ret = (node as? AST.ReturnStatement) {
          // Verify the return type from the function
          guard let fn = inFunction else {
            fatalError("statement outside of function")
          }
          if let arg = ret.expression {
            if let retType = fn.signature.ret {
              if let t = typeFor(expr: arg) {
                if t != retType {
                  errors.append(
                    .incorrectType(
                      position: arg.position!,
                      expectedType: retType,
                      actualType: t
                    )
                  )
                }
              }
            } else {
              errors.append(.returnWithTypeFromVoidFunction(position: arg.position!))
            }
          } else {
            if let retType = fn.signature.ret {
              errors.append(
                .returnWithVoidFromTypedFunction(
                  position: ret.position!,
                  expectedType: retType
                )
              )
            }
          }
        }

        outputStack.append(node)
      }
    }

    assert(outputStack.count == 1)

    return (outputStack[0] as! Self, errors)
  }

  /// Run the full decoration pipeline on the AST, returning any errors that arise.
  public func decorated(table: inout ScopeTable, fileID: String) -> (Self, [ASTDecorationError]) {
    var result = self
    (result, _) = result.insertingPositions(start: Position(fileID: fileID))
    result = result.insertingScopes(table: &table)
    var fnErrs: [FuncResolutionError]
    (result, fnErrs) = result.recordingFunctions(table: &table)
    if !fnErrs.isEmpty {
      return (result, fnErrs.map { $0 as ASTDecorationError })
    }
    var varErrs: [VariableResolutionError]
    (result, varErrs) = result.resolvingVariables(table: &table)
    if !varErrs.isEmpty {
      return (result, varErrs.map { $0 as ASTDecorationError })
    }
    var otherErrs: [TypeAndControlFlowError]
    (result, otherErrs) = result.resolvingTypesAndControlFlow(table: &table)
    if !otherErrs.isEmpty {
      return (result, otherErrs.map { $0 as ASTDecorationError })
    }
    return (result, [])
  }
}
