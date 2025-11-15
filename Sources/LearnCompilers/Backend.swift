public protocol Backend {
  /// Compile the SSA'd control flow graph.
  func compileAssembly(cfg: CFG) throws -> String
}
