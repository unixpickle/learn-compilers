/// An abstract object representing a move instruction from one location to
/// another, or to/from a single shared temporary variable.
///
/// This is used to represent single operations inside a parallel move.
public enum MoveOp<T: Hashable>: Hashable {
  case move(dst: T, src: T)
  case saveTmp(src: T)
  case loadTmp(dst: T)

  /// Encode a parallel move as a sequence of sequential moves which may use
  /// at most one temporary variable at a time.
  public static func encodeParallelMove(_ moves: [(dst: T, src: T)]) -> [MoveOp] {
    let dsts = moves.map { $0.dst }
    assert(Set(dsts).count == dsts.count, "parallel move destinations must be unique")

    var srcToDsts = [T: [T]]()
    var srcID = [T: Int]()
    for (dst, src) in moves {
      if srcID[src] == nil {
        srcID[src] = srcID.count
      }
      srcToDsts[src, default: []].append(dst)
    }

    var results = [MoveOp]()
    while let rootSrc = srcToDsts.keys.min(by: { srcID[$0]! < srcID[$1]! }) {
      let rootDsts = srcToDsts.removeValue(forKey: rootSrc)!

      var subResult = [MoveOp]()
      var foundCycle = false

      var queue: [T] = rootDsts
      while let nextSrc = queue.popLast() {
        if let dsts = srcToDsts.removeValue(forKey: nextSrc) {
          for d in dsts {
            subResult.append(.move(dst: d, src: nextSrc))
            if d != rootSrc {
              queue.append(d)
            } else {
              foundCycle = true
            }
          }
        }
      }

      subResult.reverse()
      if !foundCycle {
        for dst in rootDsts {
          subResult.append(.move(dst: dst, src: rootSrc))
        }
      } else {
        subResult.insert(.saveTmp(src: rootSrc), at: 0)
        for dst in rootDsts {
          subResult.append(.loadTmp(dst: dst))
        }
      }

      results.append(contentsOf: subResult)
    }

    return results
  }
}
