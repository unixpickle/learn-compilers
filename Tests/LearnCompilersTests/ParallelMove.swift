import Testing

@testable import LearnCompilers

@Test func testParallelMoveSimple() throws {
  let encodedMove = MoveOp.encodeParallelMove([(0, 1), (1, 2)])
  #expect(encodedMove == [.move(dst: 0, src: 1), .move(dst: 1, src: 2)])
}

@Test func testParallelMoveSwap() throws {
  let encodedMove = MoveOp.encodeParallelMove([(0, 1), (1, 0)])
  #expect(
    encodedMove == [.saveTmp(src: 0), .move(dst: 0, src: 1), .loadTmp(dst: 1)]
      || encodedMove == [.saveTmp(src: 1), .move(dst: 1, src: 0), .loadTmp(dst: 0)]
  )
}

@Test func testParallelMoveRandom() throws {
  for _ in 0..<50 {
    let slots = Array(0...10)
    let moveCount = 7
    let dsts = slots.shuffled()[..<moveCount]
    let srcs = (0..<moveCount).map { _ in slots.randomElement()! }
    let encodedMove = MoveOp.encodeParallelMove(Array(zip(dsts, srcs)))

    var memory = slots
    var tmp = -1
    for mv in encodedMove {
      switch mv {
      case .saveTmp(let src): tmp = memory[src]
      case .loadTmp(let dst): memory[dst] = tmp
      case .move(let dst, let src): memory[dst] = memory[src]
      }
    }

    var expectedMemory = slots
    for (dst, src) in zip(dsts, srcs) {
      expectedMemory[dst] = slots[src]
    }

    #expect(memory == expectedMemory)
  }
}
