package com.carvana.tic.tac.toe.game

import com.carvana.tic.tac.toe.GameSetUp
import com.carvana.tic.tac.toe.models.{Cell, Position, X, O, Move, Marker}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks._

class GameGridSpec extends AnyFlatSpec with should.Matchers with GameSetUp {
  val dimension: Int = 3

  "placeMove" should "update cells with the Move" in {
    val cells = Seq(
        Cell(Position(0,0), None), Cell(Position(0,1), None), Cell(Position(0,2), None),
        Cell(Position(1,0), None), Cell(Position(1,1), None), Cell(Position(1,2), None),
        Cell(Position(2,0), None), Cell(Position(2,1), None), Cell(Position(2,2), None),
    )
    val gg = ClassicGameGrid(cells = cells)
    val move = Move(Position(0,0), X)
    val got = gg.placeMove(move)
    assert(got.cells(0) == Cell(Position(0,0), Option(X)))
  }

  "placeMove" should "decrement unplacedPositions" in {
    val cells = Seq(
        Cell(Position(0,0), None), Cell(Position(0,1), None), Cell(Position(0,2), None),
        Cell(Position(1,0), None), Cell(Position(1,1), None), Cell(Position(1,2), None),
        Cell(Position(2,0), None), Cell(Position(2,1), None), Cell(Position(2,2), None),
    )
    val gg = ClassicGameGrid(cells = cells)
    assert(gg.unplacedPositions == 9)
    val move = Move(Position(0,0), X)
    val got = gg.placeMove(move)
    assert(got.unplacedPositions == 8)
  }

  "placeMove" should "set winningMarker" in {
    val cells = Seq(
        Cell(Position(0,0), Some(X)), Cell(Position(0,1), Some(X)), Cell(Position(0,2), Some(X)),
    )
    var gg: GameGrid = ClassicGameGrid(cells = Seq())
    val positions = Table(
      ("row", "col", "marker"),  // First tuple defines column names
      (0, 0, X),
      (0, 1, X),
      (0, 2, X),
    )
    forAll (positions) { (row: Int, col: Int, marker: Marker) =>
      val pos = Position(row, col)
      gg = gg.placeMove(Move(pos, marker))
    }
    assert(gg.cells.length == 3)
    assert(gg.winningMarker == Option(X))
  }

  "top row winner" should "return winner from checkWinner" in {
    val cells = Seq(
        Cell(Position(0,0), Option(X)), Cell(Position(0,1), Option(X)), Cell(Position(0,2), Option(X)),
    )
    val gg = ClassicGameGrid(cells = cells)
    assert(gg.checkWinner() == Option(X))
  }

  "draw" should "draw state of the grid when dimension equals 3" in {
    val cells = Seq(
        Cell(Position(0,0), Some(X)), Cell(Position(0,1), None), Cell(Position(0,2), Some(O)),
        Cell(Position(1,0), Some(O)), Cell(Position(1,1), Some(X)), Cell(Position(1,2), None),
        Cell(Position(2,0), None), Cell(Position(2,1), Some(X)), Cell(Position(2,2), Some(O)),
    )
    var gg: GameGrid = ClassicGameGrid(cells = cells)
    assert(gg.cells.length == 9)
    val want = "X-O\nOX-\n-XO"
    assert(gg.draw() == want)
  }

  "draw" should "draw state of the grid when dimension equals 9" in {
    val cells = Seq(
      Cell(Position(0,0),None), Cell(Position(0,1),Some(X)), Cell(Position(0,2),Some(X)), Cell(Position(0,3),Some(X)), Cell(Position(0,4),Some(X)), Cell(Position(0,5),Some(X)), Cell(Position(0,6),Some(X)), Cell(Position(0,7),Some(X)), Cell(Position(0,8),Some(X)),
      Cell(Position(1,0),None), Cell(Position(1,1),Some(O)), Cell(Position(1,2),Some(O)), Cell(Position(1,3),Some(O)), Cell(Position(1,4),Some(O)), Cell(Position(1,5),Some(O)), Cell(Position(1,6),Some(O)), Cell(Position(1,7),Some(O)), Cell(Position(1,8),Some(O)),
      Cell(Position(2,0),None), Cell(Position(2,1),Some(X)), Cell(Position(2,2),Some(X)), Cell(Position(2,3),Some(X)), Cell(Position(2,4),Some(X)), Cell(Position(2,5),Some(X)), Cell(Position(2,6),Some(X)), Cell(Position(2,7),Some(X)), Cell(Position(2,8),Some(X)),
      Cell(Position(3,0),None), Cell(Position(3,1),Some(O)), Cell(Position(3,2),Some(O)), Cell(Position(3,3),Some(O)), Cell(Position(3,4),Some(O)), Cell(Position(3,5),Some(O)), Cell(Position(3,6),Some(O)), Cell(Position(3,7),Some(O)), Cell(Position(3,8),Some(O)),
      Cell(Position(4,0),None), Cell(Position(4,1),Some(X)), Cell(Position(4,2),Some(X)), Cell(Position(4,3),Some(X)), Cell(Position(4,4),Some(X)), Cell(Position(4,5),Some(X)), Cell(Position(4,6),Some(X)), Cell(Position(4,7),Some(X)), Cell(Position(4,8),Some(X)),
      Cell(Position(5,0),None), Cell(Position(5,1),Some(O)), Cell(Position(5,2),Some(O)), Cell(Position(5,3),Some(O)), Cell(Position(5,4),Some(O)), Cell(Position(5,5),Some(O)), Cell(Position(5,6),Some(O)), Cell(Position(5,7),Some(O)), Cell(Position(5,8),Some(O)),
      Cell(Position(6,0),None), Cell(Position(6,1),Some(X)), Cell(Position(6,2),Some(X)), Cell(Position(6,3),Some(X)), Cell(Position(6,4),Some(X)), Cell(Position(6,5),Some(X)), Cell(Position(6,6),Some(X)), Cell(Position(6,7),Some(X)), Cell(Position(6,8),Some(X)),
      Cell(Position(7,0),None), Cell(Position(7,1),Some(O)), Cell(Position(7,2),Some(O)), Cell(Position(7,3),Some(O)), Cell(Position(7,4),Some(O)), Cell(Position(7,5),Some(O)), Cell(Position(7,6),Some(O)), Cell(Position(7,7),Some(O)), Cell(Position(7,8),Some(O)),
      Cell(Position(8,0),None), Cell(Position(8,1),Some(X)), Cell(Position(8,2),Some(X)), Cell(Position(8,3),Some(X)), Cell(Position(8,4),Some(X)), Cell(Position(8,5),Some(X)), Cell(Position(8,6),Some(X)), Cell(Position(8,7),Some(X)), Cell(Position(8,8),Some(X)),
    )
    var gg: GameGrid = ClassicGameGrid(dimension = 9, cells = cells, unplacedPositions = 9*9)
    assert(gg.cells.length == 9*9)
    val want = "-XXXXXXXX\n" + "-OOOOOOOO\n" + "-XXXXXXXX\n" + "-OOOOOOOO\n" + "-XXXXXXXX\n" + "-OOOOOOOO\n" + "-XXXXXXXX\n" + "-OOOOOOOO\n" + "-XXXXXXXX"
    //println(gg.draw())
    assert(gg.draw() == want)
  }

  "makeWinSequences" should "create expected sequences when dimension equals 9" in {
    var gg: GameGrid = ClassicGameGrid(dimension = 9, cells = Seq(), unplacedPositions = 9*9)
    val wins = gg.makeWinSequences()
    assert(wins.length == 20)
    // rows
    assert(wins.contains(Seq(Position(0,0), Position(0,1), Position(0,2), Position(0,3), Position(0,4), Position(0,5), Position(0,6), Position(0,7), Position(0,8))))
    assert(wins.contains(Seq(Position(1,0), Position(1,1), Position(1,2), Position(1,3), Position(1,4), Position(1,5), Position(1,6), Position(1,7), Position(1,8))))
    assert(wins.contains(Seq(Position(2,0), Position(2,1), Position(2,2), Position(2,3), Position(2,4), Position(2,5), Position(2,6), Position(2,7), Position(2,8))))
    assert(wins.contains(Seq(Position(3,0), Position(3,1), Position(3,2), Position(3,3), Position(3,4), Position(3,5), Position(3,6), Position(3,7), Position(3,8))))
    assert(wins.contains(Seq(Position(4,0), Position(4,1), Position(4,2), Position(4,3), Position(4,4), Position(4,5), Position(4,6), Position(4,7), Position(4,8))))
    assert(wins.contains(Seq(Position(5,0), Position(5,1), Position(5,2), Position(5,3), Position(5,4), Position(5,5), Position(5,6), Position(5,7), Position(5,8))))
    assert(wins.contains(Seq(Position(6,0), Position(6,1), Position(6,2), Position(6,3), Position(6,4), Position(6,5), Position(6,6), Position(6,7), Position(6,8))))
    assert(wins.contains(Seq(Position(7,0), Position(7,1), Position(7,2), Position(7,3), Position(7,4), Position(7,5), Position(7,6), Position(7,7), Position(7,8))))
    assert(wins.contains(Seq(Position(8,0), Position(8,1), Position(8,2), Position(8,3), Position(8,4), Position(8,5), Position(8,6), Position(8,7), Position(8,8))))
    // cols
    assert(wins.contains(Seq(Position(0,0), Position(1,0), Position(2,0), Position(3,0), Position(4,0), Position(5,0), Position(6,0), Position(7,0), Position(8,0))))
    assert(wins.contains(Seq(Position(0,1), Position(1,1), Position(2,1), Position(3,1), Position(4,1), Position(5,1), Position(6,1), Position(7,1), Position(8,1))))
    assert(wins.contains(Seq(Position(0,2), Position(1,2), Position(2,2), Position(3,2), Position(4,2), Position(5,2), Position(6,2), Position(7,2), Position(8,2))))
    assert(wins.contains(Seq(Position(0,3), Position(1,3), Position(2,3), Position(3,3), Position(4,3), Position(5,3), Position(6,3), Position(7,3), Position(8,3))))
    assert(wins.contains(Seq(Position(0,4), Position(1,4), Position(2,4), Position(3,4), Position(4,4), Position(5,4), Position(6,4), Position(7,4), Position(8,4))))
    assert(wins.contains(Seq(Position(0,5), Position(1,5), Position(2,5), Position(3,5), Position(4,5), Position(5,5), Position(6,5), Position(7,5), Position(8,5))))
    assert(wins.contains(Seq(Position(0,6), Position(1,6), Position(2,6), Position(3,6), Position(4,6), Position(5,6), Position(6,6), Position(7,6), Position(8,6))))
    assert(wins.contains(Seq(Position(0,7), Position(1,7), Position(2,7), Position(3,7), Position(4,7), Position(5,7), Position(6,7), Position(7,7), Position(8,7))))
    assert(wins.contains(Seq(Position(0,8), Position(1,8), Position(2,8), Position(3,8), Position(4,8), Position(5,8), Position(6,8), Position(7,8), Position(8,8))))
    // diags
    assert(wins.contains(Seq(Position(0,0), Position(1,1), Position(2,2), Position(3,3), Position(4,4), Position(5,5), Position(6,6), Position(7,7), Position(8,8))))
    assert(wins.contains(Seq(Position(0,8), Position(1,7), Position(2,6), Position(3,5), Position(4,4), Position(5,3), Position(6,2), Position(7,1), Position(8,0))))
  }

  "makeWinSequences" should "create expected sequences when dimension equals 3" in {
    var gg: GameGrid = ClassicGameGrid(dimension = 3, cells = Seq(), unplacedPositions = 3*3)
    val wins = gg.makeWinSequences()
    assert(wins.length == 8)
    // rows
    assert(wins.contains(Seq(Position(0,0), Position(0,1), Position(0,2))))
    assert(wins.contains(Seq(Position(1,0), Position(1,1), Position(1,2))))
    assert(wins.contains(Seq(Position(2,0), Position(2,1), Position(2,2))))
    // cols
    assert(wins.contains(Seq(Position(0,0), Position(1,0), Position(2,0))))
    assert(wins.contains(Seq(Position(0,1), Position(1,1), Position(2,1))))
    assert(wins.contains(Seq(Position(0,2), Position(1,2), Position(2,2))))
    // diags
    assert(wins.contains(Seq(Position(0,0), Position(1,1), Position(2,2))))
    assert(wins.contains(Seq(Position(0,2), Position(1,1), Position(2,0))))
  }
}
