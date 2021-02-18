package com.carvana.tic.tac.toe.game

import com.carvana.tic.tac.toe.GameSetUp
import com.carvana.tic.tac.toe.models.{Cell, Position, X, O, Move, Marker}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks._

class GameGridSpec extends AnyFlatSpec with should.Matchers with GameSetUp {

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

  "draw" should "draw state of the grid" in {
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
}
