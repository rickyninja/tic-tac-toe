package com.carvana.tic.tac.toe.game

import com.carvana.tic.tac.toe.GameSetUp
import com.carvana.tic.tac.toe.models.{Cell, Position, X, O, Move}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GameGridSpec extends AnyFlatSpec with should.Matchers with GameSetUp {

  "A GameGrid" should "have a bunch of tests implemented" in {
    assert(false)
  }

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
  }

  "top row winner" should "return winner from checkWinner" in {
    val cells = Seq(
        Cell(Position(0,0), Option(X)), Cell(Position(0,1), Option(X)), Cell(Position(0,2), Option(X)),
    )
    val gg = new ClassicGameGrid(cells = cells)
    assert(gg.checkWinner() == Option(X))
  }

}