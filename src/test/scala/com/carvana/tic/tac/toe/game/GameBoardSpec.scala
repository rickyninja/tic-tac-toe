package com.carvana.tic.tac.toe.game

import com.carvana.tic.tac.toe.GameSetUp
import com.carvana.tic.tac.toe.models.{Cell, Position, X, O, Move, Marker}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks._

class GameBoardSpec extends AnyFlatSpec with should.Matchers with GameSetUp {

  "A (clean) GameBoard" should "not be over before the game starts" in {
    val gb = ClassicGameBoard(ClassicGameGrid(cells = Seq()))
    assert(gb.isGameOver == false)
  }

  it should "not have a declared winner before the game starts" in {
    val gb = ClassicGameBoard(ClassicGameGrid(cells = Seq()))
    assert(gb.winningMarker == None)
  }

  it should "be able to make any move on the GameGrid for X" in {
    val gb = ClassicGameBoard(ClassicGameGrid(cells = Seq()))
    val positions = Table(
      ("row", "col", "marker"),  // First tuple defines column names
      (0, 0, X),
      (0, 1, X),
      (0, 2, X),
      (1, 0, X),
      (1, 1, X),
      (1, 2, X),
      (2, 0, X),
      (2, 1, X),
      (2, 2, X),
    )
    forAll (positions) { (row: Int, col: Int, marker: Marker) =>
      val pos = Position(row, col)
      val got = gb.makeMove(Move(pos, marker))
      assert(got.grid.markerInCell(pos).get == X)
    }
  }

  it should "set isGameOver true once the game has been won" in {
    var gb: GameBoard = ClassicGameBoard(ClassicGameGrid(cells = Seq()))
    val positions = Table(
      ("row", "col", "marker"),  // First tuple defines column names
      (0, 0, X),
      (0, 1, X),
      (0, 2, X),
    )
    forAll (positions) { (row: Int, col: Int, marker: Marker) =>
      val pos = Position(row, col)
      gb = gb.makeMove(Move(pos, marker))
    }
    assert(gb.isGameOver == true)
  }

  it should "be able to make any move on the GameGrid for O" in {
    val gb = ClassicGameBoard(ClassicGameGrid(cells = Seq()))
    val positions = Table(
      ("row", "col", "marker"),  // First tuple defines column names
      (0, 0, O),
      (0, 1, O),
      (0, 2, O),
      (1, 0, O),
      (1, 1, O),
      (1, 2, O),
      (2, 0, O),
      (2, 1, O),
      (2, 2, O),
    )
    forAll (positions) { (row: Int, col: Int, marker: Marker) =>
      val pos = Position(row, col)
      val got = gb.makeMove(Move(pos, marker))
      assert(got.grid.markerInCell(pos).get == O)
    }
  }

  it should "be able to make a move, resulting in a new and different state" in {
    val pos = Position(0, 0)
    val gb = ClassicGameBoard(ClassicGameGrid(cells = Seq(Cell(pos, None))))
    assert(gb.grid.markerInCell(pos) == None)
    val got = gb.makeMove(Move(pos, X))
    assert(got.grid.markerInCell(pos).get == X)
  }
}
