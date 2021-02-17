package com.carvana.tic.tac.toe.game

import com.carvana.tic.tac.toe.models.{Cell, Position, Marker, Move}

/**
  * A trait representing an instance of Tic Tac Toe GameBoard.
  * A GameBoard generally interfaces interactions between a Game, and the underlying GameGrid.
  */
trait GameBoard {

  /**
   * The underlying GameGrid, which houses the Cell's, and any Marker placement
   */
  val grid: GameGrid

  /**
   * A logical helper to indication if the Game is over, and winningMarker is a valid response.
   */
  val isGameOver: Boolean

  /**
   * A holder of the clear winner, if any
   */
  val winningMarker: Option[Marker]

  /**
   * A logical helper to determine if the Move about to be made is valid.
   * @param move The Move about to be made.
   * @return
   */
  def isMoveValid(move: Move): Boolean

  /**
   * An action to make a Move, and place a Marker into the GameGrid, resulting in a new state for the GameBoard
   * @param move The Move to make
   * @return
   */
  def makeMove(move: Move): GameBoard
}

/**
 * A classic implementation of a Tic Tac Toe GameBoard
 * @param grid
 */
case class ClassicGameBoard(grid: GameGrid, isGameOver: Boolean = false, winningMarker: Option[Marker] = None) extends GameBoard {

  override def isMoveValid(move: Move): Boolean = {
    if (move.position.row < 0 || move.position.row > 2)
      false
    else if (move.position.col < 0 || move.position.col > 2)
      false
    else if (grid.cellHasMarker(move.position))
      false
    else
      true
  }

  override def makeMove(move: Move): GameBoard = {
    // TODO throw if !isMoveValid
    val nextGrid = grid.placeMove(move)
    val isGameOver = 
      if (nextGrid.unplacedPositions == 0)
        true
      else if (nextGrid.winningMarker != None)
        true
      else 
        false
    ClassicGameBoard(grid = nextGrid, isGameOver = isGameOver, winningMarker = nextGrid.winningMarker)
  }
}
