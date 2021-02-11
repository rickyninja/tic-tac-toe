package com.carvana.tic.tac.toe.game

import com.carvana.tic.tac.toe.models.{Cell, Position, Marker, Move}

/**
 * A trait representing the state of a GameGrid. The GameGrid handles much of the logic
 * for Cells, and their contents.
 */
trait GameGrid {

  /**
   * The dimension of our grid, which is classically 3
   */
  val dimension: Int

  /**
   * A Collection of Cells, representing all possible Positions on the GameGrid.
   */
  val cells: Seq[Cell]

  /**
   * The number of un-Marked Cells
   */
  val unplacedPositions: Int

  /**
   * A logical helper to determine if a cell at a particular Position has been Marked or not.
   * @param position The position in the GameGrid.
   * @return
   */
  def cellHasMarker(position: Position): Boolean

  /**
   * A holder of the clear winner, if any
   */
  val winningMarker: Option[Marker]

  /**
   * An action to place a Marker at a Position via a Move, resulting in a new state for the GameGrid.
   * @param move The Move to place
   * @return
   */
  def placeMove(move: Move): GameGrid
}

/**
 *
 * @param dimension The dimension of our GameGrid, defaulted to 3
 * @param cells The Collection of Cells, representing all possible Positions on the GameGrid.
 */
case class ClassicGameGrid(dimension: Int = 3, cells: Seq[Cell]) extends GameGrid {

  override val unplacedPositions: Int = ???
  override val winningMarker: Option[Marker] = ???

  override def cellHasMarker(position: Position): Boolean = ???
  override def placeMove(move: Move): GameGrid = ???

}
