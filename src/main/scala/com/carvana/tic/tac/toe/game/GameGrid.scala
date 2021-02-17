package com.carvana.tic.tac.toe.game

import com.carvana.tic.tac.toe.models.{Cell, Position, Marker, Move, X, O}

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
   * See which Marker is in a cell
   */
  def markerInCell(position: Position): Option[Marker]

  /**
   * A holder of the clear winner, if any
   */
  val winningMarker: Option[Marker]

  /**
   * Scan the GameGrid to see if there is a winner
   */
  def checkWinner(): Option[Marker]

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
case class ClassicGameGrid(dimension: Int = 3, cells: Seq[Cell] = Seq(), unplacedPositions: Int = 9, winningMarker: Option[Marker] = None) extends GameGrid {

  override def cellHasMarker(position: Position): Boolean = {
    val found = cells.find(c => {
        c.position.row == position.row &&
        c.position.col == position.col
    })
    found.getOrElse(Cell(Position(row = 0, col = 0),None)).placedMarker != None
  }

  override def markerInCell(position: Position): Option[Marker] = {
    val found = cells.find(c => {
        c.position.row == position.row &&
        c.position.col == position.col
    })
    found.getOrElse(Cell(Position(row = 0, col = 0),None)).placedMarker
  }

  override def checkWinner(): Option[Marker] = {
    val xCells = cells.filter(c => c.placedMarker == X)
    val oCells = cells.filter(c => c.placedMarker == O)
    val wins = Seq(
      // rows
      Seq(Position(row = 0, col = 0), Position(row =0, col = 1), Position(row = 0, col = 2)),
      Seq(Position(row = 1, col = 0), Position(row =1, col = 1), Position(row = 1, col = 2)),
      Seq(Position(row = 2, col = 0), Position(row =2, col = 1), Position(row = 2, col = 2)),
      // cols
      Seq(Position(row = 0, col = 0), Position(row =1, col = 0), Position(row = 2, col = 0)),
      Seq(Position(row = 0, col = 1), Position(row =1, col = 1), Position(row = 2, col = 1)),
      Seq(Position(row = 0, col = 2), Position(row =1, col = 2), Position(row = 2, col = 2)),
      // diags
      Seq(Position(row = 0, col = 0), Position(row =1, col = 1), Position(row = 2, col = 2)),
      Seq(Position(row = 2, col = 0), Position(row =1, col = 1), Position(row = 0, col = 2)),
    )
    for ((marker, cells) <- List((X, xCells), (O, oCells)))
      for (w <- wins) {
        var c: Int = 0
        for (p <- w)
          if (cells.contains(Cell(Position(row = p.row, col = p.col), Option(marker)))) {
            c += 1
          }
        if (c == dimension) {
          return Option(marker)
        }
      }
    None
  }

  override def placeMove(move: Move): GameGrid = {
    val found = cells.find(c =>
        c.position.row == move.position.row &&
        c.position.col == move.position.col)
    val idx = found match {
      case None => -1
      case _ => cells.indexOf(found.get)
    }
    val nextCells = idx match {
      case idx if (idx >= 0) => cells.updated(idx, Cell(move.position, Option(move.marker)))
      case _ => cells :+ Cell(move.position, Option(move.marker))
    }
    ClassicGameGrid(dimension, nextCells, unplacedPositions -1, checkWinner())
  }
}
