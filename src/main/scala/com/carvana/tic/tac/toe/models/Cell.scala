package com.carvana.tic.tac.toe.models

/**
 * A class representing an element of the GameGrid
 * @param position The location of Cell in the GameGrid
 * @param placedMarker A played Marker, if any
 */
case class Cell(position: Position, placedMarker: Option[Marker])