package com.carvana.tic.tac.toe.models

/**
 * A class representing a player's Move
 * @param position The position of the cell in the GameGrid
 * @param marker The Marker to place at the position
 */
case class Move(position: Position, marker: Marker)
