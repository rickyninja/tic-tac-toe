package com.carvana.tic.tac.toe.game

import com.carvana.tic.tac.toe.models.{Marker, X, O}

/**
 * A trait model for a Player
 */
trait Player {
  val displayName: String
  val marker: Marker
}

case class ClassicPlayer(displayName: String, marker: Marker) extends Player {
}
