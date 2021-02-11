package com.carvana.tic.tac.toe.models

/**
 * A Player identifier
 */
sealed trait Marker
case object X extends Marker
case object O extends Marker
