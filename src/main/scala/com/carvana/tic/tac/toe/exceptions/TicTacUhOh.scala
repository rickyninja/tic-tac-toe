package com.carvana.tic.tac.toe.exceptions

/**
 * Some Exceptions that could be used for flow control
 */
sealed trait TicTacUhOh extends Exception
case object InvalidMoveException extends TicTacUhOh
case object MarkerAlreadyTakenException extends TicTacUhOh
case object OutOfTurnException extends TicTacUhOh
case object InvalidInputException extends TicTacUhOh