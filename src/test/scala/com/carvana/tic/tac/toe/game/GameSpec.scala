package com.carvana.tic.tac.toe.game

import com.carvana.tic.tac.toe.models.{Position, Move, O, X}
import com.carvana.tic.tac.toe.{GamePlayLogic, GameSetUp, TicTacIO}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should


// Implement there to take a pre-determined collection of moves that results in...
// 1. Player X wining
object PlayerXWins extends TicTacIO {
  val moves: Array[Move] = Array(
    Move(Position(0,0), X),
    Move(Position(2,0), O),
    Move(Position(0,1), X),
    Move(Position(2,1), O),
    Move(Position(0,2), X),
  )
  var index: Int = 0
  override def getMoveForPlayer(player: Player): Move = {
    val m = moves(index)
    if (index < moves.length-1)
      index += 1
    m
  }
}
// 2. Player O winning
object PlayerOWins extends TicTacIO {
  val moves: Array[Move] = Array(
    Move(Position(1,1), X),
    Move(Position(0,0), O),
    Move(Position(2,0), X),
    Move(Position(0,1), O),
    Move(Position(2,1), X),
    Move(Position(0,2), O),
  )
  var index: Int = 0
  override def getMoveForPlayer(player: Player): Move = {
    val m = moves(index)
    if (index < moves.length-1)
      index += 1
    m
  }
}
// 3. A tie
object TieGame extends TicTacIO {
  val moves: Array[Move] = Array(
    Move(Position(0,0), X),
    Move(Position(0,1), O),
    Move(Position(0,2), X),
    Move(Position(1,1), O),
    Move(Position(1,0), X),
    Move(Position(1,2), O),
    Move(Position(2,2), X),
    Move(Position(2,0), O),
    Move(Position(2,1), X),
  )
  var index: Int = 0
  override def getMoveForPlayer(player: Player): Move = {
    val m = moves(index)
    if (index < moves.length-1)
      index += 1
    m
  }
}

class GameSpec extends AnyFlatSpec with should.Matchers
  with GameSetUp with GamePlayLogic  {
  val dimension: Int = 3

  "A Game" should "make sure the currentPlayer is first in queue" in {
    assert(newGame.currentPlayer == newGame.playerQueue.head)
  }

  it should "not let the current player move twice" in {
    assert(newGame.currentPlayer != newGame.playerQueue.drop(1).head)
  }

  it should "not allow two players to use the same Marker" in {
    val player1 = newGame.currentPlayer
    val player2 = newGame.playerQueue.drop(1).head
    assert(player1.marker != player2.marker)
  }

  it should "play itself out where Player X wins in" in {
    assert(playGame(newGame)(PlayerXWins).map(_.marker).contains(X))
  }

  it should "play itself out where Player O wins in" in {
    assert(playGame(newGame)(PlayerOWins).map(_.marker).contains(O))
  }

  it should "play itself out where there is a tie" in {
    assert(playGame(newGame)(TieGame).isEmpty)
  }
}
