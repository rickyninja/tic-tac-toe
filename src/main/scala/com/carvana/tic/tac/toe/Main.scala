package com.carvana.tic.tac.toe

import com.carvana.tic.tac.toe.game.{Game, GameBoard, GameGrid, ClassicPlayer, Player, ClassicGameBoard, ClassicGameGrid, ClassicGame}
import com.carvana.tic.tac.toe.models.{Cell, Move, X, O, Position}

import scala.annotation.tailrec

/* NOTE:

The 3 Traits here are mainly for ease of hooking into Tests.
Please implement them as "defaults", for use in Main

 */


trait TicTacIO {

  /**
   *  Get moves for a player, perhaps from reading stdin?
   * @param player The player the Move is for
   * @return
   */
  def getMoveForPlayer(player: Player): Move = {
    @throws(classOf[Exception])
    def prompt(player: Player): Move = {
      println("Enter comma delimited coordinates 1,2 for Player %s".format(player.displayName))
      val line = scala.io.StdIn.readLine()
      val rowCol = line.split(",")
      if (rowCol.length != 2) {
        throw new Exception("bad coordinates. enter as 'row,col', eg '1,2'")
      }
      Move(Position(rowCol(0).toInt, rowCol(1).toInt), player.marker)
    }
    var badInput: Boolean = true
    var move: Move = Move(Position(0,0), player.marker)
    while (badInput) {
      try {
        move = prompt(player)
        badInput = false
      } catch {
        case e: Exception => println("error getting player's move: %s".format(e))
      }
    }
    move
  }

  /**
   * A utility method to display something useful about the current Game
   * state to the world.
   * @param game
   */
  def displayGameState(game: Game): Unit = {
    // print the grid
    // System.err.println()
  }

}

/**
 * A trait that builds up the default, initial state of our game.
 */
trait GameSetUp {
  // Set Up
  val dimension: Int = 3
  val emptyCells: Seq[Cell] = Seq()
  val emptyGrid: GameGrid = ClassicGameGrid(dimension, emptyCells)
  val cleanBoard: GameBoard = ClassicGameBoard(emptyGrid)
  val playerQueue: LazyList[Player] = LazyList.cons(ClassicPlayer("player X", X), LazyList.cons(ClassicPlayer("player O", O), LazyList.empty))
  val newGame: Game = ClassicGame(cleanBoard, playerQueue)
}

trait GamePlayLogic {

  /**
   * A tail-recursive method that plays a Game to completion, with input given
   * from some TicTacIO
   * @param game The game in play
   * @param ioOperator The method we are getting input to make Moves for a User
   * @return
   */
  @tailrec
  final def playGame(game: Game)(ioOperator: TicTacIO): Option[Player] = {
    var winner: Option[Player] = None
    def inner(game: Game)(ioOperator: TicTacIO): Option[Player] = {
      while (!game.gameBoard.isGameOver) {
        winner = inner(game)(ioOperator)
      }
      winner
    }
    playGame(game)(ioOperator)
    //ClassicPlayer("player fake", X) // TODO return winner
  }

}

/**
 * Our Main entry point - nothing new here _needs_ to be implemented, beyond the Traits above!
 */
object Main extends App with GameSetUp with GamePlayLogic with TicTacIO {

  // Play the game
  playGame(newGame)(ioOperator = this) match {
    case Some(player) => println(s"\n---\n${player.displayName} wins!")
    case None         => println("\n---\nThe game ended in a tie!")
  }

}