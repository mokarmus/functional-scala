package org.okarmus
package chapter13

import chapter13.IO._

case class Player(name: String, score: Int)

object Player {

  def winner(p1: Player, p2: Player): Option[Player] = {
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None
  }

  def winnerMsg(p: Option[Player]): String = p.map {
    case Player(name, _) => s"$name is the winner!"
  }.getOrElse("It's a draw.")

  def contest(p1: Player, p2: Player): IO[Unit] = PrintLine(winnerMsg(winner(p1, p2)))
}

object PlayerProgram extends App {

  val player1 = Player("Mateusz", 12)
  val player2 = Player("Marek", 30)

  Player.contest(player1, player2).run
}
