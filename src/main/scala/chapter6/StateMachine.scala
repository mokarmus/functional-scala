package org.okarmus
package chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def action(input: Input): Machine = input match {
    case Coin => handleCoinInsert()
    case Turn => handleTurn()
  }

  private[this] def handleCoinInsert(): Machine = this match {
    case Machine(true, candies, _) if candies > 0 => Machine(locked = false, candies, coins + 1)
    case _ => this //Machine does not change its state if there are no candies or it is already unlocked
  }

  private[this] def handleTurn(): Machine = this match {
    case Machine(false, candies, _) if candies > 0 => Machine(locked = true, candies - 1, coins)
    case _ => this
  }
}

object Machine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine => {
    inputs.foldLeft(((machine.coins, machine.candies), machine)) { case ((_, machine), input) =>
      val newMachine = machine.action(input)
      ((newMachine.coins, newMachine.candies), newMachine)
    }
  })
}


object StateMachine {

  def main(args: Array[String]): Unit = {
    println("We are going to test state machine here")

    val machine = Machine(locked = true, coins = 10, candies = 5)
    val simulation = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))

    println(simulation.run(machine))


  }

}
