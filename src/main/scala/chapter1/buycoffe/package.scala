package org.okarmus
package chapter1

import chapter1.buycoffe.{Cafe, Coffee, CreditCard}

package object buycoffe {

  case class CreditCard(balance: Double) {
    def charge(amount: Double): CreditCard = CreditCard(balance - amount) //We are assuming here that balance can be below zero
  }

  case class ChargeCommand(creditCard: CreditCard, amount: Double) {
    def charge(): CreditCard = creditCard.charge(amount)
    def +(other: ChargeCommand): ChargeCommand = ChargeCommand(this.creditCard, this.amount + other.amount)       //We are not validating if the credit card is different
  }

  class Coffee {
    val price: Double = 12.0
  }

  class Cafe {
    def buyCoffee(creditCard: CreditCard): (Coffee, ChargeCommand) = {
      val coffee = new Coffee()
      val charge = ChargeCommand(creditCard, coffee.price)
      (coffee, charge)
    }

    def buyCoffees(creditCard: CreditCard, number: Int): (List[Coffee], ChargeCommand) = {
      val (coffees, charges) = List.fill(number){buyCoffee(creditCard)}.unzip
      (coffees, charges.reduce(_ + _))
    }
  }
}


object Main {
  def main(args: Array[String]): Unit = {
    val cafe = new Cafe()
    val cc = CreditCard(100)

    val (coffees, creditCard) = cafe.buyCoffees(cc , 10)

    println(creditCard.charge())

  }
}


