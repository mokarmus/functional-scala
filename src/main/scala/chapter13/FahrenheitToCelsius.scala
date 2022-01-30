package org.okarmus
package chapter13

import scala.io.StdIn.readLine

object FahrenheitToCelsius {

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converterImpure(): Unit = {
    println("Enter a temperature in degrees Fahrenheit:")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

  def converter(): IO[Unit] = for {
    _ <- IO.PrintLine("Enter a temperature in degrees Fahrenheit:")
    fahrenheit <- IO.ReadLine
    celsius = fahrenheitToCelsius(fahrenheit.toDouble)
    _ <- IO.PrintLine(celsius.toString)
  } yield ()

}

object FahrenheitToCelsiusMain extends App {

  FahrenheitToCelsius.converter().run

}