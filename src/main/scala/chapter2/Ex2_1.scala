package org.okarmus
package chapter2

import chapter2.Ex21.{fib, findFirst}

import scala.annotation.tailrec
import scala.math.abs

object Ex21 {

  def fib(n: Int): Int = {
    @tailrec
    def inner(acc: Int, curr: Int, next: Int): Int = {
      if (acc == 0) curr
      else inner(acc - 1, next, curr + next)
    }
    inner(n, 0 , 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = s"The $name of $n is ${f(n)}"

  def formatAbs(x: Int) = formatResult("absolute value", x, abs)

  def formatFactorial(x: Int) = formatResult("factorial", x, factorial)

  val factorial: Int => Int = { x => x }   //This factorial is not implemented obviously

  def findFirst[A] (as: Array[A], p: A => Boolean): Option[Int] = {
    def loop(n: Int): Option[Int] = {
      if (n >= as.length) None
      else if (p(as(n))) Some(n)
      else loop(n + 1)
    }

    loop(0)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    Seq.range(0, 10).map(index => (index, fib(index)))
      .foreach({ case (index, fib) => println(s"The $index value is $fib") })

    findFirst[String](Array("as", "fds", "dsad", "ffd", "fds"), _ == "ffd")
  }
}
