package org.okarmus
package chapter2

object Ex2_3_4_5 {

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = { b: B => f(a, b) }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}
