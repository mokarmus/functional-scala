package org.okarmus
package chapter11

case class Id[A](value: A) {

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

  def map[B](f: A => B): Id[B] = flatMap(a => Id(f(a)))

}


object Id {

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa flatMap f
  }

}
