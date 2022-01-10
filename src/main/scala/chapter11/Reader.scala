package org.okarmus
package chapter11

//Ex. 11.20
case class Reader[R, A](run: R => A)

object Reader {

  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(
      r => f(fa.run(r)).run(r)
    )
  }

}
