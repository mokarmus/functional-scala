package org.okarmus
package chapter6

import chapter6.State.unit

case class State[S, +A](run: S => (A, S)) {

  //Ex 6.10
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

object State {
  //Ex 6.10
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def map2[S, A, B, C](aState: State[S, A], bState: State[S, B])(f: (A, B) => C): State[S, C] = State(s1 => {
    val (a, s2) = aState.run(s1)
    val (b, s3) = bState.run(s2)
    (f(a, b), s3)
  })

  def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] = State(s => {
    as.foldRight((List.empty[A], s)) { case (next, (list, state)) =>
      val (a, state2) = next.run(state)
      (a :: list, state2)
    }
  })

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set (f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}


object StateTesting {
  type Rand[A] = State[RNG, A]

  def main(args: Array[String]): Unit = {


  }


}