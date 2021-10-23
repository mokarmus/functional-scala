package org.okarmus
package chapter2

import scala.annotation.tailrec

object Ex2_2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(toCheck: List[A]): Boolean = toCheck match {
      case h1 :: h2 :: t => if (ordered(h1, h2)) loop(h2 :: t) else false
      case _ => true
    }
    loop(as.toList)
  }

  def main(args: Array[String]): Unit = {
    assert(isSorted[Int](Array(1, 2, 6, 8), _ <= _))
    assert(!isSorted[String](Array("dsa", "s", "daasa"), (s1, s2) => s1.length <= s2.length))
  }


}


