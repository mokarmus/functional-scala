package org.okarmus
package chapter10

import chapter8.{Gen, Prop}

trait Monoid[A] {
  def zero: A

  def op(a1: A, a2: A): A
}


