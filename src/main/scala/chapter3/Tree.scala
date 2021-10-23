package org.okarmus
package chapter3

import chapter3.Tree._

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  //Ex 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  //Ex 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  //Ex 3.27
  def depth(t: Tree[Int]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  //Ex 3.28
  def fold[A, B](tree: Tree[A], f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left, f)(g), fold(right, f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree, (_: A) => 1)(1 + _ + _)

  def maximumViaFold(tree: Tree[Int]): Int = fold(tree, (a: Int) => a)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree, (_: A) => 0)((left, right) => 1 + (left max right))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree, (a: A) => Leaf(f(a)): Tree[B])((left, right) => Branch(left, right))

}


object TreeTest {
  def main(args: Array[String]): Unit = {
    val tree = Branch(
      Branch(
        Leaf(1),
        Branch(
          Leaf(2),
          Leaf(9)
        )
      ),
      Branch(
        Leaf(6),
        Leaf(7)
      )
    )

    println(size(tree) == 9)
    println(maximum(tree) == 9)
    println(depth(tree) == 3)

    val stringTree = Branch(
      Branch(
        Leaf("a"),
        Branch(
          Leaf("aa"),
          Leaf("aaaaaaaaa")
        )
      ),
      Branch(
        Leaf("aaaaaa"),
        Leaf("aaaaaaa")
      )
    )

    println(map(stringTree)(_.length) == tree)

    println(sizeViaFold(tree) == 9)
    println(maximumViaFold(tree) == 9)
    println(depthViaFold(tree) == 3)
    println(mapViaFold(stringTree)(_.length) == tree)

  }
}


