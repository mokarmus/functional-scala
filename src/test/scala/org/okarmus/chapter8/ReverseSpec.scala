package org.okarmus
package chapter8

import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, protect}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReverseSpec extends AnyWordSpec with Matchers {

  "a list" should {
    "properly implement reverse method" in {
      val intList = Gen.listOf(Gen.choose(0, 100))

      val prop = forAll(intList) { ns => ns.reverse.reverse == ns } &&
        forAll(intList) { ns => ns.headOption == ns.reverse.lastOption }

      val failingProp = forAll(intList) { ns => ns.reverse == ns }

      prop.check()
      failingProp.check()
    }

    //Ex 8.1
    "properly sum list of ints" in {
      val intList = Gen.listOf(Gen.choose(0, 100))

      forAll(intList){ ns => ns.reverse.sum == ns.sum }.check()

      forAll(intList){ ns => ns.map(_ => ns.head).sum == ns.head * ns.size }
    }

  }

}
