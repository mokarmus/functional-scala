package org.okarmus
package chapter8

import chapter6.RNG.{Rand, nonNegativeBetween, nonNegativeInt, unit}
import chapter6.{RNG, SimpleRNG, State}
import chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases, run}
import chapter7.ParOps
import chapter7.ParOps.Par

import java.util.concurrent.{ExecutorService, Executors}

case class Gen[A](sample: State[RNG, A]) {
  self =>
  //Ex. 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] = flatMap(a => Gen.unit(f(a)))

  def map2[B, C](genB: Gen[B])(f: (A, B) => C): Gen[C] = Gen(State.map2(sample, genB.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] = this.map2(g)((_, _))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => Gen.sequence(List.fill(s)(this)))

  //Ex. 8.10
  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {
  //Ex. 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      sample = State[RNG, Int](s => nonNegativeBetween(start, stopExclusive)(s))
    )

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(gen)(rng).zip(LazyList.from(0).take(n)).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
      s"generated an exception ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  //Ex. 8.5
  def unit[A](a: => A): Gen[A] = Gen(sample = State.unit(a))

  def boolean(): Gen[Boolean] = {
    val randomBoolean: Rand[Boolean] = RNG.map[Int, Boolean](nonNegativeInt)(i => i % 2 == 0)
    Gen(sample = State[RNG, Boolean](s => randomBoolean(s)))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  //Ex. 8.6
  def sequence[A](g: List[Gen[A]]): Gen[List[A]] = Gen(sample = State.sequence(g.map(_.sample)))

  //Ex. 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean().flatMap(bool => if (bool) g1 else g2)

  //Ex. 8.19
  def getStringIntFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State { (rng: RNG) =>
      val (seed, rng2) = rng.nextInt
      val f = (s: String) => g.sample.run(SimpleRNG(seed.toLong ^ s.hashCode.toLong))._1
      (f, rng2)
    }
  }

}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop { (max, testCases, rng) =>
    this.run(max, testCases, rng) match {
      case x: Falsified => x
      case Passed => p.run(max, testCases, rng)
      case Proved => Proved
    }
  }

  def ||(p: Prop): Prop = Prop { (max, testCases, rng) =>
    this.run(max, testCases, rng) match {
      case Falsified(_, _) => p.run(max, testCases, rng)
      case Passed => Passed
      case Proved => Proved
    }
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int
  type TestCases = Int


  def run(
           p: Prop,
           maxSize: Int = 5,
           testCases: Int = 5,
           rng: RNG = SimpleRNG(System.currentTimeMillis())
         ): Unit = p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) => println(s"! Falsified aftern $n passed tests:\n $msg")
    case Passed => println(s"+ OK, passed $testCases test.")
    case Proved => println(s"+ OK, proved property.")
  }


  def check(p: => Boolean): Prop = Prop { (_, _, _) => if (p) Proved else Falsified("()", 0) }

}

sealed trait Result {
  def isFalsified: Boolean
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => forSize(n).flatMap(a => f(a)(n)))

  def map[B](f: A => B): SGen[B] = flatMap(a => SGen.unit(f(a)))
}

object SGen {

  def unit[A](a: A): SGen[A] = Gen.unit(a).unsized

  //Ex. 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))

  //Ex. 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(math.max(n, 1), g))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: LazyList[Prop] = LazyList.from(0).take((n min max) + 1).map { i => Gen.forAll(g(i))(f) }
      val prop: Prop = props.map { p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) } }.toList.reduce(_ && _)
      prop.run(max, n, rng)
  }
}

object TestingParOps {


  def checkPar(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)

  val es: Gen[ExecutorService] = Gen.unit(Executors.newCachedThreadPool())

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    Gen.forAll(es ** g) { case s ** a => f(a)(s).get() }


  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    ParOps.map2(p, p2)(_ == _)

}

object ** {
  def unapply[A, B](p: (A, B)): Some[(A, B)] = Some(p)
}


object TestingPropertyBasedTesting extends App {
  val smallInt: Gen[Int] = Gen.choose(-10, 10)
  val maxTest = SGen.forAll(SGen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  //  run(maxTest)

  //Ex. 8.14
  val sortedTest = SGen.forAll(SGen.listOf(smallInt)) { ns =>
    val nss = ns.sorted
    nss.isEmpty || nss.size == 1 || nss.zip(nss.tail).forall { case (n1, n2) => n1 <= n2 }
  }

  // run(sortedTest)

  val es = Executors.newCachedThreadPool()
  val parallelComputationTest1 = Gen.forAll(Gen.unit(ParOps.unit(1)))(p =>
    ParOps.map(p)(_ + 1)(es).get() == ParOps.unit(2)(es).get())

  //run(parallelComputationTest1)

  val parTest = Prop.check {
    val p = ParOps.map(ParOps.unit(1))(_ + 1)
    val p2 = ParOps.unit(2)
    p(es).get == p2(es).get()
  }

  // run(parTest)

  val parTest2 = TestingParOps.checkPar {
    TestingParOps.equal(
      ParOps.map(ParOps.unit(1))(_ + 1),
      ParOps.unit(2)
    )
  }

  //  run(parTest2)

  val pint = Gen.choose(0, 10).map(ParOps.unit)
  val p4 = TestingParOps.forAllPar(pint)(n => TestingParOps.equal(ParOps.map(n)(y => y), n))
  // run(p4, 1, 1)

  //Ex. 8.17
  val p5 = TestingParOps.forAllPar(pint)(n => TestingParOps.equal(ParOps.delay(n), n))
  //  run(p5, 1, 1)

  //Ex. 8.18
  val f: Int => Boolean = _ % 2 == 0
  run {
    SGen.forAll(SGen.listOf(smallInt)) { ns =>
      val takenWhile = ns.takeWhile(f)
      val dropWhile = ns.dropWhile(f)
      takenWhile ++ dropWhile == ns
    }
  }
}