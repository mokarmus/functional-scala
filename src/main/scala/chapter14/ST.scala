package org.okarmus
package chapter14

sealed trait ST[S, A] {
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, newS) = self.run(s)
      (f(a), newS)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, newS) = self.run(s)
      f(a).run(newS)
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A = st.apply[Unit].run(())._1

}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell: A = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}


object StMainProgram extends App {

  val p = new RunnableST[(Int, Int)] {
    def apply[S] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }

  val result = ST.runST(p)

  println(result)

}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {

  protected def value: Array[A]
  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  //Ex 14.1
  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())){case ((key, value), st) => st.flatMap(_ => write(key, value)) }


  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(i, x)
  } yield ()

}

object STArray {

  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = ST(
    new STArray[S, A]() {
      override protected def value: Array[A] = Array.fill(sz)(v)
    }
  )

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A]() {
    override protected def value: Array[A] = xs.toArray
  })


  //Ex. 14.2
  def noop[S] = ST[S,Unit](())
  def partition[S](a: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = for {
    vp <- a.read(pivot)
    _ <- a.swap(pivot, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop[S])((s, i) => for {
      _ <- s
      vi <- a.read(i)
      _  <- if (vi < vp) (for {
        vj <- j.read
        _  <- a.swap(i, vj)
        _  <- j.write(vj + 1)
      } yield ()) else noop[S]
    } yield ())
    x <- j.read
    _ <- a.swap(x, r)
  } yield x

  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
    pi <- partition(a, l, r, l + (r - l) / 2)
    _ <- qs(a, l, pi - 1)
    _ <- qs(a, pi + 1, r)
  } yield () else noop[S]

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr    <- STArray.fromList(xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })

}

//TODO implement 14.3 one day !! !! !! !!