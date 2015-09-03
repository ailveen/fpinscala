package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par._
import fpinscala.parallelism._
import fpinscala.state._
import fpinscala.testing.Gen._
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

//trait Prop { self =>
////  def &&(p: Prop): Prop = new Prop {
////    def check: Boolean = self.check && p.check
////  }
////  def check : Boolean
//  def check: Either[(FailedCase,SuccessCount) , SuccessCount]
//}


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def check: Result = ???

  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => this.run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case f @ Falsified(_, _) => f
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => this.run(max, n, rng) match {
      case Falsified(msg, sc) => p.tag(msg).run(max, n, rng)
      case x => x
    }
  }

  def tag(msg: String) : Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        Console println s"! Falsified after $n passed tests:\n $msg"
      case Passed =>
        Console println s"+ OK, passed $testCases tests."
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val ES: ExecutorService = Executors.newCachedThreadPool

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val p3 = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }

  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25)

  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  val p3_ = checkPar {
    equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val pint = Gen.choose(0,10) map Par.unit
  val p4 =
    forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0))((p,i) =>
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

  val forkProp = {
    forAllPar(pint2) { p => equal(Par.fork(p), p)}
  }

}

case class Gen[+A](sample: State[RNG,A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this listOfN n)

  def unsized = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def choose_idiotme(start: Int, stopExclusive: Int): Gen[Int] = {
    val s = (rng : RNG) => {
      val (i, nrng) = rng.nextInt
      if (i >= start && i < stopExclusive) (i, nrng)
      else choose_idiotme(start, stopExclusive).sample.run(nrng)
    }
    Gen(State(s))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen(g andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(g andThen (_ flatMap f))

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))
}

