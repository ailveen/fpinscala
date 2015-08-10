package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(acc : List[Int], count : Int)(rng : RNG) : (List[Int], RNG) = {
      if (count > 0) {
        val (nextInt, nextRNG) = rng.nextInt
        loop(nextInt :: acc, count - 1)(nextRNG)
      } else (acc, rng)
    }
    loop(List.empty[Int], count)(rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  val _double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) {
      (ra, acc) => map2(ra, acc) {
        _ :: _
      }
    }

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThanRaw(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThanRaw(n)(rng2)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def __map[S,A,B](s: S => (A,S))(f: A => B): S => (B,S) =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) {
      a => map(rb) {
        b => f(a, b)
      }
    }
}

import State._

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State {
      s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      }
    }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a : A) : State[S, A] = State(s => (a, s))

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty[A])) {
      (sa, sb) => sa.map2(sb) {
        _ :: _
      }
    }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def updateState(input: Input) : Machine => Machine = {
      (incomingState : Machine) =>
        (input, incomingState) match {
          case (Coin, Machine(true, candies, coins)) if candies > 0 => Machine(locked = false, candies, coins + 1)
          case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
          case (Coin, m @ Machine(false, _, _)) => m
          case (Turn, m @ Machine(true, _, _)) => m
          case (_, m) => m
        }
    }

    for {
      _ <- sequence(inputs.map {
        modify[Machine] _ compose updateState
      })
      s <- get
    } yield (s.candies, s.coins)
  }

  def simulateMachineWithAnnotationsForIdiotsLikeMe(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def updateState(input: Input) : Machine => Machine = {
      // Since we have a Return type of State[Machine, [int, Int]), then it follows that we have an implicit
      // argument of Machine representing S in State[S, A] or in its function form S => (A, S), hence, we call
      // it "incomingState"
      (incomingState : Machine) =>
        (input, incomingState) match {
          case (Coin, Machine(true, candies, coins)) if candies > 0 => Machine(locked = false, candies, coins + 1)
          case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
          case (Coin, m @ Machine(false, _, _)) => m
          case (Turn, m @ Machine(true, _, _)) => m
          case (_, m) => m
        }
    }

    for {
        // Short explanation: *underscore* since we don't need the unit value.
        // Long explanation: The return type of this sequence is State[Machine, List[Unit]] -
        // since this is an implicit flatMap, the underscore represents the List[Unit], which we don't need.
        // What we need is "Machine", representing the State, so read on.
      _ <- sequence(inputs.map {
        // Read this as modify(updateState(input)). "input" comes from the list.map fnxn.
        // The output of updatestate is a Machine => Machine type, which the modify function accepts.
        modify[Machine] _ compose updateState
      })
      // Get the implicit argument Machine based on the result of the sequence execution above.
      // Remember, state is a function of s => (a, s) - we're just getting the s which is the resulting
      // "Machine" from the sequence execution above.
      s <- get
    } yield (s.candies, s.coins)
  }
}
