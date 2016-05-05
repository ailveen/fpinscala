package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = map(ma)(a => List.fill(n)(a))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = flatMap(ma)(f)

    def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = p.flatMap(ma)(f)

    def unit[A](a: => A): P[A] = p.succeed(a)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State.unit(a)

    def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] =
      ma.flatMap(f)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma flatMap f

    def unit[A](a: => A): Id[A] = Id(a)
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader((r: R) => f(st.run(r)).run(r))
  }
}

