package fpinscala.laziness

sealed trait Stream[+A] {

  import Stream._

  def headOption : Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionViaFoldRight : Option[A] = this.foldRight(None : Option[A]) {
    (e, _) => Some(e)
  }

  def toListNonTailRec : List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListNonTailRec
  }

  def toList : List[A] = {
    val buffer = collection.mutable.ListBuffer[A]()

    @annotation.tailrec
    def loop(s : Stream[A]) : List[A] = s match {
      case Empty => buffer.toList
      case Cons(h, t) => buffer += h(); loop(t())
    }

    loop(this)
  }

  def take(n : Int) : Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  def takeViaUnfold(n : Int) : Stream[A] = unfold(this -> n) {
    case (Cons(h, _), 1) => Some(h() -> (Empty -> 0))
    case (Cons(h, t), n) if n > 1 => Some(h() -> (t() -> (n - 1)))
    case _ => None
  }

  def drop(n : Int) : Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop n - 1
    case _ => this
  }

  def takeWhile(f : A => Boolean) : Stream[A] = this match {
    case Cons(h, t) => lazy val eh = h()
                        if (f(eh)) cons(eh, t() takeWhile f) else empty
    case _ => this
  }

  def takeWhileViaUnfold(f : A => Boolean) : Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some(h(), t())
    case _ => None
  }

  def takeWhileViaFoldRight(f : A => Boolean) : Stream[A] = this.foldRight(empty[A]) {
    (e, t) => if (f(e)) cons(e, t) else empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def existsViaFoldRight(f : A => Boolean) : Boolean = this.foldRight(false) {
    (e, t) => f(e) || t
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true) {
    (e, t) => p(e) && t
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def map[B](f : A => B) : Stream[B] = this.foldRight(empty[B]) {
    (e, t) => cons(f(e), t)
  }

  def mapViaUnfold[B](f : A => B) : Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()) -> t())
    case _ => None
  }

  def filter(f : A => Boolean) : Stream[A] = this.foldRight(empty[A]) {
    (e, t) => if (f(e)) cons(e, t) else t
  }

  def append[B >: A](s : => Stream[B]) : Stream[B] =
    s match {
      case Empty => this
      case _ => this.foldRight(s)((e, t) => cons(e, t))
    }

  def appendNotOptimalForEmpty[B >: A](s : => Stream[B]) : Stream[B] = this.foldRight(s) {
    (e, t) => cons(e, t)
  }

  def flatMap[B](f : A => Stream[B]) : Stream[B] = this.foldRight(empty[B]) {
    (e, t) => f(e) append t
  }

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def zipWith[B, C](that : Stream[B])(f : (A, => B) => C) : Stream[C] = unfold(this -> that) {
    case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()) -> (t() -> t2()))
    case _ => None
  }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](that: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this -> that) {
    case (Cons(h, t), Empty) => Some((Some(h()) -> None) -> (t() -> Empty))
    case (Empty, Cons(h, t)) => Some((None -> Some(h())) -> (Empty -> t()))
    case (Cons(h, t), Cons(h2, t2)) => Some( (Some(h()) -> Some(h2()) ) -> (t() -> t2()) )
    case _ => None
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = this.zipAll(s) takeWhile {
    case (_, b) => b.isDefined
  } forAll {
    case (a, b) => a == b
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case a @ Cons(_, t) => Some(a -> t())
    case _ => None
  } append Stream(empty)

  def hasSubsequence[B >: A](s : Stream[B]) : Boolean = tails exists(_ startsWith s)

  def scanRight[B >: A](z : B)(f : (A, => B) => B) : Stream[B] = {
    val (_, res) = this.foldRight((z, Stream(z))) {
      case (e, t) => lazy val (acc, str) = t
        val ir = f(e, acc)
        ir -> cons(ir, str)
    }
    res
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h : () => A, t : () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A] : Stream[A] = Empty

  def cons[A](h : => A, t : => Stream[A]) : Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def apply[A](as : A*) : Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail : _*))
  }

  val ones : Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n : Int) : Stream[Int] = cons(n, from(n + 1))

  def fibs : Stream[Int] = {
    def loop(prev : Int, cur : Int) : Stream[Int] = cons(prev, loop(cur, prev + cur))
    loop(0, 1)
  }

  def fibsViaUnfold : Stream[Int] = unfold(0 -> 1) {
    case (prev, cur) => Some(prev, cur -> (prev + cur))
  }

  def fromViaUnfold(n : Int) : Stream[Int] = unfold(n) {
    v => Some(v -> (v + 1))
  }

  def constantViaUnfold[A](a : A) : Stream[A] = unfold(a) {
    a => Some(a -> a)
  }

  def onesViaUnfold : Stream[Int] = constantViaUnfold(1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((v, s1)) => cons(v, unfold(s1)(f))
    case None => empty
  }
}