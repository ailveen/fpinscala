package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => sys.error("passed an empty list")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => sys.error("passed an empty list")
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case Nil => Nil
    case _ => l
  }

  @annotation.tailrec
  def dropWhileNonCurried[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhileNonCurried(t, f)
    case _ => l
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def initNonTailRec[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def loop(l: List[A], buf : ListBuffer[A]) : List[A] = l match {
      case Cons(_, Nil) => List(buf.toList : _*)
      case Cons(h, t) => buf += h; loop(t, buf)
      case _ => l
    }
    loop(l, buf)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a, b) => f(b, a))

  def sumViaFoldLeft(l : List[Int]) : Int = foldLeft(l, 0)((sum, e) => sum + e)

  def productViaFoldLeft(l : List[Int]) : Int = foldLeft(l, 1)((prod, e) => prod * e)

  def lengthViaFoldLeft(l : List[Int]) : Int = foldLeft(l, 0)((ln, _) => ln + 1)

  def reverse[A](l : List[A]) = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = foldRightViaFoldLeft(a1, a2)((a, b) => Cons(a, b))

  def concat[A](l : List[List[A]]) : List[A] = foldRightViaFoldLeft(l, List[A]())((e, agg) => append(e, agg))

  // List(List(1, 2, 3), List(4, 5))
  // foldRight(List(List(1, 2, 3), List(4, 5)), List())(append)
  // append(List(1,2,3), foldRight(List(List(4, 5), List()))(append)
  // append(List(1,2,3), append(List(4, 5), foldRight(List(List())))(append)
  // append(List(1,2,3), append(List(4, 5), List()))

  // List(List(1, 2, 3), List(4, 5))
  // foldRight(List(List(1, 2, 3), List(4, 5)), List())(append(b, a))
  // append(foldRight(List(List(4, 5)), List()), List(1, 2, 3))(append(b, a))
  // append(foldRight(List(List())), append(List(4,5), List(1, 2, 3)))(append(b,a))
  // append(List(), append(List(4,5), List(1, 2, 3)))

  def increment(l : List[Int]) : List[Int] = foldRightViaFoldLeft(l, Nil : List[Int])((e, agg) => Cons(e + 1, agg))

  def doubleToString(l: List[Double]) : List[String] = foldRightViaFoldLeft(l, Nil : List[String])((e, agg) => Cons(e toString, agg))

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(as, Nil : List[B])((a, b) => Cons(f(a), b))

  def mapWithInternalMutation[A,B](as: List[A])(f: A => B): List[B] = {
    import scala.collection.mutable.ListBuffer
    val buf = ListBuffer[B]()
    @annotation.tailrec
    def loop(as : List[A]): List[B] = {
      as match {
        case Cons(h, t) => buf += f(h); loop(t)
        case Nil => List(buf.toList : _*)
      }
    }
    loop(as)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRightViaFoldLeft(as, Nil : List[A])((el, agg) => if (f(el)) Cons(el, agg) else agg)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = foldRightViaFoldLeft(as, Nil : List[B])((el, agg) => append(f(el), agg))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(el => if (f(el)) List(el) else Nil)

  def zipAdd(l1 : List[Int], l2: List[Int]) : List[Int] = (l1, l2) match {
    case (Cons(h, t), Cons(h2, t2)) => Cons(h + h2, zipAdd(t, t2))
    case (_, Nil) => Nil
    case (Nil, _) => Nil
  }

  def zipWith[A, B, C](l1 : List[A])(l2: List[B])(f : (A, B) => C) : List[C] = (l1, l2) match {
    case (Cons(h, t), Cons(h2, t2)) => Cons(f(h, h2), zipWith(t)(t2)(f))
    case (_, Nil) => Nil
    case (Nil, _) => Nil
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (_, Nil) => true
    case (_, _) if startsWith(l, sub) => true
    case (Cons(_,t), _) => hasSubsequence(t, sub)
  }
}

