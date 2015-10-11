package fpinscala.parsing

import fpinscala.parsing.Types._

import scala.util.matching.Regex

class MyParsers[+A]() {

}

trait Result[+A] {

  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e) => Failure(f(e))
    case _ => this
  }
}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]

object Types {
  type Parser[+A] = Location => Result[A]
}

object MyParsers extends Parsers[Parser] {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  def string(s: String): Parser[String] = {
    loc => {
      lazy val input = loc.input.slice(loc.offset, loc.input.length)
      (0 until (input.length max s.length)).find(i => input.lift(i) != s.lift(i)) match {
        case Some(mismatchIndex) => Failure(loc.copy(offset = loc.offset + mismatchIndex).toError(s"Expected $s"))
        // matched
        case None => Success(s, s.length)
      }
    }
  }


  def regex(r: Regex): Parser[String] = ???

  def succeed[A](a: A): Parser[A] = ???

  def slice[A](p: Parser[A]): Parser[String] = ???

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???
}