package fpinscala.monoids

import fpinscala.monoids.Monoid._
import fpinscala.testing.Prop._
import fpinscala.testing.Gen

object MonoidSpec extends App {

  private val smallInt: Gen[Int] = Gen.choose(-50, 50)
  run(monoidLaws(smallInt)(intAddition), "Int Addition")

  run(monoidLaws(smallInt)(intMultiplication), "Int Multiplication")

  run(monoidLaws(Gen.boolean)(booleanOr), "Boolean Or")

  run(monoidLaws(Gen.boolean)(booleanAnd), "Boolean And")

  private val optionGen: Gen[Option[Int]] = Gen.boolean ** smallInt map {
    case (b, i) => if (b) Some(i) else None
  }
  run(monoidLaws(optionGen)(optionMonoid), "Option Monoid")

  // fails due to function equality thingy
  run(monoidLaws(smallInt map (i => (a : Int) => i + a))(endoMonoid), "EndoMonoid")
}
