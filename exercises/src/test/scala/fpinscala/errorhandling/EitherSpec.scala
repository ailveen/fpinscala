package fpinscala.errorhandling

import org.scalatest.{Matchers, FunSpec}

import scala.{Option => _, Either => _, Left => _, Right => _, _}

class EitherSpec extends FunSpec with Matchers {
  describe("Map") {
    it("should apply f if the result is 'Right'") {
      Either.mean(Array[Double](1, 3, 5, 7, 9)).map(_ + 3) should be (Right(8))
      Either.mean(Array[Double]()) shouldBe a [Left[_]]
    }
  }

  describe("Flat map") {
    it("should convert the value A to an Either[A, B]") {
      Either.mean(Array(1d, 3, 5, 7, 9)).flatMap(v => Right(v * 3)) should be (Right(15))
      Either.mean(Array[Double]()).flatMap(v => Right(v * 3)) shouldBe a [Left[_]]
    }
  }

  describe("Or else") {
    it("returns the first Either if it’s defined; otherwise, it returns the second Either.") {
      Either.mean(Array(1d, 3, 5, 7, 9)).orElse(Right(0.0)) should be (Right(5))
      Either.mean(Array[Double]()).orElse(Right(0.0)) should be (Right(0))
    }
  }

  describe("map2") {
    it("combines two Eithers using a binary function - when one result is Left, the result is as well") {
      Right(3).map2(Right(2))(_ + _) should be (Right(5))
      Right(3).map2(Left(0))(_ + _) shouldBe a [Left[_]]
      Left(0).map2(Right(2))((a, b) => b + a) shouldBe a [Left[_]]
    }
  }

  describe("sequence") {
    it("should combines a list of Eithers into one Either containing a list of all the Right values in the original list; if the original list contains Left even once, the result of the function should be Left") {
      Either.sequence(List(Right(1), Right("choice"), Right("there"))) should be (Right(List(1, "choice", "there")))
      Either.sequence(List(Right(1), Left("notright"), Right("Choice"), Left(2))) should be (Left("notright")) // the first error is expected, not Left(2)
      Either.sequence(List()) should be (Right(Nil))
    }
  }

  describe("traverse") {
    it("should perform map and sequence in one pass") {
      Either.traverse(List("1", "2", "3"))(i => Either.Try(i.toInt)) should be (Right(List(1, 2, 3)))
      Either.traverse(List("1", "a", "3"))(i => Either.Try(i.toInt)) shouldBe a [Left[_]]
    }
  }


}
