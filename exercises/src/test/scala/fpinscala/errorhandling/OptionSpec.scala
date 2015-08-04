package fpinscala.errorhandling

import org.scalatest.{Matchers, FunSpec}

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

class OptionSpec extends FunSpec with Matchers {

  describe("Map") {
    it("should apply f if the option is not None") {
      Option.mean(List(1, 3, 5, 7, 9)).map(_ + 3) should be (Some(8))
      Option.mean(List[Double]()) should be (None)
    }
  }

  describe("Get or else") {
    it("should return the value if an instance of Some and the default if an instance of none") {
      Some("A").getOrElse("B") should be ("A")
      None.getOrElse("B") should be ("B")
    }
  }

  describe("Flat map") {
    it("should convert the value A to an Option[B]") {
      Some("A").flatMap(v => Some(v * 3)) should be (Some("AAA"))
      Option.mean(List(1, 3, 5, 7, 9)).flatMap(Some(_)) should be (Some(5))
      Option.mean(List[Double]()).flatMap(v => Some(v * 3)) should be (None)
    }
  }

  describe("Or else") {
    it("returns the first Option if it’s defined; otherwise, it returns the second Option.") {
      Option.mean(List(1, 3, 5, 7, 9)).orElse(Some(0)) should be (Some(5))
      Option.mean(List()).orElse(Some(0)) should be (Some(0))
    }
  }

  describe("Filter") {
    it("converts Some to None if the value does not satisfy the function") {
      Option.mean(List(1, 3, 5, 7, 9)).filter(v => v % 2 == 0) should be (None)
      Option.mean(List(2, 4, 6, 8, 10)).filter(v => v % 2 == 0) should be (Some(6))
    }
  }

  describe("variance") {
    it("determines the mean of math.pow(x - m, 2) for each element x in the sequence, where m is the mean of the sequence") {
      Option.variance(List(1, 3, 5, 7, 9)) should be (Some(8))
      Option.variance(List()) should be (None)
    }
  }

  describe("map2") {
    it("combines two Options using a binary function - when one Option is None, the result is as well") {
      Option.map2(Some(3), Some(2))(_ + _) should be (Some(5))
      Option.map2(Some(3), None)(_ + _) should be (None)
      Option.map2(None, Some(2))((a, b) => b + a) should be (None)
    }
  }

  describe("sequence") {
    it("should combines a list of Options into one Option containing a list of all the Some values in the original list; if the original list contains None even once, the result of the function should be None") {
      Option.sequence(List(Some(1), Some("body"), Some("times"))) should be (Some(List(1, "body", "times")))
      Option.sequenceViaTraverse(List(Some(1), Some("body"), Some("times"))) should be (Some(List(1, "body", "times")))
      Option.sequence(List(Some(1), Some("body"), None)) should be (None)
      Option.sequence(List()) should be (Some(Nil))
    }
  }

  describe("traverse") {
    it("should perform map and sequence in one pass") {
      Option.traverse(List("1", "2", "3"))(i => Option.Try(i.toInt)) should be (Some(List(1, 2, 3)))
      Option.traverse(List("1", "a", "3"))(i => Option.Try(i.toInt)) should be (None)
    }
  }
}
