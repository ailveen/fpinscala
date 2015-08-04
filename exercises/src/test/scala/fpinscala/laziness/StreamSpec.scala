package fpinscala.laziness

import org.scalatest.{Matchers, FunSpec}

class StreamSpec extends FunSpec with Matchers {

  describe("toList") {
    it("should convert a Stream to a List") {
      Stream(1, 2, 3).toListNonTailRec should be (List(1, 2, 3))
      Stream.empty.toListNonTailRec should be (Nil)

      Stream(1, 2, 3).toList should be (List(1, 2, 3))
      Stream.empty.toList should be (Nil)
    }
  }

  describe("take") {
    it("should take the first n elements from a Stream") {
      Stream(1, 2, 3, 4, 5, 6).take(3).toList should be (Stream(1, 2, 3).toList)
      Stream(1, 2, 3, 4, 5, 6).take(7).toList should be (Stream(1, 2, 3, 4, 5, 6).toList)
    }

    it("should take the first n elements from a Stream via Unfold") {
      Stream(1, 2, 3, 4, 5, 6).takeViaUnfold(3).toList should be (Stream(1, 2, 3).toList)
      Stream(1, 2, 3, 4, 5, 6).takeViaUnfold(7).toList should be (Stream(1, 2, 3, 4, 5, 6).toList)
      Stream(1, 2, 3, 4, 5, 6).takeViaUnfold(6).toList should be (Stream(1, 2, 3, 4, 5, 6).toList)
      Stream(1, 2, 3, 4, 5, 6).takeViaUnfold(5).toList should be (Stream(1, 2, 3, 4, 5).toList)
      Stream(1, 2, 3, 4, 5, 6).takeViaUnfold(0) should be (Stream.empty)
    }
  }

  describe("drop") {
    it("should drop the first n elements from a Stream") {
      Stream(1, 2, 3, 4, 5, 6).drop(2).toList should be (Stream(3, 4, 5, 6).toList)
    }
  }

  describe("takeWhile") {
    it("should take elements while the supplied function evaluates to true") {
      Stream(1, 2, 3, 4, 5).takeWhile(_ / 2d <= 1d).toList should be (Stream(1, 2) toList)
      Stream(1, 2, 3, 4, 5).takeWhile(_ % 8d == 0) should be (Stream.empty)
      Stream[Int]().takeWhile(_ / 2d <= 1d) should be (Stream.empty)

      Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ / 2d <= 1d).toList should be (Stream(1, 2) toList)
      Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ % 8d == 0) should be (Stream.empty)
      Stream[Int]().takeWhileViaFoldRight(_ / 2d <= 1d) should be (Stream.empty)
    }

    it("should take elements while the supplied function evaluates to true via Unfold") {
      Stream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ / 2d <= 1d).toList should be (Stream(1, 2) toList)
      Stream(1, 2, 3, 4, 5).takeWhileViaUnfold(_ % 8d == 0) should be (Stream.empty)
      Stream[Int]().takeWhileViaUnfold(_ / 2d <= 1d) should be (Stream.empty)

    }
  }

  describe("forall") {
    it("should test if all elements satisfy the given function") {
      Stream(11, 21, 33, 44, 55).forAll(_ % 11 == 0) should be (false)
      Stream(10, 15, 25, 35).forAll(_ % 5 == 0) should be (true)

      Stream[Int]().forAll(_ > 5) should be (true)
    }
  }

  describe("head option") {
    it("should return the head as on Option") {
      Stream(11, 12).headOption should be (Some(11))
      Stream().headOption should be (None)

      Stream(11, 12).headOptionViaFoldRight shouldEqual Some(11)
      Stream().headOptionViaFoldRight should be (None)
    }
  }

  describe("map") {
    it("should transform apply the function to each element") {
      Stream(81, 83, 85).map(_ + 2).toList should be (Stream(83, 85, 87) toList)
    }

    it ("should map but via unfold") {
      Stream(81, 83, 85).mapViaUnfold(_ + 2).toList should be (Stream(83, 85, 87) toList)
    }
  }

  describe("filter") {
    it("should filter out all elements that does not satisfy the function") {
      Stream(1, 2, 3, 4, 5).filter(_ % 2 != 0).toList should be (Stream(1, 3, 5) toList)
    }
  }

  describe("append") {
    it("should append the given Stream to the existing Stream") {
      Stream(1, 2, 3).append(Stream(4, 5, 6)).toList should be (Stream(1, 2, 3, 4, 5, 6) toList)
      Stream(1, 2, 3).append(Stream()).toList should be (Stream(1, 2, 3) toList)
      Stream().append(Stream(1, 2, 3)).toList should be (Stream(1, 2, 3) toList)
    }
  }

  describe("flatMap") {
    it("should do just like map but return a stream during mapping") {
      Stream(1,2,3).flatMap(i => Stream(i,i)).toList should be (Stream(1, 1, 2, 2, 3, 3) toList)
    }
  }

  describe("Constant") {
    it("should return an infinite stream of a given value") {
      Stream.constant(8).take(5).toList should be (List(8, 8, 8, 8, 8))
    }

    it("should return an infinite stream of a given value in terms of unfold") {
      Stream.constantViaUnfold(8).take(5).toList should be (List(8, 8, 8, 8, 8))
    }
  }

  describe("ones") {
    it("should return an infinite stream of int 1s via unfold") {
      Stream.onesViaUnfold.take(5).toList should be (List(1, 1, 1, 1, 1))
    }
  }

  describe("from") {
    it("should generate an infinite stream of integers starting from the specified") {
      Stream.from(5).take(10).toList should be (5 to 14 toList)
    }

    it("should generate an infinite stream of integers starting from the specified via Unfold") {
      Stream.fromViaUnfold(5).take(10).toList should be (5 to 14 toList)
    }
  }

  describe("fibs") {
    it("should generste an infinite steam of fibonacci numbers") {
      Stream.fibs.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
    }

    it("should generate an infinite stream of fibonacci numbers via unfold") {
      Stream.fibsViaUnfold.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
    }
  }

  describe("unfold") {
    it("should return an infinite fib sequence via unfold") {
      Stream.unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
    }
  }

  describe("Zip with") {
    it("should combine corresponding elements of the list") {
      Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ - _).toList shouldBe List(-3, -3, -3)
      Stream(1, 2, 3).zipWith(Stream(4, 5))(_ - _).toList shouldBe List(-3, -3)
    }
  }

  describe("Zip all") {
    it("should zip all elements of two streams using book solution") {

      Stream(1, 2).zipAll(Stream(3, 4)).toList shouldBe List((Some(1), Some(3)), (Some(2),Some(4)))
      Stream(1, 2).zipAll(Stream(3)).toList shouldBe List((Some(1), Some(3)), (Some(2),None))
      Stream().zipAll(Stream()).toList shouldBe Nil
    }
  }

  describe("starts with") {
    it("should test if the given stream is a substring using the book solution") {
      Stream(1, 2, 3, 4).startsWith(Stream(1, 2)) shouldBe true
      Stream(1, 2, 3, 4).startsWith(Stream(1, 3)) shouldBe false
      Stream(1, 2, 3, 4).startsWith(Stream(1, 4)) shouldBe false
      Stream(1, 2, 3, 4).startsWith(Stream(2, 4)) shouldBe false
      Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4)) shouldBe true
      Stream(1, 2, 3, 4).startsWith(Stream.empty) shouldBe true
      Stream().startsWith(Stream(1, 2)) shouldBe false
      Stream().startsWith(Stream()) shouldBe true

    }
  }

  describe("tails") {
    it("should return the Stream of suffixes of the input sequence, starting with the original Stream") {
      Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), Nil)
    }
  }

  describe("scanRight") {
    it("should apply the operation to the intermediate results") {
      Stream(1,2,3).scanRight(0)(_ + _).toList shouldBe List(6,5,3,0)
    }
  }
}
