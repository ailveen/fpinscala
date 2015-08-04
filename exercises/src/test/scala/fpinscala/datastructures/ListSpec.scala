package fpinscala.datastructures

import org.scalatest.{FunSpec, Matchers}

class ListSpec extends FunSpec with Matchers {
  describe("Pattern matching") {
    it ("should match the correct branch in") {
      List.x should be (3)
    }
  }

  describe("List tail") {
    it ("should return a new List without the first element") {
      List.tail(List(1, 2, 3)) should be (List(2, 3))

      List.tail(List(1)) should be (Nil)
    }
  }

  describe("List set head") {
    it("the list head should be set to the new value") {
      List.setHead(List(3, 4, 5), 2) should be (List(2, 4, 5))
    }
  }

  describe("List drop") {
    it("should remove the first n elements from the List") {
      List.drop(List(1, 2, 3, 4, 5), 2) should be (List(3, 4, 5))
    }
  }

  describe("List dropwhile") {
    it("should remove all elements that satisfy the predicate") {
      List.dropWhileNonCurried(List(3, 7, 2, 16, 1, 8), (x : Int) => x % 2 != 0) should be (List(2, 16, 1, 8))
      List.dropWhile(List(3, 7, 2, 16, 1, 8))(_ % 2 != 0) should be (List(2, 16, 1, 8))
    }
  }

  describe("List init (drop last element)") {
    it("should drop the last element") {
      List.init(List(1, 2, 3, 4)) should be (List(1, 2, 3))

      List.initNonTailRec(List(1, 2, 3, 4)) should be (List(1, 2, 3))
    }
  }

  describe("List length") {
    it("should return the correct list size") {
      List.length(List(1, 2, 3)) should be (3)

      List.lengthViaFoldLeft(List(1, 2, 3)) should be (3)
    }
  }

  describe("List fold-left") {
    it("should reduce the list correctly to the left") {
      List.foldLeft(List(3, 4, 5, 6), 0)(_ + _) should be (18)

      // (0 - 3) - 4 - 5 - 6
      List.foldLeft(List(3, 4, 5, 6), 0)(_ - _) should be (-18)

      List.foldLeft(List(3, 4, 5, 6), 3)(_ - _) should be (-15)

      List.foldLeftViaFoldRight(List(3, 4, 5, 6), 3)(_ - _) should be (-15)
    }
  }

  describe("List fold-right") {
    it("should reduce the list correctly to the right") {

      // 3 - (4 - (5 - (6 - (3)))) = 3 - (4 - (5 - (3)))
      List.foldRight(List(3, 4, 5, 6), 3)(_ - _) should be (1)

      List.foldRightViaFoldLeft(List(3, 4, 5, 6), 3)(_ - _) should be (1)
    }
  }

  describe("List sum") {
    it("should provide the correct sum") {
      List.sum(List(3, 5, 7)) should be (15)

      List.sumViaFoldLeft(List(3, 5, 7)) should be (15)
    }
  }

  describe("List product") {
    it("should provide the correct product") {
      List.product(List(3, 5, 7)) should be (105)

      List.productViaFoldLeft(List(3, 5, 7)) should be (105)
    }
  }

  describe("List reverse") {
    it("should reverse the list") {
      List.reverse(List(1, 2, 3)) should be (List(3, 2, 1))
    }
  }

  describe("List append") {
    it("should append the second list to the first list parameter") {
      List.append(List(1, 2, 3), List(4, 5)) should be (List(1, 2, 3, 4, 5))

      List.appendViaFold(List(1, 2, 3), List(4, 5)) should be (List(1, 2, 3, 4, 5))
    }
  }

  describe("Concat") {
    it("should transform a list of lists into a single list") {
      List.concat(List(List(1, 2, 3), List(4, 5))) should be (List(1, 2, 3, 4, 5))
    }
  }

  describe("Increment") {
    it("should add 1 to all List[Int] elements") {
      List.increment(List(5, 6, 7)) should be (List(6, 7, 8))
    }
  }

  describe("Double to String") {
    it("should transform a List[Double] into a List[String") {
      List.doubleToString(List(5, 6, 7)) should be (List("5.0", "6.0", "7.0"))
    }
  }

  describe("Map") {
    it("should return a new List with modified elements and a new type") {
      import math._
      List.map(List(1, 2, 3))(pow(_, 2)) should be (List[Double](1, 4, 9))
      List.mapWithInternalMutation(List(1, 2, 3))(pow(_, 2)) should be (List[Double](1, 4, 9))
    }
  }

  describe("Filter") {
    it("should remove elements that satisfy the given predicate") {
      List.filter(List(5, 6, 7, 8))(_ % 2 == 0) should be (List(6, 8))

      List.filterViaFlatMap(List(5, 6, 7, 8))(_ % 2 == 0) should be (List(6, 8))
    }
  }

  describe("List flatMap") {
    it("should do just like map but return a list during mapping") {
      List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1, 1, 2, 2, 3, 3))

      List.flatMap2(List(1,2,3))(i => List(i,i)) should be (List(1, 1, 2, 2, 3, 3))
    }
  }

  describe("Zip Int Add") {
    it("should add corresponding elements two Lists") {
      List.zipAdd(List(1, 2, 3), List(5, 6, 7)) should be (List(6, 8, 10))
      List.zipAdd(List(1, 2), List(5, 6, 7)) should be (List(6, 8))
    }
  }

  describe("Zip with") {
    it("should be able to apply a function to corresponding list elements") {
      List.zipWith(List(1, 2, 3))(List(5, 6, 7))(_ - _) should be (List(-4, -4, -4))
      List.zipWith(List(1, 2))(List("a", "b", "c"))(_ + _) should be (List("1a", "2b"))

      List.zipWith(List(1, 2, 3))(List(5, 6, 7))(_ - _) should be (List(-4, -4, -4))
      List.zipWith(List(1, 2))(List("a", "b", "c"))(_ + _) should be (List("1a", "2b"))
    }
  }

  describe("Has subsequence") {
    it("should test if the given list elements exist in another list") {

      List.hasSubsequence(List(1,2,3,4), List(2, 3)) shouldBe true
      List.hasSubsequence(List(1,2,3,4), List(2, 4)) shouldBe false
      List.hasSubsequence(List(1,2,3,4), List(1, 2)) shouldBe true
      List.hasSubsequence(List(1,2, 2, 3, 4), List(1, 4)) shouldBe false
      List.hasSubsequence(List(1,2, 2, 3, 4), List(2, 3)) shouldBe true
      List.hasSubsequence(List(1,2,3,4), List(5)) shouldBe false
      List.hasSubsequence(List(1,2,3,4), List(4)) shouldBe true
      List.hasSubsequence(List(1,2,3,4), List(4, 5)) shouldBe false
      List.hasSubsequence(List(1, 2), List(1, 2, 3)) shouldBe false
      List.hasSubsequence(List(1, 2), List(1, 2)) shouldBe true
      List.hasSubsequence(List(1,1,1,1,2), List(1,1,1,2)) shouldBe true
      List.hasSubsequence(List(1,1,1,1,2), List()) shouldBe true
      List.hasSubsequence(List(), List()) shouldBe true
      List.hasSubsequence(List(), List(1)) shouldBe false
    }
  }
}
