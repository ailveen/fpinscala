package fpinscala.gettingstarted

import org.scalatest.FunSpec
import org.scalatest.Matchers

class GettingStartedSpec extends FunSpec with Matchers {


  describe("Fibonacci") {
    it("should get the correct nth Fibonacci number") {
      val nthFib = MyModule.fib(5)
      nthFib should be (3)

      intercept[IllegalArgumentException] {
        MyModule.fib(0)
      }
    }
  }

  describe("Polymorphic Higher-Order Functions") {
    it("should test that the passed in Array is sorted") {
      val res = PolymorphicFunctions.isSorted[Int](Array(1, 2, 3, 4, 3), _ > _)
      res should be (false)

      val res2 = PolymorphicFunctions.isSorted[Int](Array(1, 2, 2, 3, 4, 5), _ > _)
      res2 should be (true)
    }
  }
}
