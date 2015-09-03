package fpinscala.testing

import org.scalatest.FlatSpec
import Prop._
import Gen._

class ListProps extends FlatSpec {

  "Max of a List" should "be greater than or equal to every other element in the list" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp)
  }

  "A sorted list" should "not have the first element greater than every other element in the list" in {
    val smallInt = Gen.choose(-10, 10)
    val smallestProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      !sorted.exists(sorted.head > _)
    }

    Prop.run(smallestProp)
  }

  "A sorted list" should "not have the last element smaller than every other element in the list" in {
    val smallInt = Gen.choose(-10, 10)
    val smallestProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      val lastElement = sorted.reverse.head
      !sorted.exists(lastElement < _)
    }

    Prop.run(smallestProp)
  }

  "A sorted list" should "be either empty, has one element, " +
    "or has no two consecutive elements `(a,b)` such that `a` is greater than `b` and should have all the elements of the input list " +
    "and should have no elements not in the input list" in {
    val smallInt = Gen.choose(-10, 10)
    val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
      val nss = ns.sorted
      nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
        case (a, b) => a > b
      } && !ns.exists(!nss.contains(_)) && !nss.exists(!ns.contains(_))
    }

    Prop.run(sortedProp)
  }

  "Par.map(Par.unit(1))(_ + 1)" should "equal Par.unit(2)" in {
    Prop.run(Prop.p2)
  }

}
