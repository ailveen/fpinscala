package fpinscala.datastructures

import org.scalatest.{Matchers, FunSpec}

class TreeSpec extends FunSpec with Matchers {

  describe("Size") {
    it ("should count all branches and leaves of a tree") {
      Tree.size(Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(3)), Leaf(5)))) should be (7)
      Tree.size(Leaf(2)) should be (1)
      Tree.sizeViaFold(Branch(Leaf(1), Branch(Branch(Leaf(1), Leaf(3)), Leaf(5)))) should be (7)
    }
  }

  describe("Maximum") {
    it ("should return the maximum value amongst all (leaf) nodes") {
      Tree.maximum(Branch(Leaf(11), Branch(Branch(Leaf(133), Leaf(37)), Leaf(58)))) should be (133)
      Tree.maximum(Leaf(2)) should be (2)
      Tree.maximumViaFold(Branch(Leaf(11), Branch(Branch(Leaf(133), Leaf(37)), Leaf(58)))) should be (133)
    }
  }

  describe("Depth") {
    it ("should return the max path length from the root to any leaf") {
      Tree.depth(Branch(Leaf(11), Branch(Branch(Leaf(133), Leaf(37)), Leaf(58)))) should be (4)
      Tree.depth(Leaf(2)) should be (1)
      Tree.depthViaFold(Branch(Leaf(11), Branch(Branch(Leaf(133), Leaf(37)), Leaf(58)))) should be (4)
    }
  }

  describe("Map") {
    it ("should modify the value of a leaf given the function") {
      Tree.map(Branch(Leaf(11), Branch(Branch(Leaf(133), Leaf(37)), Leaf(92))))(_ + 8) should be (Branch(Leaf(19), Branch(Branch(Leaf(141), Leaf(45)), Leaf(100))))
      Tree.map(Leaf(2))(_ + 8) should be (Leaf(10))
      Tree.mapViaFold(Branch(Leaf(11), Branch(Branch(Leaf(133), Leaf(37)), Leaf(92))))(_ + 8) should be (Branch(Leaf(19), Branch(Branch(Leaf(141), Leaf(45)), Leaf(100))))
    }
  }
}
