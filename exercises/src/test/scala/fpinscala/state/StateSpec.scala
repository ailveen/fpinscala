package fpinscala.state

import org.scalatest.{Matchers, FunSpec}

class StateSpec extends FunSpec with Matchers {

  describe("""State machine - Candy dispenser""") {
    it( """it should have a correct initial state""") {

      val ((candies, coins), machine) = CandyDispenser.simulateMachine(Nil).run(Machine(locked = true, 100, 0))
      candies shouldBe 100
      coins shouldBe 0
      machine.locked shouldBe true
    }

    it( """should still be locked when knob is turned""") {
      val ((candies, coins), machine) = CandyDispenser.simulateMachine(Turn :: Nil).run(Machine(locked = true, 100, 0))

      candies shouldBe 100
      coins shouldBe 0
      machine.locked shouldBe true
    }

    it( """should be unlocked when 1 coin is dropped""") {
      val ((candies, coins), machine) = CandyDispenser.simulateMachine(Coin :: Nil).run(Machine(locked = true, 100, 0))

      candies shouldBe 100
      coins shouldBe 1
      machine.locked shouldBe false
    }

    it( """should still be unlocked when another coin is dropped but that coin is returned (no change of state)""") {
      val ((candies, coins), machine) = CandyDispenser.simulateMachine(Coin :: Nil).run(Machine(locked = false, 100, 1))

      candies shouldBe 100
      coins shouldBe 1
      machine.locked shouldBe false
    }

    it( """should dispense candy and become locked when the knob is turned""") {
      val ((candies, coins), machine) = CandyDispenser.simulateMachine(Turn :: Nil).run(Machine(locked = false, 100, 1))

      candies shouldBe 99
      coins shouldBe 1
      machine.locked shouldBe true
    }

    it( """should be locked when all candies are exhausted by dropping coins and turning the knob""") {
      val _99coins = List.fill(99)(Coin)
      val _99turns = List.fill(99)(Turn)
      val _99interspersedCoinsAndTurns = _99coins.zip(_99turns) flatMap {
        case (coin, turn) => List(coin, turn)
      }
      val ((candies, coins), machine) = CandyDispenser.simulateMachine(_99interspersedCoinsAndTurns).run(Machine(locked = true, 99, 1))

      candies shouldBe 0
      coins shouldBe 100
      machine.locked shouldBe true
    }

    it( """should not change state anymore if given a coin since there are no more candies""") {
      val ((candies, coins), machine) = CandyDispenser.simulateMachine(Turn :: Coin :: Nil).run(Machine(locked = true, 0, 100))

      candies shouldBe 0
      coins shouldBe 100
      machine.locked shouldBe true
    }
  }

}
