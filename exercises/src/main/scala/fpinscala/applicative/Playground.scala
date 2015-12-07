package fpinscala.applicative

import fpinscala.applicative.Applicative.Const
import fpinscala.monads.Functor
import fpinscala.monoids._
import fpinscala.state.State._
import fpinscala.state._

object Playground extends App {

  private val monoidApplicative = Applicative.monoidApplicative(Monoid.optionMonoid)

  Console println monoidApplicative.unit(1d)
}

object Playground2 extends App {
  val listTraverse = Traverse.listTraverse

  Console println listTraverse.zipWithIndex(List(1, 2, 3))
}