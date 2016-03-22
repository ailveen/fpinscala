package fpinscala.monoids

import fpinscala.monoids.Monoid._
import fpinscala.testing.Gen
import fpinscala.testing.Prop._

object MonoidHomomorphismSpec extends App {

  run(homoMorphismProp(Gen.string.g(100), Monoid.stringMonoid, Monoid.intAddition)(s => s.length))

  val stringLength : String => Int = s => s.length

  run(isoMorphismProp(Gen.boolean, Gen.boolean, Monoid.booleanOr, Monoid.booleanAnd)(!_, !_))

  run(isoMorphismProp(Gen.string.g(100), Gen.string.g(100).map(_.toList), Monoid.stringMonoid, Monoid.listMonoid[Char])(a => a.toList, b => b.mkString))

  // expected to fail, non-isomorphic
  run(isoMorphismProp(Gen.string.g(100), Gen.boolean, Monoid.stringMonoid, Monoid.booleanOr)(a => if (a isEmpty) false else true, b => String.valueOf(b)))
}


