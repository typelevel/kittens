package cats.replicateH

import cats.data._
import cats.derived._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Prop._
import shapeless._

class ReplicateHSuite extends KittensSuite {
  val getAndInc: State[Int, Int] = State(i => (i + 1, i))

  property("replicating state example") {
    val getAndInc5: State[Int, Int :: Int :: Int :: Int :: Int :: HNil] = getAndInc.replicateH(5)
    getAndInc5.run(0).value ?= (5, 0 :: 1 :: 2 :: 3 :: 4 :: HNil)
  }

  property("replicating arbitrary state") {
    forAll { (initial: Int, x: State[Int, String]) =>
      val expected: State[Int, List[String]] = x.replicateA(3)
      val replicated: State[Int, String :: String :: String :: HNil] = x.replicateH(3)
      replicated.map(_.toList).run(initial).value ?= expected.run(initial).value
    }
  }

  property("replicating arbitrary lists") {
    forAll { (l: List[Long]) =>
      val expected = l.replicateA(3)
      val replicated = l.replicateH(3)
      replicated.map(_.toList) ?= expected
    }
  }

  property("replicate 0 list") {
    forAll { (l: List[Long]) =>
      val replicated = l.replicateH(0)
      replicated.map(_.toList) ?= List(Nil)
    }
  }

  property("replicate 1 list") {
    forAll { (l: List[Long]) =>
      val replicated = l.replicateH(1)
      replicated.map(_.toList) ?= l.map(List(_))
    }
  }

  property("replicate 0 state") {
    val getAndInc0: State[Int, HNil] = getAndInc.replicateH(0)
    forAll { (initial: Int) =>
      getAndInc0.run(initial).value ?= (initial, HNil)
    }
  }

  property("replicate 1 state") {
    val getAndInc1: State[Int, Int :: HNil] = getAndInc.replicateH(1)
    forAll { (initial: Int) =>
      getAndInc1.run(initial).value ?= (initial + 1, initial :: HNil)
    }
  }
}
