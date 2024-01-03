package cats.replicateH

import cats.data._
import cats.derived._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Prop._
import shapeless._

class ReplicateHSuite extends KittensSuite {
  val getAndInc: State[Int, Int] = State(i => (i + 1, i))

  test("replicating state example") {
    val replicated: State[Int, Int :: Int :: Int :: Int :: Int :: HNil] = getAndInc.replicateH(5)
    assertEquals(replicated.run(0).value, (5, 0 :: 1 :: 2 :: 3 :: 4 :: HNil))
  }

  property("replicating arbitrary state") {
    forAll { (initial: Int, x: State[Int, String]) =>
      val replicated: State[Int, String :: String :: String :: HNil] = x.replicateH(3)
      replicated.map(_.toList).run(initial).value ?= x.replicateA(3).run(initial).value
    }
  }

  property("replicating arbitrary lists") {
    forAll { (l: List[Long]) =>
      val replicated: List[Long :: Long :: Long :: HNil] = l.replicateH(3)
      replicated.map(_.toList) ?= l.replicateA(3)
    }
  }

  property("replicate 0 list") {
    forAll { (l: List[Long]) =>
      val replicated: List[HNil] = l.replicateH(0)
      replicated.map(_.toList) ?= List(Nil)
    }
  }

  property("replicate 1 list") {
    forAll { (l: List[Long]) =>
      val replicated: List[Long :: HNil] = l.replicateH(1)
      replicated.map(_.toList) ?= l.map(List(_))
    }
  }

  property("replicate 0 state") {
    val replicated: State[Int, HNil] = getAndInc.replicateH(0)
    forAll((initial: Int) => replicated.run(initial).value ?= ((initial, HNil)))
  }

  property("replicate 1 state") {
    val replicated: State[Int, Int :: HNil] = getAndInc.replicateH(1)
    forAll((initial: Int) => replicated.run(initial).value ?= ((initial + 1, initial :: HNil)))
  }

  property("parReplicate Either") {
    forAll { (e: Either[Int, String]) =>
      val replicated: Either[Int, String :: String :: String :: HNil] = e.parReplicateH(3)
      replicated.map(_.toList) ?= e.parReplicateA(3)
    }
  }
}
