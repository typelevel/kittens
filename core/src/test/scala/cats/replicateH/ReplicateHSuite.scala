package cats.replicateH

import cats.data._
import cats.instances.all._
import cats.laws.discipline.arbitrary._

import shapeless._
import cats.derived._
import org.scalacheck.Prop._


class ReplicateHSuite extends KittensSuite {

  test("replicating state example")(
    check {
      val getAndInc: State[Int, Int] = State { i => (i + 1, i)}
      val getAndInc5: State[Int, Int :: Int :: Int :: Int :: Int :: HNil] = getAndInc.replicateH(5)
      getAndInc5.run(0).value ?= (5, 0 :: 1 :: 2 :: 3 :: 4 :: HNil)
    })

  test("replicating arbitrary state")(check {
    forAll { (initial: Int, x: State[Int, String]) =>
      val expected: State[Int, List[String]] = x.replicateA(3)
      val replicated: State[Int, String :: String :: String :: HNil] = x.replicateH(3)
      replicated.map(_.toList).run(initial).value ?= expected.run(initial).value
    }
  })

  test("replicating arbitrary lists")(check {
    forAll { (l: List[Long]) =>
      val expected = l.replicateA(3)
      val replicated = l.replicateH(3)
      replicated.map(_.toList) ?= expected
    }
  })

}
