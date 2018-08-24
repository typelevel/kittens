package cats
package derived

import cats.derived.TestDefns._
import cats.kernel.laws.discipline.HashTests
import org.scalacheck.Prop.forAll

import scala.util.hashing.Hashing


class HashSuite extends KittensSuite {
  {

    import auto.hash._
    import cats.instances.int._

    checkAll("IList[Int]", HashTests[IList[Int]].hash)
  }
  {

    import auto.hash._
    import cats.instances.all._

    checkAll("Outer", HashTests[Outer].hash)
  }


  test("IList Hash consistent with universal equality")(check {

    import auto.hash._
    import cats.instances.int._

    forAll { (a: IList[Int], b: IList[Int]) =>
      Hash[IList[Int]].eqv(a, b) == (a == b)
    }
  })

  test("derives an instance for Interleaved[T]") {
    assertCompiles("semi.hash[TestDefns.Interleaved[Int]]")
  }

  test("existing Hash instances in scope are respected auto")(check {

    import auto.hash._
    import cats.instances.all._

    forAll { (a: Outer) =>
      val hashWithoutLocalInstance = {
        a.hash
      }
      val hashWithLocalInstance = {
        implicit val hashInner: Hash[Inner] = new Hash[Inner] {
          def hash(x: Inner): Int = 1
          def eqv(x: Inner, y: Inner): Boolean = x == y
        }
        a.hash
      }
      hashWithoutLocalInstance != hashWithLocalInstance
    }

  })
  test("existing Hash instances in scope are respected semi")(check {


    import cats.instances.all._

    forAll { (a: Outer) =>

      val hashWithoutLocalInstance = {
        implicit val hashOuter: Hash[Outer] = derived.semi.hash
        a.hash
      }
      val hashWithLocalInstance = {
        implicit val hashInner: Hash[Inner] = new Hash[Inner] {
          def hash(x: Inner): Int = 1
          def eqv(x: Inner, y: Inner): Boolean = x == y
        }
        implicit val hashOuter: Hash[Outer] = derived.semi.hash
        a.hash
      }
      hashWithoutLocalInstance != hashWithLocalInstance
    }

  })

  //compilation time
  {
    import auto.order._
    import cats.instances.all._
    semi.order[Large4]
  }
}
