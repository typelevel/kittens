/*
  Adapted from shapeless-contrib scalaz
 */
package cats.lift

import cats._
import cats.implicits._
import cats.derived._
import org.scalacheck.Prop.forAll

class LiftTests extends KittensSuite {

  def foo(x: Int, y: String, z: Float) = s"$x - $y - $z"
  val lifted = Applicative[Option].liftA(foo _)

  test("lifting a ternary operation")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val r1 = lifted(x, y, z)
      val expected = x |@| y |@| z map foo
      r1 == expected
    }
  })

}
