/*
  Adapted from shapeless-contrib scalaz
 */
package cats.lift

import cats._
import cats.derived._
import org.scalacheck.Prop._

class LiftSuite extends KittensSuite {
  def foo(x: Int, y: String, z: Float) = s"$x - $y - $z"
  val lifted = Applicative[Option].liftA(foo _)

  property("lifting a ternary operation") {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      lifted(x, y, z) == (x, y, z).mapN(foo)
    }
  }
}
