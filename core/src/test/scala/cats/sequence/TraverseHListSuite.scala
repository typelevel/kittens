/*
  Originally Adapted from shapeless-contrib scalaz
 */
package cats.sequence

import cats.data._
import cats.derived._
import org.scalacheck.Prop._
import shapeless._

class TraverseHListSuite extends KittensSuite {

  def optToValidation[T](opt: Option[T]): Validated[String, T] =
    Validated.fromOption(opt, "Nothing Here")

  object headOption extends Poly1 {
    implicit def caseSet[T] = at[Set[T]](_.headOption)
  }

  object optionToValidation extends Poly1 {
    implicit def caseOption[T] = at[Option[T]](optToValidation)
  }

  property("traversing Set with Set => Option") {
    forAll { (x: Set[Int], y: Set[String], z: Set[Float]) =>
      val expected = (x.headOption, y.headOption, z.headOption).mapN(_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).traverse(headOption) == expected
    }
  }

  property("traversing Option with Option => Validation") {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (optToValidation(x), optToValidation(y), optToValidation(z)).mapN(_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).traverse(optionToValidation) == expected
    }
  }
}
