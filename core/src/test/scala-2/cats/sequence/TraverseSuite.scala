/*
  Originally Adapted from shapeless-contrib scalaz
 */
package cats.sequence

import cats.data._
import cats.derived._
import org.scalacheck.Prop._
import shapeless._

class TraverseSuite extends KittensSuite {

  def o2e[T](opt: Option[T]): Either[String, T] =
    opt.toRight("Nothing Here")

  def o2v[T](opt: Option[T]): Validated[String, T] =
    Validated.fromOption(opt, "Nothing Here")

  object headOption extends Poly1 {
    implicit def caseSet[T] = at[Set[T]](_.headOption)
  }

  object optionToValidation extends Poly1 {
    implicit def caseOption[T] = at(o2v[T])
  }

  object optionToEither extends Poly1 {
    implicit def caseOption[T] = at(o2e[T])
  }

  property("traversing Set with Set => Option") {
    forAll { (x: Set[Int], y: Set[String], z: Set[Float]) =>
      val expected = (x.headOption, y.headOption, z.headOption).mapN(_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).traverse(headOption) == expected
    }
  }

  property("traversing Option with Option => Validation") {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (o2v(x), o2v(y), o2v(z)).mapN(_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).traverse(optionToValidation) == expected
    }
  }

  property("parallel traversing Option with Option => Either") {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (o2e(x), o2e(y), o2e(z)).parMapN(_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).parTraverse(optionToEither) == expected
    }
  }

  test("traversing HNil") {
    val nil: HNil = HNil
    assertEquals(nil.traverse[Validated[String, *]](optionToValidation), Validated.valid(HNil))
    assertEquals(nil.parTraverse[Either[String, *]](optionToEither), Right(HNil))
  }
}
