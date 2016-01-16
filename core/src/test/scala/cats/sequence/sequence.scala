/*
  Adapted from shapeless-contrib scalaz
 */
package cats.sequence

import cats._
import cats.data._, Xor._
import cats.implicits._
import org.scalacheck.Arbitrary, Arbitrary.arbitrary
import shapeless._, shapeless.syntax.singleton._
import cats.derived._
import org.scalacheck.Prop.forAll

class SequenceTests extends KittensSuite {
  import SequenceTests._

  test("sequencing Option")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (x |@| y |@| z) map (_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).sequence == expected
    }
  })

  test("sequencing Xor")(check {
    forAll { (x: Xor[String, Int], y: Xor[String, String], z: Xor[String, Float]) =>
      val expected = (x |@| y |@| z) map (_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).sequence == expected
    }
  })

  // note: using the ValidationNel type alias here breaks the implicit search
  test("sequencing ValidatedNel")(check {
    forAll { (x: Validated[NonEmptyList[String], Int], y: Validated[NonEmptyList[String], String], z: Validated[NonEmptyList[String], Float]) =>
      val expected = (x |@| y |@| z) map (_ :: _ :: _ :: HNil)
      sequence(x, y, z) == expected
    }
  })

  test("sequencing Function"){
    val f1 = (_: String).length
    val f2 = (_: String).reverse
    val f3 = (_: String).toDouble

    val f = (f1 :: f2 :: f3 :: HNil).sequence
    assert( f("42.0") == 4 :: "0.24" :: 42.0 :: HNil)
  }

  test("sequencing Function using ProductArgs"){
    val f1 = (_: String).length
    val f2 = (_: String).reverse
    val f3 = (_: String).toDouble

    val f = sequence(f1, f2, f3)
    assert( f("42.0") == 4 :: "0.24" :: 42.0 :: HNil )

  }

  test("sequencing record of Option")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = for ( a <- x; b <- y; c <- z ) yield ('a ->> a) :: ('b ->> b) :: ( 'c ->> c) :: HNil
      (('a ->> x) :: ('b ->> y) :: ('c ->> z) :: HNil).sequenceRecord == expected
    }
  })

  test("sequencing record of Option using RecordArgs")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>

      val expected = for ( a <- x; b <- y; c <- z ) yield ('a ->> a) :: ('b ->> b) :: ( 'c ->> c) :: HNil
      sequenceRecord(a = x, b = y, c = z) == expected
    }
  })

  test("sequencing record of Xor")(check {
    forAll { (x: Xor[String, Int], y: Xor[String, String], z: Xor[String, Float]) =>

      val expected = for ( a <- x; b <- y; c <- z ) yield ('a ->> a) :: ('b ->> b) :: ( 'c ->> c) :: HNil
      (('a ->> x) :: ('b ->> y) :: ('c ->> z) :: HNil).sequenceRecord == expected
    }
  })

  test("sequencing record of Functions through RecordArgs") {
    val f1 = (_: String).length
    val f2 = (_: String).reverse
    val f3 = (_: String).toDouble

    val f = sequenceRecord(a = f1, b = f2, c = f3)
    assert( f("42.0") == ('a ->> 4) :: ('b ->> "0.24") :: ('c ->> 42.0) :: HNil )
  }

  case class MyCase(a: Int, b: String, c: Float)

  test("sequence gen for Option")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val myGen = sequenceGeneric[MyCase]
      val expected = (x |@| y |@| z) map MyCase.apply

      myGen(a = x, b = y, c = z) == expected
    }
  })

  test("sequence gen for Xor")(check {
    forAll { (x: Xor[String, Int], y: Xor[String, String], z: Xor[String, Float]) =>
      val myGen = sequenceGeneric[MyCase]
      val expected = (x |@| y |@| z) map MyCase.apply

      myGen(a = x, b = y, c = z) == expected
    }
  })

  test("sequence gen for Functions") {
    val f1 = (_: String).length
    val f2 = (_: String).reverse
    val f3 = (_: String).toFloat

    val myGen = sequenceGeneric[MyCase]
    val f = myGen(a = f1, b = f2, c = f3)

    assert( f("42.0") == MyCase(4, "0.24", 42.0f))
  }


}

object SequenceTests {
  implicit def nelSemiGroup[A]: Semigroup[NonEmptyList[A]] = implicitly[SemigroupK[NonEmptyList]].algebra[A]

  implicit def xora[A: Arbitrary, B: Arbitrary]: Arbitrary[Xor[A, B]] = Arbitrary(
    for {
      a <- arbitrary[A]
      left <- arbitrary[Boolean]
      b <- arbitrary[B]
    } yield if(left) Left(a) else Right(b)
  )

  implicit def validatedNel[A: Arbitrary, B: Arbitrary]: Arbitrary[ValidatedNel[A, B]] = Arbitrary(
    for {
      a <- arbitrary[A]
      as <- arbitrary[List[A]]
      left <- arbitrary[Boolean]
      b <- arbitrary[B]
    } yield if(left) Validated.Invalid(NonEmptyList(a, as:_*)) else Validated.valid(b)
  )
}
