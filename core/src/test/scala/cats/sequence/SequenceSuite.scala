/*
  Originally Adapted from shapeless-contrib scalaz
 */
package cats.sequence

import cats.data._
import cats.instances.option._
import cats.instances.either._
import cats.instances.function._
import org.scalacheck.Arbitrary
import shapeless._, shapeless.syntax.singleton._
import cats.derived._
import org.scalacheck.Prop.forAll
import shapeless.record.Record
import cats.laws.discipline.arbitrary._

class SequenceSuite extends KittensSuite {

  test("sequencing Option")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (x, y, z) mapN (_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).sequence == expected
    }
  })

  test("sequencing Either")(check {
    forAll { (x: Either[String, Int], y: Either[String, String], z: Either[String, Float]) =>
      val expected = (x, y, z) mapN (_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).sequence == expected
    }
  })

  // note: using the ValidationNel type alias here breaks the implicit search
  test("sequencing ValidatedNel")(check {
    forAll { (x: Validated[NonEmptyList[String], Int], y: Validated[NonEmptyList[String], String], z: Validated[NonEmptyList[String], Float]) =>
      val expected = (x, y, z) mapN (_ :: _ :: _ :: HNil)
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

  test("sequencing Klesilis through ProductArgs") {
    val f1 = ((_: String).length) andThen Option.apply
    val f2 = ((_: String).reverse) andThen Option.apply
    val f3 = ((_: String).toDouble) andThen Option.apply

    val f = sequence(Kleisli(f1), Kleisli(f2), Kleisli(f3))
    assert( f.run("42.0") == Some(('a ->> 4) :: ('b ->> "0.24") :: ('c ->> 42.0) :: HNil))
  }

  test("sequencing record of Option")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = for ( a <- x; b <- y; c <- z ) yield ('a ->> a) :: ('b ->> b) :: ( 'c ->> c) :: HNil
      (('a ->> x) :: ('b ->> y) :: ('c ->> z) :: HNil).sequence == expected
    }
  })

  test("sequencing record of Option using RecordArgs")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>

      val expected = for ( a <- x; b <- y; c <- z ) yield ('a ->> a) :: ('b ->> b) :: ( 'c ->> c) :: HNil
      sequenceRecord(a = x, b = y, c = z) == expected
    }
  })

  test("sequencing record of Either")(check {
    forAll { (x: Either[String, Int], y: Either[String, String], z: Either[String, Float]) =>

      val expected = for ( a <- x; b <- y; c <- z ) yield ('a ->> a) :: ('b ->> b) :: ( 'c ->> c) :: HNil
      (('a ->> x) :: ('b ->> y) :: ('c ->> z) :: HNil).sequence == expected
    }
  })

  test("sequencing record of Functions through RecordArgs") {
    val f1 = (_: String).length
    val f2 = (_: String).reverse
    val f3 = (_: String).toDouble

    val f = sequenceRecord(a = f1, b = f2, c = f3)
    assert( f("42.0") == ('a ->> 4) :: ('b ->> "0.24") :: ('c ->> 42.0) :: HNil )
  }

  test("sequencing record of Kleisli through RecordArgs") {
    val f1 = ((_: String).length) andThen Option.apply
    val f2 = ((_: String).reverse) andThen Option.apply
    val f3 = ((_: String).toDouble) andThen Option.apply

    val f = sequenceRecord(a = Kleisli(f1), b = Kleisli(f2), c = Kleisli(f3))
    assert( f.run("42.0") == Some(('a ->> 4) :: ('b ->> "0.24") :: ('c ->> 42.0) :: HNil))
  }

  case class MyCase(a: Int, b: String, c: Float)

  test("sequence gen for Option")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val myGen = sequenceGeneric[MyCase]
      val expected = (x, y, z) mapN MyCase.apply

      myGen(a = x, b = y, c = z) == expected
    }
  })

  test("sequence gen with different sort")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val myGen = sequenceGeneric[MyCase]
      val expected = (x, y, z) mapN MyCase.apply

      myGen(b = y, a = x, c = z) == expected
    }
  })

  test("sequence gen for Either")(check {
    forAll { (x: Either[String, Int], y: Either[String, String], z: Either[String, Float]) =>
      val myGen = sequenceGeneric[MyCase]
      val expected = (x, y, z) mapN MyCase.apply

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

  test("sequence gen for Klesili") {
    val f1 = ((_: String).length) andThen Option.apply
    val f2 = ((_: String).reverse) andThen Option.apply
    val f3 = ((_: String).toFloat) andThen Option.apply

    val myGen = sequenceGeneric[MyCase]
    val f = myGen(a = Kleisli(f1), b = Kleisli(f2), c = Kleisli(f3))

    assert( f.run("42.0") == Some(MyCase(4, "0.24", 42.0f)))
  }

  //wait until cats 0.5.0 release to bring unapply to serializable
  test("RecordSequencer is serializable") {
    import java.io.{ ObjectOutputStream, ByteArrayOutputStream }
    val r = Record.`'a -> Option[Int], 'b -> Option[String]`
    type Rec = r.T
    val rs = the[RecordSequencer[Rec]]
    assert( isSerializable(rs) )
  }

}
