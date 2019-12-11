/*
  Originally Adapted from shapeless-contrib scalaz
 */
package cats.sequence

import cats.data._
import cats.derived._
import cats.instances.all._
import cats.laws.discipline.SerializableTests
import cats.laws.discipline.arbitrary._
import org.scalacheck.Prop.forAll
import shapeless._
import shapeless.record.Record
import shapeless.syntax.singleton._

class SequenceSuite extends KittensSuite {

  test("sequencing Option")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (x, y, z) mapN (_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).sequence == expected
    }
  })

  test("sequencing HNil with Option")(
    // We can't simply use HNil.sequence, because F would be ambiguous.
    // However, we can explicitly grab the Sequencer for Option and use it.
    check {
      implicitly[Sequencer.Aux[HNil, Option, HNil]].apply(HNil) contains HNil
    })

  test("sequencing Either")(check {
    forAll { (x: Either[String, Int], y: Either[String, String], z: Either[String, Float]) =>
      val expected = (x, y, z) mapN (_ :: _ :: _ :: HNil)
      (x :: y :: z :: HNil).sequence == expected
    }
  })

  test("parallel sequencing Either")(check {
    forAll { (x: Either[String, Int], y: Either[String, String], z: Either[String, Float]) =>
      val expected = (
        Validated.fromEither(x),
        Validated.fromEither(y),
        Validated.fromEither(z)
      ).mapN(_ :: _ :: _ :: HNil).toEither
      (x :: y :: z :: HNil).parSequence == expected
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
    assert(f("42.0") == 4 :: "0.24" :: 42.0 :: HNil)
  }

  test("sequencing Function using ProductArgs"){
    val f1 = (_: String).length
    val f2 = (_: String).reverse
    val f3 = (_: String).toDouble
    val f = sequence(f1, f2, f3)
    assert(f("42.0") == 4 :: "0.24" :: 42.0 :: HNil)
  }

  test("sequencing Klesilis through ProductArgs") {
    val f1 = ((_: String).length) andThen Option.apply
    val f2 = ((_: String).reverse) andThen Option.apply
    val f3 = ((_: String).toDouble) andThen Option.apply
    val f = sequence(Kleisli(f1), Kleisli(f2), Kleisli(f3))
    val expected = Some((Symbol("a") ->> 4) :: (Symbol("b") ->> "0.24") :: (Symbol("c") ->> 42.0) :: HNil)
    assert(f.run("42.0") == expected)
  }

  test("sequencing record of Option")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = for (a <- x; b <- y; c <- z)
        yield (Symbol("a") ->> a) :: (Symbol("b") ->> b) :: (Symbol("c") ->> c) :: HNil
      ((Symbol("a") ->> x) :: (Symbol("b") ->> y) :: (Symbol("c") ->> z) :: HNil).sequence == expected
    }
  })

  test("sequencing record of Option using RecordArgs")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = for (a <- x; b <- y; c <- z)
        yield (Symbol("a") ->> a) :: (Symbol("b") ->> b) :: (Symbol("c") ->> c) :: HNil
      sequenceRecord(a = x, b = y, c = z) == expected
    }
  })

  test("sequencing record of Either")(check {
    forAll { (x: Either[String, Int], y: Either[String, String], z: Either[String, Float]) =>
      val expected = for (a <- x; b <- y; c <- z)
        yield (Symbol("a") ->> a) :: (Symbol("b") ->> b) :: (Symbol("c") ->> c) :: HNil
      ((Symbol("a") ->> x) :: (Symbol("b") ->> y) :: (Symbol("c") ->> z) :: HNil).sequence == expected
    }
  })

  test("sequencing record of Functions through RecordArgs") {
    val f1 = (_: String).length
    val f2 = (_: String).reverse
    val f3 = (_: String).toDouble
    val f = sequenceRecord(a = f1, b = f2, c = f3)
    val expected = (Symbol("a") ->> 4) :: (Symbol("b") ->> "0.24") :: (Symbol("c") ->> 42.0) :: HNil
    assert(f("42.0") == expected)
  }

  test("sequencing record of Kleisli through RecordArgs") {
    val f1 = ((_: String).length) andThen Option.apply
    val f2 = ((_: String).reverse) andThen Option.apply
    val f3 = ((_: String).toDouble) andThen Option.apply
    val f = sequenceRecord(a = Kleisli(f1), b = Kleisli(f2), c = Kleisli(f3))
    val expected = Some((Symbol("a") ->> 4) :: (Symbol("b") ->> "0.24") :: (Symbol("c") ->> 42.0) :: HNil)
    assert(f.run("42.0") == expected)
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
    assert(f("42.0") == MyCase(4, "0.24", 42.0f))
  }

  test("sequence gen for Klesili") {
    val f1 = ((_: String).length) andThen Option.apply
    val f2 = ((_: String).reverse) andThen Option.apply
    val f3 = ((_: String).toFloat) andThen Option.apply
    val myGen = sequenceGeneric[MyCase]
    val f = myGen(a = Kleisli(f1), b = Kleisli(f2), c = Kleisli(f3))
    assert(f.run("42.0") == Some(MyCase(4, "0.24", 42.0f)))
  }

  checkAll("RecordSequencer is Serializable", SerializableTests.serializable(SequenceSuite.recordSequencer))
}

object SequenceSuite {
  val recordSequencer = the[RecordSequencer[Record.`'a -> Option[Int], 'b -> Option[String]`.T]]
}
