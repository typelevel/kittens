/*
  Originally Adapted from shapeless-contrib scalaz
 */
package cats.sequence

import cats.data._
import cats.derived._
import cats.kernel.Semigroup
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

  test("sequencing Semigroup"){
    val si = Semigroup[Int]
    val ss = Semigroup[String]
    val sis = (si :: ss :: HNil).sequence
    check {
      forAll {(i1: Int, s1: String, i2: Int, s2: String) =>
        sis.combine(i1 :: s1 :: HNil, i2 :: s2 :: HNil) == si.combine(i1, i2) :: ss.combine(s1, s2) :: HNil
      }
    }
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
        yield ("a" ->> a) :: ("b" ->> b) :: ("c" ->> c) :: HNil
      (("a" ->> x) :: ("b" ->> y) :: ("c" ->> z) :: HNil).sequence == expected
    }
  })

  test("sequencing record of Option using RecordArgs")(check {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = for (a <- x; b <- y; c <- z)
        yield ("a" ->> a) :: ("b" ->> b) :: ("c" ->> c) :: HNil
      sequenceRecord(a = x, b = y, c = z) == expected
    }
  })

  test("sequencing record of Either")(check {
    forAll { (x: Either[String, Int], y: Either[String, String], z: Either[String, Float]) =>
      val expected = for (a <- x; b <- y; c <- z)
        yield ("a" ->> a) :: ("b" ->> b) :: ("c" ->> c) :: HNil
      (("a" ->> x) :: ("b" ->> y) :: ("c" ->> z) :: HNil).sequence == expected
    }
  })

  test("sequencing record of Functions through RecordArgs") {
    val f1 = (_: String).length
    val f2 = (_: String).reverse
    val f3 = (_: String).toDouble
    val f = sequenceRecord(a = f1, b = f2, c = f3)
    val expected = ("a" ->> 4) :: ("b" ->> "0.24") :: ("c" ->> 42.0) :: HNil
    assert(f("42.0") == expected)
  }

  test("sequencing record of Kleisli through RecordArgs") {
    val f1 = ((_: String).length) andThen Option.apply
    val f2 = ((_: String).reverse) andThen Option.apply
    val f3 = ((_: String).toDouble) andThen Option.apply
    val f = sequenceRecord(a = Kleisli(f1), b = Kleisli(f2), c = Kleisli(f3))
    val expected = Some(("a" ->> 4) :: ("b" ->> "0.24") :: ("c" ->> 42.0) :: HNil)
    assert(f.run("42.0") == expected)
  }

  test("sequencing record of Semigroup through RecordArgs"){
    val si = Semigroup[Int]
    val ss = Semigroup[String]
    val sis = sequenceRecord(i = si, s = ss)
    check {
      forAll {(i1: Int, s1: String, i2: Int, s2: String) =>
        val rec1 = Record(i = i1, s = s1)
        val rec2 = Record(i = i2, s = s2)
        sis.combine(rec1, rec2) == ("i" ->> si.combine(i1, i2)) :: ("s" ->> ss.combine(s1, s2)) :: HNil
      }
    }
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

  test("sequencing gen for Semigroup"){
    val si = Semigroup[Int]
    val ss = Semigroup[String]
    val sf = Semigroup[Float]
    val myGen = sequenceGeneric[MyCase]
    val sm = myGen(a = si, b = ss, c = sf)
    check {
      forAll {(i1: Int, s1: String, f1: Float, i2: Int, s2: String, f2: Float) =>
        sm.combine(MyCase(i1, s1, f1), MyCase(i2, s2, f2)) == MyCase(si.combine(i1, i2), ss.combine(s1, s2), sf.combine(f1, f2))
      }
    }
  }

  checkAll("RecordSequencer is Serializable", SerializableTests.serializable(SequenceSuite.recordSequencer))
}

object SequenceSuite {
  val recordSequencer = the[RecordSequencer[Record.`"a" -> Option[Int], "b" -> Option[String]`.T]]
}
