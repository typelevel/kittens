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

import scala.annotation.nowarn

class SequenceSuite extends KittensSuite {
  import SequenceSuite._

  type ShortErr[A] = Either[Int, A]
  type AccumErr[A] = Validated[Int, A]

  // We can't simply use HNil.sequence, because F would be ambiguous.
  // However, we can explicitly grab the Sequencer for Option and use it.
  test("sequencing HNil") {
    assert(implicitly[Sequencer.Aux[Option, HNil, HNil]].apply(HNil).contains(HNil))
    @nowarn("cat=deprecation")
    val stream = implicitly[Sequencer.Aux[Stream, HNil, HNil]].parApply(HNil)
    assertEquals(stream(42), HNil)
  }

  property("sequencing Option") {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (x, y, z).mapN(_ :: _ :: _ :: HNil)
      sequence(x, y, z) == expected && (x :: y :: z :: HNil).sequence == expected
    }
  }

  property("sequencing Either") {
    forAll { (x: ShortErr[Int], y: ShortErr[String], z: ShortErr[Float]) =>
      val expected = (x, y, z).mapN(_ :: _ :: _ :: HNil)
      sequence(x, y, z) == expected && (x :: y :: z :: HNil).sequence == expected
    }
  }

  property("parallel sequencing Either") {
    forAll { (x: ShortErr[Int], y: ShortErr[String], z: ShortErr[Float]) =>
      val expected = (x, y, z).parMapN(_ :: _ :: _ :: HNil)
      parSequence(x, y, z) == expected && (x :: y :: z :: HNil).parSequence == expected
    }
  }

  property("parallel sequencing Stream") {
    @nowarn("cat=deprecation")
    val prop = forAll { (x: Stream[Int], y: Stream[String], z: Stream[Float]) =>
      val expected = (x, y, z).parMapN(_ :: _ :: _ :: HNil)
      parSequence(x, y, z) == expected && (x :: y :: z :: HNil).parSequence == expected
    }

    prop
  }

  property("sequencing Validated") {
    forAll { (x: AccumErr[Int], y: AccumErr[String], z: AccumErr[Float]) =>
      val expected = (x, y, z).mapN(_ :: _ :: _ :: HNil)
      sequence(x, y, z) == expected && (x :: y :: z :: HNil).sequence == expected
    }
  }

  test("sequencing Function") {
    val x = (_: String).length
    val y = (_: String).reverse
    val z = (_: String).toFloat
    val f = sequence(x, y, z)
    val g = (x :: y :: z :: HNil).sequence
    val expected = 4 :: "0.24" :: 42.0 :: HNil
    assert(f("42.0") == expected)
    assert(g("42.0") == expected)
  }

  property("sequencing Semigroup") {
    val sg1 = sequence(Semigroup[Int], Semigroup[String], Semigroup[Float])
    val sg2 = (Semigroup[Int] :: Semigroup[String] :: Semigroup[Float] :: HNil).sequence
    forAll { (i1: Int, s1: String, f1: Float, i2: Int, s2: String, f2: Float) =>
      val x = i1 :: s1 :: f1 :: HNil
      val y = i2 :: s2 :: f2 :: HNil
      val expected = (i1 + i2) :: (s1 + s2) :: (f1 + f2) :: HNil
      sg1.combine(x, y) == expected && sg2.combine(x, y) == expected
    }
  }

  test("sequencing Kleisli") {
    val x = ((_: String).length).andThen(Option.apply)
    val y = ((_: String).reverse).andThen(Option.apply)
    val z = ((_: String).toFloat).andThen(Option.apply)
    val f = sequence(Kleisli(x), Kleisli(y), Kleisli(z))
    val g = (Kleisli(x) :: Kleisli(y) :: Kleisli(z) :: HNil).sequence
    val expected = Some(4 :: "0.24" :: 42.0 :: HNil)
    assert(f("42.0") == expected)
    assert(g("42.0") == expected)
  }

  property("sequencing record of Option") {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (x, y, z).mapN((a, b, c) => Record(a = a, b = b, c = c))
      sequenceNamed(a = x, b = y, c = z) == expected &&
      Record(a = x, b = y, c = z).sequence == expected
    }
  }

  property("sequencing record of Either") {
    forAll { (x: ShortErr[Int], y: ShortErr[String], z: ShortErr[Float]) =>
      val expected = (x, y, z).mapN((a, b, c) => Record(a = a, b = b, c = c))
      sequenceNamed(a = x, b = y, c = z) == expected &&
      Record(a = x, b = y, c = z).sequence == expected
    }
  }

  property("parallel sequencing record of Either") {
    forAll { (x: ShortErr[Int], y: ShortErr[String], z: ShortErr[Float]) =>
      val expected = (x, y, z).parMapN((a, b, c) => Record(a = a, b = b, c = c))
      parSequenceNamed(a = x, b = y, c = z) == expected &&
      Record(a = x, b = y, c = z).parSequence == expected
    }
  }

  property("parallel sequencing record of Stream") {
    @nowarn("cat=deprecation")
    val prop = forAll { (x: Stream[Int], y: Stream[String], z: Stream[Float]) =>
      val expected = (x, y, z).parMapN((a, b, c) => Record(a = a, b = b, c = c))
      parSequenceNamed(a = x, b = y, c = z) == expected &&
      Record(a = x, b = y, c = z).parSequence == expected
    }

    prop
  }

  property("sequencing record of Validated") {
    forAll { (x: AccumErr[Int], y: AccumErr[String], z: AccumErr[Float]) =>
      val expected = (x, y, z).mapN((a, b, c) => Record(a = a, b = b, c = c))
      sequenceNamed(a = x, b = y, c = z) == expected &&
      Record(a = x, b = y, c = z).sequence == expected
    }
  }

  test("sequencing record of Function") {
    val x = (_: String).length
    val y = (_: String).reverse
    val z = (_: String).toFloat
    val f = sequenceNamed(a = x, b = y, c = z)
    val g = Record(a = x, b = y, c = z).sequence
    val expected = Record(a = 4, b = "0.24", c = 42.0)
    assert(f("42.0") == expected)
    assert(g("42.0") == expected)
  }

  test("sequencing record of Kleisli") {
    val x = ((_: String).length).andThen(Option.apply)
    val y = ((_: String).reverse).andThen(Option.apply)
    val z = ((_: String).toFloat).andThen(Option.apply)
    val f = sequenceNamed(a = Kleisli(x), b = Kleisli(y), c = Kleisli(z))
    val g = Record(a = Kleisli(x), b = Kleisli(y), c = Kleisli(z)).sequence
    val expected = Some(Record(a = 4, b = "0.24", c = 42.0))
    assert(f("42.0") == expected)
    assert(g("42.0") == expected)
  }

  property("sequencing record of Semigroup") {
    val sg1 = sequenceNamed(i = Semigroup[Int], s = Semigroup[String], f = Semigroup[Float])
    val sg2 = Record(i = Semigroup[Int], s = Semigroup[String], f = Semigroup[Float]).sequence
    forAll { (i1: Int, s1: String, f1: Float, i2: Int, s2: String, f2: Float) =>
      val x = Record(i = i1, s = s1, f = f1)
      val y = Record(i = i2, s = s2, f = f2)
      val expected = Record(i = i1 + i2, s = s1 + s2, f = f1 + f2)
      sg1.combine(x, y) == expected && sg2.combine(x, y) == expected
    }
  }

  property("sequencing to case class of Option") {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (x, y, z).mapN(MyCase)
      sequenceTo[MyCase].apply(x, y, z) == expected &&
      sequenceTo[MyCase].named(a = x, b = y, c = z) == expected &&
      (x :: y :: z :: HNil).sequenceTo[MyCase] == expected &&
      Record(a = x, b = y, c = z).sequenceTo[MyCase] == expected
    }
  }

  property("sequencing to case class in different order") {
    forAll { (x: Option[Int], y: Option[String], z: Option[Float]) =>
      val expected = (x, y, z).mapN(MyCase)
      sequenceTo[MyCase].named(b = y, a = x, c = z) == expected &&
      Record(b = y, a = x, c = z).sequenceTo[MyCase] == expected
    }
  }

  property("sequencing to case class of Either") {
    forAll { (x: ShortErr[Int], y: ShortErr[String], z: ShortErr[Float]) =>
      val expected = (x, y, z).mapN(MyCase)
      sequenceTo[MyCase].apply(x, y, z) == expected &&
      sequenceTo[MyCase].named(a = x, b = y, c = z) == expected &&
      (x :: y :: z :: HNil).sequenceTo[MyCase] == expected &&
      Record(a = x, b = y, c = z).sequenceTo[MyCase] == expected
    }
  }

  property("parallel sequencing to case class of Either") {
    forAll { (x: ShortErr[Int], y: ShortErr[String], z: ShortErr[Float]) =>
      val expected = (x, y, z).parMapN(MyCase)
      sequenceTo[MyCase].par(x, y, z) == expected &&
      sequenceTo[MyCase].parNamed(a = x, b = y, c = z) == expected &&
      (x :: y :: z :: HNil).parSequenceTo[ShortErr, MyCase] == expected &&
      Record(a = x, b = y, c = z).parSequenceTo[ShortErr, MyCase] == expected
    }
  }

  property("parallel sequencing to case class of Stream") {
    @nowarn("cat=deprecation")
    val prop = forAll { (x: Stream[Int], y: Stream[String], z: Stream[Float]) =>
      val expected = (x, y, z).parMapN(MyCase)
      sequenceTo[MyCase].par(x, y, z) == expected &&
      sequenceTo[MyCase].parNamed(a = x, b = y, c = z) == expected &&
      (x :: y :: z :: HNil).parSequenceTo[Stream, MyCase] == expected &&
      Record(a = x, b = y, c = z).parSequenceTo[Stream, MyCase] == expected
    }

    prop
  }

  property("sequencing to case class of Validated") {
    forAll { (x: AccumErr[Int], y: AccumErr[String], z: AccumErr[Float]) =>
      val expected = (x, y, z).mapN(MyCase)
      sequenceTo[MyCase].apply(x, y, z) == expected &&
      sequenceTo[MyCase].named(a = x, b = y, c = z) == expected &&
      (x :: y :: z :: HNil).sequenceTo[MyCase] == expected &&
      Record(a = x, b = y, c = z).sequenceTo[MyCase] == expected
    }
  }

  test("sequencing to case class of Function") {
    val x = (_: String).length
    val y = (_: String).reverse
    val z = (_: String).toFloat
    val f = sequenceTo[MyCase].apply(x, y, z)
    val g = sequenceTo[MyCase].named(a = x, b = y, c = z)
    val h = (x :: y :: z :: HNil).sequenceTo[MyCase]
    val i = Record(a = x, b = y, c = z).sequenceTo[MyCase]
    val expected = MyCase(4, "0.24", 42.0f)
    assert(f("42.0") == expected)
    assert(g("42.0") == expected)
    assert(h("42.0") == expected)
    assert(i("42.0") == expected)
  }

  test("sequencing to case class of Klesili") {
    val x = ((_: String).length).andThen(Option.apply)
    val y = ((_: String).reverse).andThen(Option.apply)
    val z = ((_: String).toFloat).andThen(Option.apply)
    val f = sequenceTo[MyCase].apply(Kleisli(x), Kleisli(y), Kleisli(z))
    val g = sequenceTo[MyCase].named(a = Kleisli(x), b = Kleisli(y), c = Kleisli(z))
    val h = (Kleisli(x) :: Kleisli(y) :: Kleisli(z) :: HNil).sequenceTo[MyCase]
    val i = Record(a = Kleisli(x), b = Kleisli(y), c = Kleisli(z)).sequenceTo[MyCase]
    val expected = Some(MyCase(4, "0.24", 42.0f))
    assert(f("42.0") == expected)
    assert(g("42.0") == expected)
    assert(h("42.0") == expected)
    assert(i("42.0") == expected)
  }

  property("sequencing to case class of Semigroup") {
    val sg1 = sequenceTo[MyCase].apply(Semigroup[Int], Semigroup[String], Semigroup[Float])
    val sg2 = sequenceTo[MyCase].named(a = Semigroup[Int], b = Semigroup[String], c = Semigroup[Float])
    val sg3 = (Semigroup[Int] :: Semigroup[String] :: Semigroup[Float] :: HNil).sequenceTo[MyCase]
    val sg4 = Record(a = Semigroup[Int], b = Semigroup[String], c = Semigroup[Float]).sequenceTo[MyCase]
    forAll { (i1: Int, s1: String, f1: Float, i2: Int, s2: String, f2: Float) =>
      val x = MyCase(i1, s1, f1)
      val y = MyCase(i2, s2, f2)
      val expected = MyCase(i1 + i2, s1 + s2, f1 + f2)
      sg1.combine(x, y) == expected && sg2.combine(x, y) == expected &&
      sg3.combine(x, y) == expected && sg4.combine(x, y) == expected
    }
  }

  checkAll("Sequencer is Serializable", SerializableTests.serializable(Sequencer[MyCaseOpt]))
  checkAll("GenericSequencer is Serializable", SerializableTests.serializable(GenericSequencer[MyCase, MyCaseOpt]))
}

object SequenceSuite {
  final case class MyCase(a: Int, b: String, c: Float)
  type MyCaseOpt = Option[Int] :: Option[String] :: Option[Float] :: HNil
}
