package cats.derived

import alleycats.*
import cats.*
import cats.derived.*
import cats.laws.discipline.{MonoidKTests, SerializableTests}
import org.scalacheck.Arbitrary

class MonoidKSuite extends KittensSuite {
  import MonoidKSuite.*
  import TestDefns.*

  def testMonoidK(context: String)(implicit
      complexProduct: MonoidK[ComplexProduct],
      caseClassWOption: MonoidK[CaseClassWOption],
      boxMul: MonoidK[BoxMul]
  ): Unit = {
    checkAll(s"$context.MonoidK[ComplexProduct]", MonoidKTests[ComplexProduct].monoidK[Char])
    checkAll(s"$context.MonoidK[CaseClassWOption]", MonoidKTests[CaseClassWOption].monoidK[Char])
    checkAll(s"$context.MonoidK[BoxMul]", MonoidKTests[BoxMul].monoidK[Char])
    checkAll(s"$context.MonoidK is Serializable", SerializableTests.serializable(MonoidK[ComplexProduct]))

    test(s"$context.MonoidK respects existing instances") {
      assert(boxMul.empty[Char] == Box(Mul[Char](1)))
      assert(boxMul.combineK(Box(Mul[Char](5)), Box(Mul[Char](5))) == Box(Mul[Char](25)))
    }
  }

  {
    import auto.monoidK.given
    testMonoidK("auto")
  }

  {
    import semiInstances.given
    testMonoidK("semi")
  }
}

object MonoidKSuite {
  import TestDefns._

  type BoxMul[A] = Box[Mul[A]]

  object semiInstances {
    implicit val complexProduct: MonoidK[ComplexProduct] = semiauto.monoidK
    implicit val caseClassWOption: MonoidK[CaseClassWOption] = semiauto.monoidK
    implicit val boxMul: MonoidK[BoxMul] = semiauto.monoidK
  }

  final case class Mul[T](value: Int)
  object Mul {

    implicit def eqv[T]: Eq[Mul[T]] = Eq.by(_.value)

    implicit def arbitrary[T]: Arbitrary[Mul[T]] =
      Arbitrary(Arbitrary.arbitrary[Int].map(apply))

    implicit val monoidK: MonoidK[Mul] = new MonoidK[Mul] {
      def empty[A] = Mul(1)
      def combineK[A](x: Mul[A], y: Mul[A]) = Mul(x.value * y.value)
    }
  }
}
