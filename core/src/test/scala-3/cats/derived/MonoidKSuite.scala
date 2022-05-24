package cats.derived

import alleycats.*
import cats.*
import cats.derived.*
import cats.laws.discipline.{MonoidKTests, SerializableTests}
import org.scalacheck.Arbitrary
import scala.compiletime.*

class MonoidKSuite extends KittensSuite {
  import MonoidKSuite.*
  import TestDefns.*

  inline def monoidKTests[F[_]]: MonoidKTests[F] = MonoidKTests[F](summonInline)

  inline def testMonoidK(context: String): Unit = {
    checkAll(s"$context.MonoidK[ComplexProduct]", monoidKTests[ComplexProduct].monoidK[Char])
    checkAll(s"$context.MonoidK[CaseClassWOption]", monoidKTests[CaseClassWOption].monoidK[Char])
    checkAll(s"$context.MonoidK[BoxMul]", monoidKTests[BoxMul].monoidK[Char])
    checkAll(s"$context.MonoidK is Serializable", SerializableTests.serializable(summonInline[MonoidK[ComplexProduct]]))

    test(s"$context.MonoidK respects existing instances") {
      val M = summonInline[MonoidK[BoxMul]]
      assert(M.empty[Char] == Box(Mul[Char](1)))
      assert(M.combineK(Box(Mul[Char](5)), Box(Mul[Char](5))) == Box(Mul[Char](25)))
    }
  }

  {
    import auto.monoidK.given
    testMonoidK("auto")
  }

  {
    import monInstances.given
    testMonoidK("semi")
  }
}

object MonoidKSuite {
  import TestDefns._

  type BoxMul[A] = Box[Mul[A]]

  object monInstances {
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

  case class Simple[A](value1: List[A], value2: Set[A]) derives MonoidK
  case class Recursive[A](first: List[A], rest: Recursive[A]) derives MonoidK
}
