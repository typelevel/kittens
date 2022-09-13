package cats.derived

import alleycats.*
import cats.*
import cats.laws.discipline.{MonoidKTests, SerializableTests}
import org.scalacheck.Arbitrary
import scala.compiletime.*

class MonoidKSuite extends KittensSuite:
  import MonoidKSuite.*
  import TestDefns.*

  inline def tests[F[_]]: MonoidKTests[F] =
    MonoidKTests[F](summonInline)

  inline def validate(instance: String): Unit =
    checkAll(s"$instance[ComplexProduct]", tests[ComplexProduct].monoidK[Char])
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].monoidK[Char])
    checkAll(s"$instance[BoxMul]", tests[BoxMul].monoidK[Char])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[MonoidK[ComplexProduct]]))
    test(s"$instance respects existing instances") {
      val M = summonInline[MonoidK[BoxMul]]
      assert(M.empty[Char] == Box(Mul[Char](1)))
      assert(M.combineK(Box(Mul[Char](5)), Box(Mul[Char](5))) == Box(Mul[Char](25)))
    }

  locally {
    import auto.monoidK.given
    validate("auto.monoidK")
  }

  locally {
    import semiMonoidK.given
    validate("semiauto.monoidK")
  }

  locally {
    import derivedMonoidK.*
    val instance = "derived.monoidK"
    checkAll(s"$instance[ComplexProduct]", tests[ComplexProduct].monoidK[Char])
    checkAll(s"$instance[CaseClassWOption]", tests[CaseClassWOption].monoidK[Char])
    checkAll(s"$instance[Simple]", tests[Simple].monoidK[Char])
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[MonoidK[ComplexProduct]]))
  }

end MonoidKSuite

object MonoidKSuite:
  import TestDefns.*

  type BoxMul[A] = Box[Mul[A]]

  object semiMonoidK:
    given MonoidK[ComplexProduct] = semiauto.monoidK
    given MonoidK[CaseClassWOption] = semiauto.monoidK
    given MonoidK[BoxMul] = semiauto.monoidK

  object derivedMonoidK:
    case class ComplexProduct[A](x: TestDefns.ComplexProduct[A]) derives MonoidK
    case class CaseClassWOption[A](x: TestDefns.CaseClassWOption[A]) derives MonoidK
    case class Simple[A](value1: List[A], value2: Set[A]) derives MonoidK
    case class Recursive[A](first: List[A], rest: Recursive[A]) derives MonoidK

  final case class Mul[T](value: Int)
  object Mul:
    given [T]: Eq[Mul[T]] = Eq.by(_.value)
    given [T]: Arbitrary[Mul[T]] = Arbitrary(Arbitrary.arbitrary[Int].map(apply))
    given MonoidK[Mul] with
      def empty[A] = Mul(1)
      def combineK[A](x: Mul[A], y: Mul[A]) = Mul(x.value * y.value)

end MonoidKSuite
