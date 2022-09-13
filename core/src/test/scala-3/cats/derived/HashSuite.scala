package cats.derived

import cats.Hash
import cats.kernel.laws.discipline.*
import scala.compiletime.*
import scala.util.hashing.MurmurHash3

class HashSuite extends KittensSuite:
  import HashSuite.*
  import ADTs.*

  inline def tests[A]: HashTests[A] =
    HashTests[A](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[IList[Int]]", tests[IList[Int]].hash)
    checkAll(s"$instance[Inner]", tests[Inner].hash)
    checkAll(s"$instance[Outer]", tests[Outer].hash)
    // FIXME: typelevel/cats#2878
    // checkAll(s"$instance[Interleaved[Int]]", tests[Interleaved[Int]].hash)
    checkAll(s"$instance[Tree[Int]]", tests[Tree[Int]].hash)
    checkAll(s"$instance[Recursive]", tests[Recursive].hash)
    checkAll(s"$instance[EnumK0]", tests[EnumK0].hash)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Hash[Inner]]))

  locally {
    import auto.hash.given
    validate("auto.hash")
  }

  locally {
    import semiInstances.given
    validate("semiauto.hash")
  }

  locally {
    import derivedInstances.*
    val instance = "derived.hash"
    checkAll(s"$instance[IList[Int]]", tests[IList[Int]].hash)
    checkAll(s"$instance[Inner]", tests[Inner].hash)
    checkAll(s"$instance[Outer]", tests[Outer].hash)
    // FIXME: typelevel/cats#2878
    // checkAll(s"$context[Interleaved[Int]]", tests[Interleaved[Int]].hash)
    checkAll(s"$instance[Tree[Int]]", tests[Tree[Int]].hash)
    checkAll(s"$instance[Recursive]", tests[Recursive].hash)
    checkAll(s"$instance[EnumK0]", tests[EnumK0].hash)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Hash[Inner]))
  }

end HashSuite

object HashSuite:
  import ADTs.*

  object semiInstances:
    given Hash[IList[Int]] = semiauto.hash
    given Hash[Inner] = semiauto.hash
    given Hash[Outer] = semiauto.hash
    given Hash[Interleaved[Int]] = semiauto.hash
    given Hash[Tree[Int]] = semiauto.hash
    given Hash[Recursive] = semiauto.hash
    given Hash[EnumK0] = semiauto.hash

  object derivedInstances:
    case class IList[A](x: ADTs.IList[A]) derives Hash
    case class Inner(x: ADTs.Inner) derives Hash
    case class Outer(x: ADTs.Outer) derives Hash
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Hash
    case class Tree[A](x: ADTs.Tree[A]) derives Hash
    case class Recursive(x: ADTs.Recursive) derives Hash
    case class EnumK0(x: ADTs.EnumK0) derives Hash

end HashSuite
