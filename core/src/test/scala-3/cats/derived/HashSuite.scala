package cats.derived

import cats.Hash
import cats.kernel.laws.discipline.{HashTests, SerializableTests}

import scala.compiletime.*
import scala.util.hashing.MurmurHash3

class HashSuite extends KittensSuite:
  import HashSuite.*
  import TestDefns.*

  inline def hashTests[A]: HashTests[A] =
    HashTests[A](summonInline)

  inline def testHash(inline context: String): Unit =
    checkAll(s"$context.Hash[IList[Int]]", hashTests[IList[Int]].hash)
    checkAll(s"$context.Hash[Inner]", hashTests[Inner].hash)
    checkAll(s"$context.Hash[Outer]", hashTests[Outer].hash)
    // FIXME: typelevel/cats#2878
    // checkAll(s"$context.Hash[Interleaved[Int]]", hashTests[Interleaved[Int]].hash)
    checkAll(s"$context.Hash[Tree[Int]]", hashTests[Tree[Int]].hash)
    // FIXME: Doesn't work for recursive case classes.
    // checkAll(s"$context.Hash[Recursive]", hashTests[Recursive].hash)
    checkAll(s"$context.Hash is Serializable", SerializableTests.serializable(summonInline[Hash[Inner]]))

  locally {
    import auto.hash.given
    testHash("auto")
  }

  locally {
    import semiInstances.given
    testHash("semiauto")
  }

end HashSuite

object HashSuite:
  import TestDefns.*

  object semiInstances:
    given Hash[IList[Int]] = semiauto.hash
    given Hash[Inner] = semiauto.hash
    given Hash[Outer] = semiauto.hash
    given Hash[Interleaved[Int]] = semiauto.hash
    given Hash[Tree[Int]] = semiauto.hash
    given Hash[Recursive] = semiauto.hash

end HashSuite
