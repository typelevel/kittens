package cats
package derived

import cats.kernel.laws.discipline.{HashTests, SerializableTests}
import cats.instances.all._

class HashSuite extends KittensSuite {
  import TestDefns._

  def testHash(context: String)(
    implicit iList: Hash[IList[Int]],
    inner: Hash[Inner],
    outer: Hash[Outer],
    interleaved: Hash[Interleaved[Int]],
    tree: Hash[Tree[Int]],
    recursive: Hash[Recursive]
  ): Unit = {
    checkAll(s"$context.Hash[IList[Int]]", HashTests[IList[Int]].hash)
    checkAll(s"$context.Hash[Inner]", HashTests[Inner].hash)
    checkAll(s"$context.Hash[Outer]", HashTests[Outer].hash)
    // FIXME: typelevel/cats#2878
    // checkAll(s"$context.Hash[Interleaved[Int]]", HashTests[Interleaved[Int]].hash)
    checkAll(s"$context.Hash[Tree[Int]]", HashTests[Tree[Int]].hash)
    checkAll(s"$context.Hash[Recursive]", HashTests[Recursive].hash)
    checkAll(s"$context.Hash is Serializable", SerializableTests.serializable(Hash[Tree[Int]]))
  }

  {
    import auto.hash._
    testHash("auto")
  }

  {
    import cached.hash._
    testHash("cached")
  }

  semiTests.run()

  object semiTests {
    implicit val iList: Hash[IList[Int]] = semi.hash
    implicit val inner: Hash[Inner] = semi.hash
    implicit val outer: Hash[Outer] = semi.hash
    implicit val interleaved: Hash[Interleaved[Int]] = semi.hash
    implicit val tree: Hash[Tree[Int]] = semi.hash
    implicit val recursive: Hash[Recursive] = semi.hash
    def run(): Unit = testHash("semi")
  }
}
