package cats
package derived

import cats.kernel.laws.discipline.{HashTests, SerializableTests}

class HashSuite extends KittensSuite {
  import HashSuite._
  import TestDefns._

  def testHash(context: String)(implicit
      iList: Hash[IList[Int]],
      inner: Hash[Inner],
      outer: Hash[Outer],
      interleaved: Hash[Interleaved[Int]],
      tree: Hash[Tree[Int]],
      recursive: Hash[Recursive],
      singletons: Hash[Singletons[Int]],
      anyVal: Hash[AnyValStr]
  ): Unit = {
    checkAll(s"$context.Hash[IList[Int]]", HashTests[IList[Int]].hash)
    checkAll(s"$context.Hash[Inner]", HashTests[Inner].hash)
    checkAll(s"$context.Hash[Outer]", HashTests[Outer].hash)
    checkAll(s"$context.Hash[Interleaved[Int]]", HashTests[Interleaved[Int]].hash)
    checkAll(s"$context.Hash[Tree[Int]]", HashTests[Tree[Int]].hash)
    checkAll(s"$context.Hash[Recursive]", HashTests[Recursive].hash)
    checkAll(s"$context.Hash[Singletons[Int]]", HashTests[Singletons[Int]].hash)
    checkAll(s"$context.Hash[AnyValStr]", HashTests[AnyValStr].hash)
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

  {
    import semiInstances._
    testHash("semiauto")
  }
}

object HashSuite {
  import TestDefns._

  object semiInstances {
    implicit val iList: Hash[IList[Int]] = semiauto.hash
    implicit val inner: Hash[Inner] = semiauto.hash
    implicit val outer: Hash[Outer] = semiauto.hash
    implicit val interleaved: Hash[Interleaved[Int]] = semiauto.hash
    implicit val tree: Hash[Tree[Int]] = semiauto.hash
    implicit val recursive: Hash[Recursive] = semiauto.hash
    implicit val singletons: Hash[Singletons[Int]] = semiauto.hash
    implicit val anyVal: Hash[AnyValStr] = semiauto.hash
  }
}
