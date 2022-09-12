/*
 * Copyright (c) 2016 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.derived

import alleycats.Empty
import cats.laws.discipline.SerializableTests

import scala.compiletime.*

class EmptySuite extends KittensSuite:
  import EmptySuite.given
  import EmptySuite.*
  import TestDefns.*

  inline def empty[A]: A =
    summonInline[Empty[A]].empty

  inline def testEmpty(inline context: String): Unit =
    test(s"$context.Empty[Foo]")(assert(empty[Foo] == Foo(0, None)))
    test(s"$context.Empty[Outer]")(assert(empty[Outer] == Outer(Inner(0))))
    test(s"$context.Empty[Interleaved[String]]")(assert(empty[Interleaved[String]] == Interleaved.empty("")))
    test(s"$context.Empty[Recursive]")(assert(empty[Recursive] == Recursive(0, None)))
    test(s"$context.Empty[IList[Dummy]]")(assert(empty[IList[Dummy]] == INil()))
    test(s"$context.Empty[Snoc[Dummy]]")(assert(empty[Snoc[Dummy]] == SNil()))
    test(s"$context.Empty respects existing instances")(assert(empty[Box[Mask]] == Box(Mask(0xffffffff))))
    checkAll(s"$context.Empty is Serializable", SerializableTests.serializable(summonInline[Empty[Foo]]))

  locally {
    import auto.empty.given
    testEmpty("auto")
    testNoAuto("Empty", "IList[Int]")
    testNoAuto("Empty", "Snoc[Int]")
    testNoAuto("Empty", "Rgb")
  }

  locally {
    import semiInstances.given
    testEmpty("semiauto")
    testNoSemi("Empty", "IList[Int]")
    testNoSemi("Empty", "Snoc[Int]")
    testNoSemi("Empty", "Rgb")
  }

end EmptySuite

object EmptySuite:
  import TestDefns.*

  // `Monoid[Option[A]]` gives us `Empty[Option[A]]` but it requires a `Semigroup[A]`.
  given [A]: Empty[Option[A]] = Empty(None)

  object semiInstances:
    given Empty[Foo] = semiauto.empty
    given Empty[Outer] = semiauto.empty
    given Empty[Interleaved[String]] = semiauto.empty
    given Empty[Recursive] = semiauto.empty
    given Empty[IList[Dummy]] = semiauto.empty
    given Empty[Snoc[Dummy]] = semiauto.empty
    given Empty[Box[Mask]] = semiauto.empty
    given Empty[Chain] = semiauto.empty

  trait Dummy
  final case class Chain(head: Int, tail: Chain)
  final case class Mask(bits: Int)
  object Mask:
    given Empty[Mask] = Empty(Mask(0xffffffff))

end EmptySuite
