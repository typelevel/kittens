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
  import ADTs.*

  inline def empty[A]: A =
    summonInline[Empty[A]].empty

  inline def validate(inline instance: String): Unit =
    test(s"$instance[Foo]")(assert(empty[Foo] == Foo(0, None)))
    test(s"$instance[Outer]")(assert(empty[Outer] == Outer(Inner(0))))
    test(s"$instance[Interleaved[String]]")(assert(empty[Interleaved[String]] == Interleaved.empty("")))
    test(s"$instance[Recursive]")(assert(empty[Recursive] == Recursive(0, None)))
    test(s"$instance[IList[Dummy]]")(assert(empty[IList[Dummy]] == INil()))
    test(s"$instance[Snoc[Dummy]]")(assert(empty[Snoc[Dummy]] == SNil()))
    test(s"$instance respects existing instances")(assert(empty[Box[Mask]] == Box(Mask(0xffffffff))))
    checkAll(s"$instance is Serializable", SerializableTests.serializable(summonInline[Empty[Foo]]))

  locally:
    import auto.empty.given
    validate("auto.empty")
    testNoGiven("Empty[IList[Int]]", "alleycats.Empty")
    testNoGiven("Empty[Snoc[Int]]", "alleycats.Empty")
    testNoGiven("Empty[Rgb]", "alleycats.Empty")

  locally:
    import semiInstances.given
    validate("semiauto.empty")
    testNoInstance("semiauto.empty", "IList[Int]")
    testNoInstance("semiauto.empty", "Snoc[Int]")
    testNoInstance("semiauto.empty", "Rgb")

  locally:
    import strictInstances.given
    validate("strict.semiauto.empty")
    testNoInstance("strict.semiauto.empty", "Rgb")
    testNoInstance("strict.semiauto.empty", "Top")
    test("No strict.semiauto.empty for IList[Int] or Snoc[Int]"):
      testNoGiven("given Empty[IList[Int]] = strict.semiauto.empty", "alleycats.Empty")
      testNoGiven("given Empty[Snoc[Int]] = strict.semiauto.empty", "alleycats.Empty")

  locally:
    import derivedInstances.*
    val instance = "derived.empty"
    test(s"$instance[Foo]")(assert(empty[Foo].x == ADTs.Foo(0, None)))
    test(s"$instance[Outer]")(assert(empty[Outer].x == ADTs.Outer(Inner(0))))
    test(s"$instance[Interleaved[String]]")(assert(empty[Interleaved[String]].x == ADTs.Interleaved.empty("")))
    test(s"$instance[Recursive]")(assert(empty[Recursive].x == ADTs.Recursive(0, None)))
    test(s"$instance[IList[Dummy]]")(assert(empty[IList[Int]].x == INil()))
    test(s"$instance[Snoc[Dummy]]")(assert(empty[Snoc[Int]].x == SNil()))
    test(s"$instance respects existing instances")(assert(empty[BoxMask].x == Box(Mask(0xffffffff))))
    checkAll(s"$instance is Serializable", SerializableTests.serializable(Empty[Foo]))

end EmptySuite

object EmptySuite:
  import ADTs.*

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

  object strictInstances:
    given Empty[Foo] = strict.semiauto.empty
    given Empty[Inner] = strict.semiauto.empty
    given Empty[Outer] = strict.semiauto.empty
    given Empty[Interleaved[String]] = strict.semiauto.empty
    given Empty[Recursive] = strict.semiauto.empty
    given Empty[IList[Dummy]] = strict.semiauto.empty
    given Empty[Snoc[Dummy]] = strict.semiauto.empty
    given Empty[Box[Mask]] = strict.semiauto.empty
    given Empty[Chain] = strict.semiauto.empty

  object derivedInstances:
    case class Foo(x: ADTs.Foo) derives Empty
    case class Outer(x: ADTs.Outer) derives Empty
    case class Interleaved[A](x: ADTs.Interleaved[A]) derives Empty
    case class Recursive(x: ADTs.Recursive) derives Empty
    case class IList[A](x: ADTs.IList[A]) derives Empty
    case class Snoc[A](x: ADTs.Snoc[A]) derives Empty
    case class BoxMask(x: Box[Mask]) derives Empty

  trait Dummy
  final case class Chain(head: Int, tail: Chain)
  final case class Mask(bits: Int)
  object Mask:
    given Empty[Mask] = Empty(Mask(0xffffffff))

end EmptySuite
