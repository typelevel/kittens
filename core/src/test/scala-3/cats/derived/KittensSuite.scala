/*
 * Copyright (c) 2015 Miles Sabin
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

import cats.{Eq, Order}
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.platform.Platform
import cats.syntax.AllSyntax
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Test.Parameters

import scala.annotation.unused
import scala.compiletime.summonInline
import scala.concurrent.duration.*
import scala.deriving.Mirror
import scala.quoted.*

/** An opinionated stack of traits to improve consistency and reduce boilerplate in Kittens tests. Note that unlike the
  * corresponding CatsSuite in the Cat project, this trait does not mix in any instances.
  */
abstract class KittensSuite extends KittensSuite.WithoutEq, ADTs.EqInstances:
  // Some tolerance for numeric underflow.
  given Order[Double] = (x, y) => if (x / y - 1).abs < 1e-9 then 0 else x.compareTo(y)
  @unused given [A <: Singleton: ValueOf]: Eq[A] = Eq.allEqual
  given [A <: Product](using mirror: Mirror.ProductOf[A], via: Eq[mirror.MirroredElemTypes]): Eq[A] =
    Eq.by(Tuple.fromProductTyped)

object KittensSuite:
  def deCapitalizeMacro(str: Expr[String])(using Quotes) =
    val value = str.valueOrAbort
    Expr(if value.isEmpty then "" else value.head.toLower +: value.tail)

  inline def deCapitalize(inline str: String): String =
    ${ deCapitalizeMacro('str) }

  /** Used to test `Eq` derivation. */
  abstract class WithoutEq extends DisciplineSuite, AllSyntax:
    override val scalaCheckTestParameters: Parameters = super.scalaCheckTestParameters
      .withMinSuccessfulTests(if Platform.isJvm then 50 else 5)
      .withMaxDiscardRatio(if Platform.isJvm then 5 else 50)
      .withWorkers(if Platform.isJvm then 2 else 1)
      .withMaxSize(if Platform.isJvm then 10 else 5)
      .withMinSize(0)

    // The default Arbitrary[Duration] causes overflow.
    given Arbitrary[Duration] = Arbitrary(Gen.chooseNum(-750.days.toNanos, 750.days.toNanos).map(Duration.fromNanos))
    given [A: Arbitrary]: Arbitrary[List[A]] = Arbitrary.arbContainer
    given [A <: Singleton: ValueOf]: Arbitrary[A] = Arbitrary(Gen.const(valueOf[A]))
    @unused given [A <: Singleton: ValueOf]: Cogen[A] = Cogen((seed, _) => seed)
    inline given [F[_]]: Isomorphisms[F] = Isomorphisms.invariant(summonInline)

    given [A <: Product](using mirror: Mirror.ProductOf[A], via: Arbitrary[mirror.MirroredElemTypes]): Arbitrary[A] =
      Arbitrary(via.arbitrary.map(mirror.fromTuple))

    given [A <: Product](using mirror: Mirror.ProductOf[A], via: Cogen[mirror.MirroredElemTypes]): Cogen[A] =
      via.contramap(Tuple.fromProductTyped)

    inline def testNoInstance(inline tc: String, inline target: String): Unit =
      val tcName = tc.drop(tc.lastIndexOf('.') + 1).capitalize
      testNoGiven(tc + "[" + target + "]", s"Could not derive $tcName for")

    inline def testNoGiven(inline code: String, message: String): Unit =
      val errors = compileErrors(code)
      test(s"No given $code")(assert(errors.contains(message), s"$errors did not contain $message"))
