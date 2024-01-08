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

import cats.Eq
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.platform.Platform
import cats.syntax.AllSyntax
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Test.Parameters

import scala.compiletime.summonInline
import scala.deriving.Mirror
import scala.quoted.*

/** An opinionated stack of traits to improve consistency and reduce boilerplate in Kittens tests. Note that unlike the
  * corresponding CatsSuite in the Cat project, this trait does not mix in any instances.
  */
abstract class KittensSuite extends KittensSuite.WithoutEq, ADTs.EqInstances:
  given [A <: Singleton: ValueOf]: Eq[A] = Eq.allEqual
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

    given [A: Arbitrary]: Arbitrary[List[A]] = Arbitrary.arbContainer
    given [A <: Singleton: ValueOf]: Arbitrary[A] = Arbitrary(Gen.const(valueOf[A]))
    given [A <: Singleton: ValueOf]: Cogen[A] = Cogen((seed, _) => seed)
    inline given [F[_]]: Isomorphisms[F] = Isomorphisms.invariant(summonInline)

    given [A <: Product](using mirror: Mirror.ProductOf[A], via: Arbitrary[mirror.MirroredElemTypes]): Arbitrary[A] =
      Arbitrary(via.arbitrary.map(mirror.fromTuple))

    given [A <: Product](using mirror: Mirror.ProductOf[A], via: Cogen[mirror.MirroredElemTypes]): Cogen[A] =
      via.contramap(Tuple.fromProductTyped)

    def assertNoInstance(errors: String): Unit =
      val message = "No given instance of type"
      assert(errors.contains(message), s"$errors did not contain $message")

    inline def testNoInstance(inline tc: String, target: String): Unit =
      test(s"No $tc for $target")(assertNoInstance(compileErrors(tc + "[" + target + "]")))
