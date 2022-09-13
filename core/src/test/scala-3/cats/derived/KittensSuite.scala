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
import cats.platform.Platform
import cats.syntax.AllSyntax
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Test.Parameters

import scala.deriving.Mirror
import scala.quoted.*

/** An opinionated stack of traits to improve consistency and reduce boilerplate in Kittens tests. Note that unlike the
  * corresponding CatsSuite in the Cat project, this trait does not mix in any instances.
  */
abstract class KittensSuite extends KittensSuite.WithoutEq, TestEqInstances:
  given [A <: Product](using mirror: Mirror.ProductOf[A], via: Eq[mirror.MirroredElemTypes]): Eq[A] =
    Eq.by(Tuple.fromProductTyped)

object KittensSuite:
  def deCapitalizeMacro(str: Expr[String])(using Quotes) =
    val value = str.valueOrAbort
    Expr(if (value.isEmpty) "" else value.head.toLower +: value.tail)

  inline def deCapitalize(inline str: String): String =
    ${ deCapitalizeMacro('str) }

  /** Used to test `Eq` derivation. */
  abstract class WithoutEq extends DisciplineSuite, AllSyntax:
    override val scalaCheckTestParameters: Parameters = super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isJvm) 50 else 5)
      .withMaxDiscardRatio(if (Platform.isJvm) 5 else 50)
      .withWorkers(if (Platform.isJvm) 2 else 1)
      .withMaxSize(if (Platform.isJvm) 10 else 5)
      .withMinSize(0)

    given [A: Arbitrary]: Arbitrary[List[A]] =
      Arbitrary.arbContainer

    given [A <: Product](using mirror: Mirror.ProductOf[A], via: Arbitrary[mirror.MirroredElemTypes]): Arbitrary[A] =
      Arbitrary(via.arbitrary.map(mirror.fromTuple))

    given [A <: Product](using mirror: Mirror.ProductOf[A], via: Cogen[mirror.MirroredElemTypes]): Cogen[A] =
      via.contramap(Tuple.fromProductTyped)

    inline def testNoInstance(inline tc: String, target: String, message: String): Unit =
      val errors = compileErrors(tc + "[" + target + "]")
      test(s"No $tc for $target")(assert(errors.contains(message), s"$errors did not contain $message"))

    inline def testNoAuto(inline tc: String, target: String): Unit =
      testNoInstance(tc, target, "No given instance of type")

    inline def testNoSemi(inline tc: String, target: String): Unit =
      testNoInstance("semiauto." + deCapitalize(tc), target, "Could not derive an instance of " + tc)
