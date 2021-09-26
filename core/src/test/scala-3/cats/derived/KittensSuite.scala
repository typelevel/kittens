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

import cats.platform.Platform
import cats.syntax.AllSyntax
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Test.Parameters

import scala.quoted.*

/** An opinionated stack of traits to improve consistency and reduce boilerplate in Kittens tests. Note that unlike the
  * corresponding CatsSuite in the Cat project, this trait does not mix in any instances.
  */
abstract class KittensSuite extends DisciplineSuite, AllSyntax, TestEqInstances:
  override val scalaCheckTestParameters: Parameters = super.scalaCheckTestParameters
    .withMinSuccessfulTests(if (Platform.isJvm) 50 else 5)
    .withMaxDiscardRatio(if (Platform.isJvm) 5 else 50)
    .withWorkers(if (Platform.isJvm) 2 else 1)
    .withMaxSize(if (Platform.isJvm) 10 else 5)
    .withMinSize(0)

  given [A: Arbitrary]: Arbitrary[List[A]] =
    Arbitrary.arbContainer

  inline def nameOf[A]: String =
    ${ KittensSuite.nameOfMacro[A] }
    
  inline def testNoInstance(inline code: String, message: String): Unit =
    test(s"No $code")(assert(compileErrors(code).contains(message)))

object KittensSuite:
  def nameOfMacro[A: Type](using Quotes) =
    Expr(Type.show[A])
