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

import cats.syntax.AllSyntax
import cats.tests.{StrictCatsEquality, TestSettings}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Kittens tests. Note that unlike the corresponding
 * CatsSuite in the Cat project, this trait does not mix in any
 * instances.
 */
abstract class KittensSuite extends AnyFunSuite
  with Matchers
  with Checkers
  with FunSuiteDiscipline
  with TestSettings
  with AllSyntax
  with StrictCatsEquality {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration
}
