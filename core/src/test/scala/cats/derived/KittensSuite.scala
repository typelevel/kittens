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
import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.Discipline

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Kittens tests. Note that unlike the corresponding
 * CatsSuite in the Cat project, this trait does not mix in any
 * instances.
 */
trait KittensSuite extends AnyFunSuite with Discipline with AllSyntax {

  // Copied from former `CatsSuite`.
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = if (Platform.isJvm) PosInt(50) else PosInt(5),
      maxDiscardedFactor = if (Platform.isJvm) PosZDouble(5.0) else PosZDouble(50.0),
      minSize = PosZInt(0),
      sizeRange = if (Platform.isJvm) PosZInt(10) else PosZInt(5),
      workers = if (Platform.isJvm) PosInt(2) else PosInt(1)
    )
}
