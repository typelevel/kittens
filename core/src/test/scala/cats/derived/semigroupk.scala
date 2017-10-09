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

package cats
package derived

import cats._, instances.all._, kernel.laws.GroupLaws
import org.scalacheck.Arbitrary, Arbitrary.arbitrary



class SemigroupKTests extends KittensSuite {
  import SemigroupKTests.ComplexProduct

  implicit val sg = derive.semigroupK[ComplexProduct].algebra[Char]
  checkAll("SemigroupK[ComplexProduct]", GroupLaws[ComplexProduct[Char]].semigroup(sg))
}

object SemigroupKTests {

  case class ComplexProduct[T](
    lbl: String,          // MkSemigroup.const
    set: Set[T],          // provided
    fns: Vector[() => T], // MkSemigroup.composedd
    opt: Eval[Option[T]]) // MkSemigroup.applied

  object ComplexProduct {
    implicit def arb[T: Arbitrary]: Arbitrary[ComplexProduct[T]] =
      Arbitrary(for {
        lbl <- arbitrary[String]
        set <- arbitrary[Set[T]]
        vec <- arbitrary[Vector[T]]
        fns = vec.map(x => () => x)
        opt <- arbitrary[Option[T]]
      } yield ComplexProduct(lbl, set, fns, Eval.now(opt)))

    implicit def eqv[T: Eq]: Eq[ComplexProduct[T]] =
      new Eq[ComplexProduct[T]] {
        val eqSet = Eq[Set[T]]
        val eqVec = Eq[Vector[T]]
        val eqOpt = Eq[Eval[Option[T]]]

        def eqv(x: ComplexProduct[T], y: ComplexProduct[T]) =
          x.lbl == y.lbl &&
            eqSet.eqv(x.set, y.set) &&
            eqVec.eqv(x.fns.map(_()), y.fns.map(_())) &&
            eqOpt.eqv(x.opt, y.opt)
      }
  }
}
