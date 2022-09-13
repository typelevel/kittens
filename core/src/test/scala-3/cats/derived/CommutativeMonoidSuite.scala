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

import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}
import cats.kernel.laws.discipline.*
import scala.compiletime.*

class CommutativeMonoidSuite extends KittensSuite:
  import CommutativeMonoidSuite.*
  import ADTs.*

  inline def tests[A]: CommutativeMonoidTests[A] =
    CommutativeMonoidTests[A](summonInline)

  inline def validate(inline instance: String): Unit =
    checkAll(s"$instance[CommutativeFoo]", tests[CommutativeFoo].commutativeMonoid)
    checkAll(s"$instance[Recursive]", tests[Recursive].commutativeMonoid)
    checkAll(s"$instance[BoxMul]", tests[BoxMul].commutativeMonoid)
    checkAll(
      s"$instance is Serializable",
      SerializableTests.serializable(summonInline[CommutativeMonoid[CommutativeFoo]])
    )
    test(s"$instance respects existing instances") {
      val box = summonInline[CommutativeMonoid[BoxMul]]
      assert(box.empty == Box(Mul(1)))
      assert(box.combine(Box(Mul(5)), Box(Mul(5))) == Box(Mul(25)))
    }

  locally {
    import auto.commutativeMonoid.given
    validate("auto.commutativeMonoid")
  }

  locally {
    import semiInstances.given
    validate("semiauto.commutativeMonoid")
  }

  locally {
    import derivedInstances.*
    val instance = "derived.commutativeMonoid"
    checkAll(s"$instance[CommutativeFoo]", tests[CommutativeFoo].commutativeMonoid)
    checkAll(s"$instance[BoxMul]", tests[BoxMul].commutativeMonoid)
    checkAll(s"$instance is Serializable", SerializableTests.serializable(CommutativeMonoid[CommutativeFoo]))
  }

end CommutativeMonoidSuite

object CommutativeMonoidSuite:
  import ADTs.*

  type BoxMul = Box[Mul]

  object semiInstances:
    given CommutativeMonoid[CommutativeFoo] = semiauto.commutativeMonoid
    given CommutativeMonoid[Recursive] = semiauto.commutativeMonoid
    given CommutativeMonoid[Box[Mul]] = semiauto.commutativeMonoid

  object derivedInstances:
    case class CommutativeFoo(x: ADTs.CommutativeFoo) derives CommutativeMonoid
    case class BoxMul(x: CommutativeMonoidSuite.BoxMul) derives CommutativeMonoid

  final case class Mul(value: Int)
  object Mul:
    given CommutativeMonoid[Mul] with
      val empty = Mul(1)
      def combine(x: Mul, y: Mul) = Mul(x.value * y.value)

end CommutativeMonoidSuite
