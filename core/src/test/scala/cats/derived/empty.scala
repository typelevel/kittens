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

import alleycats.Empty
import cats.instances.all._
import org.scalatest.FreeSpec
import TestDefns._


class EmptySuite extends FreeSpec {

  "semi auto derivation" - {
    "for simple product" in {
      implicit val E = semi.empty[Foo]
      assert(Empty[Foo].empty == Foo(0, None))
    }

    "for nested product" in {
      implicit val E = semi.empty[Outer]
      assert(Empty[Outer].empty == Outer(Inner(0)))
    }

    "for nested product respects existing instances" in {
      import EmptySuite._
      implicit val E = semi.empty[Outer]
      assert(Empty[Outer].empty == Outer(Inner(1)))
    }

    "derives an instance for Interleaved[T]" in {
      semi.empty[TestDefns.Interleaved[Int]]
    }

  }

  "full auto derivation" - {
    import auto.empty._

    "for simple product" in {
      assert(Empty[Foo].empty == Foo(0, None))
    }

    "for nested product" in {
      assert(Empty[Outer].empty == Outer(Inner(0)))
    }

    "for nested product respects existing instances" in {
      import EmptySuite._
      assert(Empty[Outer].empty == Outer(Inner(1)))
    }
  }
}

object EmptySuite {
  implicit val emptyInner: Empty[Inner] =
    Empty(Inner(1))
}
