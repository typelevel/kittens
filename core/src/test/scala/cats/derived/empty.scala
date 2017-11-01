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

import TestDefns._

class EmptySuite extends KittensSuite {
  test("for simple product") {
    implicit val E = derive.empty[Foo]
    assert(Empty[Foo].empty == Foo(0, None))
  }

  test("for nested product") {
    implicit val E = derive.empty[Outer]
    assert(Empty[Outer].empty == Outer(Inner(0)))
  }

  test("for nested product respect existing instance ") {
    import InnerEmptyInstance._
    implicit val E = derive.empty[Outer]
    assert(Empty[Outer].empty == Outer(Inner(1)))
  }
}



object InnerEmptyInstance {

  implicit def emptyInner: Empty[Inner] = new Empty[Inner]{
    override def empty: Inner = Inner(1)
  }
}
