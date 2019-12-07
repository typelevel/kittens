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

import alleycats.ConsK
import cats.laws.discipline.SerializableTests
import org.scalacheck.Arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ConsKSuite extends KittensSuite with ScalaCheckDrivenPropertyChecks {
  import ConsKSuite._
  import TestDefns._

  def checkConsK[F[_], A : Arbitrary](nil: F[A])(fromSeq: Seq[A] => F[A])(implicit F: ConsK[F]): Unit =
    forAll((xs: List[A]) => assert(xs.foldRight(nil)(F.cons) == fromSeq(xs)))

  def testConsK(context: String)(implicit iList: ConsK[IList], snoc: ConsK[Snoc]): Unit = {
    test(s"$context.ConsK[IList]")(checkConsK[IList, Int](INil())(IList.fromSeq))
    test(s"$context.ConsK[Snoc]")(checkConsK[Snoc, Int](SNil())(xs => Snoc.fromSeq(xs.reverse)))
    checkAll(s"$context.ConsK is Serializable", SerializableTests.serializable(ConsK[IList]))
  }

  {
    import auto.consK._
    testConsK("auto")
  }

  {
    import cached.consK._
    testConsK("cached")
  }

  {
    import semiInstances._
    testConsK("semi")
  }
}

object ConsKSuite {
  import TestDefns._

  object semiInstances {
    implicit val iList: ConsK[IList] = semiauto.consK[IList]
    implicit val snoc: ConsK[Snoc] = semiauto.consK[Snoc]
  }
}
