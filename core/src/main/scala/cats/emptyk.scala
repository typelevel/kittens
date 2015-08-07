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

package cats

trait EmptyK[F[_]] {
  def empty[A]: F[A]
}

object EmptyK {
  def apply[F[_]](implicit ef: EmptyK[F]): EmptyK[F] = ef

  implicit val listEmptyK: EmptyK[List] =
    new EmptyK[List] {
      def empty[A]: List[A] = Nil
    }

  implicit val optionEmptyK: EmptyK[Option] =
    new EmptyK[Option] {
      def empty[A]: Option[A] = None
    }
}


