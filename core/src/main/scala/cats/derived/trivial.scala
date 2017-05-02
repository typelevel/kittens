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

trait Trivial1[F[_]]
object Trivial1 {
  implicit def mkTrivial1[F[_]]: Trivial1[F] = new Trivial1[F] {}
}

/**
 * The "Unit type class".  The only instance of `Trivial` is given by
 * `Trivial.manifest`, and this instance is guaranteed to be in the
 * implicit scope.  Several convenience type aliases are provided in
 * companion object, covering a few common use cases and avoiding the
 * need for unnecessary lambdas (e.g. if you want a trivial type class
 * instance for a type constructor, you should use `Trivial.PH1`).
 *
 * Moved from cats.core
 */
sealed trait Trivial

object Trivial {
  type P1[A] = Trivial
  type PH1[F[_]] = Trivial
  type P1H1[F[_], A] = Trivial
  type P2[A, B] = Trivial
  type P2H1[F[_], A, B] = Trivial
  type P3[A, B, C] = Trivial
  type P3H1[F[_], A, B, C] = Trivial

  implicit val catsTrivialInstance: Trivial = new Trivial {}
}
