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

import shapeless.unexpected
import scala.annotation.implicitNotFound


/** The root of the implicit priority hierarchy.
  * Should not be used except as a type parameter bound.
  */
trait Priority extends Any
object Priority {

  /** A separate branch of the implicit priority hierarchy.
    * Enables derived instances to use available instances in scope without causing a cycle.
    */
  trait Hidden extends Priority

  /** Lowest priority in the hierarchy. */
  trait LowPriority extends Priority

  /** The priority of fallback instances defined for any type. */
  trait Default extends LowPriority

  /** The priority of automatically derived instances. */
  trait Derived extends Default

  /** The priority of instances that instantiate higher kinded type classes. */
  trait Instantiated extends Derived

  /** The priority of instances composed of other type classes. */
  trait Algebraic extends Instantiated

  /** The priority of instances available through subclasses. */
  trait Subclass extends Algebraic

  /** The priority of ad-hoc instances. */
  trait Orphan extends Subclass

  /** The highest priority in the hierarchy. */
  trait HighPriority extends Orphan
}


/** Shorthand aliases for the companion objects of `* -> *` kind type classes. */
trait Prioritized[F[_]] {
  type Hidden[A] = F[A] with Priority.Hidden
  type LowPriority[A] = F[A] with Priority.LowPriority
  type Default[A] = F[A] with Priority.Default
  type Derived[A] = F[A] with Priority.Derived
  type Instantiated[A] = F[A] with Priority.Instantiated
  type Algebraic[A] = F[A] with Priority.Algebraic
  type Subclass[A] = F[A] with Priority.Subclass
  type Orphan[A] = F[A] with Priority.Orphan
  type HighPriority[A] = F[A] with Priority.HighPriority

  def prioritized[P <: Priority, A](fa: F[A]): F[A] with P =
    fa.asInstanceOf[F[A] with P]
}


/** Shorthand aliases for the companion objects of `* -> (* -> *)` kind type classes. */
trait Prioritized1[F[_[_]]] {
  type Hidden[G[_]] = F[G] with Priority.Hidden
  type LowPriority[G[_]] = F[G] with Priority.LowPriority
  type Default[G[_]] = F[G] with Priority.Default
  type Derived[G[_]] = F[G] with Priority.Derived
  type Instantiated[G[_]] = F[G] with Priority.Instantiated
  type Algebraic[G[_]] = F[G] with Priority.Algebraic
  type Subclass[G[_]] = F[G] with Priority.Subclass
  type Orphan[G[_]] = F[G] with Priority.Orphan
  type HighPriority[G[_]] = F[G] with Priority.HighPriority

  def prioritized[P <: Priority, G[_]](fg: F[G]): F[G] with P =
    fg.asInstanceOf[F[G] with P]
}


/** Proves the absence of an implicit value of type `A`. */
@implicitNotFound("Could not refute {A}. An implicit instance was found.")
sealed trait Refute[A]
object Refute {
  def apply[A](implicit refute: Refute[A]): Refute[A] = refute
  private val instance: Refute[Any] = new Refute[Any] { }
  implicit def refute[A]: Refute[A] = instance.asInstanceOf[Refute[A]]
  implicit def ambiguous[A](implicit a: A): Refute[A] = unexpected
}
