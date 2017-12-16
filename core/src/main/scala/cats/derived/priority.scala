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
trait Priority

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

/** Proves the absence of an implicit value of type `A`. */
@implicitNotFound("Could not refute {A}. An implicit instance was found.")
sealed trait Refute[A]
object Refute {
  private val instance: Refute[Any] = new Refute[Any] { }
  implicit def refute[A]: Refute[A] = instance.asInstanceOf[Refute[A]]
  implicit def ambiguous[A](implicit a: A): Refute[A] = unexpected
}
