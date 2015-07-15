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

trait Pointed[F[_]] {
  def point[A](a: A): F[A]
}

object Pointed {
  def apply[F[_]](implicit pf: Pointed[F]): Pointed[F] = pf

  implicit def applicativeIsPointed[F[_]](implicit ev: Applicative[F]): Pointed[F] =
    new Pointed[F] {
      def point[A](a: A): F[A] = ev.pure(a)
    }

  implicit def pointedFlatMapIsMonad[F[_]](implicit p: Pointed[F], fm: FlatMap[F]): Monad[F] =
    new Monad[F] {
      def pure[A](a: A): F[A] = p.point(a)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = fm.map(fa)(f)
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = fm.flatMap(fa)(f)
    }
}


