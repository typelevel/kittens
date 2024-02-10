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
package derived

import cats.derived.util.VersionSpecific.OrElse
import shapeless._
import shapeless.labelled._

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not derive Applicative for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type λ[x => T] where T: Monoid
  * nested type λ[x => G[H[x]]] where G: Applicative and H: Applicative
  * generic case class where all fields form Applicative""")
trait MkApplicative[F[_]] extends Applicative[F]

object MkApplicative extends MkApplicativeDerivation {
  def apply[F[_]](implicit F: MkApplicative[F]): MkApplicative[F] = F
}

abstract private[derived] class MkApplicativeDerivation extends MkApplicativeNested {

  implicit val mkApplicativeHNil: MkApplicative[Const[HNil]#λ] = new MkApplicative[Const[HNil]#λ] {
    def pure[A](x: A) = HNil
    def ap[A, B](ff: HNil)(fa: HNil) = ff
    override def map[A, B](fa: HNil)(f: A => B) = fa
  }

  implicit def mkApplicativeConst[T](implicit T: Monoid[T]): MkApplicative[Const[T]#λ] =
    new MkApplicative[Const[T]#λ] {
      def pure[A](x: A) = T.empty
      def ap[A, B](ff: T)(fa: T) = T.combine(ff, fa)
      override def map[A, B](fa: T)(f: A => B) = fa
    }

  implicit def mkApplicativeFieldType[K, V[_]](implicit
      V: Strict[ApplicativeOrMk[V]]
  ): MkApplicative[λ[a => FieldType[K, V[a]]]] =
    new MkApplicative[λ[a => FieldType[K, V[a]]]] {
      private val v = V.value.unify
      def pure[A](x: A) = field[K][V[A]](v.pure(x))
      def ap[A, B](ff: FieldType[K, V[A => B]])(fa: FieldType[K, V[A]]) = field[K][V[B]](v.ap(ff)(fa))
      override def map[A, B](fa: FieldType[K, V[A]])(f: A => B) = field[K][V[B]](v.map(fa)(f))
    }
}

abstract private[derived] class MkApplicativeNested extends MkApplicativeGeneric {

  implicit def mkApplicativeNested[F[_]](implicit F: Split1[F, ApplicativeOrMk, ApplicativeOrMk]): MkApplicative[F] =
    new MkApplicative[F] {

      def pure[A](x: A) =
        F.pack(F.fo.unify.pure(F.fi.unify.pure(x)))

      def ap[A, B](ff: F[A => B])(fa: F[A]) = {
        val fo = F.fo.unify
        F.pack(fo.ap(fo.map(F.unpack(ff))(fif => F.fi.unify.ap(fif)(_: F.I[A])))(F.unpack(fa)))
      }

      override def map[A, B](fa: F[A])(f: A => B) =
        F.pack(F.fo.unify.map(F.unpack(fa))(F.fi.unify.map(_)(f)))
    }
}

abstract private[derived] class MkApplicativeGeneric {
  protected type ApplicativeOrMk[F[_]] = Applicative[F] OrElse MkApplicative[F]

  implicit def mkApplicativeHCons[F[_]](implicit F: IsHCons1[F, ApplicativeOrMk, MkApplicative]): MkApplicative[F] =
    new MkApplicative[F] {

      def pure[A](x: A) =
        F.pack((F.fh.unify.pure(x), F.ft.pure(x)))

      def ap[A, B](ff: F[A => B])(fa: F[A]) = {
        val (fhf, ftf) = F.unpack(ff)
        val (fha, fta) = F.unpack(fa)
        F.pack((F.fh.unify.ap(fhf)(fha), F.ft.ap(ftf)(fta)))
      }

      override def map[A, B](fa: F[A])(f: A => B) = {
        val (fha, fta) = F.unpack(fa)
        F.pack((F.fh.unify.map(fha)(f), F.ft.map(fta)(f)))
      }
    }

  implicit def mkApplicativeGeneric[F[_]](implicit F: Generic1[F, MkApplicative]): MkApplicative[F] =
    new MkApplicative[F] {
      def pure[A](x: A) = F.from(F.fr.pure(x))
      def ap[A, B](ff: F[A => B])(fa: F[A]) = F.from(F.fr.ap(F.to(ff))(F.to(fa)))
      override def map[A, B](fa: F[A])(f: A => B) = F.from(F.fr.map(F.to(fa))(f))
    }
}
