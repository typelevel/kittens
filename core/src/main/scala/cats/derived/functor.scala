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

import shapeless._
import scala.annotation.implicitNotFound


@implicitNotFound("Could not derive an instance of Functor[${F}]")
trait MkFunctor[F[_]] extends Functor[F] {
  def map[A, B](fa: F[A])(f: A => B) = safeMap(fa)(f.andThen(Eval.now)).value
  def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]]
}

object MkFunctor extends MkFunctorDerivation {
  def apply[F[_]](implicit functor: MkFunctor[F]): MkFunctor[F] = functor

  private[derived] implicit class SafeMap[F[_]](val F: Functor[F]) extends AnyVal {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]) = F match {
      case mk: MkFunctor[F] => mk.safeMap(fa)(f)
      case _ => Eval.later(F.map(fa)(f.andThen(_.value)))
    }
  }
}

private[derived] abstract class MkFunctorDerivation extends MkFunctor1 {
  import MkFunctor.SafeMap

  protected type FunctorOrMk[F[_]] = Functor[F] OrElse MkFunctor[F]

  implicit val mkFunctorHNil: MkFunctor[Const[HNil]#λ] = new MkFunctor[Const[HNil]#λ] {
    def safeMap[A, B](nil: HNil)(f: A => Eval[B]) = Eval.now(nil)
  }

  implicit val mkFunctorCNil: MkFunctor[Const[CNil]#λ] = new MkFunctor[Const[CNil]#λ] {
    def safeMap[A, B](nil: CNil)(f: A => Eval[B]) = Eval.now(nil)
  }

  implicit def mkFunctorHCons[F[_]](
    implicit F: IsHCons1[F, FunctorOrMk, MkFunctor]
  ): MkFunctor[F] = new MkFunctor[F] {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]) = for {
      ht <- Eval.now(F.unpack(fa))
      fh <- F.fh.unify.safeMap(ht._1)(f)
      ft <- F.ft.safeMap(ht._2)(f)
    } yield F.pack(fh, ft)
  }

  implicit def mkFunctorCCons[F[_]](
    implicit F: IsCCons1[F, FunctorOrMk, MkFunctor]
  ): MkFunctor[F] = new MkFunctor[F] {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]) = F.unpack(fa) match {
      case Left(hd)  => F.fh.unify.safeMap(hd)(f).map(fh => F.pack(Left(fh)))
      case Right(tl) => F.ft.safeMap(tl)(f).map(ft => F.pack(Right(ft)))
    }
  }

  implicit def mkFunctorGeneric[F[_]](
    implicit F: Generic1[F, MkFunctor]
  ): MkFunctor[F] = new MkFunctor[F] {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]) =
      F.fr.safeMap(F.to(fa))(f).map(F.from)
  }
}

private[derived] abstract class MkFunctor1 extends MkFunctor2 {
  this: MkFunctorDerivation =>
  import MkFunctor.SafeMap

  implicit def mkFunctorSplit[F[_]](
    implicit F: Split1[F, FunctorOrMk, FunctorOrMk]
  ): MkFunctor[F] = new MkFunctor[F] {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]) =
      F.fo.unify.safeMap(F.unpack(fa))(F.fi.unify.safeMap(_)(f)).map(F.pack)
  }
}

private[derived] abstract class MkFunctor2 {
  this: MkFunctorDerivation =>

  implicit def mkFunctorConst[T]: MkFunctor[Const[T]#λ] = new MkFunctor[Const[T]#λ] {
    def safeMap[A, B](t: T)(f: A => Eval[B]) = Eval.now(t)
  }
}
