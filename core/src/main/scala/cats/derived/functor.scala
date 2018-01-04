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
trait MkFunctor[F[_]] { self =>
  def safeMap[A, B](fa: F[A])(f: A => Eval[B]): Eval[F[B]]
  def instance: Functor[F] = new MkFunctor.Safe[F] {
    def maker = self
  }
}

object MkFunctor extends MkFunctorDerivation {
  def apply[F[_]](implicit functor: MkFunctor[F]): MkFunctor[F] = functor
}

private[derived] abstract class MkFunctorDerivation
  extends MkFunctor0 with Prioritized1[MkFunctor] {

  trait Safe[F[_]] extends Functor[F] {
    def maker: MkFunctor[F]
    def map[A, B](fa: F[A])(f: A => B) =
      maker.safeMap(fa)(f.andThen(Eval.now)).value
  }

  implicit def fromFunctor[F[_]](implicit F: Functor[F]): Hidden[F] = F match {
    case safe: Safe[F] => prioritized(safe.maker)
    case _ => new MkFunctor[F] with Priority.Hidden {
      def safeMap[A, B](fa: F[A])(f: A => Eval[B]) =
        Eval.later(F.map(fa)(f.andThen(_.value)))
    }
  }
}

private[derived] abstract class MkFunctor0 extends MkFunctor1 {
  this: MkFunctorDerivation =>

  implicit def splitFunctorInstance[F[_]](
    implicit F: Split1[F, MkFunctor, MkFunctor]
  ): Instantiated[F] = new MkFunctor[F] with Priority.Instantiated {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]) =
      F.fo.safeMap(F.unpack(fa))(F.fi.safeMap(_)(f)).map(F.pack)
  }
}

// FIXME: Merge with MkFunctor0 when scala/bug#10545 is fixed.
private[derived] abstract class MkFunctor1 extends MkFunctor2 {
  this: MkFunctorDerivation =>

  implicit val emptyProductDerivedFunctor: Derived[Const[HNil]#λ] =
    new MkFunctor[Const[HNil]#λ] with Priority.Derived {
      def safeMap[A, B](nil: HNil)(f: A => Eval[B]) = Eval.now(nil)
    }

  implicit val emptyCoroductDerivedFunctor: Derived[Const[CNil]#λ] =
    new MkFunctor[Const[CNil]#λ] with Priority.Derived {
      def safeMap[A, B](nil: CNil)(f: A => Eval[B]) = Eval.now(nil)
    }

  implicit def productDerivedFunctor[F[_]](
    implicit F: IsHCons1[F, MkFunctor, Derived]
  ): Derived[F] = new MkFunctor[F] with Priority.Derived {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]) = for {
      ht <- Eval.now(F.unpack(fa))
      h <- F.fh.safeMap(ht._1)(f)
      t <- F.ft.safeMap(ht._2)(f)
    } yield F.pack(h, t)
  }

  implicit def coproductDerivedFunctor[F[_]](
    implicit F: IsCCons1[F, MkFunctor, Derived]
  ): Derived[F] = new MkFunctor[F] with Priority.Derived {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]) = F.unpack(fa) match {
      case Left(l)  => F.fh.safeMap(l)(f).map(l => F.pack(Left(l)))
      case Right(r) => F.ft.safeMap(r)(f).map(r => F.pack(Right(r)))
    }
  }

  implicit def genericDerivedFunctor[F[_]](
    implicit F: Generic1[F, Derived]
  ): Derived[F] = new MkFunctor[F] with Priority.Derived {
    def safeMap[A, B](fa: F[A])(f: A => Eval[B]) =
      F.fr.safeMap(F.to(fa))(f).map(F.from)
  }
}

private[derived] abstract class MkFunctor2 {
  this: MkFunctorDerivation =>

  implicit def constFunctor[T]: Hidden[Const[T]#λ] =
    new MkFunctor[Const[T]#λ] with Priority.Hidden {
      def safeMap[A, B](t: T)(f: A => Eval[B]) = Eval.now(t)
    }
}
