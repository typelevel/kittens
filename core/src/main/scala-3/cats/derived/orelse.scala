package cats.derived

import shapeless3.deriving.K1
import scala.compiletime._

opaque type OrElse[A, B] = A | B
object OrElse extends OrElseInstances:
  def primary[A, B](a: A): A OrElse B = a
  def secondary[A, B](b: => B): A OrElse B = b
  extension [A, B](or: A OrElse B)
    def unify: A | B = or
  extension [A[_[_]], B[x[_]] <: A[x], F[_]](inst: K1.Instances[[x[_]] =>> A[x] OrElse B[x], F])
    def unify: K1.Instances[A, F] = inst

private[derived] sealed abstract class OrElseInstances:
  inline given [A, B]: OrElse[A, B] = summonFrom {
    case a: A => OrElse.primary(a)
    case b: B => OrElse.secondary(b)
  }
