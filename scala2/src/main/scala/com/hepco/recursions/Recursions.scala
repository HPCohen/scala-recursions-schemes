package com.hepco.recursions

import scala.language.higherKinds
import scala.language.reflectiveCalls

object Recursions {
  trait Functor[F[_]] {
    def fmap[A, B](fn: A => B)(fa: F[A]): F[B]
  }

  sealed trait ListF[+T, +R]
  final case class NilF[+T, +R]() extends ListF[T, R]
  final case class ConsF[+T, +R](x: T, xs: R) extends ListF[T, R]

  implicit def listFunctor[T]: Functor[({type lam[A] = ListF[T, A]})#lam] = new Functor[({type lam[A] = ListF[T, A]})#lam] {
    override def fmap[A, B](fn: A => B)(lst: ListF[T, A]): ListF[T, B] = lst match {
      case NilF() => NilF()
      case ConsF(x: T, xs: A) => ConsF(x, fn(xs))
    }
  }

  final case class Fix[F[_]](unfix: F[Fix[F]])
  def unfix[F[_]](fixed: Fix[F]): F[Fix[F]] = fixed.unfix

  type Algebra[F[_], A] = F[A] => A

  def cata[F[_]: Functor, T](alg: Algebra[F, T])(term: Fix[F]): T = {
    val f = implicitly[Functor[F]]
    alg(f.fmap(cata(alg))(term.unfix))
  }

  type RAlgebra[F[_], A] = F[Fix[F]] => F[A] => A

  def para[F[_]: Functor, T](rAlg: RAlgebra[F, T])(term: Fix[F]): T = {
    val f = implicitly[Functor[F]]
    rAlg(term.unfix)(f.fmap(para(rAlg))(term.unfix))
  }
}
