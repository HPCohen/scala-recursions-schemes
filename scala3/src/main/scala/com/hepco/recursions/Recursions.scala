package com.hepco.recursions

object Recursions {
  trait Functor[F[_]] {
    def (x: F[A]) fmap[A, B] (fn: A => B): F[B]
  }
  
  enum ListF[+T, +R] {
    case NilF
    case ConsF(x: T, xs: R)
  }

  implied ListFunctor[T] for Functor[[R] => ListF[T, R]] {
    def (l: ListF[T, A]) fmap[A, B](fn: A => B): ListF[T, B] = l match {
      case ListF.NilF => ListF.NilF
      case ListF.ConsF(x, xs) => ListF.ConsF(x, fn(xs))
    }
  }

  final case class Fix[F[_] : Functor](unfix: F[Fix[F]])
  def unfix[F[_]: Functor](fix: Fix[F]): F[Fix[F]] = fix.unfix

  type ListR[T] = [R] => ListF[T, R]
  type FixedList[T] = Fix[ListR[T]]
  def cons[T](x: T, xs: FixedList[T]): FixedList[T] = Fix(ListF.ConsF(x, xs))
  def nil[T]: FixedList[T] = Fix(ListF.NilF)

  type Algebra[F[_], T] = F[T] => T

  def cata[F[_] : Functor, T](alg: Algebra[F, T])(term: Fix[F]): T = {
    alg(term.unfix.fmap(cata(alg)))
  }
}
