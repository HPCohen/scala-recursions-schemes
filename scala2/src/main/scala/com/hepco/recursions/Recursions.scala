package com.hepco.recursions

object Recursions {
  trait Functor[F[_]] {
    def fmap[A, B](fn: A => B)(fa: F[A]): F[B]
  }

  sealed trait ListF[+T, +R]
  final case class NilF[+T, +R]() extends ListF[T, R]
  final case class ConsF[+T, +R](x: T, xs: R) extends ListF[T, R]

  implicit def listFunctor[T]: Functor[ListF[T, ?]] = new Functor[ListF[T, ?]] {
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
}
