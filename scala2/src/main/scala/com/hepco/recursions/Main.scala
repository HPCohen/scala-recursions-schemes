package com.hepco.recursions
import Recursions._

object Main {
  val sumAlg: Algebra[({type lam[A] = ListF[Int, A]})#lam, Int] = {
    case NilF() => 0
    case ConsF(x: Int, xs: Int) => x + xs
  }

  val doubleAlg: Algebra[({type lam[A] = ListF[Int, A]})#lam, Fix[({type lam[A] = ListF[Int, A]})#lam]] = {
    case NilF() => Fix[({type lam[A] = ListF[Int, A]})#lam](NilF())
    case ConsF(x, xs) => Fix[({type lam[A] = ListF[Int, A]})#lam](ConsF(2 * x, xs))
  }

  val lst: Fix[({type lam[A] = ListF[Int, A]})#lam] = Fix[({type lam[A] = ListF[Int, A]})#lam](
    ConsF(1, Fix[({type lam[A] = ListF[Int, A]})#lam](
      ConsF(2, Fix[({type lam[A] = ListF[Int, A]})#lam](
        ConsF(3, Fix[({type lam[A] = ListF[Int, A]})#lam](
          NilF())))))))

  def main(args: Array[String]): Unit = {
    println(cata[({type lam[A] = ListF[Int, A]})#lam, Int](sumAlg)(lst))
    println(cata[({type lam[A] = ListF[Int, A]})#lam, Fix[({type lam[A] = ListF[Int, A]})#lam]](doubleAlg)(lst))
    println((cata[({type lam[A] = ListF[Int, A]})#lam, Fix[({type lam[A] = ListF[Int, A]})#lam]](doubleAlg) andThen cata[ListF[Int, ?], Int](sumAlg))(lst))
  }
}
