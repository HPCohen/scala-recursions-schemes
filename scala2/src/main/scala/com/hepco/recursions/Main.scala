package com.hepco.recursions
import Recursions._

import scala.language.reflectiveCalls

object Main {
  val sumAlg: Algebra[({type lam[A] = ListF[Int, A]})#lam, Int] = {
    case NilF() => 0
    case ConsF(x: Int, xs: Int) => x + xs
  }

  val doubleAlg: Algebra[({type lam[A] = ListF[Int, A]})#lam, Fix[({type lam[A] = ListF[Int, A]})#lam]] = {
    case NilF() => Fix[({type lam[A] = ListF[Int, A]})#lam](NilF())
    case ConsF(x, xs) => Fix[({type lam[A] = ListF[Int, A]})#lam](ConsF(2 * x, xs))
  }

  val filterAlg: (Int => Boolean) => Algebra[({type lam[A] = ListF[Int, A]})#lam, Fix[({type lam[A] = ListF[Int, A]})#lam]] = fil => {
    case NilF() => Fix[({type lam[A] = ListF[Int, A]})#lam](NilF())
    case ConsF(x, xs) => if (fil(x)) Fix[({type lam[A] = ListF[Int, A]})#lam](ConsF(x, xs)) else xs
  }

  val printRAlg: RAlgebra[({type lam[A] = ListF[Int, A]})#lam, Unit] = fixed => {
    curr => {
      println(s"Look at current setup: $fixed")
      println(s"Do the operation on: $curr")
    }
  }

  val lst: Fix[({type lam[A] = ListF[Int, A]})#lam] = Fix[({type lam[A] = ListF[Int, A]})#lam](
    ConsF(1, Fix[({type lam[A] = ListF[Int, A]})#lam](
      ConsF(2, Fix[({type lam[A] = ListF[Int, A]})#lam](
        ConsF(3, Fix[({type lam[A] = ListF[Int, A]})#lam](
          ConsF(4, Fix[({type lam[A] = ListF[Int, A]})#lam](
            ConsF(5, Fix[({type lam[A] = ListF[Int, A]})#lam](
              ConsF(6, Fix[({type lam[A] = ListF[Int, A]})#lam](
                NilF())))))))))))))

  def main(args: Array[String]): Unit = {
    println(cata[({type lam[A] = ListF[Int, A]})#lam, Int](sumAlg)(lst))
    println(cata[({type lam[A] = ListF[Int, A]})#lam, Fix[({type lam[A] = ListF[Int, A]})#lam]](doubleAlg)(lst))
    println((cata[({type lam[A] = ListF[Int, A]})#lam, Fix[({type lam[A] = ListF[Int, A]})#lam]](doubleAlg) _ andThen cata[({type lam[A] = ListF[Int, A]})#lam, Int](sumAlg))(lst))
    println(cata[({type lam[A] = ListF[Int, A]})#lam, Fix[({type lam[A] = ListF[Int, A]})#lam]](filterAlg(_ % 2 == 0))(lst))
    para[({type lam[A] = ListF[Int, A]})#lam, Unit](printRAlg)(lst)
  }
}
