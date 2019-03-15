package com.hepco.recursions
import Recursions._

object Main {
  val sumAlg: Algebra[ListF[Int, ?], Int] = {
    case NilF() => 0
    case ConsF(x: Int, xs: Int) => x + xs
  }

  val doubleAlg: Algebra[ListF[Int, ?], Fix[ListF[Int, ?]]] = {
    case NilF() => Fix[ListF[Int, ?]](NilF())
    case ConsF(x, xs) => Fix[ListF[Int, ?]](ConsF(2 * x, xs))
  }

  val lst: Fix[ListF[Int, ?]] = Fix[ListF[Int, ?]](
    ConsF(1, Fix[ListF[Int, ?]](
      ConsF(2, Fix[ListF[Int, ?]](
        ConsF(3, Fix[ListF[Int, ?]](
          NilF())))))))

  def main(args: Array[String]): Unit = {
    println(cata[ListF[Int, ?], Int](sumAlg)(lst))
    println(cata[ListF[Int, ?], Fix[ListF[Int, ?]]](doubleAlg)(lst))
    println((cata[ListF[Int, ?], Fix[ListF[Int, ?]]](doubleAlg) andThen cata[ListF[Int, ?], Int](sumAlg))(lst))
  }
}
