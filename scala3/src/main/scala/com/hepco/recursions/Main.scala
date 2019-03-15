package com.hepco.recursions
import Recursions._

object Main {
  val sumAlg: Algebra[ListR[Int], Int] = {
    case ListF.NilF => 0
    case ListF.ConsF(i, total) => i + total
  }

  val doubleAlg: Algebra[ListR[Int], FixedList[Int]] = {
    case ListF.NilF => nil
    case ListF.ConsF(x, xs: FixedList[Int]) => cons(2 * x, xs)
  }

  val lst = cons(1, cons(2, cons(3, cons(4, nil))))

  def main(args: Array[String]): Unit = {
    println(cata[ListR[Int], Int](sumAlg)(lst))
    println(cata[ListR[Int], FixedList[Int]](doubleAlg)(lst))
    println((cata[ListR[Int], FixedList[Int]](doubleAlg) andThen cata[ListR[Int], Int](sumAlg))(lst))
  }
}
