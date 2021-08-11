package com.itechart.basics

import scala.annotation.tailrec

object Basics {

  def main(args: Array[String]): Unit = {
    println(fibonacci(10))
    println(atkinSieve(97))
  }

  def fibonacci(n: Int): Either[String, Int] = {
    @tailrec
    def go(a: Int, b: Int, n: Int): Int = {
      n match {
        case 1 => a
        case _ => go(b, a + b, n - 1)
      }
    }

    if (n <= 0) Left("n should be greater than 0")
    else Right(go(0, 1, n))
  }

  def atkinSieve(n: Int = 10000): Either[String, List[Int]] = {
    if (n <= 1) Left("n should be greater than 1")
    else if (n == 2) Right(List(2))
    else {
      val array = Array.fill(n + 1)(false)
      array(2) = true
      array(3) = true
      val sqrt = Math.sqrt(n).toInt
      for {
        x <- 1 to sqrt
        y <- 1 to sqrt
        z = (4 * x * x) + (y * y)
        if z <= n && (z % 12 == 1 || z % 12 == 5)
      } yield array(z) = !array(z)
      for {
        x <- 1 to sqrt
        y <- 1 to sqrt
        z = (3 * x * x) + (y * y)
        if z <= n && (z % 12 == 7)
      } yield array(z) = !array(z)
      for {
        x <- 1 to sqrt
        y <- 1 to sqrt
        z = (3 * x * x) - (y * y)
        if x > y && z <= n && (z % 12 == 11)
      } yield array(z) = !array(z)
      for {
        a <- 5 to sqrt
        if array(a)
        x = a * a
        i <- x to n by x
      } yield array(i) = false
      Right(array.zipWithIndex.filter(pair => pair._1).map(pair => pair._2).toList)
    }
  }

}
