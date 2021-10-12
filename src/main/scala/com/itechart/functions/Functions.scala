package com.itechart.functions

import scala.annotation.tailrec

object Functions {

  // https://www.codewars.com/kata/555eded1ad94b00403000071/train/scala

  def seriesSum(n: Int): String = {
    @tailrec
    def summarize(n: Int, denominator: Double, sum: Double): Double = {
      if (n == 0) sum
      else {
        summarize(n - 1, denominator + 3, sum + 1 / denominator)
      }
    }

    val result = summarize(n, denominator = 1, sum = 0)
    f"$result%.2f"
  }

  // https://www.codewars.com/kata/525f50e3b73515a6db000b83/train/scala

  def createPhoneNumber(numbers: Seq[Int]): String = {
    numbers.zipWithIndex
      .foldLeft("")((acc, pair) =>
        pair match {
          case (num, index) if index == 0 => acc + "(" + num
          case (num, index) if index == 3 => acc + ") " + num
          case (num, index) if index == 6 => acc + "-" + num
          case (num, _)                   => acc + num
        }
      )
  }

  def createPhoneNumber1(numbers: Seq[Int]): String = {
    "(%d%d%d) %d%d%d-%d%d%d%d".format(numbers: _*)
  }

  // https://www.codewars.com/kata/526571aae218b8ee490006f4/train/scala

  def countBits(n: Int): Int = Integer.bitCount(n)

  // https://www.codewars.com/kata/550498447451fbbd7600041c/train/scala

  def comp(seq1: Seq[Int], seq2: Seq[Int]): Boolean = {
    if ((seq1 == Nil || seq2 == Nil) && seq1 != seq2) false
    else
      seq1.sorted
        .zip(seq2.sorted)
        .forall { case (int1, int2) => int1 * int1 == int2 }
  }

  // https://www.codewars.com/kata/57eb8fcdf670e99d9b000272/train/scala

  def high(s: String): String = {
    val words = s.split("\\s+")
    val (_, index) = words
      .map(word => word.toCharArray)
      .map(chars => chars.foldLeft(0)(_ + _ - 'a' + 1))
      .zipWithIndex
      .maxBy { case (charSum, _) => charSum }
    words(index)
  }

  // https://www.codewars.com/kata/51ba717bb08c1cd60f00002f/train/scala

  def solution(xs: List[Int]): String = {
    @tailrec
    def func(previous: Int, list: List[Int], acc: String): String = {
      list match {
        case Nil            => acc
        case element :: Nil => acc + element.toString
        case element :: next :: tail =>
          if (previous + 1 == element && next - 1 == element) func(element, next :: tail, acc + "_")
          else func(element, next :: tail, acc + element + ",")
      }
    }

    func(Int.MinValue, xs, "")
      .replaceAll(",_", "_")
      .replaceAll("_+", "-")
  }

  // https://www.codewars.com/kata/556deca17c58da83c00002db/train/scala

  def tribonacci[T: Numeric](signature: List[T], n: Int): List[T] = {
    def go(a: T, b: T, c: T, count: Int)(implicit numeric: Numeric[T]): List[T] = {
      import numeric._
      val sum = a + b + c
      count match {
        case 1                         => List(sum)
        case number if number == n - 3 => a :: b :: c :: sum :: go(b, c, sum, count - 1)(numeric)
        case _                         => sum :: go(b, c, sum, count - 1)(numeric)
      }
    }

    if (n <= 3) signature.slice(0, n)
    else go(signature.head, signature(1), signature(2), n - 3)
  }

  // https://www.codewars.com/kata/529bf0e9bdf7657179000008/train/scala

  def isValid(board: Array[Array[Int]]): Boolean = {
    hasValidLines(board) && hasValidColumns(board) && hasValidSquares(board)
  }

  def hasValidLines(board: Array[Array[Int]]): Boolean = {
    board.forall(array => (1 to 9).forall(array.contains(_)))
  }

  def hasValidColumns(board: Array[Array[Int]]): Boolean = {
    def validColumn(col: Int, arrays: Array[Array[(Int, Int)]]): Boolean = {
      val column = arrays
        .flatMap(_.toList)
        .filter { case (_, index) => index == col }
        .map { case (int, _) => int }
      (1 to 9).forall(column.contains(_))
    }

    val indexArray = board.map(_.zipWithIndex)
    (0 to 8).forall(validColumn(_, indexArray))
  }

  def hasValidSquares(board: Array[Array[Int]]): Boolean = {
    def validSquare(row: Int, column: Int, board: Array[Array[Int]]): Boolean = {
      val square = board
        .slice(row, row + 3)
        .flatMap(_.slice(column, column + 3))
      (1 to 9).forall(square.contains(_))
    }

    (0 to 8 by 3).forall(row => (0 to 8 by 3).forall(column => validSquare(row, column, board)))
  }

  def main(args: Array[String]): Unit = {}
}
