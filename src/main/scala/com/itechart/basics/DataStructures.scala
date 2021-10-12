package com.itechart.basics

import scala.collection.immutable.SortedSet

object DataStructures {

  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val vegetableAmounts = Map(
    "tomatoes"  -> 17,
    "peppers"   -> 234,
    "olives"    -> 32,
    "cucumbers" -> 323,
  )

  val totalVegetableWeights: Map[String, Int] = {
    vegetableAmounts
      .filter { case (key, _) => vegetableWeights.contains(key) }
      .map { case (key, amount) => (key, vegetableWeights(key) * amount) }
  }

  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Either[String, Set[Set[A]]] = {
    def subsets(n: Int): Set[Set[A]] = {
      val singleElementSets = set.map(element => Set(element))
      if (n == 1)
        singleElementSets
      else {
        val result = for {
          previousSet      <- subsets(n - 1)
          singleElementSet <- singleElementSets
          element          <- singleElementSet
        } yield previousSet + element
        result.filter(set => set.size == n)
      }
    }

    if (n <= 0) Left("n should be greater than 0")
    else if (n > set.size) Left("n should be smaller (or equal) than set size")
    else Right(subsets(n))
  }

  def sortConsideringEqualValues[T](map: Map[T, Int])(implicit ord: Ordering[T]): List[(Set[T], Int)] = {
    map
      .groupBy { case (_, value) => value }
      .map { case (int, map) => (int, map.keys) }
      .toList
      .map { case (int, set) => (SortedSet.from(set), int) }
      .sortWith { case ((_, int1), (_, int2)) => int1 < int2 }
  }

  def main(args: Array[String]): Unit = {
    println(allSubsetsOfSizeN(Set(1, 2, 3), 2))
    println(sortConsideringEqualValues(Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)))
  }

}
