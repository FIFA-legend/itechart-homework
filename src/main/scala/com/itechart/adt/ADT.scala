package com.itechart.adt

import com.itechart.adt.ADT.Bet.ColorHalf

object ADT {

  // Implement ADTs for Roulette game

  // https://www.venetian.com/casino/table-games/roulette-basic-rules.html
  // https://www.mastersofgames.com/rules/roulette-rules.htm

  // Use material from adt workshop for implementation of the following data structures:

  sealed trait Color
  final case object Red extends Color
  final case object Black extends Color

  sealed abstract case class Square private(value: Int, color: Color)
  object Square {
    def create(value: Int): Either[String, Square] = value match {
      case num if num < 0 || num > 36 => Left("Invalid number value, it must be between 0 and 36")
      case _ => Right(new Square(value, determineColor(value)) {})
    }

    private def determineColor(value: Int): Color = {
      val reds = Set(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
      value match {
        case v if reds.contains(v) => Red
        case _ => Black
      }
    }
  }

  sealed trait Bet
  object Bet {
    final case class StraightUp(single: Square) extends Bet
    final case class Split(upperSquare: Square) extends Bet
    final case class Street(start: Square) extends Bet
    final case class CornerBet(upperLeftCorner: Square) extends Bet
    final case class Line(upperLeftCorner: Square) extends Bet
    final case class Dozen(dozen: Int) extends Bet
    final case class Column(column: Int) extends Bet
    final case class NumberHalf(half: Int) extends Bet
    final case class ColorHalf(color: Color) extends Bet
  }

  object BetUtils {
    def multiplier(bet: Bet): Int = {
      bet match {
        case Bet.StraightUp(_) => 36
        case Bet.Split(_) => 18
        case Bet.Street(_) => 12
        case Bet.CornerBet(_) => 9
        case Bet.Line(_) => 6
        case Bet.Dozen(_) => 3
        case Bet.Column(_) => 3
        case Bet.NumberHalf(_) => 2
        case Bet.ColorHalf(_) => 2
      }
    }

    def condition(bet: Bet, square: Square): Boolean = {
      bet match {
        case Bet.StraightUp(single) => square.value == single.value
        case Bet.Split(upperSquare) => square.value == upperSquare.value || square.value == upperSquare.value + 3
        case Bet.Street(start) => square.value >= start.value && square.value <= start.value + 2
        case Bet.CornerBet(start) => square.value >= start.value && square.value <= start.value + 4 && square.value != start.value + 2
        case Bet.Line(start) => square.value >= start.value || square.value <= start.value + 5
        case Bet.Dozen(dozen) => square.value > (dozen - 1) * 12 && square.value <= dozen * 12
        case Bet.Column(column) => square.value % 3 == column % 3
        case Bet.NumberHalf(half) => square.value > (half - 1) * 12 && square.value <= half * 12
        case Bet.ColorHalf(color) => square.color == color
      }
    }

    def play(bet: Bet, generatedSquare: Square, amount: Int): GameResult = {
      if (condition(bet, generatedSquare)) Win(amount * multiplier(bet))
      else Lose
    }
  }

  final case class Player(amount: Int, bet: Bet)

  sealed trait GameResult
  final case object Lose extends GameResult
  final case class Win(gain: Int) extends GameResult

  final case class RNG(seed: Long) {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = RNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    def nextInt(ceil: Int): (Int, RNG) = {
      val (value, rng) = nextInt
      val result = Math.abs(value) % ceil
      (result, rng)
    }
  }

  def generate(rng: RNG): Either[String, Square] = {
    val (int, _) = rng.nextInt(37)
    Square.create(int)
  }

  def runGame(players: List[Player]): List[GameResult] = {
    val generated = generate(RNG(20))
    generated match {
      case Left(_) => List()
      case Right(value) => players.map { case Player(amount, bet) => BetUtils.play(bet, value, amount) }
    }
  }

  def main(args: Array[String]): Unit = {
    println(runGame(List(Player(35, ColorHalf(Black)))))
    println(RNG(20).nextInt(37))
  }

  

}
