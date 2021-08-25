import com.itechart.adt.ADT.Bet.{ColorHalf, Split, StraightUp, Street}
import com.itechart.adt.ADT.{Black, Lose, RNG, Red, Square, Win, generate}
import org.scalatest.FunSuite

class ADTTest extends FunSuite {

  test("RNG Test") {
    val (int, _) = RNG(20).nextInt(37)
    assert(int === 14)
  }

  test("Square creation") {
    val square1 = Square.create(20)
    val square2 = Square.create(39)
    assert(square1.isRight)
    assert(square2.isLeft)
  }

  test("Straight up test") {
    val bet1 = Square.create(14)
    val bet2 = Square.create(17)
    val generated = generate(RNG(20))
    (bet1, bet2, generated) match {
      case (Right(value1), Right(value2), Right(value3)) => {
        assert(StraightUp(value1).play(value3, 25) === Win(900))
        assert(StraightUp(value2).play(value3, 25) === Lose)
      }
      case _ =>
    }
  }

  test("Split test") {
    val bet1 = Square.create(11)
    val bet2 = Square.create(17)
    val generated = generate(RNG(20))
    (bet1, bet2, generated) match {
      case (Right(value1), Right(value2), Right(value3)) => {
        assert(Split(value1).play(value3, 25) === Win(450))
        assert(Split(value2).play(value3, 25) === Lose)
      }
      case _ =>
    }
  }

  test("Street test") {
    val bet1 = Square.create(13)
    val bet2 = Square.create(17)
    val generated = generate(RNG(20))
    (bet1, bet2, generated) match {
      case (Right(value1), Right(value2), Right(value3)) => {
        assert(Street(value1).play(value3, 25) === Win(300))
        assert(Street(value2).play(value3, 25) === Lose)
      }
      case _ =>
    }
  }

  test("Color test") {
    val generated = generate(RNG(20))
    generated match {
      case Right(value) => {
        assert(ColorHalf(Red).play(value, 25) === Win(50))
        assert(ColorHalf(Black).play(value, 25) === Lose)
      }
      case _ =>
    }
  }

}
