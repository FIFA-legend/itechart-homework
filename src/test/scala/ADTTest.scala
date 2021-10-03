import com.itechart.adt.ADT.Bet.{ColorHalf, Split, StraightUp, Street}
import com.itechart.adt.ADT._
import org.scalatest.flatspec.AnyFlatSpec

class ADTTest extends AnyFlatSpec {

  "A generator" should "generate number according to its seed" in {
    val (int, _) = RNG(20).nextInt(37)
    assert(int === 14)
  }

  "A test" should "check square creation" in {
    val square1 = Square.create(20)
    val square2 = Square.create(39)
    assert(square1.isRight)
    assert(square2.isLeft)
  }

  "This test" should "check Straight Up work" in {
    val bet1      = Square.create(14)
    val bet2      = Square.create(17)
    val generated = generate(RNG(20))
    (bet1, bet2, generated) match {
      case (Right(value1), Right(value2), Right(value3)) =>
        assert(BetUtils.play(StraightUp(value1), value3, 25) === Win(900))
        assert(BetUtils.play(StraightUp(value2), value3, 25) === Lose)
      case _ =>
    }
  }

  "This test" should "check Split work" in {
    val bet1      = Square.create(11)
    val bet2      = Square.create(17)
    val generated = generate(RNG(20))
    (bet1, bet2, generated) match {
      case (Right(value1), Right(value2), Right(value3)) =>
        assert(BetUtils.play(Split(value1), value3, 25) === Win(450))
        assert(BetUtils.play(Split(value2), value3, 25) === Lose)
      case _ =>
    }
  }

  "This test" should "check Street work" in {
    val bet1      = Square.create(13)
    val bet2      = Square.create(17)
    val generated = generate(RNG(20))
    (bet1, bet2, generated) match {
      case (Right(value1), Right(value2), Right(value3)) =>
        assert(BetUtils.play(Street(value1), value3, 25) === Win(300))
        assert(BetUtils.play(Street(value2), value3, 25) === Lose)
      case _ =>
    }
  }

  "This test" should "check Color work" in {
    val generated = generate(RNG(20))
    generated match {
      case Right(value) =>
        assert(BetUtils.play(ColorHalf(Red), value, 25) === Win(50))
        assert(BetUtils.play(ColorHalf(Black), value, 25) === Lose)
      case _ =>
    }
  }

}
