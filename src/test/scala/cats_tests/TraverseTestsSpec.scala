package cats_tests

import com.itechart.cats.Cats
import org.scalatest.freespec.AnyFreeSpec

class TraverseTestsSpec extends AnyFreeSpec {

  "Option traverse tests" - {
    "Option traverse valid value" in {
      val input    = List(Option(10), Option(20), Option(30))
      val expected = Option(List(10, 20, 30))

      val actual = Cats.optionTraverse(input)

      assert(actual == expected)
    }

    "Option traverse invalid value" in {
      val input    = List(Option(10), Option(20), Option(30), None)
      val expected = None

      val actual = Cats.optionTraverse(input)

      assert(actual == expected)
    }
  }

  "Either traverse tests" - {
    "Either traverse valid value" in {
      val input    = List(Right(10), Right(20), Right(30))
      val expected = Right(List(10, 20, 30))

      val actual = Cats.eitherTraverse(input)

      assert(actual == expected)
    }

    "Either traverse invalid value" in {
      val input    = List(Right(10), Left("Some Error"), Right(30))
      val expected = Left("Some Error")

      val actual = Cats.eitherTraverse(input)

      assert(actual == expected)
    }

    "Either traverse fail-fast" in {
      val input    = List(Left("First Error"), Right(10), Left("Second Error"), Right(30))
      val expected = Left("First Error")

      val actual = Cats.eitherTraverse(input)

      assert(actual == expected)
    }
  }

}
