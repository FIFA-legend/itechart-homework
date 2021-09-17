package cats_tests

import com.itechart.cats.Cats
import org.scalatest.freespec.AnyFreeSpec

class MonadTestsSpec extends AnyFreeSpec {

  "Monad tests" - {
    "List Monad pure() test" in {
      val input    = 20
      val expected = List(20)

      val actual = Cats.listM.pure(input)

      assert(actual == expected)
    }

    "List Monad flatmap() test" in {
      val input    = List(1, 2, 3, 4, 5)
      val expected = List(10, 20, 30, 40, 50)

      val actual = Cats.listM.flatMap(input)(element => List(element * 10))

      assert(actual == expected)
    }

    "Option Monad pure() test" in {
      val input    = "Name"
      val expected = Some("Name")

      val actual = Cats.optionM.pure(input)

      actual match {
        case s @ Some(_) => assert(s == expected)
        case None => fail("Doesn't expect None")
      }
    }

    "Option Monad flatmap() test" in {
      val someInput    = Some("Name")
      val someExpected = Some("Some Name")
      val noneInput    = None
      val noneExpected = None

      val someActual = Cats.optionM.flatMap(someInput)(s => Option(f"Some $s"))
      val noneActual = Cats.optionM.flatMap(noneInput)(n => Option(f"Some $n"))

      assert(someActual == someExpected)
      assert(noneActual == noneExpected)
    }
  }

}
