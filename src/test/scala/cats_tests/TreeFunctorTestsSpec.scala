package cats_tests

import com.itechart.cats.Cats
import com.itechart.cats.Cats.{Branch, Leaf}
import org.scalatest.freespec.AnyFreeSpec

class TreeFunctorTestsSpec extends AnyFreeSpec {

  "Tree map() tests" - {
    "Tree map() leaf test" in {
      val input    = Leaf(20)
      val expected = Leaf(40)

      val actual = Cats.treeFunctor.map(input)(_ * 2)

      assert(actual == expected)
    }

    "Tree map() branch test" in {
      val input    = Branch(50, Branch(25, Leaf(10), Leaf(40)), Branch(75, Leaf(60), Leaf(90)))
      val expected = Branch(100, Branch(50, Leaf(20), Leaf(80)), Branch(150, Leaf(120), Leaf(180)))

      val actual = Cats.treeFunctor.map(input)(_ * 2)

      assert(actual == expected)
    }
  }

}
