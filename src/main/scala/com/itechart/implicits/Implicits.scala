package com.itechart.implicits

object Implicits {

  object Exercise2 {
    trait Show[-T] {
      def apply(value: T): String
    }

    def show[T](value: T)(implicit show: Show[T]): String = {
      show.apply(value)
    }

    object syntax {
      implicit class ShowOps[T: Show](inner: T) {
        def show: String = Exercise2.show(inner).reverse
      }
    }

    object instances {
      implicit val intShow: Show[Int] = (value: Int) => value.toString

      implicit val stringShow: Show[String] = (value: String) => value

      implicit def seqShow[T: Show]: Show[Seq[T]] = (value: Seq[T]) => value.map(show(_)).mkString("(", ",", ")")
    }

    object ShowWork {
      def examples(): Unit = {
        import Exercise2.syntax._
        import Exercise2.instances._

        println(20.show)
        println("Nikita".show)
        println(Seq(17, 20, 23, 26).show)
        println(Seq("Nikita", "Hello", "World").show)
      }
    }
  }

  /*
  Exercise 3.

  There are some type-classes in Scala standard library.

  Let's get to know them better!
   */
  object Exercise3_1 {

    /** Amount of years since the invention of the
      * hyper-drive technology (we are certainly in negative values at the moment).
      */
    case class HDEYears(value: Long)

    def secondBiggestValue[T](values: Seq[T])(implicit ord: Ordering[T]): Option[T] = {
      if (values.length < 2) None
      else
        Some(
          values.sorted.tail.head
        )
    }

    object instances {
      implicit val intOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)

      implicit val hdeOrdering: Ordering[HDEYears] = Ordering.fromLessThan(_.value > _.value)
    }
  }

  object Exercise3_2 {

    /** Custom number type!
      * For now it just wraps a Float but more interesting stuff could come in the future, who knows...
      */
    case class CustomNumber(value: Float)

    trait Summable[T] {
      def sum(a: T, b: T): T
    }

    object instances {
      implicit val intSummable: Summable[Int] = (a: Int, b: Int) => a + b

      implicit val customNumberSummable: Summable[CustomNumber] =
        (a: CustomNumber, b: CustomNumber) => CustomNumber(a.value + b.value)
    }

    def sum[T](values: Seq[T])(implicit summable: Summable[T]): Option[T] = values.reduceOption(summable.sum)
  }

  object Exercise4 {
    /*
    Generic foldLeft!

    F[_] - type constructor with a single type argument, like List[T], Option[T], etc.

    Types which are parameterized using type constructors called higher-kinded types (HKT)
    Foldable here is a HKT
     */
    trait Foldable[F[_]] {
      def foldLeft[T, S](ft: F[T], s: S)(f: (S, T) => S): S
    }

    implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
      override def foldLeft[T, S](option: Option[T], s: S)(f: (S, T) => S): S =
        option match {
          case None    => s
          case Some(t) => f(s, t)
        }
    }
    implicit val listFoldable: Foldable[List] = new Foldable[List] {
      override def foldLeft[T, S](list: List[T], s: S)(f: (S, T) => S): S = {
        list.foldLeft(s)(f)
      }
    }

    case class Triple[T](v1: T, v2: T, v3: T)

    implicit val tripleFoldable: Foldable[Triple] = new Foldable[Triple] {
      override def foldLeft[T, S](triple: Triple[T], s: S)(f: (S, T) => S): S = {
        f(f(f(s, triple.v1), triple.v2), triple.v3)
      }
    }

    trait Summable[T] {
      def plus(left: T, right: T): T

      def zero: T
    }

    implicit def numericSummable[T](implicit numeric: Numeric[T]): Summable[T] = new Summable[T] {
      override def plus(left: T, right: T): T = numeric.plus(left, right)

      override def zero: T = numeric.zero
    }

    implicit def setSummable[T](): Summable[Set[T]] = new Summable[Set[T]] {
      override def plus(left: Set[T], right: Set[T]): Set[T] = left ++ right

      override def zero: Set[T] = Set.empty
    }

    def genericSum[T[_], A](collection: T[A])(implicit foldable: Foldable[T], summable: Summable[A]): A = {
      foldable.foldLeft(collection, summable.zero)(summable.plus)
    }
  }

  def main(args: Array[String]): Unit = {
    Exercise2.ShowWork.examples()
    import Exercise3_1._
    import Exercise3_1.instances._
    println(secondBiggestValue(Seq(1, 2, 3, 4, 5)))
    println(secondBiggestValue(Seq(HDEYears(1), HDEYears(2), HDEYears(3), HDEYears(4))))
    println(secondBiggestValue(Seq(HDEYears(1))))
    import Exercise3_2._
    import Exercise3_2.instances._
    println(sum(Seq(1, 2, 43, 17)))
    println(sum(Seq(CustomNumber(17.2f), CustomNumber(35.2f), CustomNumber(14.2f))))
    import Exercise4._
    implicit val setIntSummable: Exercise4.Summable[Set[Int]] = Exercise4.setSummable()
    println(genericSum(List(1, 2, 3, 4, 5, 6)))
  }

}
