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

    /**
     * Amount of years since the invention of the
     * hyper-drive technology (we are certainly in negative values at the moment).
     */
    case class HDEYears(value: Long)

    def secondBiggestValue[T](values: Seq[T])(implicit ord: Ordering[T]): Option[T] = {
      val option = values.sorted
        .zipWithIndex
        .find { case (_, index) => index == 1 }
      option match {
        case Some((t, _)) => Some(t)
        case None => None
      }
    }

    object instances {
      implicit val intOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)

      implicit val hdeOrdering: Ordering[HDEYears] = Ordering.fromLessThan(_.value > _.value)
    }
  }

  object Exercise3_2 {

    /**
     * Custom number type!
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
          case None => s
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

    implicit val intSummable: Summable[Int] = new Summable[Int] {
      override def plus(left: Int, right: Int): Int = left + right

      override def zero: Int = 0
    }

    implicit val shortSummable: Summable[Short] = new Summable[Short] {
      override def plus(left: Short, right: Short): Short = (left + right).toShort

      override def zero: Short = 0.toShort
    }

    implicit val byteSummable: Summable[Byte] = new Summable[Byte] {
      override def plus(left: Byte, right: Byte): Byte = (left + right).toByte

      override def zero: Byte = 0.toByte
    }

    implicit val charSummable: Summable[Char] = new Summable[Char] {
      override def plus(left: Char, right: Char): Char = (left + right).toChar

      override def zero: Char = 0.toChar
    }

    implicit val longSummable: Summable[Long] = new Summable[Long] {
      override def plus(left: Long, right: Long): Long = left + right

      override def zero: Long = 0L
    }

    implicit val floatSummable: Summable[Float] = new Summable[Float] {
      override def plus(left: Float, right: Float): Float = left + right

      override def zero: Float = 0.0f
    }

    implicit val doubleSummable: Summable[Double] = new Summable[Double] {
      override def plus(left: Double, right: Double): Double = left + right

      override def zero: Double = 0.0
    }

    implicit val bigIntSummable: Summable[BigInt] = new Summable[BigInt] {
      override def plus(left: BigInt, right: BigInt): BigInt = left + right

      override def zero: BigInt = BigInt(0)
    }

    implicit val bigDecimalSummable: Summable[BigDecimal] = new Summable[BigDecimal] {
      override def plus(left: BigDecimal, right: BigDecimal): BigDecimal = left + right

      override def zero: BigDecimal = BigDecimal(0.0)
    }

    implicit def setSummable[T](): Summable[Set[T]] = new Summable[Set[T]] {
      override def plus(left: Set[T], right: Set[T]): Set[T] = left ++ right

      override def zero: Set[T] = Set.empty
    }

    //implicit val setIntSummable: Summable[Set[Int]] = setSummable()

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
    println(genericSum(Triple(Set(1), Set(2), Set(3))))
  }

}
