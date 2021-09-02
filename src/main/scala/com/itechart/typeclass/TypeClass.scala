package com.itechart.typeclass

object TypeClass {

  object TypeClassTask {

    trait HashCode[T] {
      def hash(value: T): Int
    }

    object HashCode {
      def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
    }

    object syntax {
      implicit class HashCodeSyntax[A](value: A) {
        def hash(implicit hashCode: HashCode[A]): Int = hashCode.hash(value)
      }
    }

    object instances {
      implicit val intHashcode: HashCode[Int] = (value: Int) => value.hashCode

      implicit val stringHashCode: HashCode[String] = (value: String) => value.hashCode

      implicit def listHashCode[T](implicit instance: HashCode[T]): HashCode[List[T]] = (list: List[T]) => {
        import syntax._
        list.map(_.hash).sum
      }
    }
  }

  def main(args: Array[String]): Unit = {
    import TypeClassTask.syntax._
    import TypeClassTask.instances._
    println("abc".hash)
    println(List("abc", "def", "ijk").hash)
    println(List(13, 15, 17, 18).hash)
  }

}
