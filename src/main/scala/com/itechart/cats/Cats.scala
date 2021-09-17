package com.itechart.cats

import cats.implicits.catsSyntaxEitherId

object Cats {

  trait CustomMonad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(f.andThen(pure))
  }

  /** Ex 5.1 implement CustomMonad for List
    */
  val listM: CustomMonad[List] = new CustomMonad[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = {
      fa match {
        case Nil          => Nil
        case head :: tail => f(head) ++ flatMap(tail)(f)
      }
    }
  }

  /** Ex 5.2 implement CustomMonad for Option
    */
  val optionM: CustomMonad[Option] = new CustomMonad[Option] {
    override def pure[A](a: A): Option[A] = Option(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = {
      fa match {
        case None        => None
        case Some(value) => f(value)
      }
    }
  }

  /** Ex 7.0 implement traverse function for Option
    */
  def optionTraverse[A](input: List[Option[A]]): Option[List[A]] = {
    input.foldLeft(Option(List.empty[A]))((acc, option) =>
      for {
        list  <- acc
        value <- option
      } yield list ::: List(value)
    )
  }

  /** Ex 7.1 implement traverse for Either. Use fail fast approach (the first error encountered is returned.)
    */
  def eitherTraverse[E, A](input: List[Either[E, A]]): Either[E, List[A]] = {
    input.foldLeft(List.empty[A].asRight[E])((acc, either) =>
      for {
        list  <- acc
        value <- either
      } yield list ::: List(value)
    )
  }

  def main(args: Array[String]): Unit = {}

}
