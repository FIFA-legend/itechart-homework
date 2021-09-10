package com.itechart.error_handling

import scala.util.Try

object ErrorHandlingLesson {

  // Exercise 1. Implement `parseIntEither` method, returning the parsed integer as `Right` upon success and
  // "{{string}} does not contain an integer" as `Left` upon failure.
  def parseIntEither(string: String): Either[String, Int] = {
    Try(string.toInt).toEither.left.map(_.toString)
  }

  final case class Student(username: String, age: Int)

  sealed trait ValidationError

  object ValidationError {
    final case object UsernameLengthIsInvalid extends ValidationError {
      override def toString: String = "Username must be between 3 and 30 characters"
    }

    final case object UsernameHasSpecialCharacters extends ValidationError {
      override def toString: String = "Username cannot contain special characters"
    }

    final case object AgeIsNotNumeric extends ValidationError {
      override def toString: String = "Age must be a number"
    }

    final case object AgeIsOutOfBounds extends ValidationError {
      override def toString: String = "Student must be of age 18 to 75"
    }
  }

  import cats.data.ValidatedNec
  import cats.syntax.all._
  import ValidationError._

  type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

  private def validateUsername(username: String): AllErrorsOr[String] = {

    def validateUsernameLength: AllErrorsOr[String] =
      if (username.length >= 3 && username.length <= 30) username.validNec
      else UsernameLengthIsInvalid.invalidNec

    def validateUsernameContents: AllErrorsOr[String] =
      if (username.matches("^[a-zA-Z]+$")) username.validNec
      else UsernameHasSpecialCharacters.invalidNec

    validateUsernameLength.productR(validateUsernameContents)
  }

  // Exercise 2. Implement `validateAge` method, so that it returns `AgeIsNotNumeric` if the age string is not
  // a number and `AgeIsOutOfBounds` if the age is not between 18 and 75. Otherwise the age should be
  // considered valid and returned inside `AllErrorsOr`.
  private def validateAge(age: String): AllErrorsOr[Int] = {
    if (Try(age.toInt).isFailure) AgeIsNotNumeric.invalidNec
    else if (age.toInt >= 18 && age.toInt <= 75) age.toInt.validNec
    else AgeIsOutOfBounds.invalidNec

    //parseIntEither(age).left.map(_ => AgeIsOutOfBounds).toValidatedNec
  }

  // `validate` method takes raw username and age values (for example, as received via POST request),
  // validates them, transforms as needed and returns `AllErrorsOr[Student]` as a result. `mapN` method
  // allows to map other N Validated instances at the same time.
  def validate(username: String, age: String): AllErrorsOr[Student] = (
    validateUsername(username),
    validateAge(age)
  ).mapN(Student)
}
