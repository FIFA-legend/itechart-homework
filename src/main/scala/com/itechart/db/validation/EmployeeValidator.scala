package com.itechart.db.validation

import cats.data.ValidatedNec
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.syntax.all._
import com.itechart.db.validation.errors.EmployeeValidationError._
import com.itechart.db.entity.Entities.{Name, Position, Salary, Surname}
import com.itechart.db.validation.errors.EmployeeValidationError
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.refineV

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try

object EmployeeValidator {

  type AllErrorsOr[A] = ValidatedNec[EmployeeValidationError, A]

  def validateParameter[T, P](
    parameter: T,
    error:     EmployeeValidationError
  )(
    implicit v: Validate[T, P]
  ): AllErrorsOr[Refined[T, P]] = {
    refineV(parameter).left
      .map(_ => error)
      .toValidatedNec
  }

  private def validateName(name: String): AllErrorsOr[Name] = {
    validateParameter(name, InvalidName)
  }

  private def validateSurname(surname: String): AllErrorsOr[Surname] = {
    validateParameter(surname, InvalidSurname)
  }

  private def validateSalary(salary: String): AllErrorsOr[Salary] = {
    if (Try(salary.toInt).isFailure) InvalidSalary.invalidNec
    else validateParameter(salary.toInt, SalaryIsOutOfBounds)
  }

  private def validatePosition(position: String): AllErrorsOr[Position] = {
    validateParameter(position, InvalidPosition)
  }

  private def validateBirthday(date: String): AllErrorsOr[LocalDate] = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    if (Try(LocalDate.parse(date, formatter)).isFailure) InvalidBirthday.invalidNec
    else {
      val birthday = LocalDate.parse(date, formatter)
      if (
        birthday.isAfter(LocalDate.parse("1930-01-01", formatter)) &&
        birthday.isBefore(LocalDate.parse("2004-01-01", formatter))
      ) birthday.validNec
      else BirthdayIsOutOfBounds.invalidNec
    }
  }

  def validate(
    firstName: String,
    lastName:  String,
    position:  String,
    salary:    String,
    birthday:  String
  ): AllErrorsOr[(Name, Surname, Position, Salary, LocalDate)] = {
    (
      validateName(firstName),
      validateSurname(lastName),
      validatePosition(position),
      validateSalary(salary),
      validateBirthday(birthday)
    )
      .mapN((_, _, _, _, _))
  }

}
