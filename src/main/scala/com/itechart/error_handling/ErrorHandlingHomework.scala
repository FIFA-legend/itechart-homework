package com.itechart.error_handling

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidationError.*
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval.Closed
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.api.RefType

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.Try

object ErrorHandlingHomework {

  type Owner = String Refined MatchesRegex[W.`"^([A-Za-z]+ ?){2}$"`.T]
  type Age = Int Refined Closed[W.`18`.T, W.`100`.T]
  type PassportNumber = String Refined MatchesRegex[W.`"^[A-Z]{2}[0-9]{7}$"`.T]

  type CardNumber = String Refined MatchesRegex[W.`"^([0-9]{4}-?){4}$"`.T]
  type ExpirationDate = String Refined MatchesRegex[W.`"^(0[1-9]|1[1-2])/(2[1-9])$"`.T]
  type SecurityCode = String Refined MatchesRegex[W.`"^[0-9]{3}$"`.T]

  final case class Account(person: Person, card: PaymentCard)
  final case class Person(owner: Owner, age: Age, birthDay: LocalDate, passportNumber: PassportNumber) // birthDay
  final case class PaymentCard(cardNumber: CardNumber, expirationDate: ExpirationDate, securityCode: SecurityCode)

  final case class PersonDto(owner: String, age: String, birthDay: String, passportNumber: String)
  final case class PaymentCardDto(cardNumber: String, expirationDate: String, securityCode: String)

  sealed trait AccountValidationError
  object AccountValidationError {
    final case object InvalidOwner extends AccountValidationError {
      override def toString: String = "Owner's name and surname are required"
    }

    final case object AgeIsNotNumeric extends AccountValidationError {
      override def toString: String = "Owner's age must be a number"
    }

    final case object AgeIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Owner's age must be between 18 to 100"
    }

    final case object DateIsNotNumeric extends AccountValidationError {
      override def toString: String = "Owner's birthday must be in format: yyyy-mm-dd"
    }

    final case object DateIsOutOfBounds extends AccountValidationError {
      override def toString: String = "Owner's birthday must be between: 1970-01-01 & 2004-01-01"
    }

    final case object InvalidPassportNumber extends AccountValidationError {
      override def toString: String = "Passport number must have format: CCNNNNNNN"
    }

    final case object InvalidCardNumber extends AccountValidationError {
      override def toString: String = "Card number must have format: XXXX-XXXX-XXXX-XXXX"
    }

    final case object InvalidExpirationDate extends AccountValidationError {
      override def toString: String = "Expiration date must have format: XX/XX"
    }

    final case object InvalidSecurityCode extends AccountValidationError {
      override def toString: String = "Security code must have format: XXX"
    }
  }

  object AccountValidator {

    type AllErrorsOr[A] = ValidatedNec[AccountValidationError, A]

    def validatePerson(personDto: PersonDto): AllErrorsOr[Person] = {
      def validateOwner(rawOwner: String): AllErrorsOr[Owner] = {
        RefType.applyRef[Owner](rawOwner)
          .left.map(_ => InvalidOwner)
          .toValidatedNec
      }

      def validateAge(rawAge: String): AllErrorsOr[Age] = {
        if (Try(rawAge.toInt).isFailure) AgeIsNotNumeric.invalidNec
        else RefType.applyRef[Age](rawAge.toInt)
          .left.map(_ => AgeIsOutOfBounds)
          .toValidatedNec
      }

      def validatePassportNumber(rawPassportNumber: String): AllErrorsOr[PassportNumber] = {
        RefType.applyRef[PassportNumber](rawPassportNumber)
          .left.map(_ => InvalidPassportNumber)
          .toValidatedNec
      }

      def validateBirthDay(rawBirthDay: String): AllErrorsOr[LocalDate] = {
        val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
        if (Try(LocalDate.parse(rawBirthDay, formatter)).isFailure) DateIsNotNumeric.invalidNec
        else {
          val birthday = LocalDate.parse(rawBirthDay, formatter)
          if (birthday.isAfter(LocalDate.parse("1920-01-01", formatter)) &&
          birthday.isBefore(LocalDate.parse("2004-01-01", formatter))) birthday.validNec
          else DateIsOutOfBounds.invalidNec
        }
      }

      (
        validateOwner(personDto.owner),
        validateAge(personDto.age),
        validateBirthDay(personDto.birthDay),
        validatePassportNumber(personDto.passportNumber)
      ).mapN(Person)
    }

    def validatePaymentCard(cardDto: PaymentCardDto): AllErrorsOr[PaymentCard] = {
      def validateCardNumber(rawCardNumber: String): AllErrorsOr[CardNumber] = {
        RefType.applyRef[CardNumber](rawCardNumber)
          .left.map(_ => InvalidCardNumber)
          .toValidatedNec
      }

      def validateExpirationDate(rawExpirationDate: String): AllErrorsOr[ExpirationDate] = {
        RefType.applyRef[ExpirationDate](rawExpirationDate)
          .left.map(_ => InvalidExpirationDate)
          .toValidatedNec
      }

      def validateSecurityCode(rawSecurityCode: String): AllErrorsOr[SecurityCode] = {
        RefType.applyRef[SecurityCode](rawSecurityCode)
          .left.map(_ => InvalidSecurityCode)
          .toValidatedNec
      }

      (
        validateCardNumber(cardDto.cardNumber),
        validateExpirationDate(cardDto.expirationDate),
        validateSecurityCode(cardDto.securityCode)
      ).mapN(PaymentCard)
    }

    def validate(personDto: PersonDto, cardDto: PaymentCardDto): AllErrorsOr[Account] = {
      (validatePerson(personDto), validatePaymentCard(cardDto)).mapN(Account)
    }
  }

  def main(args: Array[String]): Unit = {

  }

}
