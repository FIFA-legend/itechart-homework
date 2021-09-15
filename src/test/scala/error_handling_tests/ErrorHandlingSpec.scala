package error_handling_tests

import cats.data.{NonEmptyChain, Validated}
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidationError.{
  AgeIsNotNumeric,
  AgeIsOutOfBounds,
  DateIsNotNumeric,
  DateIsOutOfBounds,
  InvalidCardNumber,
  InvalidExpirationDate,
  InvalidOwner,
  InvalidPassportNumber,
  InvalidSecurityCode
}
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidator.{
  validate,
  validatePaymentCard,
  validatePerson
}
import com.itechart.error_handling.ErrorHandlingHomework.{
  Account,
  Age,
  CardNumber,
  ExpirationDate,
  Owner,
  PassportNumber,
  PaymentCard,
  PaymentCardDto,
  Person,
  PersonDto,
  SecurityCode
}
import org.scalatest.freespec.AnyFreeSpec
import eu.timepit.refined.auto.*

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

class PaymentCardValidationSpec extends AnyFreeSpec {

  val validCardNumber:     CardNumber     = "1111-1111-1111-1111"
  val validExpirationDate: ExpirationDate = "08/25"
  val validSecurityCode:   SecurityCode   = "102"

  "Payment Card Number validation" - {
    "valid card number: 1234-5678-8765-4321" in {
      val inputPaymentCard = PaymentCardDto("1234-5678-8765-4321", validExpirationDate, validSecurityCode)
      val expected         = PaymentCard("1234-5678-8765-4321", validExpirationDate, validSecurityCode)

      val actual = validatePaymentCard(inputPaymentCard)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid card number: 8888-9999-7777-55XY" in {
      val inputPaymentCard = PaymentCardDto("8888-9999-7777-55XY", validExpirationDate, validSecurityCode)
      val expected         = NonEmptyChain(InvalidCardNumber)

      val actual = validatePaymentCard(inputPaymentCard)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }

  "Payment Card Expiration Date validation" - {
    "valid expiration date: 06/23" in {
      val inputPaymentCard = PaymentCardDto(validCardNumber, "06/23", validSecurityCode)
      val expected         = PaymentCard(validCardNumber, "06/23", validSecurityCode)

      val actual = validatePaymentCard(inputPaymentCard)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid expiration date: 0X/2X" in {
      val inputPaymentCard = PaymentCardDto(validCardNumber, "0X/2X", validSecurityCode)
      val expected         = NonEmptyChain(InvalidExpirationDate)

      val actual = validatePaymentCard(inputPaymentCard)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }

    "invalid expiration date (is in past): 01/19" in {
      val inputPaymentCard = PaymentCardDto(validCardNumber, "01/19", validSecurityCode)
      val expected         = NonEmptyChain(InvalidExpirationDate)

      val actual = validatePaymentCard(inputPaymentCard)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }

  "Payment Card Security Code validation" - {
    "valid security code: 505" in {
      val inputPaymentCard = PaymentCardDto(validCardNumber, validExpirationDate, "505")
      val expected         = PaymentCard(validCardNumber, validExpirationDate, "505")

      val actual = validatePaymentCard(inputPaymentCard)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid security code: 1234" in {
      val inputPaymentCard = PaymentCardDto(validCardNumber, validExpirationDate, "1234")
      val expected         = NonEmptyChain(InvalidSecurityCode)

      val actual = validatePaymentCard(inputPaymentCard)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }

  "Several parameters validation" - {
    "valid several parameters" in {
      val inputPaymentCard = PaymentCardDto("9876-5432-2345-6789", "11/24", "365")
      val expected         = PaymentCard("9876-5432-2345-6789", "11/24", "365")

      val actual = validatePaymentCard(inputPaymentCard)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid several parameters" in {
      val inputPaymentCard = PaymentCardDto("XXXX-XXXX-XXXX-XXXX", "AC/DC", "XYZ")
      val expected         = NonEmptyChain(InvalidCardNumber, InvalidExpirationDate, InvalidSecurityCode)

      val actual = validatePaymentCard(inputPaymentCard)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }

}

class PersonValidationSpec extends AnyFreeSpec {

  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  val validOwner:          Owner          = "Nikita Kolodko"
  val rawAge:              String         = "20"
  val validAge:            Age            = 20
  val rawBirthday:         String         = "2001-08-05"
  val validBirthday:       LocalDate      = LocalDate.parse(rawBirthday, formatter)
  val validPassportNumber: PassportNumber = "AB1780913"

  "Card Owner validation" - {
    "valid owner: Elon Mask" in {
      val inputPerson = PersonDto("Elon Mask", rawAge, rawBirthday, validPassportNumber)
      val expected    = Person("Elon Mask", validAge, validBirthday, validPassportNumber)

      val actual = validatePerson(inputPerson)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid owner: qwerty123" in {
      val inputPerson = PersonDto("qwerty123", rawAge, rawBirthday, validPassportNumber)
      val expected    = NonEmptyChain(InvalidOwner)

      val actual = validatePerson(inputPerson)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }

  "Person age validation" - {
    "valid age: 43" in {
      val inputPerson = PersonDto(validOwner, "43", rawBirthday, validPassportNumber)
      val expected    = Person(validOwner, 43, validBirthday, validPassportNumber)

      val actual = validatePerson(inputPerson)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid age: 715" in {
      val inputPerson = PersonDto(validOwner, "715", rawBirthday, validPassportNumber)
      val expected    = NonEmptyChain(AgeIsOutOfBounds)

      val actual = validatePerson(inputPerson)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }

  "Person birthday validation" - {
    "valid birthday: 1970-01-25" in {
      val inputPerson      = PersonDto(validOwner, rawAge, "1970-01-25", validPassportNumber)
      val expectedBirthday = LocalDate.parse("1970-01-25", formatter)
      val expected         = Person(validOwner, validAge, expectedBirthday, validPassportNumber)

      val actual = validatePerson(inputPerson)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid birthday: XXX-xxx" in {
      val inputPerson = PersonDto(validOwner, rawAge, "XXX-xxx", validPassportNumber)
      val expected    = NonEmptyChain(DateIsNotNumeric)

      val actual = validatePerson(inputPerson)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }

    "invalid birthday (in future): 2077-08-09" in {
      val inputPerson = PersonDto(validOwner, rawAge, "2077-08-09", validPassportNumber)
      val expected    = NonEmptyChain(DateIsOutOfBounds)

      val actual = validatePerson(inputPerson)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }

  "Person Passport Number validation" - {
    "valid passport number: IH1735489" in {
      val inputPerson = PersonDto(validOwner, rawAge, rawBirthday, "IH1735489")
      val expected    = Person(validOwner, validAge, validBirthday, "IH1735489")

      val actual = validatePerson(inputPerson)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid passport number: KN78893655" in {
      val inputPerson = PersonDto(validOwner, rawAge, rawBirthday, "KN78893655")
      val expected    = NonEmptyChain(InvalidPassportNumber)

      val actual = validatePerson(inputPerson)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }

  "Several person parameters validation" - {
    "valid person parameters" in {
      val inputPerson = PersonDto("Lionel Messi", "34", "1987-06-24", "IH1735489")
      val expected    = Person("Lionel Messi", 34, LocalDate.parse("1987-06-24", formatter), "IH1735489")

      val actual = validatePerson(inputPerson)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid person parameters" in {
      val inputPerson = PersonDto("1233456778", "200", "XY-12-3", "VB123456789")
      val expected    = NonEmptyChain(InvalidOwner, AgeIsOutOfBounds, DateIsNotNumeric, InvalidPassportNumber)

      val actual = validatePerson(inputPerson)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }
}

class AccountValidationSpec extends AnyFreeSpec {

  "Account Validation" - {
    "valid account parameters" in {
      val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

      val inputPerson      = PersonDto("Cristiano Ronaldo", "36", "1985-02-05", "KK8937295")
      val inputPaymentCard = PaymentCardDto("1122-3344-5566-7788", "09/27", "321")
      val expected = Account(
        Person("Cristiano Ronaldo", 36, LocalDate.parse("1985-02-05", formatter), "KK8937295"),
        PaymentCard("1122-3344-5566-7788", "09/27", "321")
      )

      val actual = validate(inputPerson, inputPaymentCard)
      assert(actual.isValid)

      actual match {
        case Validated.Valid(v)   => assert(v === expected)
        case Validated.Invalid(e) => fail(s"Doesn't expect an error: $e")
      }
    }

    "invalid account parameters" in {
      val inputPerson      = PersonDto("Max Payne", "abc", "1985-04-26", "KH9913132257")
      val inputPaymentCard = PaymentCardDto("1122-3344-5566-7788-7788", "09/27", "ABC")
      val expected         = NonEmptyChain(AgeIsNotNumeric, InvalidPassportNumber, InvalidCardNumber, InvalidSecurityCode)

      val actual = validate(inputPerson, inputPaymentCard)
      assert(actual.isInvalid)

      actual match {
        case Validated.Valid(v)   => fail(s"Doesn't expect a Valid value: $v")
        case Validated.Invalid(e) => assert(e === expected)
      }
    }
  }

}
