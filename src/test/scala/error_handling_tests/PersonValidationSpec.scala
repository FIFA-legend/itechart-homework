package error_handling_tests

import cats.data.{NonEmptyChain, Validated}
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidationError.{
  AgeIsOutOfBounds,
  DateIsNotNumeric,
  DateIsOutOfBounds,
  InvalidOwner,
  InvalidPassportNumber
}
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidator.validatePerson
import com.itechart.error_handling.ErrorHandlingHomework.{Age, Owner, PassportNumber, Person, PersonDto}
import org.scalatest.freespec.AnyFreeSpec
import eu.timepit.refined.auto._

import java.time.LocalDate
import java.time.format.DateTimeFormatter

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
