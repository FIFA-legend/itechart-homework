package error_handling_tests

import cats.data.{NonEmptyChain, Validated}
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidationError.{
  InvalidCardNumber,
  InvalidExpirationDate,
  InvalidSecurityCode
}
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidator.validatePaymentCard
import com.itechart.error_handling.ErrorHandlingHomework.{
  CardNumber,
  ExpirationDate,
  PaymentCard,
  PaymentCardDto,
  SecurityCode
}
import org.scalatest.freespec.AnyFreeSpec
import eu.timepit.refined.auto.*

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
