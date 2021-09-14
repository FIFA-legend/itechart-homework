package error_handling_tests

import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidationError.{
  InvalidCardNumber,
  InvalidExpirationDate,
  InvalidSecurityCode
}
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidator.{
  validateCardNumber,
  validateExpirationDate,
  validateSecurityCode
}
import org.scalatest.freespec.AnyFreeSpec

import scala.annotation.nowarn

@nowarn
class PaymentCardValidation extends AnyFreeSpec {

  "Payment Card Number validation" - {
    "valid card number: 1234-5678-8765-4321" in {
      val inputCardNumber = "1234-5678-8765-4321"
      val expected        = "1234-5678-8765-4321"

      val actual = validateCardNumber(inputCardNumber)
      assert(actual.isValid)

      actual.fold(
        e => fail(s"Doesn't expect an error: $e"),
        v => assert(v.value == expected)
      )
    }

    "invalid card number: 8888-9999-7777-5555-" in {
      val inputCardNumber = "8888-9999-7777-55XY"
      val expected        = InvalidCardNumber.invalidNec

      val actual = validateCardNumber(inputCardNumber)
      assert(actual.isInvalid)

      for {
        expectedVal <- expected
        actualVal   <- actual
      } assert(expectedVal == actualVal)
    }
  }

  "Payment Card Expiration Date validation" - {
    "valid expiration date: 06/23" in {
      val inputExpirationDate = "06/23"
      val expected            = "06/23"

      val actual = validateExpirationDate(inputExpirationDate)
      assert(actual.isValid)

      actual.fold(
        e => fail(s"Doesn't expect an error: $e"),
        v => assert(v.value == expected)
      )
    }

    "invalid expiration date: 0X/2X" in {
      val inputExpirationDate = "0X/2X"
      val expected            = InvalidExpirationDate.invalidNec

      val actual = validateExpirationDate(inputExpirationDate)
      assert(actual.isInvalid)

      for {
        expectedVal <- expected
        actualVal   <- actual
      } assert(expectedVal == actualVal)
    }

    "invalid expiration date. Is in past: 01/19" in {
      val inputExpirationDate = "01/19"
      val expected            = InvalidExpirationDate.invalidNec

      val actual = validateExpirationDate(inputExpirationDate)
      assert(actual.isInvalid)

      for {
        expectedVal <- expected
        actualVal   <- actual
      } assert(expectedVal == actualVal)
    }
  }

  "Payment Card Security Code validation" - {
    "valid security code: 505" in {
      val inputSecurityCode = "505"
      val expected          = "505"

      val actual = validateSecurityCode(inputSecurityCode)
      assert(actual.isValid)

      actual.fold(
        e => fail(s"Doesn't expect an error: $e"),
        v => assert(v.value == expected)
      )
    }

    "invalid security code: 1234" in {
      val inputSecurityCode = "1234"
      val expected          = InvalidSecurityCode.invalidNec

      val actual = validateSecurityCode(inputSecurityCode)
      assert(actual.isInvalid)

      for {
        expectedVal <- expected
        actualVal   <- actual
      } assert(expectedVal == actualVal)
    }
  }

}

class PersonValidation extends AnyFreeSpec {}
