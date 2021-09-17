package error_handling_tests

import cats.data.{NonEmptyChain, Validated}
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidationError.{
  AgeIsNotNumeric,
  InvalidCardNumber,
  InvalidPassportNumber,
  InvalidSecurityCode
}
import com.itechart.error_handling.ErrorHandlingHomework.AccountValidator.validate
import com.itechart.error_handling.ErrorHandlingHomework.{Account, PaymentCard, PaymentCardDto, Person, PersonDto}
import org.scalatest.freespec.AnyFreeSpec
import eu.timepit.refined.auto._

import java.time.LocalDate
import java.time.format.DateTimeFormatter

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
