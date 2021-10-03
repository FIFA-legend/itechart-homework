package tagless_final

import cats.data.NonEmptyChain
import cats.data.Validated.Valid
import cats.effect.IO
import com.itechart.tagless_final.EmployeeService
import com.itechart.tagless_final.EmployeeValidator.EmployeeValidationError.{
  BirthdayIsOutOfBounds,
  InvalidName,
  InvalidSurname
}
import com.itechart.tagless_final.employee.Employee
import org.scalatest.freespec.AnyFreeSpec
import eu.timepit.refined.auto._

import java.time.LocalDate
import java.time.format.DateTimeFormatter

class EmployeeServiceIT extends AnyFreeSpec {

  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  "EmployeeService core tests" - {
    "all()" in {
      val service      = prepareService
      val expectedSize = 2

      for {
        employees <- service
        result    <- employees.all
      } yield assert(expectedSize == result.size)
    }

    "create()" in {
      val service = prepareService
      val expectedValid =
        Valid(Employee(2, LocalDate.parse("1985-02-17", formatter), "Cristiano", "Ronaldo", 30000000, "Striker"))
      val expectedInvalid = NonEmptyChain(InvalidName, InvalidSurname, BirthdayIsOutOfBounds)

      for {
        employees <- service
        result    <- employees.create("1985-02-17", "Cristiano", "Ronaldo", "30000000", "Striker")
      } yield assert(expectedValid == result)

      for {
        employees <- service
        result    <- employees.create("2020-01-01", "qwerty123", "qwerty123", "30000000", "Striker")
      } yield assert(expectedInvalid == result)
    }

    "update()" in {
      val service = prepareService
      val expectedValid =
        Valid(Employee(2, LocalDate.parse("1985-02-17", formatter), "Cristiano", "Ronaldo", 30000000, "Striker"))

      for {
        employees <- service
        result    <- employees.update(2, "1985-02-17", "Cristiano", "Ronaldo", "30000000", "Striker")
      } yield assert(expectedValid == result)
    }

    "get()" in {
      val service = prepareService
      val expectedById2 =
        Some(Employee(2, LocalDate.parse("1987-06-24", formatter), "Lionel", "Messi", 35000000, "Capitan"))
      val expectedById3 = None

      for {
        employees <- service
        result    <- employees.find(2)
      } yield assert(expectedById2 == result)

      for {
        employees <- service
        result    <- employees.find(3)
      } yield assert(expectedById3 == result)
    }

    "delete()" in {
      val service                   = prepareService
      val expectedAfterRemovalById1 = 1
      val expectedAfterRemovalById3 = 1

      for {
        employees <- service
        bool      <- employees.delete(1)
        all       <- employees.all
      } yield assert(bool && all.size == expectedAfterRemovalById1)

      for {
        employees <- service
        bool      <- employees.delete(3)
        all       <- employees.all
      } yield assert(!bool && all.size == expectedAfterRemovalById3)
    }

  }

  def prepareService: IO[EmployeeService[IO]] = {
    for {
      service <- EmployeeService.of[IO]
      _       <- service.create("2001-08-05", "Nikita", "Kolodko", "1000", "Position1")
      _       <- service.create("1987-06-24", "Lionel", "Messi", "35000000", "Capitan")
    } yield service
  }

}
