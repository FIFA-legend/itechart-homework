package com.itechart.tagless_final

import cats.data.{Kleisli, OptionT, ValidatedNec}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.syntax.all._
import cats.{Applicative, MonadError}
import com.itechart.tagless_final.EmployeeValidator.AllErrorsOr
import com.itechart.tagless_final.EmployeeValidator.EmployeeValidationError._
import com.itechart.tagless_final.employee._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.{refineV, W}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.io.StdIn
import scala.util.Try

object employee {

  type Name     = String Refined MatchesRegex[W.`"^[A-Za-z]+$"`.T]
  type Surname  = String Refined MatchesRegex[W.`"^[A-Za-z]+$"`.T]
  type Position = NonEmptyString
  type Salary   = Int Refined Greater[W.`200`.T]

  final case class Employee(
    id:        Long,
    birthday:  LocalDate,
    firstName: Name,
    lastName:  Surname,
    salary:    Salary,
    position:  Position
  )

}

object EmployeeValidator {
  sealed trait EmployeeValidationError extends Throwable

  object EmployeeValidationError {
    final case object InvalidName extends EmployeeValidationError {
      override def toString: String = "Employee's name must not be empty and must contain characters a-z"
    }

    final case object InvalidSurname extends EmployeeValidationError {
      override def toString: String = "Employee's surname must not be empty and must contain characters a-z"
    }

    final case object InvalidBirthday extends EmployeeValidationError {
      override def toString: String = "Employee's birthday must be in format: yyyy-mm-dd"
    }

    final case object BirthdayIsOutOfBounds extends EmployeeValidationError {
      override def toString: String = "Employee's birthday must be between 2004-01-01 and 1930-01-01"
    }

    final case object InvalidSalary extends EmployeeValidationError {
      override def toString: String = "Employee's salary must be a number"
    }

    final case object SalaryIsOutOfBounds extends EmployeeValidationError {
      override def toString: String = "Employee's salary must be greater than 200"
    }

    final case object InvalidPosition extends EmployeeValidationError {
      override def toString: String = "Employee's position must not be empty"
    }
  }

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

trait EmployeeService[F[_]] {
  def all: F[Map[Long, Employee]]
  def create(
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    position:  String
  ): F[AllErrorsOr[Employee]]
  def update(
    id:        Long,
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    position:  String
  ): F[AllErrorsOr[Boolean]]
  def find(id:   Long): F[Option[Employee]]
  def delete(id: Long): F[Boolean]
}

object EmployeeService {

  def of[F[_]: Sync]: F[EmployeeService[F]] = {
    for {
      counter   <- Ref.of[F, Long](0)
      employees <- Ref.of[F, Map[Long, Employee]](Map.empty)
    } yield new InMemoryEmployeeService(counter, employees)
  }

  final private class InMemoryEmployeeService[F[_]: Sync](
    counter:   Ref[F, Long],
    employees: Ref[F, Map[Long, Employee]]
  ) extends EmployeeService[F] {
    override def all: F[Map[Long, Employee]] = employees.get

    override def create(
      birthday:  String,
      firstName: String,
      lastName:  String,
      salary:    String,
      position:  String
    ): F[AllErrorsOr[Employee]] = {
      EmployeeValidator.validate(firstName, lastName, position, salary, birthday).traverse {
        case (firstName, lastName, position, salary, date) =>
          for {
            id      <- counter.updateAndGet(_ + 1)
            employee = Employee(id, date, firstName, lastName, salary, position)
            _       <- employees.update(_.updated(id, employee))
          } yield employee
      }
    }

    override def update(
      id:        Long,
      birthday:  String,
      firstName: String,
      lastName:  String,
      salary:    String,
      position:  String
    ): F[AllErrorsOr[Boolean]] = {
      EmployeeValidator
        .validate(
          firstName,
          lastName,
          position,
          salary,
          birthday
        )
        .traverse { case (firstName, lastName, position, salary, date) =>
          employees.modify { map =>
            val employee = Employee(id, date, firstName, lastName, salary, position)
            if (map.contains(id)) (map.updated(id, employee), true)
            else (map, false)
          }
        }
    }

    override def find(id: Long): F[Option[Employee]] = employees.get.map(_.get(id))

    override def delete(id: Long): F[Boolean] = {
      employees.modify { map =>
        (map.removed(id), map.contains(id))
      }
    }
  }
}

trait Console[F[_]] {
  def writeLine(line: String): F[Unit]
  def readLine: F[String]
}

object Console {

  def apply[F[_]: Console](implicit console: Console[F]): Console[F] = console

  implicit def console[F[_]: Sync]: Console[F] = new Console[F] {
    override def writeLine(line: String): F[Unit] = Sync[F].delay(println(line))
    override def readLine: F[String] = Sync[F].delay(StdIn.readLine())
  }

}

trait ConsoleInterface[F[_]] {
  def cmd: F[Unit]
}

object ConsoleInterface {

  def apply[F[_]: Console: MonadError[*[_], Throwable]](employeeService: EmployeeService[F]): ConsoleInterface[F] =
    new ConsoleInterface[F] {
      override def cmd: F[Unit] = {
        val operation = for {
          _ <- Console[F].writeLine(
            """Commands:
              |1. all
              |2. create [name] [surname] [salary (greater than 100)] [position] [birthday (yyyy-mm-dd)]
              |3. update [id] [name] [surname] [salary (greater than 100)] [position] [birthday (yyyy-mm-dd)]
              |4. get [id]
              |5. delete [id]""".stripMargin
          )
          command     <- Console[F].readLine
          args         = command.split("\\s+").toList
          result      <- route(employeeService).apply(args).value
          resultString = result.getOrElse("Choose correct command")
          _           <- Console[F].writeLine(resultString)
        } yield ()

        operation.handleErrorWith { error =>
          Console[F].writeLine(s"ERROR: ${error.getMessage}")
        } >> cmd
      }
    }

  def route[F[_]: Console: MonadError[*[_], Throwable]](
    employeeService: EmployeeService[F]
  ): Kleisli[OptionT[F, *], List[String], String] = Kleisli[OptionT[F, *], List[String], String] {
    case "all" :: Nil                   => allEmployees(employeeService)
    case "create" :: employeeParameters => createEmployee(employeeService).apply(employeeParameters)
    case "update" :: employeeParameters => updateEmployee(employeeService).apply(employeeParameters)
    case "get" :: id :: Nil             => getEmployee(employeeService).apply(id)
    case "delete" :: id :: Nil          => deleteEmployee(employeeService).apply(id)
    case _                              => OptionT.none[F, String]
  }

  private def allEmployees[F[_]: Applicative](employeeService: EmployeeService[F]): OptionT[F, String] = {
    OptionT.liftF {
      for {
        employees <- employeeService.all
      } yield employees.values.toString
    }
  }

  private def createEmployee[F[_]: Applicative](
    employeeService: EmployeeService[F]
  ): Kleisli[OptionT[F, *], List[String], String] = Kleisli[OptionT[F, *], List[String], String] {
    case name :: surname :: salary :: position :: birthday :: _ =>
      OptionT.liftF {
        for {
          employee <- employeeService.create(birthday, name, surname, salary, position)
        } yield employee.toString
      }
    case _ => OptionT.none
  }

  private def updateEmployee[F[_]: Applicative](
    employeeService: EmployeeService[F]
  ): Kleisli[OptionT[F, *], List[String], String] = Kleisli[OptionT[F, *], List[String], String] {
    case id :: name :: surname :: salary :: position :: birthday :: _ =>
      OptionT.liftF {
        for {
          result <- employeeService.update(
            Try(id.toLong).toOption.getOrElse(0L),
            birthday,
            name,
            surname,
            salary,
            position
          )
        } yield result.toString
      }
    case _ => OptionT.none
  }

  private def getEmployee[F[_]: Applicative](
    employeeService: EmployeeService[F]
  ): Kleisli[OptionT[F, *], String, String] = Kleisli[OptionT[F, *], String, String] { id =>
    OptionT.liftF {
      for {
        employee <- employeeService.find(Try(id.toLong).toOption.getOrElse(0L))
      } yield employee.toString
    }
  }

  private def deleteEmployee[F[_]: Applicative](
    employeeService: EmployeeService[F]
  ): Kleisli[OptionT[F, *], String, String] = Kleisli[OptionT[F, *], String, String] { id =>
    OptionT.liftF {
      for {
        result <- employeeService.delete(Try(id.toLong).toOption.getOrElse(0L))
      } yield result.toString
    }
  }

}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      employeeService <- EmployeeService.of[IO]
      _               <- ConsoleInterface(employeeService).cmd
    } yield ExitCode.Success
  }
}
