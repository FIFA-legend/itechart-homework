package com.itechart.db

import cats.data.{Kleisli, OptionT, ValidatedNec}
import cats.effect.{Async, Blocker, ContextShift, ExitCode, IO, IOApp, Resource, Sync}
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.syntax.all._
import cats.{Applicative, MonadError}
import com.itechart.db.EmployeeValidator.AllErrorsOr
import com.itechart.db.EmployeeValidator.EmployeeValidationError._
import com.itechart.db.employee._
import doobie._
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.util.fragment.Fragment
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.predicates.all.NonEmpty
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.{refineV, W}
import eu.timepit.refined.auto._

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
    position:  Position,
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
  def find(id:    Long): F[Option[Employee]]
  def delete(id:  Long): F[Boolean]
  def archive(id: Long, flag: Boolean): F[Boolean]
}

object EmployeeService {

  object DataBaseConfiguration {
    val dbDriverName = "com.mysql.cj.jdbc.Driver"
    val dbUrl        = "jdbc:mysql://localhost:3306/employee_base?serverTimezone=Europe/Moscow"
    val dbUser       = "root"
    val dbPassword   = "0987654321KnKn"
  }

  object DataBaseTransactor {
    def make[F[_]: ContextShift: Async]: Resource[F, Transactor[F]] =
      Blocker[F].map { be =>
        Transactor.fromDriverManager[F](
          driver  = DataBaseConfiguration.dbDriverName,
          url     = DataBaseConfiguration.dbUrl,
          user    = DataBaseConfiguration.dbUser,
          pass    = DataBaseConfiguration.dbPassword,
          blocker = be,
        )
      }
  }

  def of[F[_]: Async: ContextShift]: F[EmployeeService[F]] = {
    val script =
      """CREATE TABLE IF NOT EXISTS employees (
        |id BIGINT PRIMARY KEY,
        |birthday DATE,
        |first_name VARCHAR(100) NOT NULL,
        |last_name VARCHAR(100) NOT NULL,
        |salary INT NOT NULL,
        |position VARCHAR(100) NOT NULL,
        |is_archived BOOLEAN
        |);""".stripMargin

    def setup(): ConnectionIO[Unit] =
      for {
        _ <- Fragment.const(script).update.run
      } yield ()

    DataBaseTransactor.make[F].use { xa =>
      for {
        _ <- setup().transact(xa)
      } yield new DataBaseEmployeeService[F]
    }
  }

  final private class DataBaseEmployeeService[F[_]: Async: ContextShift](
  ) extends EmployeeService[F] {
    private def covertParameter[T, P](
      parameter: T,
      default:   Refined[T, P]
    )(
      implicit v: Validate[T, P]
    ): Refined[T, P] = {
      refineV(parameter).getOrElse(default)
    }

    implicit val nameMeta: Meta[Name] =
      Meta[String].timap(covertParameter[String, MatchesRegex[W.`"^[A-Za-z]+$"`.T]](_, "Name"))(_.toString())
    implicit val positionMeta: Meta[Position] =
      Meta[String].timap(covertParameter[String, NonEmpty](_, "Position"))(_.toString())
    implicit val salaryMeta: Meta[Salary] =
      Meta[String].timap(a => covertParameter[Int, Greater[W.`200`.T]](a.toInt, 300))(_.toString())

    override def all: F[Map[Long, Employee]] = {
      val fragment =
        fr"SELECT id, birthday, first_name, last_name, salary, position FROM employees WHERE is_archived = false;"
      val query = fragment.query[Employee]
      DataBaseTransactor.make[F].use { xa =>
        for {
          list <- query.to[List].transact(xa)
          map   = list.map(employee => (employee.id, employee)).toMap
        } yield map
      }
    }

    override def create(
      birthday:  String,
      firstName: String,
      lastName:  String,
      salary:    String,
      position:  String
    ): F[AllErrorsOr[Employee]] = {
      EmployeeValidator.validate(firstName, lastName, position, salary, birthday).traverse {
        case (firstName, lastName, position, salary, date) =>
          DataBaseTransactor.make[F].use { xa =>
            val fragment =
              fr"INSERT INTO employees (birthday, first_name, last_name, salary, position, is_archived) VALUES ($date, $firstName, $lastName, $salary, $position, false);"
            val idFragment = fr"SELECT LAST_INSERT_ID();"
            val query      = fragment.update.run
            val idQuery    = idFragment.query[Long]
            for {
              id <- (query *> idQuery.unique).transact(xa)
            } yield Employee(id, date, firstName, lastName, salary, position)
          }
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
          DataBaseTransactor.make[F].use { xa =>
            val birthdayFragment = fr"UPDATE employees SET birthday = $date WHERE id = $id AND is_archived = false;"
            val firstNameFragment =
              fr"UPDATE employees SET first_name = $firstName WHERE id = $id AND is_archived = false;"
            val lastNameFragment =
              fr"UPDATE employees SET last_name = $lastName WHERE id = $id AND is_archived = false;"
            val salaryFragment   = fr"UPDATE employees SET salary = $salary WHERE id = $id AND is_archived = false;"
            val positionFragment = fr"UPDATE employees SET position = $position WHERE id = $id AND is_archived = false;"
            for {
              result <- (birthdayFragment.update.run *>
                firstNameFragment.update.run *>
                lastNameFragment.update.run *>
                salaryFragment.update.run *>
                positionFragment.update.run).transact(xa)
            } yield result != 0
          }
        }
    }

    override def find(id: Long): F[Option[Employee]] = {
      val fragment =
        fr"SELECT id, birthday, first_name, last_name, salary, position FROM employees WHERE is_archived = false AND id = $id;"
      val query = fragment.query[Employee]
      DataBaseTransactor.make[F].use { xa =>
        for {
          result <- query.option.transact(xa)
        } yield result
      }
    }

    override def delete(id: Long): F[Boolean] = {
      DataBaseTransactor.make[F].use { xa =>
        val fragment = fr"DELETE FROM employees WHERE id = $id AND is_archived = false;"
        val query    = fragment.update.run
        for {
          result <- query.transact(xa)
        } yield result != 0
      }
    }

    override def archive(id: Long, flag: Boolean): F[Boolean] = {
      DataBaseTransactor.make[F].use { xa =>
        val fragment = fr"UPDATE employees SET is_archived = $flag WHERE id = $id;"
        for {
          result <- fragment.update.run.transact(xa)
        } yield result != 0
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
              |5. delete [id]
              |6. archive [id] [flag]""".stripMargin
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
    case "archive" :: id :: flag :: Nil => archiveEmployee(employeeService).apply(id, flag)
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

  private def archiveEmployee[F[_]: Applicative](
    employeeService: EmployeeService[F]
  ): Kleisli[OptionT[F, *], (String, String), String] = Kleisli[OptionT[F, *], (String, String), String] {
    case (id, flag) =>
      OptionT.liftF {
        val (parsedId, parsedFlag) = parseValuesToArchive(id, flag)
        for {
          result <- employeeService.archive(parsedId, parsedFlag)
        } yield result.toString
      }
    case _ => OptionT.none
  }

  private def parseValuesToArchive(id: String, isArchived: String): (Long, Boolean) = {
    Try(isArchived.toBoolean).toOption match {
      case None       => (0L, true)
      case Some(bool) => (Try(id.toLong).toOption.getOrElse(0L), bool)
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
