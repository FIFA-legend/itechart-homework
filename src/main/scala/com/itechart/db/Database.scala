package com.itechart.db

import cats.data.{Validated, ValidatedNec}
import cats.effect.{Async, Blocker, ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Resource, Sync, Timer}
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.syntax.all._
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
import io.circe.Encoder
import io.circe.generic.JsonCodec
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredEncoder
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeServerBuilder

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext
import scala.util.Try

object employee {

  type Name     = String Refined MatchesRegex[W.`"^[A-Za-z]+$"`.T]
  type Surname  = String Refined MatchesRegex[W.`"^[A-Za-z]+$"`.T]
  type Position = NonEmptyString
  type Salary   = Int Refined Greater[W.`200`.T]

  implicit val config: Configuration = Configuration.default

  implicit val nameEncoder:     Encoder[Name]     = Encoder.encodeString.contramap[Name](_.toString)
  implicit val salaryEncoder:   Encoder[Salary]   = Encoder.encodeString.contramap[Salary](_.toString)
  implicit val positionEncoder: Encoder[Position] = Encoder.encodeString.contramap[Position](_.toString)
  implicit val employeeEncoder: Encoder[Employee] = deriveConfiguredEncoder[Employee]

  case class Employee(
    id:        Long,
    birthday:  LocalDate,
    firstName: Name,
    lastName:  Surname,
    salary:    Salary,
    position:  Position,
  )

  @JsonCodec case class EmployeeDto(
    birthday:  String,
    firstName: String,
    lastName:  String,
    salary:    String,
    position:  String,
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

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      service <- EmployeeService.of[IO]
      _       <- serverConfiguration[IO](service).use(_ => IO.never)
    } yield ExitCode.Success
  }

  private def serverConfiguration[F[_]: ContextShift: ConcurrentEffect: Timer](
    employeeService: EmployeeService[F]
  ): Resource[F, Server[F]] = {
    val httpApp = routes[F](employeeService).orNotFound
    for {
      server <- BlazeServerBuilder[F](ExecutionContext.global)
        .bindHttp(port = 8080, host = "localhost")
        .withHttpApp(httpApp)
        .resource
    } yield server
  }

  def routes[F[_]: Sync](employeeService: EmployeeService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    def allEmployees: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / "all" =>
      for {
        map      <- employeeService.all
        employees = map.values.toList
        response <- Ok(employees)
      } yield response
    }

    def getEmployee: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / id =>
      def processResult(employee: Option[Employee]): F[Response[F]] = {
        employee match {
          case None        => BadRequest("No employee with this id")
          case Some(value) => Ok(value)
        }
      }

      Try(id.toLong).toOption match {
        case None => BadRequest("Incorrect id")
        case Some(parsedId) =>
          for {
            employee <- employeeService.find(parsedId)
            result   <- processResult(employee)
          } yield result
      }
    }

    def createEmployee: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "create" =>
      val res = for {
        employeeDto <- req.as[EmployeeDto]
        result <- employeeService.create(
          employeeDto.birthday,
          employeeDto.firstName,
          employeeDto.lastName,
          employeeDto.salary,
          employeeDto.position
        )
      } yield result

      processResult(res)
    }

    def updateEmployee: HttpRoutes[F] = HttpRoutes.of[F] { case req @ PUT -> Root / "update" / id =>
      Try(id.toLong).toOption match {
        case None => BadRequest("Incorrect id")
        case Some(parsedId) =>
          val res = for {
            employeeDto <- req.as[EmployeeDto]
            isUpdated <- employeeService.update(
              parsedId,
              employeeDto.birthday,
              employeeDto.firstName,
              employeeDto.lastName,
              employeeDto.salary,
              employeeDto.position
            )
          } yield isUpdated

          processResult(res)
      }
    }

    def deleteEmployee: HttpRoutes[F] = HttpRoutes.of[F] { case DELETE -> Root / "delete" / id =>
      Try(id.toLong).toOption match {
        case None => BadRequest("Incorrect id")
        case Some(parsedId) =>
          for {
            isDeleted <- employeeService.delete(parsedId)
            status    <- booleanToStatus(isDeleted)
          } yield status
      }
    }

    object BooleanVar {
      def unapply(value: String): Option[Boolean] =
        Try(value.toBoolean).toOption
    }

    def archiveEmployee: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / "archive" / id / BooleanVar(archived) =>
      Try(id.toLong).toOption match {
        case None => BadRequest("Incorrect id")
        case Some(parsedId) =>
          for {
            archived <- employeeService.archive(parsedId, archived)
            status   <- booleanToStatus(archived)
          } yield status
      }
    }

    def booleanToStatus(bool: Boolean): F[Response[F]] = {
      if (bool) Ok("Updated")
      else BadRequest("Request is not completed")
    }

    def processResult[A](entity: F[AllErrorsOr[A]])(implicit E: EntityEncoder[F, A]): F[Response[F]] = {
      entity
        .flatMap {
          case Validated.Valid(a)   => Created(a)
          case Validated.Invalid(e) => BadRequest(e.toList.toString())
        }
        .handleErrorWith { ex =>
          InternalServerError(ex.getMessage)
        }
    }

    allEmployees <+> getEmployee <+> createEmployee <+> updateEmployee <+> deleteEmployee <+> archiveEmployee
  }
}
