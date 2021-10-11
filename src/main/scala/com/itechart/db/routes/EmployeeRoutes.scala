package com.itechart.db.routes

import cats.data.Validated
import cats.effect.Sync
import cats.syntax.all._
import com.itechart.db.entity.Entities.{Employee, EmployeeDto}
import com.itechart.db.repository.EmployeeService
import com.itechart.db.validation.EmployeeValidator.AllErrorsOr
import org.http4s.{EntityEncoder, HttpRoutes, Response}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl

import scala.util.Try

object EmployeeRoutes {
  def routes[F[_]: Sync](employeeService: EmployeeService[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    object BooleanVar {
      def unapply(value: String): Option[Boolean] = Try(value.toBoolean).toOption
    }

    object LongVar {
      def unapply(value: String): Option[Long] = Try(value.toLong).toOption
    }

    def allEmployeesRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / "all" =>
      for {
        map      <- employeeService.all
        employees = map.values.toList
        response <- Ok(employees)
      } yield response
    }

    def getEmployeeRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / LongVar(id) =>
      def processFoundEmployee(employee: Option[Employee]): F[Response[F]] = {
        employee match {
          case None        => BadRequest("No employee with this id")
          case Some(value) => Ok(value)
        }
      }

      for {
        employee <- employeeService.find(id)
        result   <- processFoundEmployee(employee)
      } yield result
    }

    def createEmployeeRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "create" =>
      val res = for {
        employeeDto <- req.as[EmployeeDto]
        result      <- employeeService.create(employeeDto)
      } yield result

      processResultOrErrors(res)
    }

    def updateEmployeeRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ PUT -> Root / "update" / LongVar(id) =>
      val result = for {
        employeeDto <- req.as[EmployeeDto]
        isUpdated   <- employeeService.update(id, employeeDto)
      } yield isUpdated

      processResultOrErrors(result)

    }

    def deleteEmployeeRoute: HttpRoutes[F] = HttpRoutes.of[F] { case DELETE -> Root / "delete" / LongVar(id) =>
      for {
        isDeleted <- employeeService.delete(id)
        status    <- booleanToStatus(isDeleted)
      } yield status
    }

    def archiveEmployeeRoute: HttpRoutes[F] = HttpRoutes.of[F] {
      case GET -> Root / "archive" / LongVar(id) / BooleanVar(archived) =>
        for {
          archived <- employeeService.archive(id, archived)
          status   <- booleanToStatus(archived)
        } yield status
    }

    def booleanToStatus(bool: Boolean): F[Response[F]] = {
      if (bool) Ok("Updated")
      else BadRequest("Request is not completed")
    }

    def processResultOrErrors[A](entity: F[AllErrorsOr[A]])(implicit E: EntityEncoder[F, A]): F[Response[F]] = {
      entity
        .flatMap {
          case Validated.Valid(a)   => Created(a)
          case Validated.Invalid(e) => BadRequest(e.toList.toString())
        }
        .handleErrorWith { ex =>
          InternalServerError(ex.getMessage)
        }
    }

    allEmployeesRoute <+> getEmployeeRoute <+> createEmployeeRoute <+> updateEmployeeRoute <+> deleteEmployeeRoute <+> archiveEmployeeRoute
  }
}
