package com.itechart.db

import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Resource, Timer}
import com.itechart.db.repository.EmployeeService
import com.itechart.db.routes.EmployeeRoutes

import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

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
    val httpApp = EmployeeRoutes.routes[F](employeeService).orNotFound
    for {
      server <- BlazeServerBuilder[F](ExecutionContext.global)
        .bindHttp(port = 8080, host = "localhost")
        .withHttpApp(httpApp)
        .resource
    } yield server
  }
}
