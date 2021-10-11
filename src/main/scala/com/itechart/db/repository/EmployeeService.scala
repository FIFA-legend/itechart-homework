package com.itechart.db.repository

import cats.effect.{Async, ContextShift}
import cats.syntax.all._
import com.itechart.db.entity.Entities.{Employee, EmployeeDto}
import com.itechart.db.repository.doobie_repository.{DoobieEmployeeService, DoobieTransactor}
import com.itechart.db.validation.EmployeeValidator.AllErrorsOr
import doobie._
import doobie.implicits._
import doobie.util.fragment.Fragment

trait EmployeeService[F[_]] {
  def all: F[Map[Long, Employee]]
  def create(dto: EmployeeDto): F[AllErrorsOr[Employee]]
  def update(
    id:  Long,
    dto: EmployeeDto
  ): F[AllErrorsOr[Boolean]]
  def find(id:    Long): F[Option[Employee]]
  def delete(id:  Long): F[Boolean]
  def archive(id: Long, flag: Boolean): F[Boolean]
}

object EmployeeService {
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

    DoobieTransactor.make[F].use { xa =>
      for {
        _ <- setup().transact(xa)
      } yield new DoobieEmployeeService[F]
    }
  }
}
