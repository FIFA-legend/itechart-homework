package com.itechart.db.repository.doobie_repository

import cats.{FlatMap, Monad}
import cats.syntax.all._
import cats.effect.{Async, ContextShift}
import com.itechart.db.entity.Entities.{Employee, EmployeeDto}
import com.itechart.db.repository.EmployeeService
import com.itechart.db.validation.EmployeeValidator
import com.itechart.db.validation.EmployeeValidator.AllErrorsOr
import doobie.implicits._
import implicits.MetaImplicits._
import doobie.implicits.javatime._

final class DoobieEmployeeService[F[_]: Monad: FlatMap: Async: ContextShift](
) extends EmployeeService[F] {
  override def all: F[Map[Long, Employee]] = {
    val fragment =
      fr"SELECT id, birthday, first_name, last_name, salary, position FROM employees WHERE is_archived = false;"
    val query = fragment.query[Employee]
    DoobieTransactor.make[F].use { xa =>
      for {
        list <- query.to[List].transact(xa)
        map   = list.map(employee => (employee.id, employee)).toMap
      } yield map
    }
  }

  override def create(
    dto: EmployeeDto
  ): F[AllErrorsOr[Employee]] = {
    EmployeeValidator.validate(dto.firstName, dto.lastName, dto.position, dto.salary, dto.birthday).traverse {
      case (firstName, lastName, position, salary, date) =>
        DoobieTransactor.make[F].use { xa =>
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
    id:  Long,
    dto: EmployeeDto
  ): F[AllErrorsOr[Boolean]] = {
    EmployeeValidator
      .validate(
        dto.firstName,
        dto.lastName,
        dto.position,
        dto.salary,
        dto.birthday
      )
      .traverse { case (firstName, lastName, position, salary, date) =>
        DoobieTransactor.make[F].use { xa =>
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
    DoobieTransactor.make[F].use { xa =>
      for {
        result <- query.option.transact(xa)
      } yield result
    }
  }

  override def delete(id: Long): F[Boolean] = {
    DoobieTransactor.make[F].use { xa =>
      val fragment = fr"DELETE FROM employees WHERE id = $id AND is_archived = false;"
      val query    = fragment.update.run
      for {
        result <- query.transact(xa)
      } yield result != 0
    }
  }

  override def archive(id: Long, flag: Boolean): F[Boolean] = {
    DoobieTransactor.make[F].use { xa =>
      val fragment = fr"UPDATE employees SET is_archived = $flag WHERE id = $id;"
      for {
        result <- fragment.update.run.transact(xa)
      } yield result != 0
    }
  }
}
