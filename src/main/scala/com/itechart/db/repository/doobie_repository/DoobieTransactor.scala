package com.itechart.db.repository.doobie_repository

import cats.effect.{Async, Blocker, ContextShift, Resource}
import doobie.Transactor

object DoobieTransactor {
  object DataBaseConfiguration {
    val dbDriverName = "com.mysql.cj.jdbc.Driver"
    val dbUrl        = "jdbc:mysql://localhost:3306/employee_base?serverTimezone=Europe/Moscow"
    val dbUser       = "root"
    val dbPassword   = "0987654321KnKn"
  }

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
