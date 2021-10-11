package com.itechart.db.repository.doobie_repository.implicits

import com.itechart.db.entity.Entities.{Name, Position, Salary}
import doobie.Meta
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.{refineV, W}
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.predicates.all.NonEmpty
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.auto._

object MetaImplicits {

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

}
