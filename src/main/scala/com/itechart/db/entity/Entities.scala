package com.itechart.db.entity

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.generic.JsonCodec
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredEncoder

import java.time.LocalDate

object Entities {

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
