package com.itechart.db.validation.errors

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
