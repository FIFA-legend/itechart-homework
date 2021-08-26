package com.itechart.basics

import java.io.{File, FileWriter}
import java.util.regex.Pattern
import scala.io.Source
import scala.util.Try

object Spreadsheet {

  type Cell = String
  type ErrorMessage = String

  trait Reader {
    def read(): Either[ErrorMessage, List[String]]
  }

  class SpreadsheetReader(val pathToFile: String) extends Reader {
    override def read(): Either[ErrorMessage, List[String]] = {
      Try {
        val reader = Source.fromFile(pathToFile)
        val lines = reader.getLines().toList
        reader.close()
        lines
      }.toEither.left.map(_.getMessage)
    }
  }

  trait Parser {
    def parse(list: List[String]): Either[ErrorMessage, List[(String, Cell)]]
  }

  class SpreadsheetParser extends Parser {
    override def parse(list: List[String]): Either[ErrorMessage, List[(String, Cell)]] = {
      val firstLine = list.head.strip()
      val rows = firstLine.split("\\s+")(0).toInt
      val columns = firstLine.split("\\s+")(1).toInt
      for {
        validatedList <- validateTable(rows, columns, list)

        cellContent = content(validatedList)
        cellNames = cells(rows, columns)
      } yield cellContent.zip(cellNames)
    }

    private def validateTable(rows: Int, columns: Int, list: List[String]): Either[ErrorMessage, List[String]] = {
      val secondLine = list.tail.head.strip()
      if (rows + 1 != list.length)
        Left("Wrong 1st argument in the first line")
      else if (columns != secondLine.split("\t").length)
        Left("Wrong 2nd argument in the first line")
      else Right(list.tail)
    }

    private def cells(rows: Int, columns: Int): List[Cell] = {
      for {
        row <- (1 to rows).toList
        column <- (0 until columns).map(num => ('A' + num).toChar.toString)
      } yield column + row
    }

    private def content(list: List[String]): List[String] = {
      for {
        line <- list
        content <- line.strip().split("\t")
      } yield content
    }
  }

  sealed trait CellContent {
    def evaluateToString(): String

    def evaluateToInt(): Either[ErrorMessage, Int]
  }

  final case class Text(content: String) extends CellContent {
    override def evaluateToString(): String = content

    override def evaluateToInt(): Either[ErrorMessage, Int] = Left("Error: Operand can't be converted to int")
  }

  final case class Number(value: Int) extends CellContent {
    override def evaluateToString(): String = value.toString

    override def evaluateToInt(): Either[ErrorMessage, Int] = Right(value)
  }

  final case class Reference(ref: String, list: List[(String, Cell)]) extends CellContent {
    override def evaluateToString(): String = {
      val option = list.find { case (_, cell) => ref == cell }
      option match {
        case Some((content, _)) => new SpreadsheetConverter().processCell(content, list).evaluateToString()
        case None => "Error: Incorrect cell reference"
      }
    }

    override def evaluateToInt(): Either[ErrorMessage, Int] = {
      val option = list.find { case (_, cell) => ref == cell }
      option match {
        case Some((content, _)) => new SpreadsheetConverter().processCell(content, list).evaluateToInt()
        case None => Left("Error: Incorrect cell reference")
      }
    }
  }

  final case class Expression(operands: List[CellContent], operators: List[Char]) extends CellContent {
    override def evaluateToString(): String = {
      val evaluated = evaluateToInt()
      evaluated match {
        case Left(message) => message
        case Right(int) => int.toString
      }
    }

    override def evaluateToInt(): Either[ErrorMessage, Int] = {
      val eitherList = operands.map(operand => operand.evaluateToInt())
      if (eitherList.count(either => either.isLeft) > 0) eitherList.filter(either => either.isLeft).head
      else {
        val evaluated = eitherList.map(either => either.getOrElse(0))
        val acc = evaluated.head
        Right(
          evaluated.tail.zip(operators)
            .foldLeft(acc){ case (op1, (op2, sign)) => performOperation(op1, op2, sign) }
        )
      }
    }

    private def performOperation(a: Int, b: Int, op: Char): Int = {
      op match {
        case '+' => a + b
        case '-' => a - b
        case '*' => a * b
        case '/' => a / b
      }
    }
  }

  final case class Error(description: String) extends CellContent {
    override def evaluateToString(): String = description

    override def evaluateToInt(): Either[ErrorMessage, Int] = Left(description)
  }

  trait Converter {
    def convert(list: List[(String, Cell)]): List[(String, Cell)]
  }

  class SpreadsheetConverter extends Converter {
    override def convert(list: List[(String, Cell)]): List[(String, Cell)] = {
      list.map { case (str, cell) => (processCell(str, list), cell) }
        .map { case (content, cell) => (content.evaluateToString(), cell) }
    }

    def processCell(string: String, list: List[(String, Cell)]): CellContent = {
      string match {
        case s if s.isEmpty => Text(s)
        case s if s.startsWith("\'") => Text(s.substring(1))
        case s if Try(s.toInt).isSuccess => Number(s.toInt)
        case s if s.startsWith("=") => parseExpression(s.substring(1), list)
        case _ => Error("#Error: Cell content of incorrect type")
      }
    }

    private def parseExpression(line: String, list: List[(String, Cell)]): CellContent = {
      val operators = line.toCharArray.toList.filter(ch => isOperation(ch))
      val operands = line.split("[*+\\-/]")
      if (operators.length + 1 != operands.length) Error("#Error: Incorrect arithmetic")
      else Expression(operands.map(op => parseOperand(op, list)).toList, operators)
    }

    private def parseOperand(operand: String, list: List[(String, Cell)]): CellContent = {
      val pattern = Pattern.compile("^(\\D+)(\\d+)$")
      val matcher = pattern.matcher(operand)
      operand match {
        case s if Try(s.toInt).isSuccess => Number(s.toInt)
        case s if matcher.matches() => Reference(s, list)
        case _ => Error("#Error: Cell contains wrong operator")
      }
    }

    private def isOperation(char: Char): Boolean = {
      char == '+' || char == '-' || char == '*' || char == '/'
    }
  }

  /*trait Converter {
    def convert(list: List[(String, Cell)]): List[(Any, Cell)]
  }

  class SpreadsheetConverter extends Converter {
    override def convert(list: List[(String, Cell)]): List[(Any, Cell)] = {
      list.map { case (str, cell) => (processCell(str, list), cell) }
    }

    private def processCell(string: String, list: List[(String, Cell)]): Any = {
      string match {
        case s if s.isEmpty => s
        case s if s.startsWith("\'") => s.substring(1)
        case s if Try(s.toInt).isSuccess => s.toInt
        case s if s.startsWith("=") => processExpression(s.substring(1), list)
        case _ => "#Error: Wrong message type"
      }
    }

    private def processExpression(line: String, list: List[(String, Cell)]): Any = {
      val operations = line.toCharArray.toList.filter(ch => isOperation(ch))
      val operands = line.split("[*+\\-/]")
      if (operations.length + 1 != operands.length) "#Error: Wrong arithmetic"
      else {
        val eitherList = operands.map(op => processOperand(op, list))
        if (eitherList.count(either => either.isLeft) > 0) eitherList.filter(either => either.isLeft)(0)
        else {
          val ints = eitherList.map(either => either.getOrElse(0))
          var result = ints(0)
          var i = 0
          while (i < operations.length) {
            result = performOperation(result, ints(i + 1), operations(i))
            i = i + 1
          }
          result
        }
      }
    }

    private def performOperation(a: Int, b: Int, op: Char): Int = {
      op match {
        case '+' => a + b
        case '-' => a - b
        case '*' => a * b
        case '/' => a / b
      }
    }

    private def isOperation(char: Char): Boolean = {
      char == '+' || char == '-' || char == '*' || char == '/'
    }

    private def processOperand(str: String, list: List[(String, Cell)]): Either[String, Int] = {
      val pattern = Pattern.compile("^(\\D+)(\\d+)$")
      val matcher = pattern.matcher(str)
      str match {
        case s if Try(s.toInt).isSuccess => Right(s.toInt)
        case s if matcher.matches() =>
          val optionCell = list.find { case (_, cell) => s == cell }
          optionCell match {
            case Some(value) =>
              val returnedValue = processCell(value._1, list)
              if (Try(returnedValue.toString.toInt).isSuccess) Right(returnedValue.toString.toInt)
              else Left("#Error: Wrong cell reference")
            case None => Left("#Error: Wrong cell reference")
          }
        case _ => Left("#Error: Wrong operand")
      }
    }
  }*/

  trait SpreadsheetWriter {
    def write(list: List[(String, Cell)]): Either[ErrorMessage, Unit]

    def format(list: List[(String, Cell)], f: String => Unit): Unit = {
      list.map { case (content, cell) =>
        cell match {
          case c if c == "A1" => content
          case c if c.startsWith("A") => "\n" + content
          case _ => "\t" + content
        }
      }
        .foreach(str => f(str))
    }
  }

  class SpreadsheetFileWriter(pathToFile: String) extends SpreadsheetWriter {
    override def write(list: List[(String, Cell)]): Either[ErrorMessage, Unit] = {
      Try {
        val writer = new FileWriter(new File(pathToFile))
        format(list, str => writer.write(str))
        writer.close()
      }.toEither.left.map(_.getMessage)
    }
  }

  class SpreadsheetConsoleWriter extends SpreadsheetWriter {
    override def write(list: List[(String, Cell)]): Either[ErrorMessage, Unit] = {
      Right(format(list, print(_)))
    }
  }

  def selectWriter(args: Array[String]): SpreadsheetWriter = {
    if (args.length >= 2)
      new SpreadsheetFileWriter(args(1))
    else
      new SpreadsheetConsoleWriter
  }

  def main(args: Array[String]): Unit = {
    val reader = new SpreadsheetReader(args(0))
    val parser = new SpreadsheetParser()
    val converter = new SpreadsheetConverter()
    val writer = selectWriter(args)

    val result = for {
      read <- reader.read()
      parsed <- parser.parse(read)
      converted = converter.convert(parsed)
      _ <- writer.write(converted)
    } yield ()
    if (result.isLeft) println(result)
  }

}
