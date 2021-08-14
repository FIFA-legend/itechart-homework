package com.itechart.basics

import java.io.{File, FileWriter}
import java.util.regex.Pattern
import scala.io.Source
import scala.util.Try

object Spreadsheet {

  trait Reader {
    def read(): Option[List[String]]
  }

  class SpreadsheetReader(val pathToFile: String) extends Reader {
    override def read(): Option[List[String]] = {
      Try {
        val reader = Source.fromFile(pathToFile)
        val lines = reader.getLines().toList
        reader.close()
        lines
      }.toOption
    }
  }

  type Cell = String

  trait Parser {
    def parse(list: List[String]): Option[List[(String, Cell)]]
  }

  class SpreadsheetParser extends Parser {
    override def parse(list: List[String]): Option[List[(String, Cell)]] = {
      val firstLine = list.head
      val rowsCount = firstLine.strip().split("\\s+")(0).toInt
      val columnsCount = firstLine.strip().split("\\s+")(1).toInt
      if (rowsCount + 1 != list.length || columnsCount != list.tail.head.split("\t").length) None
      else {
        val cellContent = for {
          line <- list.tail
          content <- line.strip().split("\t")
        } yield content
        val letters = columns(columnsCount)
        val cellName = for {
          number <- 1 to rowsCount
          letter <- letters
        } yield letter + number
        Option(cellContent.zip(cellName))
      }
    }

    private def columns(number: Int): List[Cell] = {
      (0 until number).toList.map(num => ('A' + num).toChar.toString)
    }
  }

  trait Converter {
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
      val operands = line.split("[+\\-*/]")
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

    private def isOperation(char: Char): Boolean = char == '+' || char == '-' || char == '*' || char == '/'

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
  }

  trait Writer {
    def write(list: List[(Any, Cell)]): Unit

    def format(list: List[(Any, Cell)], f: String => Unit): Unit = {
      list.map { case (content, cell) =>
        cell match {
          case c if c == "A1" => content.toString
          case c if c.startsWith("A") => "\n" + content.toString
          case _ => "\t" + content.toString
        }
      }
        .foreach(str => f(str))
    }
  }

  class SpreadsheetFileWriter(pathToFile: String) extends Writer {
    override def write(list: List[(Any, Cell)]): Unit = {
      Try {
        val writer = new FileWriter(new File(pathToFile))
        format(list, str => writer.write(str))
        writer.close()
      }.toEither match {
        case Left(value) => println(value)
        case Right(_) =>
      }
    }
  }

  class SpreadsheetConsoleWriter extends Writer {
    override def write(list: List[(Any, Cell)]): Unit = {
      format(list, str => print(str))
    }
  }

  def main(args: Array[String]): Unit = {
    val reader = new SpreadsheetReader(args(0))
    val lines = reader.read()
    lines match {
      case None => println("File " + args(0) + " doesn't exist")
      case Some(value) =>
        val parser = new SpreadsheetParser()
        val parsedText = parser.parse(value)
        if (parsedText.isEmpty) println("Wrong rows or columns values")
        else {
          val converter = new SpreadsheetConverter()
          val output = converter.convert(parsedText.get)
          if (args.length >= 2) {
            val writer = new SpreadsheetFileWriter(args(1))
            writer.write(output)
          } else {
            val writer = new SpreadsheetConsoleWriter()
            writer.write(output)
          }
        }
    }
  }

}
