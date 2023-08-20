package com.github.pvunderink.CalcLang

import scala.util.parsing.combinator.RegexParsers

object CalcLang {
  sealed trait Expr

  final case class Num(n: Double) extends Expr

  final case class Neg(e: Expr) extends Expr

  final case class Add(l: Expr, r: Expr) extends Expr

  final case class Sub(l: Expr, r: Expr) extends Expr

  final case class Mul(l: Expr, r: Expr) extends Expr

  final case class Div(l: Expr, r: Expr) extends Expr

  final case class Fun(id: String, exprs: List[Expr]) extends Expr

  final val functions = Map(
    "sqrt" -> (1, (args: List[Double]) => math.sqrt(args.head)),
    "log" -> (1, (args: List[Double]) => math.log(args.head)),
    "log10" -> (1, (args: List[Double]) => math.log10(args.head)),
    "exp" -> (1, (args: List[Double]) => math.exp(args.head)),
    "pow" -> (2, (args: List[Double]) => math.pow(args.head, args.tail.head))
  )

  final case class ParseException(msg: String) extends RuntimeException(msg)

  class ExprParser extends RegexParsers {
    def number: Parser[Expr] = """\d+(\.\d*)?""".r ^^ { n => Num(n.toDouble) }

    def id: Parser[String] = """[a-zA-Z][a-zA-Z0-9]*""".r

    def const: Parser[Expr] = "pi" ^^ {_ => Num(math.Pi)} | "e" ^^ {_ => Num(math.E)}

    def factor_without_unop: Parser[Expr] = number | const | fun | "(" ~> expr <~ ")"

    def factor: Parser[Expr] = factor_without_unop | unop

    def unop: Parser[Expr] = "-" ~ factor_without_unop ^^ {
      case "-" ~ e => Neg(e)
    }

    def term: Parser[Expr] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "*" ~ y) => Mul(x, y)
        case (x, "/" ~ y) => Div(x, y)
      }
    }

    def expr: Parser[Expr] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "+" ~ y) => Add(x, y)
        case (x, "-" ~ y) => Sub(x, y)
      }
    }

    def fun: Parser[Expr] = id ~ "(" ~ repsep(log(expr)("Argument"), ",") ~ ")" ^^ {
      case funName ~ "(" ~ list ~ ")" => Fun(funName, list)
    }

    def apply(input: String): Expr = parseAll(expr, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw ParseException(failure.msg)
    }
  }

  final case class InterpException(msg: String) extends RuntimeException(msg)

  class ExprInterpreter {
    def interp(expr: Expr): Double = expr match {
      case Num(n) => n
      case Neg(e) => -interp(e)
      case Add(l, r) => interp(l) + interp(r)
      case Sub(l, r) => interp(l) - interp(r)
      case Mul(l, r) => interp(l) * interp(r)
      case Div(l, r) => interp(l) / interp(r)
      case Fun(id, exprs) => applyFunction(id, exprs.map(interp))
    }

    def applyFunction(id: String, values: List[Double]): Double = functions.get(id) match {
      case Some((n, fun)) => if (values.length == n) {
        fun(values)
      } else {
        throw InterpException(s"Function '${id}' requires ${n} argument(s), but ${values.length} were provided.")
      }
      case None => throw InterpException(s"Use of unknown function '${id}'.")
    }
  }
}