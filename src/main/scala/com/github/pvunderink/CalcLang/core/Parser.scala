package com.github.pvunderink.CalcLang.core

import com.github.pvunderink.CalcLang.core.CalcLang.{Add, Assign, Bool, BoolT, Div, Expr, Fun, FunApp, FunT, Mul, Neg, Num, NumT, Reassign, Remainder, Str, StrT, Sub, Type, Var, Void}

import scala.util.parsing.combinator.RegexParsers
class Parser extends RegexParsers {
  private final case class ParseException(msg: String) extends RuntimeException(msg)

  private def keyword(word: String): Parser[String] = s"""\\b$word\\b""".r ^^ { _ => word }

  private def number: Parser[Expr] = """\d+(\.\d*)?""".r ^^ { n => Num(n.toDouble) }

  private def bool: Parser[Expr] = keyword("true") ^^ { _ => Bool(true) } | keyword("false") ^^ { _ => Bool(false) }

  private def str: Parser[Expr] = """"[\w\u0020-\u0021\u0023-\u1eff]*"""".r ^^ { s => Str(s.substring(1, s.length - 1)) }

  private def id: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

  private def variable: Parser[Expr] = id ^^ { id => Var(id) }

  private def factor_without_unop: Parser[Expr] = number | bool | str | fun_app | fun_def | assign | reassign | variable | function | grouped_expr

  private def grouped_expr: Parser[Expr] = "(" ~> expr <~ ")"

  private def factor: Parser[Expr] = factor_without_unop | unary_op

  private def unary_op: Parser[Expr] = "-" ~> factor_without_unop ^^ (e => Neg(e))

  private def assign: Parser[Expr] = (keyword("var") ~> id <~ "=") ~ expr ^^ {
    case id ~ expr => Assign(id, expr)
  }

  private def reassign: Parser[Expr] = (id <~ "=") ~ expr ^^ {
    case id ~ expr => Reassign(id, expr)
  }

  private def typ: Parser[Type] = keyword("num") ^^ { _ => NumT() } | keyword("bool") ^^ { _ => BoolT() } | keyword("str") ^^ { _ => StrT() } | funtyp

  private def funtyp: Parser[Type] = ("(" ~> repsep(typ, ",") <~ ")" <~ "->") ~ typ ^^ {
    case params ~ ret => FunT(params, ret, None)
  }

  private def fun_param: Parser[(String, Type)] = (id <~ ":") ~ typ ^^ {
    case id ~ typ => (id, typ)
  }

  private def fun_params: Parser[List[(String, Type)]] = repsep(fun_param, ",")

  private def fun_def: Parser[Expr] = keyword("def") ~> id ~ function ^^ {
    case id ~ fun => Assign(id, fun)
  }

  private def function: Parser[Fun] = ("(" ~> fun_params <~ ")") ~ ("{" ~> seq <~ "}") ^^ {
    case params ~ body => Fun(params, body)
  }

  private def fun_app: Parser[Expr] = (variable | grouped_expr) ~ rep1("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case expr ~ list => list.foldLeft(expr)((expr, params) => FunApp(expr, params))
  }

  private def term: Parser[Expr] = factor ~ rep("*" ~ factor | "/" ~ factor | "%" ~ factor) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "*" ~ y) => Mul(x, y)
      case (x, "/" ~ y) => Div(x, y)
      case (x, "%" ~ y) => Remainder(x, y)
    }
  }

  private def expr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => Add(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }

  private def stmt: Parser[Expr] = fun_def <~ opt(";") | expr <~ ";"

  private def seq: Parser[List[Expr]] = rep(stmt) ~ opt(expr) ^^ {
    case exprs ~ Some(expr) => exprs ++ List(expr)
    case exprs ~ None => exprs ++ List(Void())
    case Nil ~ Some(expr) => List(expr)
    case Nil ~ None => List(Void())
  }

  def apply(input: String): List[Expr] = parseAll(seq, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => throw ParseException(failure.msg)
  }
}
