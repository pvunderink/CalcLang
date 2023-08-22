package com.github.pvunderink.CalcLang

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object CalcLang {
  sealed trait Expr

  private final case class Num(n: Double) extends Expr

  private final case class Bool(b: Boolean) extends Expr

  private final case class Str(str: String) extends Expr

  private final case class Var(id: String) extends Expr

  private final case class Neg(e: Expr) extends Expr

  private final case class Add(l: Expr, r: Expr) extends Expr

  private final case class Sub(l: Expr, r: Expr) extends Expr

  private final case class Mul(l: Expr, r: Expr) extends Expr

  private final case class Div(l: Expr, r: Expr) extends Expr

  private final case class Remainder(l: Expr, r: Expr) extends Expr

  private final case class FunApp(id: String, args: List[Expr]) extends Expr

  private final case class FunDef(id: String, params: List[(String, Type)], body: List[Expr]) extends Expr

  private final case class Assign(id: String, e: Expr) extends Expr

  private final case class Reassign(id: String, e: Expr) extends Expr

  sealed trait TypedExpr {
    def typ: Type
  }

  private final case class TypedNum(n: Double) extends TypedExpr {
    override def typ: Type = NumT()
  }

  private final case class TypedBool(b: Boolean) extends TypedExpr {
    override def typ: Type = BoolT()
  }

  private final case class TypedStr(str: String) extends TypedExpr {
    override def typ: Type = StrT()
  }

  private final case class TypedVar(id: String, typ: Type) extends TypedExpr

  private final case class TypedNeg(e: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  private final case class TypedAdd(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  private final case class TypedConcat(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = StrT()
  }

  private final case class TypedSub(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  private final case class TypedMul(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  private final case class TypedDiv(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  private final case class TypedRemainder(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  private final case class TypedFunApp(id: String, args: List[TypedExpr], typ: Type) extends TypedExpr

  private final case class TypedFunDef(id: String, params: List[(String, Type)], body: List[TypedExpr], ret: Type) extends TypedExpr {
    override def typ: Type = FunT(params.map(_._2), ret)
  }

  private final case class TypedAssign(id: String, e: TypedExpr, typ: Type) extends TypedExpr

  private final case class TypedReassign(id: String, e: TypedExpr, typ: Type) extends TypedExpr


  sealed trait Value

  private final case class NumVal(n: Double) extends Value {
    override def toString: String = n.toString
  }

  private final case class BoolVal(b: Boolean) extends Value {
    override def toString: String = b.toString
  }

  private final case class StrVal(str: String) extends Value {
    override def toString: String = str
  }

  private final case class FunVal(id: String, params: List[(String, Type)], body: List[TypedExpr], ret: Type) extends Value {
    override def toString: String = s"(${params.map(t => t._1 + ": " + t._2.toString).foldLeft("")((acc, elem) => acc + ", " + elem)}) -> ${ret.toString} { ... }"
  }


  sealed trait Type

  private final case class NumT() extends Type {
    override def toString: String = "num"
  }

  private final case class BoolT() extends Type {
    override def toString: String = "bool"
  }

  private final case class StrT() extends Type {
    override def toString: String = "str"
  }

  private final case class FunT(args: List[Type], ret: Type) extends Type {
    override def toString: String = s"(${args.map(_.toString).foldLeft("")((acc, elem) => acc + ", " + elem)}) -> ${ret.toString} { ... }"
  }

  private final val builtin_functions: Map[String, (FunT, List[Value] => Value)] = Map(
    "sqrt" ->
      (
        FunT(List(NumT()), NumT()),
        args => args.head match {
          case NumVal(n) => NumVal(math.sqrt(n))
        }
      ),
    "log" ->
      (
        FunT(List(NumT()), NumT()),
        args => args.head match {
          case NumVal(n) => NumVal(math.log(n))
        }
      ),
    "log10" ->
      (
        FunT(List(NumT()), NumT()),
        args => args.head match {
          case NumVal(n) => NumVal(math.log10(n))
        }
      ),
    "exp" ->
      (
        FunT(List(NumT()), NumT()),
        args => args.head match {
          case NumVal(n) => NumVal(math.exp(n))
        }
      ),
    "pow" ->
      (
        FunT(List(NumT(), NumT()), NumT()),
        args => (args.head, args.tail.head) match {
          case (NumVal(x), NumVal(y)) => NumVal(math.pow(x, y))
        }
      ),
  )

  private final val builtin_constants: Map[String, (Type, Value)] = Map(
    "pi" -> (NumT(), NumVal(math.Pi)),
    "e" -> (NumT(), NumVal(math.E))
  )

  class Parser extends RegexParsers {
    private final case class ParseException(msg: String) extends RuntimeException(msg)

    private def keyword(word: String): Parser[String] = s"""\\b$word\\b""".r ^^ { _ => word }

    private def number: Parser[Expr] = """\d+(\.\d*)?""".r ^^ { n => Num(n.toDouble) }

    private def bool: Parser[Expr] = keyword("true") ^^ { _ => Bool(true) } | keyword("false") ^^ { _ => Bool(false) }

    private def str: Parser[Expr] = """"[\w\u0020-\u0021\u0023-\u1eff]*"""".r ^^ { s => Str(s.substring(1, s.length - 1)) }

    private def id: Parser[String] = """[a-zA-Z][a-zA-Z0-9]*""".r

    private def variable: Parser[Expr] = id ^^ { id => Var(id) }

    private def factor_without_unop: Parser[Expr] = number | bool | str | fun_app | fun_def | assign | reassign | variable | "(" ~> expr <~ ")"

    private def factor: Parser[Expr] = factor_without_unop | unary_op

    private def unary_op: Parser[Expr] = "-" ~ factor_without_unop ^^ {
      case "-" ~ e => Neg(e)
    }

    private def assign: Parser[Expr] = keyword("var") ~ id ~ "=" ~ expr ^^ {
      case "var" ~ id ~ "=" ~ expr => Assign(id, expr)
    }

    private def reassign: Parser[Expr] = id ~ "=" ~ expr ^^ {
      case id ~ "=" ~ expr => Reassign(id, expr)
    }

    private def typ: Parser[Type] = keyword("num") ^^ { _ => NumT() } | keyword("bool") ^^ { _ => BoolT() } | keyword("str") ^^ { _ => StrT() } | funtyp

    private def funtyp: Parser[Type] = "(" ~ repsep(typ, ",") ~ ")" ~ "->" ~ typ ^^ {
      case "(" ~ params ~ ")" ~ "->" ~ ret => FunT(params, ret)
    }

    private def fun_param: Parser[(String, Type)] = id ~ ":" ~ typ ^^ {
      case id ~ ":" ~ typ => (id, typ)
    }

    private def fun_params: Parser[List[(String, Type)]] = repsep(fun_param, ",")

    private def fun_def: Parser[Expr] = keyword("def") ~ id ~ "(" ~ fun_params ~ ")" ~ "{" ~ seq ~ "}" ^^ {
      case "def" ~ id ~ "(" ~ params ~ ")" ~ "{" ~ body ~ "}" => FunDef(id, params, body)
    }

    private def fun_app: Parser[Expr] = id ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
      case funName ~ "(" ~ list ~ ")" => FunApp(funName, list)
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

    private def seq: Parser[List[Expr]] = repsep(expr, ";")

    def apply(input: String): List[Expr] = parseAll(seq, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw ParseException(failure.msg)
    }
  }


  class Inferencer {
    private final case class TypeException(msg: String) extends RuntimeException(msg)

    private type Env = List[(String, Type)]

    private def expectType(expr: Expr, expected: Type, env: Env): (TypedExpr, Env) = infer(expr, env) match {
      case (expr, newEnv) if expr.typ == expected => (expr, newEnv)
      case (expr, _) => throw TypeException(s"Expected $expected, but found ${expr.typ}.")
    }

    private def expectEach(actual: List[Expr], expected: Type, env: Env): (List[TypedExpr], Env) = actual match {
      case expr :: exprs =>
        val (typedExpr, newEnv) = expectType(expr, expected, env)
        val t = expectEach(exprs, expected, newEnv)
        (typedExpr :: t._1, t._2)
      case Nil => (Nil, env)
    }

    private def expectBoth(left: Expr, right: Expr, expected: Type, env: Env): (TypedExpr, TypedExpr, Env) = {
      expectEach(List(left, right), expected, env) match {
        case (typedLeft :: typedRight :: Nil, env) => (typedLeft, typedRight, env)
      }
    }

    private def expectAll(actual: List[Expr], expected: List[Type], env: Env): (List[TypedExpr], Env) = actual match {
      case expr :: exprs =>
        expected match {
          case expected :: types =>
            val (typedExpr, newEnv) = expectType(expr, expected, env)
            val result = expectAll(exprs, types, newEnv)
            (typedExpr :: result._1, result._2)
          case Nil => throw TypeException("More expressions than expected.")
        }
      case _ if expected.nonEmpty => throw TypeException("Expected additional expressions.")
      case Nil => (Nil, env)
    }

    @tailrec
    private def lookup(id: String, env: Env): Type = env match {
      case (id2, typ) :: _ if id == id2 => typ
      case _ :: tail => lookup(id, tail)
      case Nil => builtin_constants.get(id) match {
        case Some(t) => t._1
        case None => builtin_functions.get(id) match {
          case Some(t) => t._1
          case None => throw TypeException(s"'$id' is not defined.")
        }
      }
    }

    private def tryInferConcat(l: Expr, r: Expr, env: Env): Option[(TypedExpr, Env)] = {
      val (leftExpr, newEnv1) = infer(l, env)
      val (rightExpr, newEnv2) = infer(r, newEnv1)

      if (leftExpr.typ == StrT() || rightExpr.typ == StrT())
        Some(TypedConcat(leftExpr, rightExpr), newEnv2)
      else
        None
    }

    private var env: Env = Nil

    def reset(): Unit = {
      env = Nil
    }

    def inferProgram(exprs: List[Expr]): List[TypedExpr] = {
      val (typedExprs, newEnv) = inferAll(exprs, env)
      this.env = newEnv
      typedExprs
    }

    private def inferAll(exprs: List[Expr], env: Env): (List[TypedExpr], Env) = exprs.foldLeft[(List[TypedExpr], Env)]((Nil, env))((t, expr) => {
      val (typedExpr, newEnv) = infer(expr, t._2)
      (t._1 ++ List(typedExpr), newEnv)
    })

    private def infer(expr: Expr, env: Env): (TypedExpr, Env) = expr match {
      case Num(n) => (TypedNum(n), env)
      case Bool(b) => (TypedBool(b), env)
      case Str(s) => (TypedStr(s), env)
      case Var(id) => (TypedVar(id, lookup(id, env)), env)
      case Neg(e) => expectType(e, NumT(), env)
      case Add(l, r) => tryInferConcat(l, r, env) match {
        case Some(concat) => concat
        case None =>
          val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env)
          (TypedAdd(tl, tr), newEnv)
      }
      case Sub(l, r) =>
        val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env)
        (TypedSub(tl, tr), newEnv)
      case Mul(l, r) =>
        val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env)
        (TypedMul(tl, tr), newEnv)
      case Div(l, r) =>
        val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env)
        (TypedDiv(tl, tr), newEnv)
      case Remainder(l, r) =>
        val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env)
        (TypedRemainder(tl, tr), newEnv)
      case FunApp(id, args) =>
        lookup(id, env) match {
          case FunT(params, ret) =>
            val (typedArgs, newEnv) = expectAll(args, params, env)
            (TypedFunApp(id, typedArgs, ret), newEnv)
          case _ => throw TypeException(s"'$id is not a function")
        }
      case FunDef(id, params, body) =>
        val (typedBody, _) = inferAll(body, params ++ env)
        val funExpr = TypedFunDef(id, params, typedBody, typedBody.last.typ)
        (funExpr, (id, funExpr.typ) :: env)
      case Assign(id, e) =>
        val (typedExpr, newEnv) = infer(e, env)
        (TypedAssign(id, typedExpr, typedExpr.typ), (id, typedExpr.typ) :: newEnv)
      case Reassign(id, e) =>
        val typ = lookup(id, env)
        val (typedExpr, newEnv) = expectType(e, typ, env)
        (TypedReassign(id, typedExpr, typedExpr.typ), newEnv)
    }
  }

  class Interpreter {

    private final case class InterpException(msg: String) extends RuntimeException(msg)

    private type Loc = Int

    private class Memory {
      private val storage: mutable.ArrayBuffer[Value] = mutable.ArrayBuffer.empty
      private val freeLocs: mutable.Queue[Loc] = mutable.Queue.empty

      def store(value: Value): Loc = {
        if (freeLocs.isEmpty) {
          storage += value
          storage.length - 1
        } else {
          val loc = freeLocs.dequeue()
          storage(loc) = value
          loc
        }
      }

      def update(loc: Loc, value: Value): Unit = {
        storage(loc) = value
      }

      def load(loc: Loc): Value = {
        storage(loc)
      }

      def free(loc: Loc): Unit = {
        freeLocs.enqueue(loc)
      }
    }

    private type Env = List[(String, Loc)]
    private var memory = new Memory
    private var env: Env = Nil

    def interpProgram(exprs: List[TypedExpr]): Value = {
      println(env)
      val (value, newEnv) = interpAll(exprs, env, Nil)
      this.env = newEnv
      value
    }

    def reset(): Unit = {
      memory = new Memory
      env = Nil
    }

    @tailrec
    private def interpAll(exprs: List[TypedExpr], localEnv: Env, parentEnv: Env): (Value, Env) = exprs match {
      case expr :: Nil =>
        val (result, newEnv) = interp(expr, localEnv, parentEnv)
        (result, newEnv)
      case expr :: tail =>
        val (_, newEnv) = interp(expr, localEnv, parentEnv)
        interpAll(tail, newEnv, parentEnv)
      case Nil => throw InterpException("Expression expected.")
    }

    private def interp(expr: TypedExpr, localEnv: Env, parentEnv: Env): (Value, Env) = expr match {
      case TypedNum(n) => (NumVal(n), localEnv)
      case TypedBool(b) => (BoolVal(b), localEnv)
      case TypedStr(str) => (StrVal(str), localEnv)
      case TypedVar(id, _) => (lookup(id, localEnv ++ parentEnv), localEnv)
      case TypedNeg(e) => interp(e, localEnv, parentEnv) match {
        case (NumVal(n), newEnv) => (NumVal(-n), newEnv)
      }
      case TypedAdd(l, r) => interpNumericBinOp(l, r, _ + _, localEnv, parentEnv)
      case TypedSub(l, r) => interpNumericBinOp(l, r, _ - _, localEnv, parentEnv)
      case TypedMul(l, r) => interpNumericBinOp(l, r, _ * _, localEnv, parentEnv)
      case TypedDiv(l, r) => interpNumericBinOp(l, r, _ / _, localEnv, parentEnv)
      case TypedRemainder(l, r) => interpNumericBinOp(l, r, _ % _, localEnv, parentEnv)
      case TypedConcat(l, r) =>
        val (leftVal, newEnv1) = interp(l, localEnv, parentEnv)
        val (rightVal, newEnv2) = interp(r, newEnv1, parentEnv)
        (StrVal(leftVal.toString + rightVal.toString), newEnv2)
      case TypedAssign(id, expr, _) =>
        val (newValue, newEnv) = interp(expr, localEnv, parentEnv)

        safeLocLookup(id, localEnv) match {
          case Some(loc) =>
            // If the variable exists in the current scope, overwrite it at its existing location
            // this can be done since the previous variable with the same name in this scope is now shadowed and can no
            // longer be accessed
            memory.update(loc, newValue)
            (newValue, newEnv)
          case None =>
            // If the variable does not exist in the current scope, a new memory location is created
            // this allows the shadowing of variables with the same name in higher-up scopes
            val loc = memory.store(newValue)
            (newValue, (id, loc) :: newEnv)
        }
      case TypedReassign(id, expr, _) =>
        val loc = locLookup(id, localEnv ++ parentEnv)
        val (newValue, newEnv) = interp(expr, localEnv, parentEnv)
        memory.update(loc, newValue)
        (newValue, newEnv)
      case TypedFunDef(id, params, body, ret) =>
        val fun = FunVal(id, params, body, ret)
        val loc = memory.store(fun)
        (fun, (id, loc) :: localEnv)
      case TypedFunApp(id, args, _) =>
        // Interp arguments
        val (argVals, newEnv) = args.foldLeft[(List[Value], Env)]((Nil, localEnv))((t, arg) => {
          val (value, newEnv) = interp(arg, t._2, parentEnv)
          (t._1 ++ List(value), newEnv)
        })

        safeLookup(id, localEnv ++ parentEnv) match {
          case Some(FunVal(_, params, body, _)) =>
            // Create new local env with function arguments
            val newLocalEnv = params.map(_._1).zip(argVals).map(t => (t._1, memory.store(t._2)))

            val (returnValue, funEnv) = interpAll(body, newLocalEnv, localEnv ++ parentEnv)
            // free local memory
            funEnv.foreach(t => memory.free(t._2))

            (returnValue, newEnv)
          case None => (applyBuiltinFunction(id, argVals), localEnv)
        }
    }

    private def interpNumericBinOp(left: TypedExpr, right: TypedExpr, op: (Double, Double) => Double, localEnv: Env, parentEnv: Env): (Value, Env) = {
      val (NumVal(l), newEnv) = interp(left, localEnv, parentEnv)
      val (NumVal(r), newEnv2) = interp(right, newEnv, parentEnv)
      (NumVal(op(l, r)), newEnv2)
    }

    private def applyBuiltinFunction(id: String, args: List[Value]): Value = builtin_functions.get(id) match {
      case Some((_, fun)) => fun(args)
      case None => throw InterpException(s"Function '$id' is not defined.")
    }

    private def locLookup(id: String, env: Env): Loc = safeLocLookup(id, env) match {
      case Some(loc) => loc
      case None => throw InterpException(s"'$id' is not a variable.")
    }

    @tailrec
    private def safeLocLookup(id: String, env: Env): Option[Loc] = env match {
      case (id2, loc) :: _ if id2 == id => Some(loc)
      case _ :: tail => safeLocLookup(id, tail)
      case _ => None
    }

    @tailrec
    private def safeLookup(id: String, env: Env): Option[Value] = env match {
      case (id2, loc) :: _ if id2 == id => Some(memory.load(loc))
      case _ :: tail => safeLookup(id, tail)
      case Nil => builtin_constants.get(id).map(_._2)
    }

    private def lookup(id: String, env: Env): Value = safeLookup(id, env) match {
      case Some(value) => value
      case None => throw InterpException(s"'$id' is not defined.")
    }
  }
}