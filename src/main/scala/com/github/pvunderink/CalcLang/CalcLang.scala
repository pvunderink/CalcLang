package com.github.pvunderink.CalcLang

import scala.annotation.{tailrec, unused}
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

  private final case class FunApp(expr: Expr, args: List[Expr]) extends Expr

  private final case class Fun(params: List[(String, Type)], body: List[Expr]) extends Expr

  private final case class Assign(id: String, e: Expr) extends Expr

  private final case class Reassign(id: String, e: Expr) extends Expr

  private final case class Void() extends Expr

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

  private final case class TypedVar(id: String, typ: Type, region: Inferencer.Region) extends TypedExpr

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

  private final case class TypedFunApp(fun: TypedExpr, args: List[TypedExpr], ret: Type) extends TypedExpr {
    override def typ: Type = ret
  }

  private final case class TypedFun(params: List[(String, Type)],
                                    body: List[TypedExpr],
                                    ret: Type,
                                    env: List[(String, Type)],
                                    regionSet: Set[Inferencer.Region]) extends TypedExpr {
    override def typ: Type = FunT(params.map(_._2), ret)
  }

  private final case class TypedAssign(id: String, e: TypedExpr, typ: Type) extends TypedExpr

  private final case class TypedReassign(id: String, e: TypedExpr, typ: Type) extends TypedExpr

  private final case class TypedVoid() extends TypedExpr {
    override def typ: Type = VoidT()
  }

  sealed trait Value {
    def size: Int
  }

  private final case class NumVal(n: Double) extends Value {
    override def toString: String = n.toString

    override def size: Int = 8
  }

  private final case class BoolVal(b: Boolean) extends Value {
    override def toString: String = b.toString

    override def size: Int = 1
  }

  private final case class StrVal(str: String) extends Value {
    override def toString: String = str

    override def size: Int = str.length * 2
  }

  private final case class FunVal(params: List[(String, Type)], body: List[TypedExpr], ret: Type, env: List[(String, Value)]) extends Value {
    override def toString: String = s"(${if (params.nonEmpty) params.map(t => t._2.toString).reduce((acc, elem) => acc + ", " + elem) else ""}) -> ${ret.toString}"

    override def size: Int = env.map(_._2.size).sum
  }

  private final case class InternalFunVal(typ: FunT, executor: List[Value] => Value) extends Value {
    override def toString: String = typ.toString

    override def size: Int = 0
  }

  private final case class VoidVal() extends Value {
    override def toString: String = "void"

    override def size: Int = 0
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
    override def toString: String = s"(${if (args.nonEmpty) args.map(t => t.toString).reduce((acc, elem) => acc + ", " + elem) else ""}) -> ${ret.toString}"
  }

  private final case class VoidT() extends Type {
    override def toString: String = "void"
  }

  private final val builtin_functions: Map[String, InternalFunVal] = Map(
    "sqrt" ->
      InternalFunVal(
        FunT(List(NumT()), NumT()),
        args => args.head match {
          case NumVal(n) => NumVal(math.sqrt(n))
        }
      ),
    "log" ->
      InternalFunVal(
        FunT(List(NumT()), NumT()),
        args => args.head match {
          case NumVal(n) => NumVal(math.log(n))
        }
      ),
    "log10" ->
      InternalFunVal(
        FunT(List(NumT()), NumT()),
        args => args.head match {
          case NumVal(n) => NumVal(math.log10(n))
        }
      ),
    "exp" ->
      InternalFunVal(
        FunT(List(NumT()), NumT()),
        args => args.head match {
          case NumVal(n) => NumVal(math.exp(n))
        }
      ),
    "pow" ->
      InternalFunVal(
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

    private def factor_without_unop: Parser[Expr] = number | bool | str | fun_app | fun_def | assign | reassign | variable | function | grouped_expr

    private def grouped_expr: Parser[Expr] = "(" ~> expr <~ ")"

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

    private def funtyp: Parser[Type] = ("(" ~> repsep(typ, ",") <~ ")" <~ "->") ~ typ ^^ {
      case params ~ ret => FunT(params, ret)
    }

    private def fun_param: Parser[(String, Type)] = id ~ ":" ~ typ ^^ {
      case id ~ ":" ~ typ => (id, typ)
    }

    private def fun_params: Parser[List[(String, Type)]] = repsep(fun_param, ",")

    private def fun_def: Parser[Expr] = keyword("def") ~ id ~ function ^^ {
      case "def" ~ id ~ fun => Assign(id, fun)
    }

    private def function: Parser[Fun] = "(" ~ fun_params ~ ")" ~ "{" ~ seq ~ "}" ^^ {
      case "(" ~ params ~ ")" ~ "{" ~ body ~ "}" => Fun(params, body)
    }

    private def fun_app: Parser[Expr] = (variable | grouped_expr) ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
      case expr ~ "(" ~ list ~ ")" => FunApp(expr, list)
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

    private def stmt: Parser[Expr] = expr <~ ";" | fun_def

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

  object Inferencer {
    final case class Region(id: Integer)
  }


  class Inferencer {

    import Inferencer.Region

    private final case class TypeException(msg: String) extends RuntimeException(msg)

    private type Env = List[(String, Type, Region)]
    private type Context = List[Region]
    private type LookupFun = (String, Env, Boolean) => (Type, Region)

    private def expectType(expr: Expr, expected: Type, env: Env, region: Region, ctx: Context, lookupFun: LookupFun): (TypedExpr, Env) =
      infer(expr, env, region, ctx, lookupFun) match {
        case (expr, newEnv) if expr.typ == expected => (expr, newEnv)
        case (expr, _) => throw TypeException(s"Expected $expected, but found ${expr.typ}.")
      }

    private def expectEach(actual: List[Expr], expected: Type, env: Env, region: Region, ctx: Context, lookupFun: LookupFun): (List[TypedExpr], Env) =
      actual match {
        case expr :: exprs =>
          val (typedExpr, newEnv) = expectType(expr, expected, env, region, ctx, lookupFun)
          val t = expectEach(exprs, expected, newEnv, region, ctx, lookupFun)
          (typedExpr :: t._1, t._2)
        case Nil => (Nil, env)
      }

    private def expectBoth(left: Expr, right: Expr, expected: Type, env: Env, region: Region, ctx: Context, lookupFun: LookupFun): (TypedExpr, TypedExpr, Env) = {
      expectEach(List(left, right), expected, env, region, ctx, lookupFun) match {
        case (typedLeft :: typedRight :: Nil, env) => (typedLeft, typedRight, env)
      }
    }

    private def expectAll(actual: List[Expr], expected: List[Type], env: Env, region: Region, ctx: Context, lookupFun: LookupFun): (List[TypedExpr], Env) = actual match {
      case expr :: exprs =>
        expected match {
          case expected :: types =>
            val (typedExpr, newEnv) = expectType(expr, expected, env, region, ctx, lookupFun)
            val result = expectAll(exprs, types, newEnv, region, ctx, lookupFun)
            (typedExpr :: result._1, result._2)
          case Nil => throw TypeException("More expressions than expected.")
        }
      case _ if expected.nonEmpty => throw TypeException("Expected additional expressions.")
      case Nil => (Nil, env)
    }

    private def lookup(id: String, env: Env, @unused alter: Boolean): (Type, Region) = safeLookup(id, env).getOrElse(throw TypeException(s"'$id' is not defined."))

    @tailrec
    private def safeLookup(id: String, env: Env): Option[(Type, Region)] = env match {
      case (id2, typ, reg) :: _ if id == id2 => Some(typ, reg)
      case _ :: tail => safeLookup(id, tail)
      case Nil => builtin_constants.get(id) match {
        case Some(t) => Some(t._1, globalRegion)
        case None => builtin_functions.get(id) match {
          case Some(fun) => Some(fun.typ, globalRegion)
          case None => None
        }
      }
    }

    private def tryInferConcat(l: Expr, r: Expr, env: Env, region: Region, ctx: Context, lookupFun: LookupFun): Option[(TypedExpr, Env)] = {
      val (leftExpr, newEnv1) = infer(l, env, region, ctx, lookupFun)
      val (rightExpr, newEnv2) = infer(r, newEnv1, region, ctx, lookupFun)

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
      val (typedExprs, newEnv) = inferAll(exprs, env, globalRegion, List(globalRegion), lookup)
      this.env = newEnv
      typedExprs
    }

    private def inferAll(exprs: List[Expr], env: Env, region: Region, ctx: Context, lookupFun: LookupFun): (List[TypedExpr], Env) =
      exprs.foldLeft[(List[TypedExpr], Env)]((Nil, env))((t, expr) => {
        val (typedExpr, newEnv) = infer(expr, t._2, region, ctx, lookupFun)
        (t._1 ++ List(typedExpr), newEnv)
      })

    private def infer(expr: Expr, env: Env, region: Region, ctx: Context, lookupFun: LookupFun): (TypedExpr, Env) = expr match {
      case Num(n) => (TypedNum(n), env)
      case Bool(b) => (TypedBool(b), env)
      case Str(s) => (TypedStr(s), env)
      case Void() => (TypedVoid(), env)
      case Var(id) =>
        val (typ, reg) = lookupFun(id, env, false)
        (TypedVar(id, typ, reg), env)
      case Neg(e) =>
        val (te, newEnv) = expectType(e, NumT(), env, region, ctx, lookupFun)
        (TypedNeg(te), newEnv)
      case Add(l, r) => tryInferConcat(l, r, env, region, ctx, lookupFun) match {
        case Some(concat) => concat
        case None =>
          val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, region, ctx, lookupFun)
          (TypedAdd(tl, tr), newEnv)
      }
      case Sub(l, r) =>
        val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, region, ctx, lookupFun)
        (TypedSub(tl, tr), newEnv)
      case Mul(l, r) =>
        val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, region, ctx, lookupFun)
        (TypedMul(tl, tr), newEnv)
      case Div(l, r) =>
        val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, region, ctx, lookupFun)
        (TypedDiv(tl, tr), newEnv)
      case Remainder(l, r) =>
        val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, region, ctx, lookupFun)
        (TypedRemainder(tl, tr), newEnv)
      case FunApp(expr, args) =>
        infer(expr, env, region, ctx, lookupFun) match {
          case (expr, newEnv1) => expr.typ match {
            case FunT(params, ret) =>
              val (typedArgs, newEnv2) = expectAll(args, params, newEnv1, region, ctx, lookupFun)
              (TypedFunApp(expr, typedArgs, ret), newEnv2)
            case typ => throw TypeException(s"Expected a function, but got $typ")
          }
        }
      case Fun(params, body) =>
        val freeVariables: mutable.ListBuffer[(String, Type)] = mutable.ListBuffer.empty
        val regionSet: mutable.Set[Region] = mutable.Set.empty

        val parentEnv = env

        // Custom lookup function that keeps track of accesses outside the local scope
        def customLookupFun(id: String, localEnv: Env, alter: Boolean): (Type, Region) = safeLookup(id, localEnv) match {
          case Some(typ) => typ
          case None => safeLookup(id, parentEnv) match {
            case Some((typ, reg)) =>
              freeVariables.append((id, typ))
              if (alter) {
                // If the variable is being changed, add it to the region set
                regionSet.add(reg)
              }
              (typ, reg)
            case None => throw TypeException(s"'$id' is not defined.")
          }
        }

        val funRegion = newRegion()
        val (typedBody, _) = inferAll(body, params.map(p => (p._1, p._2, funRegion)), funRegion, region :: ctx, customLookupFun)
        val retExprType = typedBody.last

        retExprType match {
          case TypedFun(_, _, _, _, regionSet) =>
            if (!isValid(regionSet, ctx))
              throw TypeException(s"Cannot return a function that alters variables in a scope that will no longer exist after this function exits.")
          case _ => ()
        }

        (TypedFun(params, typedBody, retExprType.typ, freeVariables.toList, regionSet.toSet), env)
      case Assign(id, e) =>
        val (typedExpr, newEnv) = infer(e, env, region, ctx, lookupFun)

        typedExpr.typ match {
          case VoidT() => throw TypeException("Cannot assign void to a variable.")
          case _ => ()
        }

        (TypedAssign(id, typedExpr, typedExpr.typ), (id, typedExpr.typ, region) :: newEnv)
      case Reassign(id, e) =>
        val (typ, _) = lookupFun(id, env, true)
        val (typedExpr, newEnv) = expectType(e, typ, env, region, ctx, lookupFun)
        (TypedReassign(id, typedExpr, typedExpr.typ), newEnv)
    }

    private var global_region_id = 0
    private val globalRegion = newRegion() // IMPORTANT: Must be after `global_region_id = 0`

    // Region logic
    private def newRegion(): Region = {
      val region = Region(global_region_id)
      global_region_id += 1
      region
    }

    @tailrec
    private def isLive(region: Region, ctx: Context): Boolean = ctx match {
      case r2 :: _ if region.id == r2.id => true
      case _ :: tail => isLive(region, tail)
      case Nil => false
    }

    private def isValid(set: Set[Region], ctx: Context): Boolean = set.forall(isLive(_, ctx))
  }


  class Interpreter {

    private type Loc = Int
    private type Env = List[(String, Loc)]

    private final case class InterpException(msg: String) extends RuntimeException(msg)

    class Memory {
      private val storage: mutable.ArrayBuffer[Value] = mutable.ArrayBuffer.empty
      private val freeLocs: mutable.Set[Loc] = mutable.Set.empty

      def store(value: Value): Loc = {
        if (freeLocs.isEmpty) {
          storage += value
          storage.length - 1
        } else {
          val loc = freeLocs.drop(1).head
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
        freeLocs.add(loc)
      }

      def usage: Int =
        storage.view.zipWithIndex.map(t => {
          if (!freeLocs.contains(t._2)) {
            t._1.size
          } else {
            0
          }
        }).sum

    }

    private var internal_memory = new Memory

    def memory: Memory = internal_memory

    private var env: Env = Nil

    def interpProgram(exprs: List[TypedExpr]): Value = {
      val (value, newEnv) = interpAll(exprs, env, Nil)
      this.env = newEnv
      value
    }

    def reset(): Unit = {
      internal_memory = new Memory
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
      case TypedVar(id, _, _) => (lookup(id, localEnv ++ parentEnv), localEnv)
      case TypedVoid() => (VoidVal(), localEnv)
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
      case TypedFun(params, body, ret, funEnv, _) =>
        (FunVal(params, body, ret, funEnv.map(t => (t._1, lookup(t._1, localEnv ++ parentEnv)))), localEnv)
      case TypedFunApp(expr, args, _) =>
        // Interp arguments
        val (argVals, newEnv1) = args.foldLeft[(List[Value], Env)]((Nil, localEnv))((t, arg) => {
          val (value, newEnv) = interp(arg, t._2, parentEnv)
          (t._1 ++ List(value), newEnv)
        })

        interp(expr, newEnv1, parentEnv) match {
          case (funVal: FunVal, newEnv2) =>
            // Create new local env with function arguments
            val funEnv = (funVal.env ++ funVal.params.map(_._1).zip(argVals)).map(t => (t._1, memory.store(t._2)))

            val (returnValue, newFunEnv) = interpAll(funVal.body, funEnv, Nil)

            // free local memory
            newFunEnv.foreach(t => memory.free(t._2))

            (returnValue, newEnv2)
          case (internalFunVal: InternalFunVal, newEnv) =>
            (internalFunVal.executor(argVals), newEnv)
        }
    }

    private def interpNumericBinOp(left: TypedExpr, right: TypedExpr, op: (Double, Double) => Double, localEnv: Env, parentEnv: Env): (Value, Env) = {
      val (NumVal(l), newEnv) = interp(left, localEnv, parentEnv)
      val (NumVal(r), newEnv2) = interp(right, newEnv, parentEnv)
      (NumVal(op(l, r)), newEnv2)
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
      case Nil => builtin_constants.get(id).map(_._2).orElse(builtin_functions.get(id))
    }

    private def lookup(id: String, env: Env): Value = safeLookup(id, env) match {
      case Some(value) => value
      case None => throw InterpException(s"'$id' is not defined.")
    }
  }
}