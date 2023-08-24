package com.github.pvunderink.CalcLang.core

object CalcLang {

  /**
   * Core expressions
   */

  sealed trait Expr

  final case class Num(n: Double) extends Expr

  final case class Bool(b: Boolean) extends Expr

  final case class Str(str: String) extends Expr

  final case class Var(id: String) extends Expr

  final case class Neg(e: Expr) extends Expr

  final case class Add(l: Expr, r: Expr) extends Expr

  final case class Sub(l: Expr, r: Expr) extends Expr

  final case class Mul(l: Expr, r: Expr) extends Expr

  final case class Div(l: Expr, r: Expr) extends Expr

  final case class Remainder(l: Expr, r: Expr) extends Expr

  final case class FunApp(expr: Expr, args: List[Expr]) extends Expr

  final case class Fun(params: List[(String, Type)], body: List[Expr]) extends Expr

  final case class Assign(id: String, e: Expr) extends Expr

  final case class Reassign(id: String, e: Expr) extends Expr

  final case class Void() extends Expr


  /**
   * Typed expressions
   */

  sealed trait TypedExpr {
    def typ: Type
  }

  final case class TypedNum(n: Double) extends TypedExpr {
    override def typ: Type = NumT()
  }

  final case class TypedBool(b: Boolean) extends TypedExpr {
    override def typ: Type = BoolT()
  }

  final case class TypedStr(str: String) extends TypedExpr {
    override def typ: Type = StrT()
  }

  final case class TypedVar(id: String, typ: Type, region: Inferencer.Region) extends TypedExpr

  final case class TypedNeg(e: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  final case class TypedAdd(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  final case class TypedConcat(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = StrT()
  }

  final case class TypedSub(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  final case class TypedMul(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  final case class TypedDiv(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  final case class TypedRemainder(l: TypedExpr, r: TypedExpr) extends TypedExpr {
    override def typ: Type = NumT()
  }

  final case class TypedFunApp(fun: TypedExpr, args: List[TypedExpr], ret: Type) extends TypedExpr {
    override def typ: Type = ret
  }

  final case class TypedFun(params: List[(String, Type)],
                            body: List[TypedExpr],
                            ret: Type,
                            env: List[(String, Type)],
                            regionSet: Set[Inferencer.Region]) extends TypedExpr {
    override def typ: Type = FunT(params.map(_._2), ret, Some(regionSet))
  }

  final case class TypedAssign(id: String, e: TypedExpr, typ: Type) extends TypedExpr

  final case class TypedReassign(id: String, e: TypedExpr, typ: Type) extends TypedExpr

  final case class TypedVoid() extends TypedExpr {
    override def typ: Type = VoidT()
  }


  /**
   * Types
   */

  sealed trait Type

  final case class NumT() extends Type {
    override def toString: String = "num"
  }

  final case class BoolT() extends Type {
    override def toString: String = "bool"
  }

  final case class StrT() extends Type {
    override def toString: String = "str"
  }

  final case class FunT(args: List[Type], ret: Type, regionSet: Option[Set[Inferencer.Region]]) extends Type {
    override def toString: String = s"(${if (args.nonEmpty) args.map(t => t.toString).reduce((acc, elem) => acc + ", " + elem) else ""}) -> ${ret.toString}"

    override def equals(obj: Any): Boolean = obj match {
      case FunT(args, ret, _) => args == this.args && ret == this.ret
      case _ => false
    }

    override def hashCode(): Int = args.hashCode ^ ret.hashCode
  }

  final case class VoidT() extends Type {
    override def toString: String = "void"
  }

  /**
   * Values
   */

  sealed trait Value {
    def size: Int
    def typ: Type
  }

  final case class NumVal(n: Double) extends Value {
    override def toString: String = n.toString

    override def size: Int = 8

    override def typ: Type = NumT()
  }

  final case class BoolVal(b: Boolean) extends Value {
    override def toString: String = b.toString

    override def size: Int = 1

    override def typ: Type = BoolT()
  }

  final case class StrVal(str: String) extends Value {
    override def toString: String = str

    override def size: Int = str.length * 2

    override def typ: Type = StrT()
  }

  final case class FunVal(params: List[(String, Type)], body: List[TypedExpr], ret: Type, env: List[(String, Value)]) extends Value {
    override def toString: String = s"(${if (params.nonEmpty) params.map(t => t._2.toString).reduce((acc, elem) => acc + ", " + elem) else ""}) -> ${ret.toString}"

    override def size: Int = env.map(_._2.size).sum

    override def typ: Type = FunT(params.map(_._2), ret, None)
  }

  final case class InternalFunVal(typ: FunT, executor: List[Value] => Value) extends Value {
    override def toString: String = typ.toString

    override def size: Int = 0
  }

  final case class VoidVal() extends Value {
    override def toString: String = "void"

    override def size: Int = 0

    override def typ: Type = VoidT()
  }

}
