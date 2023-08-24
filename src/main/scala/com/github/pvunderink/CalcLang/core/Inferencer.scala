package com.github.pvunderink.CalcLang.core

import com.github.pvunderink.CalcLang.core.CalcLang._

import scala.annotation.{tailrec, unused}
import scala.collection.mutable

object Inferencer {
  final case class Region(id: Integer)
}

class Inferencer(val internalLookup: String => Option[Type]) {

  import Inferencer.Region

  private final case class TypeException(msg: String) extends RuntimeException(msg)

  private type Env = List[(String, Type, Region)]
  private type Context = List[Region]
  private type LookupFun = (String, Env, Boolean) => (Type, Region)

  private def expectType(expr: Expr, expected: Type, env: Env, parentEnv: Env, region: Region, ctx: Context, lookupFun: LookupFun): (TypedExpr, Env) =
    infer(expr, env, parentEnv, region, ctx, lookupFun) match {
      case (expr, newEnv) if expr.typ == expected => (expr, newEnv)
      case (expr, _) => throw TypeException(s"Expected $expected, but found ${expr.typ}.")
    }

  private def expectEach(actual: List[Expr], expected: Type, env: Env, parentEnv: Env, region: Region, ctx: Context, lookupFun: LookupFun): (List[TypedExpr], Env) =
    actual match {
      case expr :: exprs =>
        val (typedExpr, newEnv) = expectType(expr, expected, env, parentEnv, region, ctx, lookupFun)
        val t = expectEach(exprs, expected, newEnv, parentEnv, region, ctx, lookupFun)
        (typedExpr :: t._1, t._2)
      case Nil => (Nil, env)
    }

  private def expectBoth(left: Expr,
                         right: Expr,
                         expected: Type,
                         env: Env,
                         parentEnv: Env,
                         region: Region,
                         ctx: Context,
                         lookupFun: LookupFun): (TypedExpr, TypedExpr, Env) = {
    expectEach(List(left, right), expected, env, parentEnv, region, ctx, lookupFun) match {
      case (typedLeft :: typedRight :: Nil, env) => (typedLeft, typedRight, env)
    }
  }

  private def expectAll(actual: List[Expr], expected: List[Type], env: Env, parentEnv: Env, region: Region, ctx: Context, lookupFun: LookupFun): (List[TypedExpr], Env) = actual match {
    case expr :: exprs =>
      expected match {
        case expected :: types =>
          val (typedExpr, newEnv) = expectType(expr, expected, env, parentEnv, region, ctx, lookupFun)
          val result = expectAll(exprs, types, newEnv, parentEnv, region, ctx, lookupFun)
          (typedExpr :: result._1, result._2)
        case Nil => throw TypeException("More expressions than expected.")
      }
    case _ if expected.nonEmpty => throw TypeException("Expected additional expressions.")
    case Nil => (Nil, env)
  }

  private def lookup(id: String, env: Env, @unused alter: Boolean): (Type, Region) =
    safeLookup(id, env).getOrElse(throw TypeException(s"'$id' is not defined."))

  @tailrec
  private def safeLookup(id: String, env: Env): Option[(Type, Region)] = env match {
    case (id2, typ, reg) :: _ if id == id2 => Some(typ, reg)
    case _ :: tail => safeLookup(id, tail)
    case Nil => internalLookup(id).map(t => (t, globalRegion))
  }

  private def tryInferConcat(l: Expr,
                             r: Expr,
                             env: Env,
                             parentEnv: Env,
                             region: Region,
                             ctx: Context,
                             lookupFun: LookupFun): Option[(TypedExpr, Env)] = {
    val (leftExpr, newEnv1) = infer(l, env, parentEnv, region, ctx, lookupFun)
    val (rightExpr, newEnv2) = infer(r, newEnv1, parentEnv, region, ctx, lookupFun)

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
    val (typedExprs, newEnv) = inferAll(exprs, env, Nil, globalRegion, List(globalRegion), lookup)
    this.env = newEnv
    typedExprs
  }

  private def inferAll(exprs: List[Expr],
                       env: Env,
                       parentEnv: Env,
                       region: Region,
                       ctx: Context,
                       lookupFun: LookupFun): (List[TypedExpr], Env) =
    exprs.foldLeft[(List[TypedExpr], Env)]((Nil, env))((t, expr) => {
      val (typedExpr, newEnv) = infer(expr, t._2, parentEnv, region, ctx, lookupFun)
      (t._1 ++ List(typedExpr), newEnv)
    })

  private def infer(expr: Expr,
                    env: Env,
                    parentEnv: Env,
                    region: Region,
                    ctx: Context,
                    lookupFun: LookupFun): (TypedExpr, Env) = expr match {
    case Num(n) => (TypedNum(n), env)
    case Bool(b) => (TypedBool(b), env)
    case Str(s) => (TypedStr(s), env)
    case Void() => (TypedVoid(), env)
    case Var(id) =>
      val (typ, reg) = lookupFun(id, env, false)
      (TypedVar(id, typ, reg), env)
    case Neg(e) =>
      val (te, newEnv) = expectType(e, NumT(), env, parentEnv, region, ctx, lookupFun)
      (TypedNeg(te), newEnv)
    case Add(l, r) => tryInferConcat(l, r, env, parentEnv, region, ctx, lookupFun) match {
      case Some(concat) => concat
      case None =>
        val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, parentEnv, region, ctx, lookupFun)
        (TypedAdd(tl, tr), newEnv)
    }
    case Sub(l, r) =>
      val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, parentEnv, region, ctx, lookupFun)
      (TypedSub(tl, tr), newEnv)
    case Mul(l, r) =>
      val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, parentEnv, region, ctx, lookupFun)
      (TypedMul(tl, tr), newEnv)
    case Div(l, r) =>
      val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, parentEnv, region, ctx, lookupFun)
      (TypedDiv(tl, tr), newEnv)
    case Remainder(l, r) =>
      val (tl, tr, newEnv) = expectBoth(l, r, NumT(), env, parentEnv, region, ctx, lookupFun)
      (TypedRemainder(tl, tr), newEnv)
    case FunApp(expr, args) =>
      infer(expr, env, parentEnv, region, ctx, lookupFun) match {
        case (expr, newEnv1) => expr.typ match {
          case FunT(params, ret, _) =>
            val (typedArgs, newEnv2) = expectAll(args, params, newEnv1, parentEnv, region, ctx, lookupFun)
            (TypedFunApp(expr, typedArgs, ret), newEnv2)
          case typ => throw TypeException(s"Expected a function, but got $typ")
        }
      }
    case Fun(params, body) =>
      val freeVariables: mutable.HashMap[String, Type] = mutable.HashMap.empty
      val editedVariables: mutable.ListBuffer[String] = mutable.ListBuffer.empty
      val regionSet: mutable.Set[Region] = mutable.Set.empty

      // Custom lookup function that keeps track of accesses outside the local scope
      def customLookupFun(id: String, localEnv: Env, alter: Boolean): (Type, Region) = safeLookup(id, localEnv) match {
        case Some(typ) => typ
        case None => safeLookup(id, env ++ parentEnv) match {
          case Some((typ, reg)) =>
            freeVariables(id) = typ
            if (alter) {
              editedVariables.append(id)
              // If the variable is being re-assigned, add it to the region set.
              // Re-assigning can be thought of de-referencing a pointer and updating the value at that pointer.
              // TODO: When adding more pointer-like behaviour or actual pointers to the language, these must also be properly accounted for during region analysis
              regionSet.add(reg)
            }
            (typ, reg)
          case None => throw TypeException(s"'$id' is not defined.")
        }
      }

      val funRegion = newRegion()
      // Infer / type-check the function body
      val (typedBody, _) = inferAll(body, params.map(p => (p._1, p._2, funRegion)), env ++ parentEnv, funRegion, region :: ctx, customLookupFun)
      val retExprType = typedBody.last

      // Remove all variables that are re-assigned (i.e. treated as pointers) from the free variables
      // such that they are not shadowed
      editedVariables.foreach(x => freeVariables.remove(x))

      retExprType.typ match {
        case FunT(_, _, Some(regionSet)) =>
          if (!isValid(regionSet, ctx))
            throw TypeException(s"Cannot return a function that alters variables in a scope that does not outlive the returning function.")
        case FunT(_, _, None) =>
          throw TypeException(s"Region set has not been calculated, this should not happen and is a bug in the type checker.")
        case _ => ()
      }

      (TypedFun(params, typedBody, retExprType.typ, freeVariables.toList, regionSet.toSet), env)
    case Assign(id, e) =>
      val (typedExpr, newEnv) = infer(e, env, parentEnv, region, ctx, lookupFun)

      typedExpr.typ match {
        case VoidT() => throw TypeException("Cannot assign void to a variable.")
        case _ => ()
      }

      (TypedAssign(id, typedExpr, typedExpr.typ), (id, typedExpr.typ, region) :: newEnv)
    case Reassign(id, e) =>
      val (typ, _) = lookupFun(id, env, true)
      val (typedExpr, newEnv) = expectType(e, typ, env, parentEnv, region, ctx, lookupFun)
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