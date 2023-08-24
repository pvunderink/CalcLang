package com.github.pvunderink.CalcLang.core

import com.github.pvunderink.CalcLang.core.CalcLang._

import scala.annotation.tailrec
import scala.collection.mutable

class Interpreter(val internalLookup: String => Option[Value]) {

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
        val loc = freeLocs.head
        freeLocs.remove(loc)
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
      storage.view.zipWithIndex.map(t =>
        if (freeLocs.contains(t._2)) {
          0
        } else {
          t._1.size
        }
      ).sum

    def count: Int =
      storage.view.zipWithIndex.flatMap(t =>
        if (freeLocs.contains(t._2)) {
          None
        } else {
          Some(t)
        }
      ).size
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

          val (returnValue, newFunEnv) = interpAll(funVal.body, funEnv, localEnv ++ parentEnv)

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
    case None => throw InterpException(s"'$id' is not currently in memory. This should not happen and is likely a result of a bug in the type-checker or interpreter.")
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
    case Nil => internalLookup(id)
  }

  private def lookup(id: String, env: Env): Value = safeLookup(id, env) match {
    case Some(value) => value
    case None => throw InterpException(s"'$id' is not defined. This should not happen and is likely a result of a bug in the type-checker or interpreter.")
  }
}
