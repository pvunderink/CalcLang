package com.github.pvunderink.CalcLang

import com.github.pvunderink.CalcLang.core.CalcLang._

object BuiltInGlobals {
  private final val builtin_globals: Map[String, Value] = Map(
    "sqrt" ->
      InternalFunVal(
        FunT(List(NumT()), NumT(), Some(Set())),
        args => args.head match {
          case NumVal(n) => NumVal(math.sqrt(n))
        }
      ),
    "log" ->
      InternalFunVal(
        FunT(List(NumT()), NumT(), Some(Set())),
        args => args.head match {
          case NumVal(n) => NumVal(math.log(n))
        }
      ),
    "log10" ->
      InternalFunVal(
        FunT(List(NumT()), NumT(), Some(Set())),
        args => args.head match {
          case NumVal(n) => NumVal(math.log10(n))
        }
      ),
    "exp" ->
      InternalFunVal(
        FunT(List(NumT()), NumT(), Some(Set())),
        args => args.head match {
          case NumVal(n) => NumVal(math.exp(n))
        }
      ),
    "pow" ->
      InternalFunVal(
        FunT(List(NumT(), NumT()), NumT(), Some(Set())),
        args => (args.head, args.tail.head) match {
          case (NumVal(x), NumVal(y)) => NumVal(math.pow(x, y))
        }
      ),
    "pi" -> NumVal(math.Pi),
    "e" -> NumVal(math.E)
  )

  def builtInGlobalLookup(id: String): Option[Value] = builtin_globals.get(id)

}
