package com.github.pvunderink.CalcLang

import com.github.pvunderink.CalcLang.CalcLang.{ExprInterpreter, ExprParser}

object Main {
  def main(args: Array[String]): Unit = {
    val parser = new ExprParser
    val interpreter = new ExprInterpreter

    val expr = parser.apply("exp(2)")
    println(expr)

    val result = interpreter.interp(expr)
    println(result)
  }
}
