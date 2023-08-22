package com.github.pvunderink.CalcLang

import com.github.pvunderink.CalcLang.CalcLang.{Inferencer, Interpreter, Parser}

object Main {
  def main(args: Array[String]): Unit = {
    val interpreter = new ProgramInterpreter

    interpreter.run("var x = 4")
    val result = interpreter.run("x")

    println(result)

    interpreter.reset()

    interpreter.run("x")

    println(result)
  }
}
