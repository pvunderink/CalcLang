package com.github.pvunderink.CalcLang

object Main {
  def main(args: Array[String]): Unit = {
    val interpreter = new ProgramInterpreter

    println(interpreter.parser.apply("-4"))

    val result = interpreter.run("-4")
    println(result)
  }
}
