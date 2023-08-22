package com.github.pvunderink.CalcLang

object Main {
  def main(args: Array[String]): Unit = {
    val interpreter = new ProgramInterpreter

    interpreter.run("def x() {4}")
    val result = interpreter.run("x")

    println(result)
  }
}
