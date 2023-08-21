package com.github.pvunderink.CalcLang

import com.github.pvunderink.CalcLang.CalcLang.{Inferencer, Interpreter, Parser}

object Main {
  def main(args: Array[String]): Unit = {
    val parser = new Parser
    val parsedProgram = parser.apply("def test(x: num, y: num) {x + \" \" + y}; test(1, 2)")
    println(parsedProgram)

    val inferencer = new Inferencer
    val typedProgram = inferencer.inferProgram(parsedProgram)
    println(typedProgram)

    val interpreter = new Interpreter
    val result = interpreter.interpProgram(typedProgram)
    println(result)
  }
}
