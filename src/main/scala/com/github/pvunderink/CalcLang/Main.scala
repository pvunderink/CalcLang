package com.github.pvunderink.CalcLang

import com.github.pvunderink.CalcLang.CalcLang.{Inferencer, Interpreter, Parser}

object Main {
  def main(args: Array[String]): Unit = {
    val parser = new Parser
    val parsedProgram = parser.apply("def const() {5}; const")
    println(parsedProgram)

    val inferencer = new Inferencer
    val typedProgram = inferencer.inferProgram(parsedProgram)
    println(typedProgram)

    val interpreter = new Interpreter
    val result = interpreter.interpProgram(typedProgram)
    println(result)
  }
}
