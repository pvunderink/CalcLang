package com.github.pvunderink.CalcLang

object Main {
  def main(args: Array[String]): Unit = {
    val interpreter = new ProgramInterpreter

    val program =
      """
        | def func() { var x = 30; () { x } }
        |""".stripMargin

    println(interpreter.parser.apply(program))

    val result = interpreter.run(program)

    interpreter.run("func()")

    println(result)
  }
}
