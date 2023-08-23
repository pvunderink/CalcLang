package com.github.pvunderink.CalcLang

object Main {
  def main(args: Array[String]): Unit = {
    val interpreter = new ProgramInterpreter

    val program =
      """
        | def high_func(f: () -> num) { f() }
        |""".stripMargin

    println(interpreter.parser.apply(program))

    interpreter.run(program)

    val result = interpreter.run("high_func(() { 10 })")

    println(result)
  }
}
