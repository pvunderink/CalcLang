package com.github.pvunderink.CalcLang

object Main {
  def main(args: Array[String]): Unit = {
    val interpreter = new ProgramInterpreter

    val program =
      """
        | def error() {
        |   var x = 0;
        |   def f () { x = 42 };
        |   f
        | }
        |""".stripMargin

    println(interpreter.parser.apply(program))

    interpreter.run(program)

    val result = interpreter.run("error()")

    println(result)
  }
}
