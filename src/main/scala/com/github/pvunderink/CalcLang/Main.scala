package com.github.pvunderink.CalcLang

object Main {
  def main(args: Array[String]): Unit = {
    val interpreter = new ProgramInterpreter

    val program =
      """
        | var x = 0;
        | def func() {
        |   (y: num) { x = y }
        | }
        | func()(42);
        | x
        |""".stripMargin

    println(interpreter.parser.apply(program))

    val result = interpreter.run(program)

    println(result);
  }
}
