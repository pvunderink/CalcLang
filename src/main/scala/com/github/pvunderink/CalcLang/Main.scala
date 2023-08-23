package com.github.pvunderink.CalcLang

object Main {
  def main(args: Array[String]): Unit = {
    val interpreter = new ProgramInterpreter

    val program =
      """
        | var str = "some string"
        |""".stripMargin

    """
      | def func() {
      |   var y = 10;
      |   y
      | };
      | func()
      | // allowed
      |
      | def func() {
      |   var y = 10;
      |   () { y }
      | };
      | func()
      | // allowed
      |
      | def func() {
      |   var y = 10;
      |   var f = () { y = y + 1 };
      |   f()
      | };
      | func()
      | // allowed
      |
      | def func() {
      |   var y = 10;
      |   () { y = y + 1 }
      | };
      | func()
      | // not allowed
      |""".stripMargin

    println(interpreter.parser.apply(program))

    val result = interpreter.run(program)
    println(result)
  }
}
