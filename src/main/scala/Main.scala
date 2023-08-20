import CalcLang.{ExprInterpreter, ExprParser}

object Main {
  def main(args: Array[String]): Unit = {
    val parser = new ExprParser
    val interpreter = new ExprInterpreter

    val expr = parser.apply("2*pi/e")
    println(expr)

    val result = interpreter.interp(expr)
    println(result)
  }
}
