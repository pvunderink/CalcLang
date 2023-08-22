package com.github.pvunderink.CalcLang

import com.github.pvunderink.CalcLang.CalcLang.{Inferencer, Interpreter, Parser, Value}

class ProgramInterpreter(prelude: Option[String]) {

  def this() = {
    this(None)
  }

  val parser = new Parser
  val inferencer = new Inferencer
  val interpreter: Interpreter = new Interpreter
  var preludeInterpreted = false

  def reset(): Unit = {
    inferencer.reset()
    interpreter.reset()
    preludeInterpreted = false
  }

  def run(program: String): Value = {
    if (!preludeInterpreted) {
      preludeInterpreted = true
      prelude match {
        case Some(prelude) =>
          run(prelude)
        case None => ()
      }
    }

    val parsedProgram = parser.apply(program)
    val typedProgram = inferencer.inferProgram(parsedProgram)
    interpreter.interpProgram(typedProgram)
  }

}
