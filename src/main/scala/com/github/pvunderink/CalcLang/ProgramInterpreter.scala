package com.github.pvunderink.CalcLang

import com.github.pvunderink.CalcLang.core.CalcLang.Value
import com.github.pvunderink.CalcLang.core.{Inferencer, Interpreter, Parser}

class ProgramInterpreter(val prelude: String,
                         val internalLookup: String => Option[Value]) {

  def this() = {
    this("", _ => None)
  }

  def this(prelude: String) = {
    this(prelude, _ => None)
  }

  def this(internalLookup: String => Option[Value]) = {
    this("", internalLookup)
  }

  val parser = new Parser
  val inferencer = new Inferencer(id => internalLookup(id).orElse(BuiltInGlobals.builtInGlobalLookup(id)).map(_.typ))
  val interpreter: Interpreter = new Interpreter(id => internalLookup(id).orElse(BuiltInGlobals.builtInGlobalLookup(id)))
  private var preludeInterpreted = false

  def reset: Unit = {
    inferencer.reset()
    interpreter.reset()
    preludeInterpreted = false
  }

  def usage: Int = interpreter.memory.usage

  def run(program: String): Value = {
    if (!preludeInterpreted) {
      preludeInterpreted = true
      if (prelude.nonEmpty) {
        run(prelude)
      }
    }

    val parsedProgram = parser.apply(program)
    val typedProgram = inferencer.inferProgram(parsedProgram)
    interpreter.interpProgram(typedProgram)
  }

}
