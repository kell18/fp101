package ru.itclover.fp101.expression


object Launcher extends App {
  import Expression._

  val expr1: Expression = Plus(Value(2.0), Minus(Value(10.0), Value(3.0)))

  println(Expression.evaluate(expr1))

  val jsonValue = JsonWriter.convertToJson(expr1)

  println(JsonWriter.write(jsonValue))
}
