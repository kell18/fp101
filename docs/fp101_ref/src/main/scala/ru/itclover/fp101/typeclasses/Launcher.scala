package ru.itclover.fp101.expression


object Launcher extends App {
  import JsonConverterInstances._

  val expr1: Expression = Plus(Value(1.0), Value(13.0))
  val expr2 = ("a", 3.0)

  println(JsonWriter.write(expr1))
  println(JsonWriter.write(expr2))

}
