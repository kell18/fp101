package ru.itclover.fp101.expression

sealed trait Expression

case class Value(num: Double) extends Expression
case class Plus(left: Expression, Right: Expression) extends Expression
case class Minus(left: Expression, right: Expression) extends Expression

object Expression {
  def evaluate(expr: Expression): Double = expr match {
    case Value(num) => num
    case Plus(l, r) => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
  }

  implicit val converter = new JsonConverter[Expression] {
    override def convertToJson(value: Expression) = value match {
      case Value(num) => JsonNumber(num)
      case Plus(l, r) => JsonObject(Map(
        "op" -> JsonString("+"),
        "left" -> convertToJson(l),
        "right" -> convertToJson(r)
      ))
      case Minus(l, r) => JsonObject(Map(
        "op" -> JsonString("-"),
        "left" -> convertToJson(l),
        "right" -> convertToJson(r)
      ))
    }
  }
}
