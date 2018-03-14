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

  def evaluate[T](value: T)(implicit conv: ExpressionConverter[T]): Double = evaluate(conv.toExpression(value))

  implicit val jsonConverter = new JsonConverter[Expression] {
    override def toJson(value: Expression): JsonValue = value match {
      case Value(num) => JsonNumber(num)
      case Plus(l, r) => JsonObject(Map(
        "op" -> JsonString("+"),
        "left" -> toJson(l),
        "right" -> toJson(r)
      ))
      case Minus(l, r) => JsonObject(Map(
        "op" -> JsonString("-"),
        "left" -> toJson(l),
        "right" -> toJson(r)
      ))
    }
  }
}

trait ExpressionConverter[T] {
  def toExpression(value: T): Expression
}

object ExpressionConverterInstances {
  implicit val tuple3ToExpr = new ExpressionConverter[(Double, (Double, Double))] {
    override def toExpression(value: (Double, (Double, Double))) = {
      val (a, (b, c)) = value
      Plus(Value(a), Minus(Value(b), Value(c)))
    }
  }
}
