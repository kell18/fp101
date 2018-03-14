package ru.itclover.fp101.expression


sealed trait JsonValue

case class JsonObject(entries: Map[String, JsonValue]) extends JsonValue
case class JsonArray(array: Seq[JsonValue]) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonNumber(value: Double) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case object JsonNull extends JsonValue


object JsonWriter {

  def write(json: JsonValue): String = json match {
    case JsonObject(entries) =>
      val items = for { (key, value) <- entries } yield key + ": " + write(value)
      items.mkString("{", ", ", "}")
    case JsonArray(array) => array.map(i => write(i)).mkString("[", ", ", "]")
    case JsonString(v) => s"'$v'"
    case JsonNumber(value) => value.toString
    case JsonBoolean(value) => value.toString
    case JsonNull => "null"
  }

  def write[T](value: T)(implicit converter: JsonConverter[T]): String = write(converter.toJson(value))
}


trait JsonConverter[T] {
  def toJson(value: T): JsonValue
}

object JsonConverterInstances {
  implicit val pairJsonConverter = new JsonConverter[(String, Double)] {
    override def toJson(value: (String, Double)) = JsonObject(Map(value._1 -> JsonNumber(value._2)))
  }
}
