package examples.json

import examples.printable.Person

sealed trait Json
case class JsonString(value: String) extends Json
case class JsonObject(m: Map[String, Json]) extends Json
case class JsonNumber[A : Numeric](num: A) extends Json
case class JsonBoolean(b: Boolean) extends Json

//typeclass
trait JsonWriter[A] {
  def toJson(a: A): Json
}

//instances
object JsonInstances {
  implicit val stringToJson = new JsonWriter[String] {
    def toJson(str: String) = JsonString(str)
  }

  implicit val personToJson = new JsonWriter[Person] {
    def toJson(p: Person) = JsonObject(
      Map(
        "id"      -> JsonNumber[Int](p.id),
        "name"    -> JsonString(p.name),
        "address" -> JsonString(p.address)
      )
    )
  }
}

//plain interface
object JsonWriter {
  def apply[A](implicit a: JsonWriter[A]) = a
  def toJson[A](value: A)(implicit a: JsonWriter[A]) = a.toJson(value)
}

//syntax
object JsonWriterOps {
  implicit class JsonWriterSyntax[A : JsonWriter](a: A) {
    def toJson = JsonWriter[A].toJson(a)
  }
}

object JsonWriterMain extends App {
  import JsonInstances._
  import JsonWriterOps._

  val spidey = Person(1, "Spider-Man", "Manhattan")

  val jsonSpidey = JsonWriter.toJson(spidey)
  val jsonSpiderMan = spidey.toJson

  println(s"This $jsonSpidey should be the same with $jsonSpiderMan")
}
