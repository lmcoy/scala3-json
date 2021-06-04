
case class Address(street: String, city: String) extends Product

case class Person(firstName: String, lastName: String, age: Int, address: Address) extends Product

@main def hello: Unit = 
  import JsonEncoder.given
  val x = 4

  given JsonEncoder[Person] = JsonEncoder.derived

  val person = Person("Joh\"n", "Doe", 42, Address("Kurfurstendamm", "Berlin"))
  println("hello")
  println(x.toJson)
  println("hello".toJson)
  println(List("test", "test2").toJson)
  val personJson = person.toJson
  println(personJson)

  println(person.toJson.fourSpaces)
  import JsonFormatter.Indent.twoSpaces
  println(List(person, person, person).toJson.format)

  import JsonDecoder.given 
  import JsonValue._
  println(JsonInt(4).as[BigInt])
  println(JsonArray(List(JsonInt(4), JsonInt(5), JsonInt(6))).as[List[BigInt]])

  given JsonDecoder[Person] = JsonDecoder.derived
  println(personJson.as[Person])