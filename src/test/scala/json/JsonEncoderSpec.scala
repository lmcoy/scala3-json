package json

import org.scalatest.flatspec.AnyFlatSpec
import JsonValue._
import JsonEncoder.given
import org.scalatest.matchers.should.Matchers._

class JsonEncoderSpec extends AnyFlatSpec {

  "A String" should "be converted into a JsonString" in {
    assert("hello".toJson == JsonString("hello"))
  }

  "An Int" should "be converted into a JsonInt" in {
    assert(4.toJson == JsonInt(4))
  }

  "A Boolean" should "be converted into a JsonBoolean" in {
    assert(true.toJson == JsonBoolean(true))
    assert(false.toJson == JsonBoolean(false))
  }

  "A List of Int" should "be converted into a JsonArray" in {
    val list = List(1, 2, 3, 4, 5)
    val expected = list.map(i => JsonInt(i))
    list.toJson match
      case JsonArray(elements) =>
        elements should contain theSameElementsInOrderAs expected
      case _ => fail("expected JsonArray")
  }

  "A case class" should "be converted into a JsonObject" in {
    case class Address(street: String, zipCode: String, city: String)
    case class Person(
        firstName: String,
        lastName: String,
        addresses: List[Address]
    )

    val person = Person(
      firstName = "John",
      lastName = "Doe",
      addresses = List(
        Address(street = "Main Street", zipCode = "12345", city = "Berlin")
      )
    )

    given personEncoder: JsonEncoder[Person] = JsonEncoder.derived

    val personJson = person.toJson

    personJson match {
      case JsonObject(fields) =>
        fields should contain("firstName", JsonString("John"))
        fields should contain("lastName", JsonString("Doe"))
        fields.toMap.get("addresses") match
          case Some(addresses) =>
            addresses match
              case JsonArray(elements) =>
                elements.size should equal(1)
                elements(0) match
                  case JsonObject(addressFields) =>
                    addressFields should contain(
                      "street",
                      JsonString("Main Street")
                    )
                    addressFields should contain("zipCode", JsonString("12345"))
                    addressFields should contain("city", JsonString("Berlin"))
                  case _ =>
                    fail(
                      s"expected JsonObject in `addresses` array but got ${elements(0).getClass}"
                    )
              case _ =>
                fail(
                  s"expected JsonArray in `addresses` but got ${addresses.getClass}"
                )
          case None =>
            fail(s"expected a field `addresses` but got ${fields.toMap.keys
              .mkString("(", ",", ")")}")
      case _ => fail(s"expected JsonObject got ${personJson.getClass}")
    }
  }

}
