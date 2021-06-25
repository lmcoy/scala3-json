package json

import org.scalatest.flatspec.AnyFlatSpec
import json.JsonValue._
import json.JsonDecoder.given
import org.scalatest.matchers.should.Matchers._
import json.JsonDecoderSpec._

class JsonDecoderSpec extends AnyFlatSpec {

  "A JsonString" should "be decoded to a String" in {
    JsonString("Hello World!").as[String] should equal(Right("Hello World!"))
  }

  "A JsonString conversion to Int" should "fail with an error message" in {
    JsonString("hello world").as[Int] match
      case Left(error) =>
        error.getMessage should fullyMatch regex """cannot convert .+ to Int"""
      case _ => fail("expected the conversion to fail")
  }

  "A JsonInt" should "be decoded to a Int" in {
    JsonInt(10).as[Int] should equal(Right(10))
  }

  "A JsonBoolean" should "be decoded to Boolean" in {
    JsonBoolean(true).as[Boolean] should equal(Right(true))
    JsonBoolean(false).as[Boolean] should equal(Right(false))
  }

  "A JsonObject" should "be decoded into case class" in {
    val jobj = JsonObject(
      List(
        ("lastName", JsonString("Doe")),
        ("firstName", JsonString("John")),
        ("age", JsonInt(42)),
        (
          "addresses",
          JsonArray(
            List(
              JsonObject(
                List(
                  ("street", JsonString("Hauptstrasse")),
                  ("city", JsonString("berlin"))
                )
              ),
              JsonObject(
                List(
                  ("street", JsonString("main street")),
                  ("city", JsonString("london"))
                )
              )
            )
          )
        )
      )
    )
    val expected = Person(
      "John",
      "Doe",
      42,
      List(Address("Hauptstrasse", "berlin"), Address("main street", "london"))
    )
    jobj.as[Person] should equal(Right(expected))
  }

}

object JsonDecoderSpec {

  case class Address(street: String, city: String)
  case class Person(
      firstName: String,
      lastName: String,
      age: Int,
      addresses: List[Address]
  )
}
