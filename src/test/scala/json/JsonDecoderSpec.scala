package json

import org.scalatest.flatspec.AnyFlatSpec
import json.JsonValue._
import json.JsonDecoder.given
import org.scalatest.matchers.should.Matchers._

class JsonDecoderSpec extends AnyFlatSpec {

    "A JsonString" should "be decoded to a String" in {
        JsonString("Hello World!").as[String] should equal (Right("Hello World!"))
    }

    "A JsonString conversion to Int" should "fail with an error message" in {
        JsonString("hello world").as[Int] match
            case Left(error) => 
                error.getMessage should fullyMatch regex """cannot convert .+ to Int"""
            case _ => fail("expected the conversion to fail")
    }

}