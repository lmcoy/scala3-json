package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.lang.StringBuilder
import json._
import json.JsonValue._


class ParserSpec extends AnyFlatSpec {

    "parse" should "parse a string containing a json object into a JsonValue" in {
        val json = 
          """
          |{
          | "x": [[4],[5,4],[6]],
          |  "a": {},
          |  "b": {"c":4}
          |}
          """.stripMargin('|')
        val result = Parser.parse(json)
        val expected = JsonObject(List(
          ("x", JsonArray(List(
            JsonArray(List(JsonInt(4))),
            JsonArray(List(JsonInt(5), JsonInt(4))),
            JsonArray(List(JsonInt(6)))))
          ),
          ("a", JsonObject(Nil)),
          ("b", JsonObject(List(("c", JsonInt(4))))
        )))

        result match
          case Left(e) => fail(e.getMessage)
          case Right(r) => 
            r should equal (expected)
    }

    it should "parse an integer into JsonInt" in {
        val json = "4"
        val result = Parser.parse(json)
        val expected = JsonInt(4)

        result match
          case Left(e) => fail(e.getMessage)
          case Right(r) => 
            r should equal (expected)
    }

    it should "parse a string into JsonString" in {
        val json = "\"s\\ntr\\t\\b\\r\\\\\\f\\\"😀\\u00fc\""
        val result = Parser.parse(json)
        val expected = JsonString("s\ntr\t\b\r\\\f\"😀ü")

        result match
          case Left(e) => fail(e.getMessage)
          case Right(r) => 
            r should equal (expected)
    }
  
    it should "fail if a string containing a control char is parsed" in {
        val json = "\"a\n\""
        val result = Parser.parse(json)
        
        result match
          case Left(e) => 
            println(e.getMessage)
            e.getMessage should equal ("error at line 0 col 2: control char in string")
          case Right(r) => fail(s"expected failure $r")
    }
}
