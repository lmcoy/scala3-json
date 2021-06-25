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
    val expected = JsonObject(
      List(
        (
          "x",
          JsonArray(
            List(
              JsonArray(List(JsonInt(4))),
              JsonArray(List(JsonInt(5), JsonInt(4))),
              JsonArray(List(JsonInt(6)))
            )
          )
        ),
        ("a", JsonObject(Nil)),
        ("b", JsonObject(List(("c", JsonInt(4)))))
      )
    )

    result match
      case Left(e) => fail(e.getMessage)
      case Right(r) =>
        r should equal(expected)
  }

  it should "parse an integer into JsonInt" in {
    val json = "4"
    val result = Parser.parse(json)
    val expected = JsonInt(4)

    result match
      case Left(e) => fail(e.getMessage)
      case Right(r) =>
        r should equal(expected)
  }

  it should "parse a string into JsonString" in {
    val json = "\"s\\ntr\\t\\b\\r\\\\\\f\\\"😀\\u00fc\""
    val result = Parser.parse(json)
    val expected = JsonString("s\ntr\t\b\r\\\f\"😀ü")

    result match
      case Left(e) => fail(e.getMessage)
      case Right(r) =>
        r should equal(expected)
  }

  it should "fail if a string containing a control char is parsed" in {
    val json = "\"a\n\""
    val result = Parser.parse(json)

    result match
      case Left(e) =>
        e.getMessage should equal(
          "error at line 0 col 2: control char in string"
        )
      case Right(r) => fail(s"expected failure $r")
  }

  it should "parse an array" in {
    val json = "[4, 6, 9, 3]"
    val result = Parser.parse(json)

    result match
      case Left(e) => fail(s"expected success, got $e")
      case Right(r) =>
        r should equal(
          JsonArray(List(JsonInt(4), JsonInt(6), JsonInt(9), JsonInt(3)))
        )
  }

  it should "parse an empty array" in {
    Parser.parse("[]") match
      case Left(e) => fail(s"expected successs, got $e")
      case Right(r) =>
        r should equal(JsonArray(Nil))
  }

  it should "fail when a closing ] is missing" in {
    val json = "[4, 5, 7"

    Parser.parse(json) match
      case Left(e) =>
        e.getMessage should equal("error at line 0 col 7: expected , or ]")
        e.reason should equal("expected , or ]")
        e.col should equal(7)
        e.line should equal(0)
      case Right(r) => fail(s"expected failure but got $r")
  }

  it should "fail when a , is missing in array" in {
    Parser.parse("[4 5]") match
      case Left(e) =>
        e.getMessage should equal("error at line 0 col 3: expected , or ]")
        e.reason should equal("expected , or ]")
        e.col should equal(3)
        e.line should equal(0)
      case Right(r) => fail(s"expected failure but got $r")
  }

  it should "parse an empty object" in {
    Parser.parse("{}") match
      case Left(e) => fail(s"expected successs, got $e")
      case Right(r) =>
        r should equal(JsonObject(Nil))
  }

  it should "fail when a } is missing" in {
    Parser.parse("{ \"a\": 4 ") match
      case Left(e) =>
        e.getMessage should equal(
          "error at line 0 col 5: unexpected EOF, expected `,` or `}`"
        )
        e.reason should equal("unexpected EOF, expected `,` or `}`")
        e.col should equal(5)
        e.line should equal(0)
      case Right(r) => fail(s"expected failure but got $r")
  }

  it should "fail when the field name is not a string" in {
    Parser.parse("{ 5: 4 }") match
      case Left(e) =>
        e.getMessage should equal(
          "error at line 0 col 2: expected string as field name"
        )
        e.reason should equal("expected string as field name")
        e.col should equal(2)
        e.line should equal(0)
      case Right(r) => fail(s"expected failure but got $r")
  }

  it should "fail when a colon is missing" in {
    Parser.parse("{ \"a\": 5, \"b\" 4 }") match
      case Left(e) =>
        e.getMessage should equal("error at line 0 col 14: expected `:`")
        e.reason should equal("expected `:`")
        e.col should equal(14)
        e.line should equal(0)
      case Right(r) => fail(s"expected failure but got $r")
  }

  it should "fail when a comma is missing" in {
    Parser.parse("{ \"a\": 5 \"b\": 4 }") match
      case Left(e) =>
        e.getMessage should equal("error at line 0 col 9: expected `,` or `}`")
        e.reason should equal("expected `,` or `}`")
        e.col should equal(9)
        e.line should equal(0)
      case Right(r) => fail(s"expected failure but got $r")
  }
}
