package json

import org.scalatest.flatspec.AnyFlatSpec
import JsonValue._
import JsonFormatter.given
import org.scalatest.matchers.should.Matchers._
import java.lang.StringBuilder

class JsonFormatterSpec extends AnyFlatSpec {

  "MinimalFormatter" should "format a JsonInt" in {
    JsonInt(10).minimal should equal("10")
    JsonInt(-10).minimal should equal("-10")
    JsonInt(0).minimal should equal("0")
  }

  it should "format a JsonBoolean" in {
    JsonBoolean(true).minimal should equal("true")
    JsonBoolean(false).minimal should equal("false")
  }

  it should "format a JsonString" in {
    JsonString("hello world").minimal should equal("\"hello world\"")
    JsonString("a\n\b\t\r\\\"").minimal should equal(
      "\"a\\n\\b\\t\\r\\\\\\\"\""
    )
    JsonString(
      new StringBuilder().appendCodePoint(1).appendCodePoint(19).toString
    ).minimal should equal(""""\\u0001\\u0013"""")
  }

  it should "format a JsonArray" in {
    JsonArray(Nil).minimal should equal("[]")
    JsonArray(JsonInt(5) :: Nil).minimal should equal("[5]")
    JsonArray(
      JsonInt(-10) :: JsonInt(0) :: JsonInt(10) :: Nil
    ).minimal should equal("[-10,0,10]")
  }

}
