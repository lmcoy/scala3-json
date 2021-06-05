package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.lang.StringBuilder
import json._


class ParserSpec extends AnyFlatSpec {

    "xxx" should "xxx" in {
        val json = 
          """
          |{
          | "x": [[4],[5,4],[6]],
          |  "a": {},
          |  "b": {"c":4}
          |}
          """.stripMargin('|')
        val result = Parser.parse(json)
        println(result)

        result match
        case Left(e) => fail(e.getMessage)
        case Right(r) => println(r.fourSpaces)


    }
  
}
