package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.lang.StringBuilder
import Tokenizer.Error


class TokenizerSpec extends AnyFlatSpec {
  "unsignedInteger" should "tokenize a string containing only a number" in {
      val tokenizer = Tokenizer("12345")
      val (offset, number) = tokenizer.unsignedInteger(0, new StringBuilder())
      offset should equal (5)
      number.toString should equal ("12345")
  }

  it should "read only the leading integer part" in {
     val tokenizer = Tokenizer("12345,t")
      val (offset, number) = tokenizer.unsignedInteger(0, new StringBuilder())
      offset should equal (5)
      number.toString should equal ("12345") 
  }

  "signedInteger" should "tokenize a string containing only an unsigned number" in {
      val tokenizer = Tokenizer("12345")
      val (offset, number) = tokenizer.signedInteger(0, new StringBuilder())
      offset should equal (5)
      number.toString should equal ("12345")
  }

  it should "tokenize a string containing negative number" in {
      val tokenizer = Tokenizer("-12345")
      val (offset, number) = tokenizer.signedInteger(0, new StringBuilder())
      offset should equal (6)
      number.toString should equal ("-12345")
  }

  it should "tokenize a string containing signed positive number" in {
      val tokenizer = Tokenizer("+12345")
      val (offset, number) = tokenizer.signedInteger(0, new StringBuilder())
      offset should equal (6)
      number.toString should equal ("+12345")
  }

  it should "tokenize the leading number in a string" in {
      val tokenizer = Tokenizer("+12345djakls")
      val (offset, number) = tokenizer.signedInteger(0, new StringBuilder())
      offset should equal (6)
      number.toString should equal ("+12345")
  }

  def numberToken(s: String): (Int, Token.IntegerNumber | Token.RationalNumber) = {
     val tokenizer = Tokenizer(s) 
     tokenizer.number(0) match
        case Some(r) => r 
        case None => fail("expected correctly parsed number") 
  } 

  "number" should "create tokens for valid numbers" in {
     numberToken("+12345") should equal ((6, Token.IntegerNumber("+12345")))
     numberToken("-12345") should equal ((6, Token.IntegerNumber("-12345")))
     numberToken("12345") should equal ((5, Token.IntegerNumber("12345")))
     numberToken("-.4") should equal ((3, Token.RationalNumber("-.4")))
     numberToken("-0.4") should equal ((4, Token.RationalNumber("-0.4")))
     numberToken("+.4") should equal ((3, Token.RationalNumber("+.4")))
     numberToken("-101.") should equal ((5, Token.RationalNumber("-101.")))
     numberToken(".4") should equal ((2, Token.RationalNumber(".4")))
     numberToken("1e4") should equal ((3, Token.RationalNumber("1e4")))
     numberToken("1.0e4") should equal ((5, Token.RationalNumber("1.0e4")))
     numberToken("1E-4") should equal ((4, Token.RationalNumber("1E-4")))
     numberToken("-1E-4") should equal ((5, Token.RationalNumber("-1E-4")))
     numberToken("-1.E-4") should equal ((6, Token.RationalNumber("-1.E-4")))
     numberToken("-.1E-4") should equal ((6, Token.RationalNumber("-.1E-4")))
     numberToken(".1E-4") should equal ((5, Token.RationalNumber(".1E-4")))
  }

  "string" should "tokenize a string" in {
    Tokenizer("\"hello\"").string(0) should equal (Right((7, Token.Str("hello"))))
    Tokenizer(""""h\"el\nlo"""").string(0) should equal (Right((11, Token.Str("h\"el\nlo"))))
  }

  it should "fail when the string doesn't start with a quotation mark" in {
      Tokenizer("hello").string(0) should equal (Left(Error("does not start with \"", 0,0)))
  }

  it should "fail when the closing quotation mark is missing" in {
      Tokenizer("\"hello").string(0) should equal (Left(Error("unexpected end of string", 0,6)))
  }

  it should "handle all escaped control characters" in {
    Tokenizer("\" \\n \\r \\f \\b \\t \\\\ \\\" \"").string(0) should equal (Right((24, Token.Str(" \n \r \f \b \t \\ \" "))))
  }

  it should "fail if any control character is found" in {
    Tokenizer("\"\n\"").string(0) should equal (Left(Error("control char in string",0,1)))
    Tokenizer("\"\r\"").string(0) should equal (Left(Error("control char in string",0,1)))
    Tokenizer("\"\f\"").string(0) should equal (Left(Error("control char in string",0,1)))
    Tokenizer("\"\b\"").string(0) should equal (Left(Error("control char in string",0,1)))
    Tokenizer("\"\t\"").string(0) should equal (Left(Error("control char in string",0,1)))
  }

  it should "handle unicode chars" in {
    Tokenizer("\"😀\\u00fc\"").string(0) should equal (Right((10,Token.Str("😀ü"))))
    Tokenizer("\"a😀\\u00fcb\"").string(0) should equal (Right((12,Token.Str("a😀üb"))))
    Tokenizer("\"😀\\u00FC\"").string(0) should equal (Right((10,Token.Str("😀ü"))))
    Tokenizer("\"a😀\\u00FCb\"").string(0) should equal (Right((12,Token.Str("a😀üb"))))
    Tokenizer("\"😀\\u00fC\"").string(0) should equal (Right((10,Token.Str("😀ü"))))
    Tokenizer("\"a😀\\u00Fcb\"").string(0) should equal (Right((12,Token.Str("a😀üb"))))
  }

  it should "fail when the unicode is invalid" in {
    Tokenizer("\"\\u00zc\"").string(0) should equal (Left(Error("invalid unicode \\u00zc", 0, 1)))
    Tokenizer("\"a\\u0xfcb\"").string(0) should equal (Left(Error("invalid unicode \\u0xfc", 0, 2)))
  }

  it should "fail when the unicode is incomplete" in {
    Tokenizer("\"\\u00a\"").string(0) should equal (Left(Error("invalid unicode \\u00a\"", 0, 1)))
    Tokenizer("\"\\u00a").string(0) should equal (Left(Error("unexpected end of string in \\uxxxx", 0, 1)))
  }

  it should "fail when the control char ends unexpectedly" in {
    Tokenizer("\"\\\"").string(0) should equal (Left(Error("unexpected end of string", 0, 3)))
    Tokenizer("\"\\").string(0) should equal (Left(Error("unexpected end of string, incomplete control char \\", 0, 1)))
  }

  "tokenize" should "tokenize a valid json" in {
      import Token._
      val tokenizer = Tokenizer(
          """
          |{
          |    "test":  "123",
          |     "test2"  : [1,4],
          |            "bool":true,
          |      "bool2": false,
          |     "numbers": {
          |   "negative": [ -0.1, -1., -.4 ],
          |    "positive": [+4., 5, +.2]
          | } 
          |}
          """.stripMargin('|')
      )
      val tokens = tokenizer.tokenize(0, Right(Nil),0,0)
      val expected = Right(List(RightBrace(Some(Position(10,0))), RightBrace(Some(Position(9,1))), RightBracket(Some(Position(8,28))), RationalNumber("+.2",Some(Position(8,25))), Comma(Some(Position(8,23))), IntegerNumber("5",Some(Position(8,22))), Comma(Some(Position(8,20))), RationalNumber("+4.",Some(Position(8,17))), LeftBracket(Some(Position(8,16))), Colon(Some(Position(8,14))), Str("positive",Some(Position(8,4))), Comma(Some(Position(7,33))), RightBracket(Some(Position(7,32))), RationalNumber("-.4",Some(Position(7,28))), Comma(Some(Position(7,26))), RationalNumber("-1.",Some(Position(7,23))), Comma(Some(Position(7,21))), RationalNumber("-0.1",Some(Position(7,17))), LeftBracket(Some(Position(7,15))), Colon(Some(Position(7,13))), Str("negative",Some(Position(7,3))), LeftBrace(Some(Position(6,16))), Colon(Some(Position(6,14))), Str("numbers",Some(Position(6,5))), Comma(Some(Position(5,20))), True(Some(Position(5,15))), Colon(Some(Position(5,13))), Str("bool2",Some(Position(5,6))), Comma(Some(Position(4,23))), True(Some(Position(4,19))), Colon(Some(Position(4,18))), Str("bool",Some(Position(4,12))), Comma(Some(Position(3,21))), RightBracket(Some(Position(3,20))), IntegerNumber("4",Some(Position(3,19))), Comma(Some(Position(3,18))), IntegerNumber("1",Some(Position(3,17))), LeftBracket(Some(Position(3,16))), Colon(Some(Position(3,14))), Str("test2",Some(Position(3,5))), Comma(Some(Position(2,18))), Str("123",Some(Position(2,13))), Colon(Some(Position(2,10))), Str("test",Some(Position(2,4))), LeftBrace(Some(Position(1,0)))))
      tokens should equal (expected)
    }

    it should "fail when it finds an invalid number" in {
        val tokens = Tokenizer("""{ "number": . }""").tokenize(0, Right(Nil), 0,0)
        tokens should equal (Left(Error("could not parse number", 0, 12)))
    }

    it should "fail when an unexpected character is found" in {
        Tokenizer("""{ "number": u }""").tokenize(0, Right(Nil), 0,0) should equal (Left(Error("unexpected char `u`", 0, 12)))
        Tokenizer("""{ "number": tru }""").tokenize(0, Right(Nil), 0,0) should equal (Left(Error("unexpected char `t`", 0, 12)))
    }

    it should "fail when there is an error in a string and give the correct location" in {
        Tokenizer("""{ "number: u }""").tokenize(0, Right(Nil), 0,0) should equal (Left(Error("unexpected end of string", 0, 14)))
    }
}
