package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.lang.StringBuilder


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

  "xxx" should "xxx" in {
      val tokenizer = Tokenizer(
          """
          |{
          |    "test":  "123",
          |     "test2"  : [1,4],
          |         "bool":true
          |}
          """.stripMargin('|')
      )
      val tokens = tokenizer.tokenize(0, Right(Nil),0,0)
      println(tokens)
  }
}
