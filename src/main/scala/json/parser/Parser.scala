package parser
import json.JsonValue
import json.JsonValue._
import parser.Parser._
import scala.annotation.tailrec


object Parser {

  /**
   * Parse the json in string `s` into `JsonValue`.
   */
  final def parse(s: String): Either[Error, JsonValue] = {
    val tokenizer = Tokenizer(s)
    val result = for {
     tokens <- tokenizer.tokenize(0, Right(Nil),0,0).left.map(e => Error(e.reason, e.line, e.col))
     maybeResult <- parseElement(tokens.reverse)
    } yield maybeResult
    result.flatMap{
        r => r.unparsed match 
                case Nil => Right(r.jsonValue)
                case t :: ts => Left(Error(s"expected EOF but found $t", t.line.getOrElse(-1), t.col.getOrElse(-1)))
    }
  }

  private def parseElement(tokens: List[Token]): Either[Error, Result] = {
    tokens match 
    case Nil => Left(Error("unexpected end", -1, -1))
    case Token.Str(value,_) :: ts => Right(Result(JsonString(value), ts))
    case Token.LeftBrace(_) :: ts => parseObject(ts, Nil)
    case Token.LeftBracket(_) :: ts => parseList(ts, Nil)
    case Token.IntegerNumber(value,_) :: ts => Right(Result(JsonInt(value.toInt), ts)) // todo overflow?
    case t :: ts => Left(Error(s"unexpected token: $t", t.line.getOrElse(-1), t.col.getOrElse(-1)))
  }

  @tailrec
  private def parseObject(tokens:List[Token],acc: List[(String, JsonValue)]): Either[Error, Result] = {
      tokens match 
      case Nil => Left(Error("unexpected EOF in object", -1, -1))
      case Token.Str(field, p) :: ts =>
        lazy val line = p.map(_.line).getOrElse(-1)
        lazy val col = p.map(_.col + field.length).getOrElse(-1)
        ts match 
        case Nil => Left(Error(s"expected `:`", line, col)) 
        case Token.Colon(p) :: ts2 =>
            parseElement(ts2) match
            case Left(e) => Left(e)
            case Right(r) => 
                lazy val line = p.map(_.line).getOrElse(-1)
                lazy val col = p.map(_.col).getOrElse(-1)
                r.unparsed match 
                case Nil => Left(Error("unexpected EOF, expected `:`", line, col))
                case Token.Comma(_) :: ts => parseObject(ts, (field, r.jsonValue) :: acc)
                case Token.RightBrace(_) :: ts => Right(Result(JsonObject(((field, r.jsonValue) :: acc).reverse), ts))
                case token :: ts => Left(Error("expected , or }", token.line.getOrElse(-1), token.col.getOrElse(-1))) 
        case t :: ts2 =>
            Left(Error(s"expected `:` but got $t", t.line.getOrElse(-1), t.col.getOrElse(-1)))
      case Token.RightBrace(_) :: ts if acc.length == 0 =>
        Right(Result(JsonObject(Nil), ts))
      case t :: ts =>
        Left(Error(s"expected string as field name but got $t", t.line.getOrElse(-1), t.col.getOrElse(-1)))
  }

  @tailrec
  private def parseList(tokens: List[Token], acc: List[JsonValue]): Either[Error, Result] = {
      tokens match 
      case Nil => Left(Error("unexpect EOF in array", -1, -1))
      case t :: _ => 
        val result = parseElement(tokens)
        result match 
        case Left(e) => Left(e)
        case Right(r) => 
            lazy val line = t.line.getOrElse(-1)
            lazy val col = t.col.getOrElse(-1)
            r.unparsed match
            case Nil => Left(Error("expected , or ]", line, col))
            case Token.Comma(_) :: ts => parseList(ts, r.jsonValue :: acc)
            case Token.RightBracket(_) :: ts => Right(Result(JsonArray((r.jsonValue :: acc).reverse), ts))
            case token :: _ => Left(Error("expected , or ]", token.line.getOrElse(-1), token.col.getOrElse(-1)))
  }

  case class Error(reason: String, line: Int, col: Int) extends Exception(s"error at line $line col $col: $reason")

  private case class Result(jsonValue: JsonValue, unparsed: List[Token])
}