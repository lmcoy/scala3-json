package parser

import java.lang.StringBuilder
import scala.collection.mutable
import Tokenizer.Error
import scala.annotation.tailrec

class Tokenizer(str: String) {
    @tailrec
    final def tokenize(offset: Int, acc: Either[Error, List[Token]], line: Int, col: Int): Either[Error, List[Token]] = {
        if (offset >= str.length) acc
        else {
            val codePoint = str.codePointAt(offset)
            val nextOffset = offset + Character.charCount(codePoint)

            acc match
                case Left(error) => acc
                case Right(tokens) =>
                    inline def f(x: Option[Position] => Token) = Right(x(Some(Position(line, col))):: tokens)
                    inline def g(x: Option[Position] => Token) = tokenize(nextOffset, f(x), line, col+1) 
                    codePoint match
                        case '{' => g(Token.LeftBrace.apply)
                        case '}' => g(Token.RightBrace.apply)
                        case ':' => g(Token.Colon.apply)
                        case '[' => g(Token.LeftBracket.apply)
                        case ']' => g(Token.RightBracket.apply)
                        case ',' => g(Token.Comma.apply)
                        case '\n' => tokenize(nextOffset, acc, line + 1, 0)
                        case '\r' => tokenize(nextOffset, acc, line, col+1)
                        case c if Character.isSpaceChar(c) => tokenize(nextOffset, acc, line, col+1)
                        case c if c == '-' || c == '+' || c == '.' || Character.isDigit(c) =>
                            val maybeNumber = number(offset)
                            maybeNumber match
                                case Some(number) => tokenize(number._1, Right(number._2 :: tokens), line, col+(number._1-offset))
                                case None => Left(Error("could not parse number", line, col))
                        case '"' =>
                            val s = string(offset)
                            s match
                                case Left(error) => 
                                    Left(error.copy(line=line, col=col+error.col))
                                case Right(o) => tokenize(o._1, Right(o._2.copy(pos = Some(Position(line,col))) :: tokens), line, col+(o._1-offset))
                        case 't' if offset + 4 < str.length && str.substring(offset, offset+4) == "true" => 
                            tokenize(offset + 4, Right(Token.True(Some(Position(line, col))) :: tokens),line, col+4)
                        case 'f' if offset + 5 < str.length && str.substring(offset, offset+5) == "false" => 
                            tokenize(offset + 5, Right(Token.True(Some(Position(line, col))) :: tokens),line, col+5)
                        case c =>
                            Left(Error(s"unexpected char `${new StringBuffer().appendCodePoint(c).toString}`", line, col))
        }
    }

    @tailrec
    final def unsignedInteger(offset: Int, acc: StringBuilder): (Int, StringBuilder) = {
        if (offset >= str.length) (offset, acc)
        else {
            val codePoint = str.codePointAt(offset)
            val nextOffset = offset + Character.charCount(codePoint) 
            if (Character.isDigit(codePoint)) {
                acc.appendCodePoint(codePoint)
                unsignedInteger(nextOffset, acc)
            } else {
                (offset, acc)
            }
        }
    }

    def signedInteger(offset: Int, acc: StringBuilder): (Int, StringBuilder)  = {
       val codePointOffset= str.codePointAt(offset) 
       if (codePointOffset == '-' || codePointOffset == '+') {
           acc.appendCodePoint(codePointOffset)
           unsignedInteger(offset+1, acc)
       } else unsignedInteger(offset, acc)
    }

    def number(offset: Int): Option[(Int, Token.IntegerNumber | Token.RationalNumber)] = {
        val acc = new StringBuilder()
        val codePointOffset= str.codePointAt(offset) 
        val offUnsigend = if (codePointOffset == '-' || codePointOffset == '+') {
            acc.appendCodePoint(codePointOffset)
            offset+1
        } else offset

        def f(offset: Int, startAtOffset: Boolean, emptyAllowed: Boolean, predicate: Int => Boolean, read: (Int, StringBuilder) => (Int, StringBuilder)): (Int, Boolean) = {
            if (offset < str.length && predicate(str.codePointAt(offset))) {
                val off = if (!startAtOffset) {
                    acc.appendCodePoint(str.codePointAt(offset))
                    offset + 1
                } else offset
                val (o,s) = read(off, new StringBuilder())
                if (!emptyAllowed && s.length == 0)
                    (offset, false)
                else {
                    acc.append(s)
                    (o, true)
                }
            } else (offset, false)
        }

        val (offFraction, number) = f(offUnsigend, true, true, _ != '.', unsignedInteger)

        val (offExp, fraction) = f(offFraction, false, true, _ == '.', unsignedInteger)
        val (offFinal, exponent) = f(offExp, false, false, Character.toLowerCase(_) == 'e', signedInteger)

        if(!number && !fraction) None
        else if (!fraction && !exponent) Some((offFinal,Token.IntegerNumber(acc.toString)))
        else Some((offFinal, Token.RationalNumber(acc.toString)))
    }

    def string(offset: Int): Either[Error, (Int, Token.Str)] = {
        val acc = StringBuffer()

        @tailrec
        def go(o: Int): Either[Error, Int] = {
            def error(msg: String) = Left(Error(msg, 0, o - offset))
            if (o >= str.length) error("unexpected end of string")
            else {
                val codePoint = str.codePointAt(o)
                val nextOffset = o + Character.charCount(codePoint)
                codePoint match
                    case '\\' => 
                        if (nextOffset < str.length) {
                            val c = str.codePointAt(nextOffset)
                            c match 
                            case 'n' =>
                                acc.appendCodePoint('\n')
                                go(nextOffset+Character.charCount(c))
                            case '"' =>
                                acc.appendCodePoint('"')
                                go(nextOffset+Character.charCount(c))
                            case 't' =>
                                acc.appendCodePoint('\t')
                                go(nextOffset+Character.charCount(c))
                            case 'r' =>
                                acc.appendCodePoint('\r')
                                go(nextOffset+Character.charCount(c))
                            case 'b' =>
                                acc.appendCodePoint('\b')
                                go(nextOffset+Character.charCount(c))
                            case 'f' =>
                                acc.appendCodePoint('\f')
                                go(nextOffset+Character.charCount(c))
                            case '\\' =>
                                acc.appendCodePoint('\\')
                                go(nextOffset+Character.charCount(c))
                            case 'u' =>
                                val startIndex = nextOffset+Character.charCount(c)
                                val endIndex = startIndex + 4
                                if (endIndex <= str.length) then
                                    val hex = str.substring(startIndex, endIndex)
                                    if "[0-9a-fA-F]{4}".r.matches(hex) then
                                        acc.appendCodePoint(Integer.parseInt(hex, 16))
                                        go(endIndex)
                                    else error(s"invalid unicode \\u$hex")
                                else error("unexpected end of string in \\uxxxx")
                        } else error("unexpected end of string, incomplete control char \\")
                    case '"' => Right(o + 1)
                    case c if c < ' ' => error("control char in string")
                    case _ => 
                        acc.appendCodePoint(codePoint)
                        go(nextOffset)
            }
        }

        if (offset >= str.length) Left(Error("does not start with \"", 0, 0))
        else go(offset+1).map(o => (o,Token.Str(acc.toString))) 
    }

}

object Tokenizer {

    case class Error(reason: String, line: Int, col: Int) extends Exception(s"error at line $line col $col: $reason")
    
}
