package parser

case class Position(line: Int, col: Int)

enum Token(pos: Option[Position]):
  def line: Option[Int] = pos.map(_.line)
  def col: Option[Int] = pos.map(_.col)
  case IntegerNumber(value: String, pos: Option[Position] = None)
      extends Token(pos)
  case RationalNumber(value: String, pos: Option[Position] = None)
      extends Token(pos)
  case True(pos: Option[Position] = None) extends Token(pos)
  case False(pos: Option[Position] = None) extends Token(pos)
  case Comma(pos: Option[Position] = None) extends Token(pos)
  case Colon(pos: Option[Position] = None) extends Token(pos)
  case LeftBrace(pos: Option[Position] = None) extends Token(pos)
  case RightBrace(pos: Option[Position] = None) extends Token(pos)
  case LeftBracket(pos: Option[Position] = None) extends Token(pos)
  case RightBracket(pos: Option[Position] = None) extends Token(pos)
  case Str(value: String, pos: Option[Position] = None) extends Token(pos)
