package json

/** Convert json to string */
trait JsonFormatter:
  def format(json: JsonValue): String

extension (json: JsonValue)
  /** json without spaces or newlines */
  def minimal = JsonFormatter.Minimal.format(json)
  /** json with 4 space indentation */ 
  def fourSpaces = JsonFormatter.Indent.fourSpaces.format(json)
  /** json using a custom formatter */
  def format(using formatter: JsonFormatter) = formatter.format(json)


object JsonFormatter:

  /** 
   * Convert a java string into as json string.
   * 
   * This function adds quotation marks at beginning and end of the string
   * and escapes special characters like quotation marks, newlines etc.
   */
  private def toEscapedJsonString(s: String): String = 
    if (s.isEmpty) ""
    else {
    val sb = new StringBuilder(s.size+2)
    sb.append('"')
    for(i <- 0 until s.size) {
      s.charAt(i) match
        case '\\' => sb.append("\\\\")
        case '"' => sb.append("\\\"")
        case '\b' => sb.append("\\b")
        case '\t' => sb.append("\\t")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case c => 
          if (c < ' ') 
            // print control characters as unicode hex, e.g. \u0002
            sb.append(f"\\u${c.toInt}%04x") 
          else sb.append(c)
    }
    sb.append('"')
    sb.toString
  }

  trait Minimal extends JsonFormatter:
    import JsonValue._
    def format(json: JsonValue): String = 
      json match
        case JsonString(s) => toEscapedJsonString(s)
        case JsonInt(i) => i.toString
        case JsonBoolean(b) => if (b) "true" else "false"
        case JsonArray(l) => l.map(format).mkString("[",",","]")
        case JsonObject(fields) => fields.map{ (name, value) => s"""${toEscapedJsonString(name)}:${format(value)}"""}.mkString("{",",","}")

  object Minimal extends Minimal:
    given minimalFormatter: JsonFormatter = this

  case class Indent(indent: String) extends JsonFormatter:
    import JsonValue._
    def format(json: JsonValue): String = 
      val sb = new StringBuilder()
      formatWithLevel(json, 0, sb)
      sb.toString

    private def addIndentation(sb: StringBuilder, level: Int): Unit = 
      for(i <- 0 until level) sb.append(indent)

    private def formatWithLevel(json: JsonValue, level: Int, sb: StringBuilder): StringBuilder =
      def formatListlike[T](open: String, formatElem: T => Unit, close: String)(l: Seq[T]) =
        sb.append(open)
        sb.append("\n")
        for (i <- 0 until l.size) {
          val element = l(i)
          addIndentation(sb, level+1)
          formatElem(element)
          if (i < l.size-1) sb.append(',') 
              sb.append('\n')
        }
        addIndentation(sb, level)
        sb.append(close)
      json match
        case JsonString(s) => sb.append(toEscapedJsonString(s))
        case JsonInt(i) => sb.append(i.toString)
        case JsonBoolean(b) => if (b) sb.append("true") else sb.append("false")
        case JsonArray(elements) =>
          formatListlike("[", (element: JsonValue) => {
            formatWithLevel(element, level+1, sb)
          }, "]")(elements) 
        case JsonObject(fields) =>
          formatListlike("{", (element: (String, JsonValue)) => {
            val (name, value) = element
            sb.append(toEscapedJsonString(name))
            sb.append(": ")
            formatWithLevel(value, level+1, sb)
          }, "}")(fields)
    end formatWithLevel
  end Indent

  object Indent:
    given fourSpaces: JsonFormatter = Indent("    ")
    given twoSpaces: JsonFormatter = Indent("  ")
