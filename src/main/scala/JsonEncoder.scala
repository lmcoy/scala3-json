package json

import scala.deriving.*
import scala.CanEqual.derived
import scala.compiletime.{erasedValue, summonInline}

trait JsonEncoder[A]:
  def toJsonI(a: A): JsonValue

  /** convert to [[JsonValue]] */
  extension (x: A) def toJson: JsonValue = toJsonI(x)

object JsonEncoder:
  import JsonValue._
  
  given JsonEncoder[String] with 
    def toJsonI(s: String): JsonValue = JsonString(s)

  given JsonEncoder[Int] with
    def toJsonI(i: Int): JsonValue = JsonInt(i)

  given JsonEncoder[Boolean] with
    def toJsonI(b: Boolean): JsonValue = JsonBoolean(b)

  given listEncoder[A](using a: JsonEncoder[A]): JsonEncoder[List[A]] with
    def toJsonI(l: List[A]): JsonValue = JsonArray(l.map(a => a.toJson))

  inline def summonAll[T <: Tuple]: List[JsonEncoder[_]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[JsonEncoder[t]] :: summonAll[ts]

  def jsonEncoderProduct[T](p: Mirror.ProductOf[T], elems: => List[JsonEncoder[_]]): JsonEncoder[T] = 
    new JsonEncoder[T]:
      def toJsonI(t: T): JsonValue = 
        val product = t.asInstanceOf[Product]
        JsonObject {
        product.productIterator.zip(product.productElementNames).zip(elems.iterator).map {
          case ((value, name), elemJsonEncoder) => 
            val jsonValue = elemJsonEncoder.asInstanceOf[JsonEncoder[Any]].toJsonI(value)
            (name, jsonValue)
        }.toList
        }
      
  inline given derived[T](using m: Mirror.Of[T]): JsonEncoder[T] =
    lazy val jsonEncoders = summonAll[m.MirroredElemTypes]
    inline m match
      case p: Mirror.ProductOf[T] => jsonEncoderProduct(p, jsonEncoders)
      
end JsonEncoder