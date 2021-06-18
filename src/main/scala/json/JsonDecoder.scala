package json

import scala.deriving.*
import scala.CanEqual.derived
import scala.compiletime.{erasedValue, summonInline, constValue}

/**
 * Typeclass to decode a [[JsonValue]] to scala object
 */
trait JsonDecoder[A]:
    def decode(json: JsonValue): Either[JsonDecoder.Error, A]


/**
 * Convert the [[JsonValue]] to a scala object of type `A`. 
 */
extension (json: JsonValue) def as[A](using decoder: JsonDecoder[A]): Either[JsonDecoder.Error,A] = {
    decoder.decode(json)
}

object JsonDecoder:
    import JsonValue._

    case class Error(msg: String) extends RuntimeException(msg)

    given JsonDecoder[BigInt] with
        def decode(json: JsonValue): Either[Error, BigInt] = {
            json match
                case JsonInt(i) => Right(i)
                case _ => Left(Error(s"cannot convert ${json.getClass.getCanonicalName} to BigInt"))
        }

    
    given JsonDecoder[Int] with
        def decode(json: JsonValue): Either[Error, Int] = {
            json match
                case JsonInt(i) => if i.isValidInt then Right(i.toInt) else Left(Error(s"value doesn't fit into Int: $i"))
                case _ => Left(Error(s"cannot convert ${json.getClass.getCanonicalName} to Int"))
        }

    given JsonDecoder[String] with
        def decode(json:JsonValue) : Either[Error, String] = {
            json match
                case JsonString(s) => Right(s)
                case _ => Left(Error(s"cannot convert ${json.getClass.getCanonicalName} to String"))
        }

    given JsonDecoder[Boolean] with
        def decode(json: JsonValue): Either[Error, Boolean] = {
            json match
                case JsonBoolean(b) => Right(b)
                case _ => Left(Error(s"cannot convert ${json.getClass.getCanonicalName} to Boolean"))
        }

    given listDecoder[A](using elementDecoder: JsonDecoder[A]): JsonDecoder[List[A]] with
        def decode(json: JsonValue): Either[Error, List[A]] = {
            json match 
                case JsonArray(elements) =>
                    elements.foldRight[Either[Error, List[A]]](Right(List.empty[A])){case (a, acc) => 
                        acc.flatMap{l => 
                            elementDecoder.decode(a) match
                                case Left(ex) => Left(ex)
                                case Right(decoded) => Right(decoded :: l)
                        }
                    }
                case _ => Left(Error("expected List"))
        }
    
    /**
     * get the names of the elements of a Product
     * 
     * @return returns a list of all the names of a case class
     */ 
    private inline def getElemLabels[A <: Tuple]: List[String] = inline erasedValue[A] match {
        case _: EmptyTuple => Nil
        case _: (t *: ts) => constValue[t].toString :: getElemLabels[ts]
    }

    /**
     * get a type class instance for all elements of a product
     * 
     * @return returns a list of JsonEncoders for all fields of a case class
     */
    private inline def summonAll[T <: Tuple]: List[JsonDecoder[_]] =
        inline erasedValue[T] match
        case _: EmptyTuple => Nil
        case _: (t *: ts) => summonInline[JsonDecoder[t]] :: summonAll[ts]

    /**
     * Derive a case class for Product, e.g. case class.
     */
    inline given derived[A](using m: Mirror.Of[A]): JsonDecoder[A] = {
        lazy val decoders = summonAll[m.MirroredElemTypes]
        val elemLabels = getElemLabels[m.MirroredElemLabels]

        inline m match {
        case p: Mirror.ProductOf[A] =>
            new JsonDecoder[A] {
               def decode(json: JsonValue): Either[Error, A] = {
                   json match {
                       case JsonObject(fields) =>
                       val maybeElements = elemLabels.zip(decoders).map{ case (label,decoder) =>
                                fields
                                    .toMap
                                    .get(label)
                                    .toRight(Error(s"object has no field $label"))
                                    .flatMap(value => decoder.decode(value))
                       }.foldRight[Either[Error, List[Any]]](Right(List.empty[Any])){case (a, acc) => 
                            acc.flatMap{l => 
                                a match
                                    case Left(ex) => Left(ex)
                                    case Right(decoded) => Right(decoded :: l)
                            }
                        }
                        maybeElements.map{ elements =>
                            val product: Product = new Product {
                                override def productArity: Int = fields.size
                                override def productElement(n: Int): Any = elements(n)
                                override def canEqual(that: Any): Boolean = false
                            }
                            p.fromProduct(product)
                        }
                       case _ => Left(Error("expected object"))
                   }
               }
            }
        }
    }
