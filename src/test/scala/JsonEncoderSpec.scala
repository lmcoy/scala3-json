import org.scalatest.flatspec.AnyFlatSpec
import JsonValue._

class JsonEncoderSpec extends AnyFlatSpec {

  "A String" should "be converted into a JsonString" in {
    import JsonEncoder.given
    assert("hello".toJson == JsonString("hello"))
  }

  "An Int" should "be converted into a JsonInt" in {
    import JsonEncoder.given
    assert(4.toJson == JsonInt(4))
  }


}
