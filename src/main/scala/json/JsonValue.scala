package json

/** Representation of json */
enum JsonValue:
  case JsonString(s: String)
  case JsonInt(i: BigInt)
  case JsonBoolean(b: Boolean)
  case JsonObject(fields: List[(String, JsonValue)])
  case JsonArray(values: List[JsonValue])
  case JsonNull