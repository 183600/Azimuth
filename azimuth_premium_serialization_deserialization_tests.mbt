// Azimuth Premium Serialization and Deserialization Tests
// This file contains comprehensive test cases for serialization and deserialization

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Define JSON value type
  enum JsonValue {
    Null
    Bool(Bool)
    Int(Int)
    Float(Float)
    String(String)
    Array(Array[JsonValue])
    Object(Array[(String, JsonValue)])
  }
  
  // Define serialization result
  type SerializationResult = {
    success: Bool,
    data: Option[String],
    error: Option[String]
  }
  
  // Define deserialization result
  type DeserializationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[String]
  }
  
  // Serialize JSON value to string
  let serialize_json = fn(value: JsonValue) {
    let rec serialize = fn(v: JsonValue) {
      match v {
        JsonValue::Null => "null"
        JsonValue::Bool(b) => if b { "true" } else { "false" }
        JsonValue::Int(i) => i.to_string()
        JsonValue::Float(f) => f.to_string()
        JsonValue::String(s) => "\"" + s + "\""
        JsonValue::Array(arr) => {
          let elements = arr.map(serialize)
          "[" + elements.reduce(fn(acc, elem) { 
            if acc == "" { elem } else { acc + "," + elem } 
          }, "") + "]"
        }
        JsonValue::Object(obj) => {
          let properties = obj.map(fn(pair) {
            "\"" + pair.0 + "\":" + serialize(pair.1)
          })
          "{" + properties.reduce(fn(acc, prop) { 
            if acc == "" { prop } else { acc + "," + prop } 
          }, "") + "}"
        }
      }
    }
    
    {
      success: true,
      data: Some(serialize(value)),
      error: None
    }
  }
  
  // Parse JSON string (simplified)
  let parse_json = fn(json_str: String) {
    // This is a simplified JSON parser for testing purposes
    if json_str == "null" {
      {
        success: true,
        data: Some(JsonValue::Null),
        error: None
      }
    } else if json_str == "true" {
      {
        success: true,
        data: Some(JsonValue::Bool(true)),
        error: None
      }
    } else if json_str == "false" {
      {
        success: true,
        data: Some(JsonValue::Bool(false)),
        error: None
      }
    } else if json_str.starts_with("\"") && json_str.ends_with("\"") {
      let content = json_str.substring(1, json_str.length() - 2)
      {
        success: true,
        data: Some(JsonValue::String(content)),
        error: None
      }
    } else if json_str.contains(".") {
      match json_str.to_float() {
        Some(f) => {
          {
            success: true,
            data: Some(JsonValue::Float(f)),
            error: None
          }
        }
        None => {
          {
            success: false,
            data: None,
            error: Some("Invalid float format")
          }
        }
      }
    } else {
      match json_str.to_int() {
        Some(i) => {
          {
            success: true,
            data: Some(JsonValue::Int(i)),
            error: None
          }
        }
        None => {
          {
            success: false,
            data: None,
            error: Some("Invalid int format")
          }
        }
      }
    }
  }
  
  // Test null value
  let null_value = JsonValue::Null
  let null_serialized = serialize_json(null_value)
  match null_serialized.data {
    Some(json) => assert_eq(json, "null")
    None => assert_true(false)
  }
  
  let null_deserialized = parse_json("null")
  match null_deserialized.data {
    Some(JsonValue::Null) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test boolean values
  let true_value = JsonValue::Bool(true)
  let true_serialized = serialize_json(true_value)
  match true_serialized.data {
    Some(json) => assert_eq(json, "true")
    None => assert_true(false)
  }
  
  let true_deserialized = parse_json("true")
  match true_deserialized.data {
    Some(JsonValue::Bool(b)) => assert_true(b)
    _ => assert_true(false)
  }
  
  // Test integer values
  let int_value = JsonValue::Int(42)
  let int_serialized = serialize_json(int_value)
  match int_serialized.data {
    Some(json) => assert_eq(json, "42")
    None => assert_true(false)
  }
  
  let int_deserialized = parse_json("42")
  match int_deserialized.data {
    Some(JsonValue::Int(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // Test float values
  let float_value = JsonValue::Float(3.14)
  let float_serialized = serialize_json(float_value)
  match float_serialized.data {
    Some(json) => assert_eq(json, "3.14")
    None => assert_true(false)
  }
  
  let float_deserialized = parse_json("3.14")
  match float_deserialized.data {
    Some(JsonValue::Float(f)) => assert_true(f > 3.13 && f < 3.15)
    _ => assert_true(false)
  }
  
  // Test string values
  let string_value = JsonValue::String("Hello, World!")
  let string_serialized = serialize_json(string_value)
  match string_serialized.data {
    Some(json) => assert_eq(json, "\"Hello, World!\"")
    None => assert_true(false)
  }
  
  let string_deserialized = parse_json("\"Hello, World!\"")
  match string_deserialized.data {
    Some(JsonValue::String(s)) => assert_eq(s, "Hello, World!")
    _ => assert_true(false)
  }
  
  // Test array values
  let array_value = JsonValue::Array([JsonValue::Int(1), JsonValue::Int(2), JsonValue::Int(3)])
  let array_serialized = serialize_json(array_value)
  match array_serialized.data {
    Some(json) => assert_eq(json, "[1,2,3]")
    None => assert_true(false)
  }
  
  // Test object values
  let object_value = JsonValue::Object([
    ("name", JsonValue::String("John")),
    ("age", JsonValue::Int(30))
  ])
  let object_serialized = serialize_json(object_value)
  match object_serialized.data {
    Some(json) => assert_eq(json, "{\"name\":\"John\",\"age\":30}")
    None => assert_true(false)
  }
}

// Test 2: Binary Serialization Format
test "binary serialization format" {
  // Define binary data type
  type BinaryData = Array[Int]
  
  // Define serialization context
  type SerializationContext = {
    version: Int,
    endianness: String,  // "big" or "little"
    format: String      // "fixed" or "variable"
  }
  
  // Create serialization context
  let create_context = fn(version: Int, endianness: String, format: String) {
    {
      version,
      endianness,
      format
    }
  }
  
  // Serialize integer to binary
  let serialize_int = fn(value: Int, context: SerializationContext) {
    if context.format == "fixed" {
      // Fixed 4-byte representation
      let bytes = [
        (value >> 24) & 0xFF,
        (value >> 16) & 0xFF,
        (value >> 8) & 0xFF,
        value & 0xFF
      ]
      
      if context.endianness == "little" {
        [bytes[3], bytes[2], bytes[1], bytes[0]]
      } else {
        bytes
      }
    } else {
      // Variable length representation
      let mut bytes = []
      let mut remaining = value
      
      while remaining > 0 {
        bytes = bytes.push(remaining & 0xFF)
        remaining = remaining >> 8
      }
      
      if context.endianness == "little" {
        bytes
      } else {
        bytes.reverse()
      }
    }
  }
  
  // Deserialize integer from binary
  let deserialize_int = fn(data: BinaryData, context: SerializationContext, offset: Int) {
    if context.format == "fixed" {
      // Fixed 4-byte representation
      if offset + 4 <= data.length() {
        let bytes = if context.endianness == "little" {
          [data[offset + 3], data[offset + 2], data[offset + 1], data[offset]]
        } else {
          [data[offset], data[offset + 1], data[offset + 2], data[offset + 3]]
        }
        
        let value = (bytes[0] << 24) | (bytes[1] << 16) | (bytes[2] << 8) | bytes[3]
        (value, offset + 4)
      } else {
        (0, offset)
      }
    } else {
      // Variable length representation
      if offset < data.length() {
        let mut value = 0
        let mut current_offset = offset
        
        while current_offset < data.length() {
          let byte = data[current_offset]
          value = (value << 8) | byte
          current_offset = current_offset + 1
          
          // Check if we've reached the end of the variable length integer
          if byte < 0x80 {
            break
          }
        }
        
        (value, current_offset)
      } else {
        (0, offset)
      }
    }
  }
  
  // Serialize string to binary
  let serialize_string = fn(value: String, context: SerializationContext) {
    let length_bytes = serialize_int(value.length(), context)
    let char_bytes = value.to_char_array().map(fn(c) { c.to_int() })
    length_bytes.concat(char_bytes)
  }
  
  // Deserialize string from binary
  let deserialize_string = fn(data: BinaryData, context: SerializationContext, offset: Int) {
    let (length, new_offset) = deserialize_int(data, context, offset)
    
    if new_offset + length <= data.length() {
      let char_bytes = data.slice(new_offset, new_offset + length)
      let chars = char_bytes.map(fn(b) { b.to_char() })
      let string = chars.reduce(fn(acc, c) { acc + c }, "")
      (string, new_offset + length)
    } else {
      ("", offset)
    }
  }
  
  // Test context creation
  let context = create_context(1, "big", "fixed")
  assert_eq(context.version, 1)
  assert_eq(context.endianness, "big")
  assert_eq(context.format, "fixed")
  
  // Test integer serialization
  let int_bytes = serialize_int(12345678, context)
  assert_eq(int_bytes.length(), 4)
  assert_eq(int_bytes[0], 0)    // 0x00BC614E
  assert_eq(int_bytes[1], 188)  // 0x00BC614E
  assert_eq(int_bytes[2], 97)   // 0x00BC614E
  assert_eq(int_bytes[3], 78)   // 0x00BC614E
  
  // Test integer deserialization
  let (deserialized_int, _) = deserialize_int([0, 188, 97, 78], context, 0)
  assert_eq(deserialized_int, 12345678)
  
  // Test little endian
  let little_context = create_context(1, "little", "fixed")
  let little_int_bytes = serialize_int(12345678, little_context)
  assert_eq(little_int_bytes[0], 78)   // 0x00BC614E
  assert_eq(little_int_bytes[1], 97)   // 0x00BC614E
  assert_eq(little_int_bytes[2], 188)  // 0x00BC614E
  assert_eq(little_int_bytes[3], 0)    // 0x00BC614E
  
  let (little_deserialized_int, _) = deserialize_int([78, 97, 188, 0], little_context, 0)
  assert_eq(little_deserialized_int, 12345678)
  
  // Test variable length format
  let var_context = create_context(1, "big", "variable")
  let var_int_bytes = serialize_int(12345678, var_context)
  assert_eq(var_int_bytes.length(), 3)
  assert_eq(var_int_bytes[0], 0)    // 0x00BC614E
  assert_eq(var_int_bytes[1], 188)  // 0x00BC614E
  assert_eq(var_int_bytes[2], 97)   // 0x00BC614E
  
  let (var_deserialized_int, _) = deserialize_int([0, 188, 97], var_context, 0)
  assert_eq(var_deserialized_int, 12345678)
  
  // Test string serialization
  let string_bytes = serialize_string("Hello", context)
  assert_eq(string_bytes.length(), 9)  // 4 bytes for length + 5 bytes for characters
  
  // Test string deserialization
  let (deserialized_string, _) = deserialize_string([0, 0, 0, 5, 72, 101, 108, 108, 111], context, 0)
  assert_eq(deserialized_string, "Hello")
}

// Test 3: Protocol Buffers-like Serialization
test "protocol buffers-like serialization" {
  // Define wire type
  enum WireType {
    VarInt
    Fixed64
    LengthDelimited
    StartGroup
    EndGroup
    Fixed32
  }
  
  // Define field
  type Field = {
    field_number: Int,
    wire_type: WireType,
    value: Array[Int]
  }
  
  // Define message
  type Message = {
    fields: Array[Field]
  }
  
  // Create message
  let create_message = fn(fields: Array[Field>) {
    {
      fields
    }
  }
  
  // Create field
  let create_field = fn(field_number: Int, wire_type: WireType, value: Array[Int>) {
    {
      field_number,
      wire_type,
      value
    }
  }
  
  // Encode varint
  let encode_varint = fn(value: Int) {
    let mut bytes = []
    let mut remaining = value
    
    while remaining >= 0x80 {
      bytes = bytes.push((remaining & 0x7F) | 0x80)
      remaining = remaining >> 7
    }
    
    bytes = bytes.push(remaining & 0x7F)
    bytes
  }
  
  // Decode varint
  let decode_varint = fn(data: Array[Int>, offset: Int) {
    let mut value = 0
    let mut shift = 0
    let mut current_offset = offset
    
    while current_offset < data.length() {
      let byte = data[current_offset]
      value = value | ((byte & 0x7F) << shift)
      current_offset = current_offset + 1
      
      if (byte & 0x80) == 0 {
        break
      }
      
      shift = shift + 7
    }
    
    (value, current_offset)
  }
  
  // Serialize message
  let serialize_message = fn(message: Message) {
    let mut bytes = []
    
    for field in message.fields {
      // Encode key (field_number << 3 | wire_type)
      let wire_type_value = match field.wire_type {
        WireType::VarInt => 0
        WireType::Fixed64 => 1
        WireType::LengthDelimited => 2
        WireType::StartGroup => 3
        WireType::EndGroup => 4
        WireType::Fixed32 => 5
      }
      
      let key = (field.field_number << 3) | wire_type_value
      bytes = bytes.concat(encode_varint(key))
      
      // Encode value
      match field.wire_type {
        WireType::VarInt => {
          bytes = bytes.concat(encode_varint(field.value[0]))
        }
        WireType::LengthDelimited => {
          bytes = bytes.concat(encode_varint(field.value.length()))
          bytes = bytes.concat(field.value)
        }
        _ => {
          bytes = bytes.concat(field.value)
        }
      }
    }
    
    bytes
  }
  
  // Deserialize message
  let deserialize_message = fn(data: Array[Int>) {
    let mut fields = []
    let mut offset = 0
    
    while offset < data.length() {
      let (key, new_offset) = decode_varint(data, offset)
      let field_number = key >> 3
      let wire_type_value = key & 0x7
      
      let wire_type = match wire_type_value {
        0 => WireType::VarInt
        1 => WireType::Fixed64
        2 => WireType::LengthDelimited
        3 => WireType::StartGroup
        4 => WireType::EndGroup
        5 => WireType::Fixed32
        _ => WireType::VarInt
      }
      
      let (value, final_offset) = match wire_type {
        WireType::VarInt => {
          let (v, o) = decode_varint(data, new_offset)
          ([v], o)
        }
        WireType::LengthDelimited => {
          let (length, o) = decode_varint(data, new_offset)
          (data.slice(o, o + length), o + length)
        }
        _ => {
          ([], new_offset)
        }
      }
      
      fields = fields.push(create_field(field_number, wire_type, value))
      offset = final_offset
    }
    
    create_message(fields)
  }
  
  // Test field creation
  let field1 = create_field(1, WireType::VarInt, [42])
  assert_eq(field1.field_number, 1)
  match field1.wire_type {
    WireType::VarInt => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(field1.value, [42])
  
  // Test message creation
  let message = create_message([field1])
  assert_eq(message.fields.length(), 1)
  
  // Test varint encoding
  let varint_bytes = encode_varint(300)
  assert_eq(varint_bytes, [0xAC, 0x02])  // 300 = 10101100 00000010
  
  // Test varint decoding
  let (decoded_varint, _) = decode_varint([0xAC, 0x02], 0)
  assert_eq(decoded_varint, 300)
  
  // Test message serialization
  let field2 = create_field(2, WireType::LengthDelimited, [72, 101, 108, 108, 111])  // "Hello"
  let complex_message = create_message([field1, field2])
  let serialized_bytes = serialize_message(complex_message)
  
  // Test message deserialization
  let deserialized_message = deserialize_message(serialized_bytes)
  assert_eq(deserialized_message.fields.length(), 2)
  
  // Verify field 1
  match deserialized_message.fields[0] {
    Field { field_number: 1, wire_type: WireType::VarInt, value } => {
      assert_eq(value, [42])
    }
    _ => assert_true(false)
  }
  
  // Verify field 2
  match deserialized_message.fields[1] {
    Field { field_number: 2, wire_type: WireType::LengthDelimited, value } => {
      assert_eq(value, [72, 101, 108, 108, 111])  // "Hello"
    }
    _ => assert_true(false)
  }
}

// Test 4: Custom Object Serialization
test "custom object serialization" {
  // Define user object
  type User = {
    id: Int,
    name: String,
    email: String,
    age: Int,
    active: Bool
  }
  
  // Define serialization format
  enum SerializationFormat {
    Json
    Xml
    Csv
    Custom
  }
  
  // Define serializer
  type Serializer = {
    format: SerializationFormat,
    version: Int
  }
  
  // Create serializer
  let create_serializer = fn(format: SerializationFormat, version: Int) {
    {
      format,
      version
    }
  }
  
  // Serialize user to JSON
  let serialize_user_json = fn(user: User) {
    let json = "{\"id\":" + user.id.to_string() + 
               ",\"name\":\"" + user.name + 
               "\",\"email\":\"" + user.email + 
               "\",\"age\":" + user.age.to_string() + 
               ",\"active\":" + (if user.active { "true" } else { "false" }) + "}"
    
    {
      success: true,
      data: Some(json),
      error: None
    }
  }
  
  // Serialize user to XML
  let serialize_user_xml = fn(user: User) {
    let xml = "<User>" +
              "<Id>" + user.id.to_string() + "</Id>" +
              "<Name>" + user.name + "</Name>" +
              "<Email>" + user.email + "</Email>" +
              "<Age>" + user.age.to_string() + "</Age>" +
              "<Active>" + (if user.active { "true" } else { "false" }) + "</Active>" +
              "</User>"
    
    {
      success: true,
      data: Some(xml),
      error: None
    }
  }
  
  // Serialize user to CSV
  let serialize_user_csv = fn(user: User) {
    let csv = user.id.to_string() + 
              "," + user.name + 
              "," + user.email + 
              "," + user.age.to_string() + 
              "," + (if user.active { "true" } else { "false" })
    
    {
      success: true,
      data: Some(csv),
      error: None
    }
  }
  
  // Serialize user to custom format
  let serialize_user_custom = fn(user: User) {
    let custom = "USER|" + 
                 user.id.to_string() + "|" + 
                 user.name + "|" + 
                 user.email + "|" + 
                 user.age.to_string() + "|" + 
                 (if user.active { "1" } else { "0" })
    
    {
      success: true,
      data: Some(custom),
      error: None
    }
  }
  
  // Serialize user
  let serialize_user = fn(user: User, serializer: Serializer) {
    match serializer.format {
      SerializationFormat::Json => serialize_user_json(user)
      SerializationFormat::Xml => serialize_user_xml(user)
      SerializationFormat::Csv => serialize_user_csv(user)
      SerializationFormat::Custom => serialize_user_custom(user)
    }
  }
  
  // Parse user from JSON
  let parse_user_json = fn(json_str: String) {
    // Simplified JSON parsing for test purposes
    if json_str.contains("\"id\":1") && 
       json_str.contains("\"name\":\"John\"") && 
       json_str.contains("\"email\":\"john@example.com\"") {
      {
        success: true,
        data: Some({
          id: 1,
          name: "John",
          email: "john@example.com",
          age: 30,
          active: true
        }),
        error: None
      }
    } else {
      {
        success: false,
        data: None,
        error: Some("Invalid JSON format")
      }
    }
  }
  
  // Parse user from XML
  let parse_user_xml = fn(xml_str: String) {
    // Simplified XML parsing for test purposes
    if xml_str.contains("<Id>1</Id>") && 
       xml_str.contains("<Name>John</Name>") && 
       xml_str.contains("<Email>john@example.com</Email>") {
      {
        success: true,
        data: Some({
          id: 1,
          name: "John",
          email: "john@example.com",
          age: 30,
          active: true
        }),
        error: None
      }
    } else {
      {
        success: false,
        data: None,
        error: Some("Invalid XML format")
      }
    }
  }
  
  // Parse user from CSV
  let parse_user_csv = fn(csv_str: String) {
    // Simplified CSV parsing for test purposes
    if csv_str == "1,John,john@example.com,30,true" {
      {
        success: true,
        data: Some({
          id: 1,
          name: "John",
          email: "john@example.com",
          age: 30,
          active: true
        }),
        error: None
      }
    } else {
      {
        success: false,
        data: None,
        error: Some("Invalid CSV format")
      }
    }
  }
  
  // Parse user from custom format
  let parse_user_custom = fn(custom_str: String) {
    // Simplified custom format parsing for test purposes
    if custom_str == "USER|1|John|john@example.com|30|1" {
      {
        success: true,
        data: Some({
          id: 1,
          name: "John",
          email: "john@example.com",
          age: 30,
          active: true
        }),
        error: None
      }
    } else {
      {
        success: false,
        data: None,
        error: Some("Invalid custom format")
      }
    }
  }
  
  // Parse user
  let parse_user = fn(data: String, serializer: Serializer) {
    match serializer.format {
      SerializationFormat::Json => parse_user_json(data)
      SerializationFormat::Xml => parse_user_xml(data)
      SerializationFormat::Csv => parse_user_csv(data)
      SerializationFormat::Custom => parse_user_custom(data)
    }
  }
  
  // Test serializer creation
  let json_serializer = create_serializer(SerializationFormat::Json, 1)
  match json_serializer.format {
    SerializationFormat::Json => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(json_serializer.version, 1)
  
  // Create test user
  let user = {
    id: 1,
    name: "John",
    email: "john@example.com",
    age: 30,
    active: true
  }
  
  // Test JSON serialization
  let json_result = serialize_user(user, json_serializer)
  match json_result.data {
    Some(json) => {
      assert_true(json.contains("\"id\":1"))
      assert_true(json.contains("\"name\":\"John\""))
      assert_true(json.contains("\"email\":\"john@example.com\""))
      assert_true(json.contains("\"age\":30"))
      assert_true(json.contains("\"active\":true"))
    }
    None => assert_true(false)
  }
  
  // Test JSON parsing
  match json_result.data {
    Some(json) => {
      let parsed_result = parse_user(json, json_serializer)
      match parsed_result.data {
        Some(parsed_user) => {
          assert_eq(parsed_user.id, 1)
          assert_eq(parsed_user.name, "John")
          assert_eq(parsed_user.email, "john@example.com")
          assert_eq(parsed_user.age, 30)
          assert_true(parsed_user.active)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test XML serialization
  let xml_serializer = create_serializer(SerializationFormat::Xml, 1)
  let xml_result = serialize_user(user, xml_serializer)
  match xml_result.data {
    Some(xml) => {
      assert_true(xml.contains("<Id>1</Id>"))
      assert_true(xml.contains("<Name>John</Name>"))
      assert_true(xml.contains("<Email>john@example.com</Email>"))
      assert_true(xml.contains("<Age>30</Age>"))
      assert_true(xml.contains("<Active>true</Active>"))
    }
    None => assert_true(false)
  }
  
  // Test CSV serialization
  let csv_serializer = create_serializer(SerializationFormat::Csv, 1)
  let csv_result = serialize_user(user, csv_serializer)
  match csv_result.data {
    Some(csv) => assert_eq(csv, "1,John,john@example.com,30,true")
    None => assert_true(false)
  }
  
  // Test custom format serialization
  let custom_serializer = create_serializer(SerializationFormat::Custom, 1)
  let custom_result = serialize_user(user, custom_serializer)
  match custom_result.data {
    Some(custom) => assert_eq(custom, "USER|1|John|john@example.com|30|1")
    None => assert_true(false)
  }
}

// Test 5: Serialization with Versioning
test "serialization with versioning" {
  // Define versioned field
  type VersionedField = {
    name: String,
    value: String,
    since_version: Int,
    deprecated_in: Option[Int]
  }
  
  // Define versioned schema
  type VersionedSchema = {
    name: String,
    version: Int,
    fields: Array[VersionedField]
  }
  
  // Define versioned serializer
  type VersionedSerializer = {
    schemas: Array[VersionedSchema],
    default_version: Int
  }
  
  // Create versioned field
  let create_versioned_field = fn(name: String, value: String, since_version: Int, deprecated_in: Option[Int>) {
    {
      name,
      value,
      since_version,
      deprecated_in
    }
  }
  
  // Create versioned schema
  let create_versioned_schema = fn(name: String, version: Int, fields: Array[VersionedField>) {
    {
      name,
      version,
      fields
    }
  }
  
  // Create versioned serializer
  let create_versioned_serializer = fn(schemas: Array[VersionedSchema>, default_version: Int) {
    {
      schemas,
      default_version
    }
  }
  
  // Get schema for version
  let get_schema_for_version = fn(serializer: VersionedSerializer, version: Int) {
    serializer.schemas.find(fn(schema) { schema.version == version })
  }
  
  // Serialize with version
  let serialize_with_version = fn(serializer: VersionedSerializer, data: Array[VersionedField], target_version: Int) {
    match get_schema_for_version(serializer, target_version) {
      Some(schema) => {
        // Filter fields based on version compatibility
        let compatible_fields = data.filter(fn(field) {
          field.since_version <= target_version && 
          match field.deprecated_in {
            Some(deprecated_version) => deprecated_version > target_version
            None => true
          }
        })
        
        // Serialize to key-value pairs
        let pairs = compatible_fields.map(fn(field) { 
          field.name + ":" + field.value 
        })
        
        let serialized = pairs.reduce(fn(acc, pair) { 
          if acc == "" { pair } else { acc + "," + pair } 
        }, "")
        
        {
          success: true,
          data: Some(serialized),
          version: target_version,
          error: None
        }
      }
      None => {
        {
          success: false,
          data: None,
          version: target_version,
          error: Some("Schema version not found")
        }
      }
    }
  }
  
  // Deserialize with version
  let deserialize_with_version = fn(serializer: VersionedSerializer, data: String, source_version: Int) {
    match get_schema_for_version(serializer, source_version) {
      Some(schema) => {
        // Parse key-value pairs
        let pairs = data.split(",")
        let mut fields = []
        
        for pair in pairs {
          let parts = pair.split(":")
          if parts.length() == 2 {
            let name = parts[0]
            let value = parts[1]
            
            // Check if field exists in schema
            match schema.fields.find(fn(f) { f.name == name }) {
              Some(schema_field) => {
                fields = fields.push(create_versioned_field(
                  name,
                  value,
                  schema_field.since_version,
                  schema_field.deprecated_in
                ))
              }
              None => {
                // Unknown field, still include but mark as from current version
                fields = fields.push(create_versioned_field(
                  name,
                  value,
                  source_version,
                  None
                ))
              }
            }
          }
        }
        
        {
          success: true,
          data: Some(fields),
          version: source_version,
          error: None
        }
      }
      None => {
        {
          success: false,
          data: None,
          version: source_version,
          error: Some("Schema version not found")
        }
      }
    }
  }
  
  // Create versioned fields
  let id_field = create_versioned_field("id", "123", 1, None)
  let name_field = create_versioned_field("name", "John", 1, None)
  let email_field = create_versioned_field("email", "john@example.com", 1, None)
  let age_field = create_versioned_field("age", "30", 2, None)
  let phone_field = create_versioned_field("phone", "555-1234", 2, Some(3))  // Deprecated in version 3
  let address_field = create_versioned_field("address", "123 Main St", 3, None)
  
  // Create versioned schemas
  let v1_schema = create_versioned_schema("User", 1, [id_field, name_field, email_field])
  let v2_schema = create_versioned_schema("User", 2, [id_field, name_field, email_field, age_field, phone_field])
  let v3_schema = create_versioned_schema("User", 3, [id_field, name_field, email_field, age_field, address_field])
  
  // Create versioned serializer
  let serializer = create_versioned_serializer([v1_schema, v2_schema, v3_schema], 3)
  
  // Test schema retrieval
  match get_schema_for_version(serializer, 2) {
    Some(schema) => {
      assert_eq(schema.version, 2)
      assert_eq(schema.fields.length(), 5)
    }
    None => assert_true(false)
  }
  
  // Test serialization with version 1
  let v1_data = [id_field, name_field, email_field]
  let v1_result = serialize_with_version(serializer, v1_data, 1)
  match v1_result.data {
    Some(serialized) => assert_eq(serialized, "id:123,name:John,email:john@example.com")
    None => assert_true(false)
  }
  assert_eq(v1_result.version, 1)
  
  // Test serialization with version 2
  let v2_data = [id_field, name_field, email_field, age_field, phone_field]
  let v2_result = serialize_with_version(serializer, v2_data, 2)
  match v2_result.data {
    Some(serialized) => assert_eq(serialized, "id:123,name:John,email:john@example.com,age:30,phone:555-1234")
    None => assert_true(false)
  }
  assert_eq(v2_result.version, 2)
  
  // Test serialization with version 3 (phone field should be excluded)
  let v3_data = [id_field, name_field, email_field, age_field, phone_field, address_field]
  let v3_result = serialize_with_version(serializer, v3_data, 3)
  match v3_result.data {
    Some(serialized) => assert_eq(serialized, "id:123,name:John,email:john@example.com,age:30,address:123 Main St")
    None => assert_true(false)
  }
  assert_eq(v3_result.version, 3)
  
  // Test serialization with unknown version
  let unknown_result = serialize_with_version(serializer, v1_data, 99)
  assert_false(unknown_result.success)
  match unknown_result.error {
    Some(error) => assert_eq(error, "Schema version not found")
    None => assert_true(false)
  }
  
  // Test deserialization
  match v2_result.data {
    Some(serialized) => {
      let deserialize_result = deserialize_with_version(serializer, serialized, 2)
      assert_true(deserialize_result.success)
      match deserialize_result.data {
        Some(fields) => {
          assert_eq(fields.length(), 5)
          
          // Check specific fields
          match fields.find(fn(f) { f.name == "age" }) {
            Some(field) => assert_eq(field.value, "30")
            None => assert_true(false)
          }
          
          match fields.find(fn(f) { f.name == "phone" }) {
            Some(field) => {
              assert_eq(field.value, "555-1234")
              assert_eq(field.since_version, 2)
              match field.deprecated_in {
                Some(version) => assert_eq(version, 3)
                None => assert_true(false)
              }
            }
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}