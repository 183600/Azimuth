// Azimuth Premium Serialization Tests
// This file contains high-quality test cases for serialization and deserialization

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Simple JSON value types
  type JsonValue {
    Null
    Bool(Bool)
    Number(Float)
    String(String)
    Array(Array[JsonValue])
    Object(Array[(String, JsonValue)])
  }
  
  // JSON serialization
  let json_to_string = fn(value : JsonValue) -> String {
    match value {
      Null => "null"
      Bool(b) => if b { "true" } else { "false" }
      Number(n) => n.to_string()
      String(s) => "\"" + s + "\""
      Array(arr) => {
        let mut result = "["
        for i = 0; i < arr.length(); i = i + 1 {
          if i > 0 { result = result + "," }
          result = result + json_to_string(arr[i])
        }
        result = result + "]"
        result
      }
      Object(obj) => {
        let mut result = "{"
        for i = 0; i < obj.length(); i = i + 1 {
          if i > 0 { result = result + "," }
          result = result + "\"" + obj[i].0 + "\":" + json_to_string(obj[i].1)
        }
        result = result + "}"
        result
      }
    }
  }
  
  // Test basic JSON values
  let null_value = Null
  let bool_value = Bool(true)
  let number_value = Number(42.5)
  let string_value = String("hello")
  
  assert_eq(json_to_string(null_value), "null")
  assert_eq(json_to_string(bool_value), "true")
  assert_eq(json_to_string(Bool(false)), "false")
  assert_eq(json_to_string(number_value), "42.5")
  assert_eq(json_to_string(string_value), "\"hello\"")
  
  // Test JSON array
  let json_array = Array([String("a"), String("b"), Number(1), Bool(true)])
  let array_string = json_to_string(json_array)
  assert_eq(array_string, "[\"a\",\"b\",1,true]")
  
  // Test JSON object
  let json_object = Object([
    ("name", String("John")),
    ("age", Number(30)),
    ("active", Bool(true)),
    ("tags", Array([String("admin"), String("user")]))
  ])
  
  let object_string = json_to_string(json_object)
  assert_eq(object_string, "{\"name\":\"John\",\"age\":30,\"active\":true,\"tags\":[\"admin\",\"user\"]}")
  
  // Test nested structures
  let nested_object = Object([
    ("user", Object([
      ("id", Number(123)),
      ("profile", Object([
        ("name", String("Alice")),
        ("email", String("alice@example.com"))
      ]))
    ])),
    ("permissions", Array([
      String("read"),
      String("write"),
      String("admin")
    ]))
  ])
  
  let nested_string = json_to_string(nested_object)
  assert_eq(nested_string, "{\"user\":{\"id\":123,\"profile\":{\"name\":\"Alice\",\"email\":\"alice@example.com\"}},\"permissions\":[\"read\",\"write\",\"admin\"]}")
}

// Test 2: Binary Serialization
test "binary serialization" {
  // Simple binary protocol
  type BinaryData {
    data : Array[Byte]
  }
  
  let int_to_bytes = fn(value : Int) -> Array[Byte] {
    [
      (value >> 24) & 0xFF,
      (value >> 16) & 0xFF,
      (value >> 8) & 0xFF,
      value & 0xFF
    ]
  }
  
  let bytes_to_int = fn(bytes : Array[Byte]) -> Int {
    (bytes[0] << 24) | (bytes[1] << 16) | (bytes[2] << 8) | bytes[3]
  }
  
  let string_to_bytes = fn(s : String) -> Array[Byte] {
    let chars = s.to_char_array()
    let mut bytes = []
    
    for char in chars {
      bytes.push(char.to_int() & 0xFF)
    }
    
    bytes
  }
  
  let bytes_to_string = fn(bytes : Array[Byte]) -> String {
    let mut chars = []
    
    for byte in bytes {
      chars.push(byte.to_char())
    }
    
    String::from_char_array(chars)
  }
  
  // Test integer serialization
  let test_int = 0x12345678
  let int_bytes = int_to_bytes(test_int)
  assert_eq(int_bytes.length(), 4)
  assert_eq(int_bytes[0], 0x12)
  assert_eq(int_bytes[1], 0x34)
  assert_eq(int_bytes[2], 0x56)
  assert_eq(int_bytes[3], 0x78)
  
  let restored_int = bytes_to_int(int_bytes)
  assert_eq(restored_int, test_int)
  
  // Test string serialization
  let test_string = "Hello, World!"
  let string_bytes = string_to_bytes(test_string)
  assert_eq(string_bytes.length(), 13)
  
  let restored_string = bytes_to_string(string_bytes)
  assert_eq(restored_string, test_string)
  
  // Test complex binary structure
  type Person {
    name : String
    age : Int
    active : Bool
  }
  
  let serialize_person = fn(person : Person) -> Array[Byte] {
    let mut result = []
    
    // Serialize name length and name
    let name_bytes = string_to_bytes(person.name)
    let name_length = name_bytes.length()
    
    // Add name length as 4 bytes
    let length_bytes = int_to_bytes(name_length)
    for byte in length_bytes {
      result.push(byte)
    }
    
    // Add name bytes
    for byte in name_bytes {
      result.push(byte)
    }
    
    // Add age as 4 bytes
    let age_bytes = int_to_bytes(person.age)
    for byte in age_bytes {
      result.push(byte)
    }
    
    // Add active as 1 byte
    result.push(if person.active { 1 } else { 0 })
    
    result
  }
  
  let deserialize_person = fn(bytes : Array[Byte]) -> Person {
    let mut index = 0
    
    // Read name length
    let name_length = bytes_to_int([bytes[index], bytes[index+1], bytes[index+2], bytes[index+3]])
    index = index + 4
    
    // Read name
    let name_bytes = bytes.slice(index, index + name_length)
    let name = bytes_to_string(name_bytes)
    index = index + name_length
    
    // Read age
    let age = bytes_to_int([bytes[index], bytes[index+1], bytes[index+2], bytes[index+3]])
    index = index + 4
    
    // Read active
    let active = bytes[index] == 1
    
    Person { name: name, age: age, active: active }
  }
  
  // Test person serialization
  let person = Person { name: "John Doe", age: 30, active: true }
  let serialized = serialize_person(person)
  
  let deserialized = deserialize_person(serialized)
  assert_eq(deserialized.name, person.name)
  assert_eq(deserialized.age, person.age)
  assert_eq(deserialized.active, person.active)
}

// Test 3: Protocol Buffers Style Serialization
test "protocol buffers style serialization" {
  // Wire format types
  type WireType {
    Varint = 0
    Fixed64 = 1
    LengthDelimited = 2
    Fixed32 = 5
  }
  
  // Encode varint
  let encode_varint = fn(value : Int) -> Array[Byte] {
    let mut result = []
    let mut v = value
    
    while v >= 0x80 {
      result.push((v & 0x7F) | 0x80)
      v = v >> 7
    }
    
    result.push(v & 0x7F)
    result
  }
  
  // Decode varint
  let decode_varint = fn(bytes : Array[Byte]) -> (Int, Int) {
    let mut result = 0
    let mut shift = 0
    let mut index = 0
    
    while index < bytes.length() && (bytes[index] & 0x80) != 0 {
      result = result | ((bytes[index] & 0x7F) << shift)
      shift = shift + 7
      index = index + 1
    }
    
    if index < bytes.length() {
      result = result | ((bytes[index] & 0x7F) << shift)
      index = index + 1
    }
    
    (result, index)
  }
  
  // Test varint encoding/decoding
  let test_values = [0, 1, 127, 128, 255, 300, 16384]
  
  for value in test_values {
    let encoded = encode_varint(value)
    let (decoded, _) = decode_varint(encoded)
    assert_eq(decoded, value)
  }
  
  // Encode field
  let encode_field = fn(field_number : Int, wire_type : WireType, value : Array[Byte]) -> Array[Byte] {
    let key = (field_number << 3) | (wire_type as Int)
    let mut result = encode_varint(key)
    
    for byte in value {
      result.push(byte)
    }
    
    result
  }
  
  // Test field encoding
  let field_value = encode_varint(42)
  let encoded_field = encode_field(1, Varint, field_value)
  
  // Decode field key
  let (decoded_key, key_length) = decode_varint(encoded_field)
  let field_number = decoded_key >> 3
  let wire_type = decoded_key & 0x07
  
  assert_eq(field_number, 1)
  assert_eq(wire_type, Varint as Int)
  
  // Decode field value
  let value_bytes = encoded_field.slice(key_length, encoded_field.length())
  let (decoded_value, _) = decode_varint(value_bytes)
  assert_eq(decoded_value, 42)
  
  // Test message serialization
  type SimpleMessage {
    id : Int
    name : String
    active : Bool
  }
  
  let serialize_message = fn(msg : SimpleMessage) -> Array[Byte] {
    let mut result = []
    
    // Serialize id field (field 1, varint)
    let id_bytes = encode_varint(msg.id)
    let id_field = encode_field(1, Varint, id_bytes)
    for byte in id_field {
      result.push(byte)
    }
    
    // Serialize name field (field 2, length-delimited)
    let name_bytes = string_to_bytes(msg.name)
    let mut name_field = encode_field(2, LengthDelimited, encode_varint(name_bytes.length()))
    for byte in name_bytes {
      name_field.push(byte)
    }
    for byte in name_field {
      result.push(byte)
    }
    
    // Serialize active field (field 3, varint)
    let active_bytes = encode_varint(if msg.active { 1 } else { 0 })
    let active_field = encode_field(3, Varint, active_bytes)
    for byte in active_field {
      result.push(byte)
    }
    
    result
  }
  
  // Test message serialization
  let message = SimpleMessage { id: 123, name: "Test Message", active: true }
  let serialized_message = serialize_message(message)
  
  assert_true(serialized_message.length() > 0)
}

// Test 4: CSV Serialization
test "csv serialization" {
  // CSV escaping rules
  let escape_csv_field = fn(field : String) -> String {
    let needs_quotes = field.contains(",") || 
                      field.contains("\"") || 
                      field.contains("\n") || 
                      field.contains("\r")
    
    if needs_quotes {
      let escaped = field.replace("\"", "\"\"")
      "\"" + escaped + "\""
    } else {
      field
    }
  }
  
  // Serialize row to CSV
  let serialize_csv_row = fn(row : Array[String]) -> String {
    let mut result = ""
    
    for i = 0; i < row.length(); i = i + 1 {
      if i > 0 { result = result + "," }
      result = result + escape_csv_field(row[i])
    }
    
    result
  }
  
  // Test CSV escaping
  assert_eq(escape_csv_field("simple"), "simple")
  assert_eq(escape_csv_field("contains,comma"), "\"contains,comma\"")
  assert_eq(escape_csv_field("contains\"quote"), "\"contains\"\"quote\"")
  assert_eq(escape_csv_field("contains\nnewline"), "\"contains\nnewline\"")
  
  // Test CSV row serialization
  let simple_row = ["field1", "field2", "field3"]
  let simple_csv = serialize_csv_row(simple_row)
  assert_eq(simple_csv, "field1,field2,field3")
  
  let complex_row = ["field1", "contains,comma", "field\"with\"quotes", "last"]
  let complex_csv = serialize_csv_row(complex_row)
  assert_eq(complex_csv, "field1,\"contains,comma\",\"field\"\"with\"\"quotes\",last")
  
  // Serialize multiple rows
  let serialize_csv = fn(rows : Array[Array[String]]) -> String {
    let mut result = ""
    
    for i = 0; i < rows.length(); i = i + 1 {
      if i > 0 { result = result + "\n" }
      result = result + serialize_csv_row(rows[i])
    }
    
    result
  }
  
  let csv_data = [
    ["id", "name", "active"],
    ["1", "John Doe", "true"],
    ["2", "Jane, Smith", "false"],
    ["3", "Bob \"The Builder\"", "true"]
  ]
  
  let csv_string = serialize_csv(csv_data)
  let expected = "id,name,active\n1,John Doe,true\n2,\"Jane, Smith\",false\n3,\"Bob \"\"The Builder\"\"\",true"
  assert_eq(csv_string, expected)
}

// Test 5: XML Serialization
test "xml serialization" {
  // Simple XML element
  type XmlElement {
    name : String
    attributes : Array[(String, String)]
    children : Array[XmlChild]
  }
  
  type XmlChild {
    Element(XmlElement)
    Text(String)
    Comment(String)
  }
  
  // Escape XML text
  let escape_xml_text = fn(text : String) -> String {
    text
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&apos;")
  }
  
  // Serialize XML element
  let serialize_xml_element = fn(element : XmlElement, indent : String) -> String {
    let mut result = indent + "<" + element.name
    
    // Add attributes
    for (name, value) in element.attributes {
      result = result + " " + name + "=\"" + escape_xml_text(value) + "\""
    }
    
    if element.children.length() == 0 {
      // Self-closing tag
      result = result + " />"
    } else {
      result = result + ">"
      
      // Check if all children are text
      let all_text = element.children.all(|child| {
        match child {
          Text(_) => true
          _ => false
        }
      })
      
      if all_text && element.children.length() == 1 {
        // Inline text content
        match element.children[0] {
          Text(text) => {
            result = result + escape_xml_text(text)
          }
          _ => ()
        }
        result = result + "</" + element.name + ">"
      } else {
        // Multi-line content
        result = result + "\n"
        
        for child in element.children {
          match child {
            Element(child_elem) => {
              result = result + serialize_xml_element(child_elem, indent + "  ") + "\n"
            }
            Text(text) => {
              if text.trim().length() > 0 {
                result = result + indent + "  " + escape_xml_text(text) + "\n"
              }
            }
            Comment(text) => {
              result = result + indent + "  <!--" + escape_xml_text(text) + "-->\n"
            }
          }
        }
        
        result = result + indent + "</" + element.name + ">"
      }
    }
    
    result
  }
  
  // Test simple element
  let simple_element = XmlElement {
    name: "item",
    attributes: [("id", "123"), ("active", "true")],
    children: []
  }
  
  let simple_xml = serialize_xml_element(simple_element, "")
  assert_eq(simple_xml, "<item id=\"123\" active=\"true\" />")
  
  // Test element with text content
  let text_element = XmlElement {
    name: "title",
    attributes: [],
    children: [Text("Hello & World")]
  }
  
  let text_xml = serialize_xml_element(text_element, "")
  assert_eq(text_xml, "<title>Hello &amp; World</title>")
  
  // Test complex nested structure
  let complex_element = XmlElement {
    name: "person",
    attributes: [("id", "456")],
    children: [
      Comment("Person information"),
      Element(XmlElement {
        name: "name",
        attributes: [],
        children: [Text("John Doe")]
      }),
      Element(XmlElement {
        name: "age",
        attributes: [],
        children: [Text("30")]
      }),
      Element(XmlElement {
        name: "address",
        attributes: [],
        children: [
          Element(XmlElement {
            name: "street",
            attributes: [],
            children: [Text("123 Main St")]
          }),
          Element(XmlElement {
            name: "city",
            attributes: [],
            children: [Text("Anytown")]
          })
        ]
      })
    ]
  }
  
  let complex_xml = serialize_xml_element(complex_element, "")
  let expected = "<person id=\"456\">\n  <!--Person information-->\n  <name>John Doe</name>\n  <age>30</age>\n  <address>\n    <street>123 Main St</street>\n    <city>Anytown</city>\n  </address>\n</person>"
  assert_eq(complex_xml, expected)
}

// Test 6: Custom Binary Protocol
test "custom binary protocol" {
  // Define message types
  type MessageType {
    Handshake = 1
    Data = 2
    Ack = 3
    Error = 4
  }
  
  // Message header
  type MessageHeader {
    type : MessageType
    length : Int
    sequence : Int
  }
  
  // Serialize header
  let serialize_header = fn(header : MessageHeader) -> Array[Byte] {
    let mut result = []
    
    // Message type (1 byte)
    result.push(header.type as Byte)
    
    // Length (4 bytes, big-endian)
    let length_bytes = int_to_bytes(header.length)
    for byte in length_bytes {
      result.push(byte)
    }
    
    // Sequence (4 bytes, big-endian)
    let sequence_bytes = int_to_bytes(header.sequence)
    for byte in sequence_bytes {
      result.push(byte)
    }
    
    result
  }
  
  // Deserialize header
  let deserialize_header = fn(bytes : Array[Byte]) -> MessageHeader {
    let msg_type = match bytes[0] {
      1 => Handshake
      2 => Data
      3 => Ack
      4 => Error
      _ => Handshake // Default
    }
    
    let length = bytes_to_int([bytes[1], bytes[2], bytes[3], bytes[4]])
    let sequence = bytes_to_int([bytes[5], bytes[6], bytes[7], bytes[8]])
    
    MessageHeader { 
      type: msg_type, 
      length: length, 
      sequence: sequence 
    }
  }
  
  // Test header serialization
  let header = MessageHeader { 
    type: Data, 
    length: 100, 
    sequence: 42 
  }
  
  let serialized_header = serialize_header(header)
  assert_eq(serialized_header.length(), 9)
  
  let deserialized_header = deserialize_header(serialized_header)
  assert_eq(deserialized_header.type as Int, Data as Int)
  assert_eq(deserialized_header.length, 100)
  assert_eq(deserialized_header.sequence, 42)
  
  // Test complete message
  type Message {
    header : MessageHeader
    payload : Array[Byte]
  }
  
  let serialize_message = fn(msg : Message) -> Array[Byte] {
    let mut result = serialize_header(msg.header)
    
    for byte in msg.payload {
      result.push(byte)
    }
    
    result
  }
  
  let deserialize_message = fn(bytes : Array[Byte]) -> Message {
    let header = deserialize_header(bytes.slice(0, 9))
    let payload_start = 9
    let payload_end = payload_start + header.length
    
    let payload = if payload_end <= bytes.length() {
      bytes.slice(payload_start, payload_end)
    } else {
      []
    }
    
    Message { 
      header: header, 
      payload: payload 
    }
  }
  
  // Test complete message
  let payload = string_to_bytes("Hello, World!")
  let message = Message { 
    header: MessageHeader { 
      type: Data, 
      length: payload.length(), 
      sequence: 123 
    }, 
    payload: payload 
  }
  
  let serialized_message = serialize_message(message)
  let deserialized_message = deserialize_message(serialized_message)
  
  assert_eq(deserialized_message.header.type as Int, Data as Int)
  assert_eq(deserialized_message.header.length, payload.length())
  assert_eq(deserialized_message.header.sequence, 123)
  assert_eq(deserialized_message.payload, payload)
}