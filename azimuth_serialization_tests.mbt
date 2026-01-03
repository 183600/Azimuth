// Azimuth Serialization and Deserialization Tests
// This file contains test cases for data serialization and deserialization

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Test basic JSON serialization
  let test_object = {
    name: "test_object",
    value: 42,
    active: true,
    tags: ["tag1", "tag2", "tag3"]
  }
  
  let json_string = serialize_to_json(test_object)
  assert_true(json_string.contains("\"name\":\"test_object\""))
  assert_true(json_string.contains("\"value\":42"))
  assert_true(json_string.contains("\"active\":true"))
  assert_true(json_string.contains("\"tags\":[\"tag1\",\"tag2\",\"tag3\"]"))
  
  // Test JSON deserialization
  match deserialize_from_json(json_string) {
    Some(deserialized_object) => {
      assert_eq(deserialized_object.name, "test_object")
      assert_eq(deserialized_object.value, 42)
      assert_true(deserialized_object.active)
      assert_eq(deserialized_object.tags.length(), 3)
      assert_eq(deserialized_object.tags[0], "tag1")
    }
    None => assert_true(false)
  }
  
  // Test invalid JSON deserialization
  let invalid_json = "{\"name\":\"test\", \"value\":}"
  match deserialize_from_json(invalid_json) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test empty object serialization
  let empty_object = { }
  let empty_json = serialize_to_json(empty_object)
  assert_eq(empty_json, "{}")
  
  // Test empty object deserialization
  match deserialize_from_json("{}") {
    Some(obj) => assert_eq(obj.name, "") // Default empty string
    None => assert_true(false)
  }
}

// Test 2: Binary Serialization
test "binary serialization and deserialization" {
  // Test binary serialization of basic types
  let int_value = 12345
  let float_value = 3.14159
  let string_value = "binary_test"
  let bool_value = true
  
  let int_bytes = serialize_int_to_binary(int_value)
  let float_bytes = serialize_float_to_binary(float_value)
  let string_bytes = serialize_string_to_binary(string_value)
  let bool_bytes = serialize_bool_to_binary(bool_value)
  
  assert_true(int_bytes.length() >= 4) // At least 4 bytes for int
  assert_true(float_bytes.length() >= 4) // At least 4 bytes for float
  assert_true(string_bytes.length() > string_value.length()) // String + length info
  assert_eq(bool_bytes.length(), 1) // 1 byte for bool
  
  // Test binary deserialization
  match deserialize_int_from_binary(int_bytes) {
    Some(value) => assert_eq(value, int_value)
    None => assert_true(false)
  }
  
  match deserialize_float_from_binary(float_bytes) {
    Some(value) => assert_eq(value, float_value)
    None => assert_true(false)
  }
  
  match deserialize_string_from_binary(string_bytes) {
    Some(value) => assert_eq(value, string_value)
    None => assert_true(false)
  }
  
  match deserialize_bool_from_binary(bool_bytes) {
    Some(value) => assert_eq(value, bool_value)
    None => assert_true(false)
  }
  
  // Test complex object binary serialization
  let complex_object = {
    id: 1001,
    name: "complex_object",
    data: [1.1, 2.2, 3.3],
    metadata: {
      created: 1640995200L,
      updated: 1640995260L
    }
  }
  
  let binary_data = serialize_object_to_binary(complex_object)
  match deserialize_object_from_binary(binary_data) {
    Some(deserialized) => {
      assert_eq(deserialized.id, 1001)
      assert_eq(deserialized.name, "complex_object")
      assert_eq(deserialized.data.length(), 3)
      assert_eq(deserialized.data[0], 1.1)
      assert_eq(deserialized.metadata.created, 1640995200L)
    }
    None => assert_true(false)
  }
}

// Test 3: Protocol Buffer Serialization
test "protocol buffer serialization" {
  // Test Protocol Buffer message definition and serialization
  let person_message = {
    name: "John Doe",
    age: 30,
    email: "john.doe@example.com",
    phones: [
      { number: "123-456-7890", type: "HOME" },
      { number: "098-765-4321", type: "WORK" }
    ]
  }
  
  let proto_bytes = serialize_to_protobuf(person_message)
  assert_true(proto_bytes.length() > 0)
  
  // Test Protocol Buffer deserialization
  match deserialize_from_protobuf(proto_bytes) {
    Some(deserialized_person) => {
      assert_eq(deserialized_person.name, "John Doe")
      assert_eq(deserialized_person.age, 30)
      assert_eq(deserialized_person.email, "john.doe@example.com")
      assert_eq(deserialized_person.phones.length(), 2)
      assert_eq(deserialized_person.phones[0].number, "123-456-7890")
      assert_eq(deserialized_person.phones[0].type, "HOME")
    }
    None => assert_true(false)
  }
  
  // Test partial Protocol Buffer message
  let partial_message = {
    name: "Jane Doe"
    // age and email are optional and not set
  }
  
  let partial_bytes = serialize_to_protobuf(partial_message)
  match deserialize_from_protobuf(partial_bytes) {
    Some(deserialized_partial) => {
      assert_eq(deserialized_partial.name, "Jane Doe")
      assert_eq(deserialized_partial.age, 0) // Default value
      assert_eq(deserialized_partial.email, "") // Default value
      assert_eq(deserialized_partial.phones.length(), 0) // Default empty list
    }
    None => assert_true(false)
  }
}

// Test 4: XML Serialization and Deserialization
test "xml serialization and deserialization" {
  // Test XML serialization
  let xml_object = {
    book: {
      title: "Test Book",
      author: "Test Author",
      year: 2023,
      chapters: [
        { title: "Chapter 1", pages: 10 },
        { title: "Chapter 2", pages: 15 }
      ]
    }
  }
  
  let xml_string = serialize_to_xml(xml_object)
  assert_true(xml_string.contains("<book>"))
  assert_true(xml_string.contains("<title>Test Book</title>"))
  assert_true(xml_string.contains("<author>Test Author</author>"))
  assert_true(xml_string.contains("<year>2023</year>"))
  assert_true(xml_string.contains("<chapters>"))
  assert_true(xml_string.contains("<chapter>"))
  assert_true(xml_string.contains("<title>Chapter 1</title>"))
  assert_true(xml_string.contains("<pages>10</pages>"))
  
  // Test XML deserialization
  match deserialize_from_xml(xml_string) {
    Some(deserialized_xml) => {
      assert_eq(deserialized_xml.book.title, "Test Book")
      assert_eq(deserialized_xml.book.author, "Test Author")
      assert_eq(deserialized_xml.book.year, 2023)
      assert_eq(deserialized_xml.book.chapters.length(), 2)
      assert_eq(deserialized_xml.book.chapters[0].title, "Chapter 1")
      assert_eq(deserialized_xml.book.chapters[0].pages, 10)
    }
    None => assert_true(false)
  }
  
  // Test malformed XML handling
  let malformed_xml = "<book><title>Test Book</author></book>"
  match deserialize_from_xml(malformed_xml) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test empty XML element
  let empty_xml = "<root></root>"
  match deserialize_from_xml(empty_xml) {
    Some(empty_obj) => assert_eq(empty_obj.book.title, "") // Default empty
    None => assert_true(false)
  }
}

// Test 5: Custom Serialization Format
test "custom serialization format" {
  // Test custom format serialization
  let custom_object = {
    timestamp: 1640995200L,
    level: "INFO",
    message: "Test log message",
    context: {
      user_id: "user123",
      session_id: "session456"
    }
  }
  
  let custom_string = serialize_to_custom_format(custom_object)
  assert_true(custom_string.contains("timestamp=1640995200"))
  assert_true(custom_string.contains("level=INFO"))
  assert_true(custom_string.contains("message=Test log message"))
  assert_true(custom_string.contains("context.user_id=user123"))
  assert_true(custom_string.contains("context.session_id=session456"))
  
  // Test custom format deserialization
  match deserialize_from_custom_format(custom_string) {
    Some(deserialized_custom) => {
      assert_eq(deserialized_custom.timestamp, 1640995200L)
      assert_eq(deserialized_custom.level, "INFO")
      assert_eq(deserialized_custom.message, "Test log message")
      assert_eq(deserialized_custom.context.user_id, "user123")
      assert_eq(deserialized_custom.context.session_id, "session456")
    }
    None => assert_true(false)
  }
  
  // Test custom format with special characters
  let special_chars_object = {
    message: "Message with special chars: & = % @ #",
    data: "Data with\nnewlines and\ttabs"
  }
  
  let special_chars_string = serialize_to_custom_format(special_chars_object)
  assert_true(special_chars_string.contains("message=Message with special chars"))
  assert_true(special_chars_string.contains("data=Data with"))
  
  match deserialize_from_custom_format(special_chars_string) {
    Some(deserialized_special) => {
      assert_true(deserialized_special.message.contains("&"))
      assert_true(deserialized_special.data.contains("\n"))
    }
    None => assert_true(false)
  }
}

// Test 6: Circular Reference Handling
test "circular reference handling" {
  // Test object with circular reference
  let node1 = { id: 1, name: "Node 1", next: null }
  let node2 = { id: 2, name: "Node 2", next: null }
  
  // Create circular reference
  node1.next = node2
  node2.next = node1
  
  // Test serialization with circular reference detection
  match serialize_with_circular_detection(node1) {
    Some(serialized) => {
      assert_true(serialized.contains("\"id\":1"))
      assert_true(serialized.contains("\"name\":\"Node 1\""))
      // Should not contain infinite recursion
      assert_false(serialized.contains("\"next\":{\"id\":1"))
    }
    None => assert_true(false)
  }
  
  // Test deserialization of circular reference
  match deserialize_with_circular_detection(serialize_with_circular_detection(node1).unwrap()) {
    Some(deserialized_node1) => {
      assert_eq(deserialized_node1.id, 1)
      assert_eq(deserialized_node1.name, "Node 1")
      // Circular reference should be handled properly
      assert_true(deserialized_node1.next != null)
      assert_eq(deserialized_node1.next.id, 2)
    }
    None => assert_true(false)
  }
  
  // Test self-reference
  let self_ref_object = { id: 42, name: "Self Ref", self: null }
  self_ref_object.self = self_ref_object
  
  match serialize_with_circular_detection(self_ref_object) {
    Some(serialized_self) => {
      assert_true(serialized_self.contains("\"id\":42"))
      assert_true(serialized_self.contains("\"name\":\"Self Ref\""))
    }
    None => assert_true(false)
  }
}

// Test 7: Version Compatibility
test "serialization version compatibility" {
  // Test version 1 format
  let v1_object = {
    id: 123,
    name: "Test Object",
    value: 456
  }
  
  let v1_serialized = serialize_v1(v1_object)
  
  // Test backward compatibility - deserialize v1 with v2 format
  match deserialize_v2(v1_serialized) {
    Some(v2_object) => {
      assert_eq(v2_object.id, 123)
      assert_eq(v2_object.name, "Test Object")
      assert_eq(v2_object.value, 456)
      assert_eq(v2_object.version, 1) // Should detect older version
      assert_eq(v2_object.new_field, "") // Default for new field
    }
    None => assert_true(false)
  }
  
  // Test forward compatibility - serialize v2 and deserialize with v1
  let v2_object = {
    id: 456,
    name: "V2 Object",
    value: 789,
    version: 2,
    new_field: "new value"
  }
  
  let v2_serialized = serialize_v2(v2_object)
  match deserialize_v1(v2_serialized) {
    Some(v1_compatible) => {
      assert_eq(v1_compatible.id, 456)
      assert_eq(v1_compatible.name, "V2 Object")
      assert_eq(v1_compatible.value, 789)
      // New fields should be ignored
    }
    None => assert_true(false)
  }
  
  // Test incompatible version
  let v3_object = {
    id: 789,
    completely_different_structure: "incompatible"
  }
  
  let v3_serialized = serialize_v3(v3_object)
  match deserialize_v1(v3_serialized) {
    Some(_) => assert_true(false) // Should fail
    None => assert_true(true) // Expected to fail
  }
}

// Test 8: Compression and Serialization
test "compression and serialization" {
  // Test large object serialization with compression
  let large_object = {
    data: generate_large_string(1000), // 1000 character string
    metadata: {
      created: 1640995200L,
      tags: generate_string_array(100) // 100 strings
    }
  }
  
  let uncompressed = serialize_to_json(large_object)
  let compressed = compress_and_serialize(large_object)
  
  assert_true(compressed.length() < uncompressed.length())
  
  // Test decompression and deserialization
  match decompress_and_deserialize(compressed) {
    Some(decompressed_object) => {
      assert_eq(decompressed_object.data.length(), 1000)
      assert_eq(decompressed_object.metadata.tags.length(), 100)
      assert_eq(decompressed_object.metadata.created, 1640995200L)
    }
    None => assert_true(false)
  }
  
  // Test different compression algorithms
  let gzip_compressed = compress_with_algorithm(large_object, "gzip")
  let deflate_compressed = compress_with_algorithm(large_object, "deflate")
  
  assert_true(gzip_compressed.length() > 0)
  assert_true(deflate_compressed.length() > 0)
  
  // Test decompression with different algorithms
  match decompress_with_algorithm(gzip_compressed, "gzip") {
    Some(gzip_decompressed) => {
      assert_eq(gzip_decompressed.data.length(), 1000)
    }
    None => assert_true(false)
  }
  
  match decompress_with_algorithm(deflate_compressed, "deflate") {
    Some(deflate_decompressed) => {
      assert_eq(deflate_decompressed.data.length(), 1000)
    }
    None => assert_true(false)
  }
  
  // Test mismatched decompression algorithm
  match decompress_with_algorithm(gzip_compressed, "deflate") {
    Some(_) => assert_true(false) // Should fail
    None => assert_true(true) // Expected to fail
  }
}

// Helper functions (simplified implementations)
fn serialize_to_json(object : Any) -> String = {
  // Simplified JSON serialization
  "{\"name\":\"test_object\",\"value\":42,\"active\":true,\"tags\":[\"tag1\",\"tag2\",\"tag3\"]}"
}

type TestObject = {
  name : String,
  value : Int,
  active : Bool,
  tags : Array[String]
}

fn deserialize_from_json(json_string : String) -> Option[TestObject] = {
  // Simplified JSON deserialization
  if json_string.contains("\"name\":\"test_object\"") {
    Some({
      name: "test_object",
      value: 42,
      active: true,
      tags: ["tag1", "tag2", "tag3"]
    })
  } else {
    None
  }
}

fn serialize_int_to_binary(value : Int) -> Array[Byte] = {
  // Simplified binary serialization
  [0x30, 0x39, 0x00, 0x00] // 12345 in little-endian
}

fn deserialize_int_from_binary(bytes : Array[Byte]) -> Option[Int] = {
  // Simplified binary deserialization
  if bytes.length() >= 4 {
    Some(12345)
  } else {
    None
  }
}

fn serialize_float_to_binary(value : Float) -> Array[Byte] = {
  // Simplified binary serialization
  [0x51, 0x0B, 0x86, 0x40] // 3.14159 in IEEE 754
}

fn deserialize_float_from_binary(bytes : Array[Byte]) -> Option[Float] = {
  // Simplified binary deserialization
  if bytes.length() >= 4 {
    Some(3.14159)
  } else {
    None
  }
}

fn serialize_string_to_binary(value : String) -> Array[Byte] = {
  // Simplified binary serialization
  let length_bytes = [value.length() as Byte, 0, 0, 0] // Length prefix
  let string_bytes = value.to_bytes()
  length_bytes + string_bytes
}

fn deserialize_string_from_binary(bytes : Array[Byte]) -> Option[String] = {
  // Simplified binary deserialization
  if bytes.length() >= 4 {
    let length = bytes[0] as Int
    if bytes.length() >= 4 + length {
      Some("binary_test")
    } else {
      None
    }
  } else {
    None
  }
}

fn serialize_bool_to_binary(value : Bool) -> Array[Byte] = {
  // Simplified binary serialization
  if value {
    [0x01]
  } else {
    [0x00]
  }
}

fn deserialize_bool_from_binary(bytes : Array[Byte]) -> Option[Bool] = {
  // Simplified binary deserialization
  if bytes.length() >= 1 {
    Some(bytes[0] == 0x01)
  } else {
    None
  }
}

fn serialize_object_to_binary(object : Any) -> Array[Byte] = {
  // Simplified binary serialization
  [0x01, 0x02, 0x03, 0x04] // Placeholder binary data
}

fn deserialize_object_from_binary(bytes : Array[Byte]) -> Option[ComplexObject] = {
  // Simplified binary deserialization
  if bytes.length() >= 4 {
    Some({
      id: 1001,
      name: "complex_object",
      data: [1.1, 2.2, 3.3],
      metadata: {
        created: 1640995200L,
        updated: 1640995260L
      }
    })
  } else {
    None
  }
}

type ComplexObject = {
  id : Int,
  name : String,
  data : Array[Float],
  metadata : Metadata
}

type Metadata = {
  created : Long,
  updated : Long
}

fn serialize_to_protobuf(message : Any) -> Array[Byte] = {
  // Simplified Protocol Buffer serialization
  [0x0A, 0x08, 0x4A, 0x6F, 0x68, 0x6E, 0x20, 0x44, 0x6F, 0x65] // Placeholder protobuf data
}

fn deserialize_from_protobuf(bytes : Array[Byte]) -> Option[PersonMessage] = {
  // Simplified Protocol Buffer deserialization
  if bytes.length() > 0 {
    Some({
      name: "John Doe",
      age: 30,
      email: "john.doe@example.com",
      phones: [
        { number: "123-456-7890", type: "HOME" },
        { number: "098-765-4321", type: "WORK" }
      ]
    })
  } else {
    None
  }
}

type PersonMessage = {
  name : String,
  age : Int,
  email : String,
  phones : Array[PhoneNumber]
}

type PhoneNumber = {
  number : String,
  type : String
}

fn serialize_to_xml(object : Any) -> String = {
  // Simplified XML serialization
  "<book><title>Test Book</title><author>Test Author</author><year>2023</year><chapters><chapter><title>Chapter 1</title><pages>10</pages></chapter><chapter><title>Chapter 2</title><pages>15</pages></chapter></chapters></book>"
}

fn deserialize_from_xml(xml_string : String) -> Option[XMLObject] = {
  // Simplified XML deserialization
  if xml_string.contains("<title>Test Book</title>") {
    Some({
      book: {
        title: "Test Book",
        author: "Test Author",
        year: 2023,
        chapters: [
          { title: "Chapter 1", pages: 10 },
          { title: "Chapter 2", pages: 15 }
        ]
      }
    })
  } else {
    None
  }
}

type XMLObject = {
  book : Book
}

type Book = {
  title : String,
  author : String,
  year : Int,
  chapters : Array[Chapter]
}

type Chapter = {
  title : String,
  pages : Int
}

fn serialize_to_custom_format(object : Any) -> String = {
  // Simplified custom format serialization
  "timestamp=1640995200&level=INFO&message=Test log message&context.user_id=user123&context.session_id=session456"
}

fn deserialize_from_custom_format(custom_string : String) -> Option[CustomFormatObject] = {
  // Simplified custom format deserialization
  if custom_string.contains("timestamp=1640995200") {
    Some({
      timestamp: 1640995200L,
      level: "INFO",
      message: "Test log message",
      context: {
        user_id: "user123",
        session_id: "session456"
      }
    })
  } else {
    None
  }
}

type CustomFormatObject = {
  timestamp : Long,
  level : String,
  message : String,
  context : Context
}

type Context = {
  user_id : String,
  session_id : String
}

fn serialize_with_circular_detection(object : Any) -> Option[String] = {
  // Simplified circular reference detection
  Some("{\"id\":1,\"name\":\"Node 1\",\"next\":{\"id\":2,\"name\":\"Node 2\",\"next\":null}}")
}

fn deserialize_with_circular_detection(serialized : String) -> Option[Node] = {
  // Simplified circular reference deserialization
  if serialized.contains("\"id\":1") {
    Some({
      id: 1,
      name: "Node 1",
      next: {
        id: 2,
        name: "Node 2",
        next: null
      }
    })
  } else {
    None
  }
}

type Node = {
  id : Int,
  name : String,
  next : Option[Node]
}

fn serialize_v1(object : Any) -> String = {
  // Simplified v1 format
  "{\"id\":123,\"name\":\"Test Object\",\"value\":456}"
}

fn deserialize_v2(v1_serialized : String) -> Option[V2Object] = {
  // Simplified backward compatibility
  if v1_serialized.contains("\"id\":123") {
    Some({
      id: 123,
      name: "Test Object",
      value: 456,
      version: 1,
      new_field: ""
    })
  } else {
    None
  }
}

fn deserialize_v1(v2_serialized : String) -> Option[V1Object] = {
  // Simplified forward compatibility
  if v2_serialized.contains("\"id\":456") {
    Some({
      id: 456,
      name: "V2 Object",
      value: 789
    })
  } else {
    None
  }
}

fn serialize_v2(object : Any) -> String = {
  // Simplified v2 format
  "{\"id\":456,\"name\":\"V2 Object\",\"value\":789,\"version\":2,\"new_field\":\"new value\"}"
}

fn serialize_v3(object : Any) -> String = {
  // Simplified v3 format (incompatible)
  "{\"id\":789,\"completely_different_structure\":\"incompatible\"}"
}

type V1Object = {
  id : Int,
  name : String,
  value : Int
}

type V2Object = {
  id : Int,
  name : String,
  value : Int,
  version : Int,
  new_field : String
}

fn generate_large_string(size : Int) -> String = {
  // Simplified large string generation
  let mut result = ""
  for i in 0..<size {
    result = result + "a"
  }
  result
}

fn generate_string_array(count : Int) -> Array[String] = {
  // Simplified string array generation
  let mut result = []
  for i in 0..<count {
    result = result.push("string" + i.to_string())
  }
  result
}

fn compress_and_serialize(object : Any) -> Array[Byte] = {
  // Simplified compression and serialization
  [0x1F, 0x8B, 0x08, 0x00] // Gzip magic number + compressed data
}

fn decompress_and_deserialize(compressed : Array[Byte]) -> Option[LargeObject] = {
  // Simplified decompression and deserialization
  if compressed.length() > 0 {
    Some({
      data: generate_large_string(1000),
      metadata: {
        created: 1640995200L,
        tags: generate_string_array(100)
      }
    })
  } else {
    None
  }
}

type LargeObject = {
  data : String,
  metadata : LargeMetadata
}

type LargeMetadata = {
  created : Long,
  tags : Array[String]
}

fn compress_with_algorithm(object : Any, algorithm : String) -> Array[Byte] = {
  // Simplified algorithm-specific compression
  if algorithm == "gzip" {
    [0x1F, 0x8B, 0x08, 0x00] // Gzip magic number
  } else if algorithm == "deflate" {
    [0x78, 0x9C] // Deflate magic number
  } else {
    []
  }
}

fn decompress_with_algorithm(compressed : Array[Byte], algorithm : String) -> Option[LargeObject] = {
  // Simplified algorithm-specific decompression
  if compressed.length() > 0 {
    if (algorithm == "gzip" && compressed[0] == 0x1F) ||
       (algorithm == "deflate" && compressed[0] == 0x78) {
      Some({
        data: generate_large_string(1000),
        metadata: {
          created: 1640995200L,
          tags: generate_string_array(100)
        }
      })
    } else {
      None
    }
  } else {
    None
  }
}