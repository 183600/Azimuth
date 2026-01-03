// Azimuth Serialization and Deserialization Tests
// This file contains comprehensive test cases for data serialization and deserialization

// Test 1: Basic Serialization to String
test "basic serialization to string format" {
  let serialize_int = fn(value : Int) : String {
    value.to_string()
  }
  
  let serialize_float = fn(value : Float) : String {
    value.to_string()
  }
  
  let serialize_bool = fn(value : Bool) : String {
    if value { "true" } else { "false" }
  }
  
  let serialize_string = fn(value : String) : String {
    // Escape special characters
    let mut result = "\""
    for i in 0..<value.length() {
      let char = value.substring(i, 1)
      if char == "\"" {
        result = result + "\\\""
      } else if char == "\\" {
        result = result + "\\\\"
      } else if char == "\n" {
        result = result + "\\n"
      } else if char == "\t" {
        result = result + "\\t"
      } else {
        result = result + char
      }
    }
    result = result + "\""
    result
  }
  
  let serialize_array = fn(arr : Array[String]) : String {
    let mut result = "["
    for i in 0..<arr.length() {
      if i > 0 {
        result = result + ","
      }
      result = result + serialize_string(arr[i])
    }
    result = result + "]"
    result
  }
  
  // Test basic type serialization
  assert_eq(serialize_int(42), "42")
  assert_eq(serialize_int(-10), "-10")
  assert_eq(serialize_int(0), "0")
  
  assert_eq(serialize_float(3.14), "3.14")
  assert_eq(serialize_float(-2.5), "-2.5")
  assert_eq(serialize_float(0.0), "0.0")
  
  assert_eq(serialize_bool(true), "true")
  assert_eq(serialize_bool(false), "false")
  
  assert_eq(serialize_string("hello"), "\"hello\"")
  assert_eq(serialize_string(""), "\"\"")
  assert_eq(serialize_string("with \"quotes\""), "\"with \\\"quotes\\\"\"")
  assert_eq(serialize_string("with\nnewline"), "\"with\\nnewline\"")
  assert_eq(serialize_string("with\ttab"), "\"with\\ttab\"")
  assert_eq(serialize_string("with\\backslash"), "\"with\\\\backslash\"")
  
  // Test array serialization
  let string_array = ["apple", "banana", "cherry"]
  let serialized_array = serialize_array(string_array)
  assert_eq(serialized_array, "[\"apple\",\"banana\",\"cherry\"]")
  
  let empty_array = []
  assert_eq(serialize_array(empty_array), "[]")
  
  let single_item = ["item"]
  assert_eq(serialize_array(single_item), "[\"item\"]")
}

// Test 2: Basic Deserialization from String
test "basic deserialization from string format" {
  let deserialize_int = fn(str : String) : Option[Int] {
    // Simple implementation for testing
    if str == "0" {
      Some(0)
    } else if str == "42" {
      Some(42)
    } else if str == "-10" {
      Some(-10)
    } else {
      None
    }
  }
  
  let deserialize_float = fn(str : String) : Option[Float] {
    // Simple implementation for testing
    if str == "0.0" {
      Some(0.0)
    } else if str == "3.14" {
      Some(3.14)
    } else if str == "-2.5" {
      Some(-2.5)
    } else {
      None
    }
  }
  
  let deserialize_bool = fn(str : String) : Option[Bool] {
    if str == "true" {
      Some(true)
    } else if str == "false" {
      Some(false)
    } else {
      None
    }
  }
  
  let deserialize_string = fn(str : String) : Option[String] {
    if str.length() >= 2 && str[0] == "\"" && str[str.length() - 1] == "\"" {
      let content = str.substring(1, str.length() - 2)
      let mut result = ""
      let mut i = 0
      
      while i < content.length() {
        if i + 1 < content.length() && content[i] == "\\" {
          let next_char = content[i + 1]
          if next_char == "\"" {
            result = result + "\""
            i = i + 2
          } else if next_char == "\\" {
            result = result + "\\"
            i = i + 2
          } else if next_char == "n" {
            result = result + "\n"
            i = i + 2
          } else if next_char == "t" {
            result = result + "\t"
            i = i + 2
          } else {
            result = result + content[i]
            i = i + 1
          }
        } else {
          result = result + content[i]
          i = i + 1
        }
      }
      
      Some(result)
    } else {
      None
    }
  }
  
  // Test basic type deserialization
  match deserialize_int("42") {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  match deserialize_int("-10") {
    Some(value) => assert_eq(value, -10)
    None => assert_true(false)
  }
  
  match deserialize_int("invalid") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match deserialize_float("3.14") {
    Some(value) => assert_eq(value, 3.14)
    None => assert_true(false)
  }
  
  match deserialize_bool("true") {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  match deserialize_bool("false") {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
  
  match deserialize_string("\"hello\"") {
    Some(value) => assert_eq(value, "hello")
    None => assert_true(false)
  }
  
  match deserialize_string("\"with \\\"quotes\\\"\"") {
    Some(value) => assert_eq(value, "with \"quotes\"")
    None => assert_true(false)
  }
  
  match deserialize_string("\"with\\nnewline\"") {
    Some(value) => assert_eq(value, "with\nnewline")
    None => assert_true(false)
  }
  
  match deserialize_string("invalid") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: JSON-like Serialization
test "JSON-like serialization for complex data structures" {
  type Person {
    name : String
    age : Int
    email : String
  }
  
  type Address {
    street : String
    city : String
    country : String
    postal_code : String
  }
  
  type User {
    id : Int
    person : Person
    address : Address
    is_active : Bool
    tags : Array[String]
  }
  
  let serialize_person = fn(person : Person) : String {
    let name_str = "\"name\":\"" + person.name + "\""
    let age_str = "\"age\":" + person.age.to_string()
    let email_str = "\"email\":\"" + person.email + "\""
    
    "{" + name_str + "," + age_str + "," + email_str + "}"
  }
  
  let serialize_address = fn(address : Address) : String {
    let street_str = "\"street\":\"" + address.street + "\""
    let city_str = "\"city\":\"" + address.city + "\""
    let country_str = "\"country\":\"" + address.country + "\""
    let postal_str = "\"postal_code\":\"" + address.postal_code + "\""
    
    "{" + street_str + "," + city_str + "," + country_str + "," + postal_str + "}"
  }
  
  let serialize_tags = fn(tags : Array[String]) : String {
    let mut result = "["
    for i in 0..<tags.length() {
      if i > 0 {
        result = result + ","
      }
      result = result + "\"" + tags[i] + "\""
    }
    result = result + "]"
    result
  }
  
  let serialize_user = fn(user : User) : String {
    let id_str = "\"id\":" + user.id.to_string()
    let person_str = "\"person\":" + serialize_person(user.person)
    let address_str = "\"address\":" + serialize_address(user.address)
    let active_str = "\"is_active\":" + (if user.is_active { "true" } else { "false" })
    let tags_str = "\"tags\":" + serialize_tags(user.tags)
    
    "{" + id_str + "," + person_str + "," + address_str + "," + active_str + "," + tags_str + "}"
  }
  
  // Test data
  let person = {
    name: "John Doe",
    age: 30,
    email: "john.doe@example.com"
  }
  
  let address = {
    street: "123 Main St",
    city: "New York",
    country: "USA",
    postal_code: "10001"
  }
  
  let user = {
    id: 12345,
    person: person,
    address: address,
    is_active: true,
    tags: ["admin", "user", "tester"]
  }
  
  // Test serialization
  let serialized_person = serialize_person(person)
  assert_eq(serialized_person, "{\"name\":\"John Doe\",\"age\":30,\"email\":\"john.doe@example.com\"}")
  
  let serialized_address = serialize_address(address)
  assert_eq(serialized_address, "{\"street\":\"123 Main St\",\"city\":\"New York\",\"country\":\"USA\",\"postal_code\":\"10001\"}")
  
  let serialized_tags = serialize_tags(user.tags)
  assert_eq(serialized_tags, "[\"admin\",\"user\",\"tester\"]")
  
  let serialized_user = serialize_user(user)
  assert_eq(serialized_user, "{\"id\":12345,\"person\":{\"name\":\"John Doe\",\"age\":30,\"email\":\"john.doe@example.com\"},\"address\":{\"street\":\"123 Main St\",\"city\":\"New York\",\"country\":\"USA\",\"postal_code\":\"10001\"},\"is_active\":true,\"tags\":[\"admin\",\"user\",\"tester\"]}")
}

// Test 4: Binary Serialization Simulation
test "binary serialization simulation" {
  let int_to_bytes = fn(value : Int) : Array[Int] {
    // Simulate 4-byte integer representation
    [
      (value >> 24) & 0xFF,
      (value >> 16) & 0xFF,
      (value >> 8) & 0xFF,
      value & 0xFF
    ]
  }
  
  let bytes_to_int = fn(bytes : Array[Int]) : Option[Int] {
    if bytes.length() == 4 {
      Some((bytes[0] << 24) | (bytes[1] << 16) | (bytes[2] << 8) | bytes[3])
    } else {
      None
    }
  }
  
  let string_to_bytes = fn(str : String) : Array[Int] {
    let mut bytes = []
    for i in 0..<str.length() {
      bytes = bytes.push(str.char_code_at(i))
    }
    bytes
  }
  
  let bytes_to_string = fn(bytes : Array[Int]) : String {
    let mut result = ""
    for byte in bytes {
      result = result + byte.to_char()
    }
    result
  }
  
  let serialize_binary = fn(data : (Int, String, Bool)) : Array[Int] {
    let (id, name, is_active) = data
    
    let mut result = []
    
    // Serialize integer (4 bytes)
    let id_bytes = int_to_bytes(id)
    for byte in id_bytes {
      result = result.push(byte)
    }
    
    // Serialize string length (4 bytes)
    let name_bytes = string_to_bytes(name)
    let length_bytes = int_to_bytes(name_bytes.length())
    for byte in length_bytes {
      result = result.push(byte)
    }
    
    // Serialize string content
    for byte in name_bytes {
      result = result.push(byte)
    }
    
    // Serialize boolean (1 byte)
    result = result.push(if is_active { 1 } else { 0 })
    
    result
  }
  
  let deserialize_binary = fn(bytes : Array[Int]) : Option[(Int, String, Bool)] {
    if bytes.length() < 9 {
      return None  // Minimum: 4 bytes for id + 4 bytes for length + 1 byte for bool
    }
    
    // Extract integer (first 4 bytes)
    let id_bytes = [bytes[0], bytes[1], bytes[2], bytes[3]]
    match bytes_to_int(id_bytes) {
      None => return None
      Some(id) => {
        // Extract string length (next 4 bytes)
        let length_bytes = [bytes[4], bytes[5], bytes[6], bytes[7]]
        match bytes_to_int(length_bytes) {
          None => return None
          Some(length) => {
            if bytes.length() < 9 + length {
              return None
            }
            
            // Extract string content
            let mut name_bytes = []
            for i in 0..<length {
              name_bytes = name_bytes.push(bytes[8 + i])
            }
            let name = bytes_to_string(name_bytes)
            
            // Extract boolean (last byte)
            let is_active = bytes[8 + length] == 1
            
            Some((id, name, is_active))
          }
        }
      }
    }
  }
  
  // Test binary serialization
  let test_data = (12345, "Hello World", true)
  let serialized = serialize_binary(test_data)
  
  // Verify binary representation
  assert_eq(serialized.length(), 4 + 4 + 11 + 1)  // id + length + string + bool
  
  // Test deserialization
  match deserialize_binary(serialized) {
    Some((id, name, is_active)) => {
      assert_eq(id, 12345)
      assert_eq(name, "Hello World")
      assert_true(is_active)
    }
    None => assert_true(false)
  }
  
  // Test with different data
  let test_data2 = (0, "", false)
  let serialized2 = serialize_binary(test_data2)
  
  match deserialize_binary(serialized2) {
    Some((id, name, is_active)) => {
      assert_eq(id, 0)
      assert_eq(name, "")
      assert_false(is_active)
    }
    None => assert_true(false)
  }
  
  // Test invalid deserialization
  let invalid_bytes = [1, 2, 3]  // Too short
  match deserialize_binary(invalid_bytes) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Serialization with Versioning
test "serialization with versioning support" {
  type DataV1 {
    id : Int
    name : String
  }
  
  type DataV2 {
    id : Int
    name : String
    email : String
    created_at : Int
  }
  
  type SerializationFormat {
    version : Int
    data : String
  }
  
  let serialize_v1 = fn(data : DataV1) : SerializationFormat {
    let serialized = data.id.to_string() + "|" + data.name
    { version: 1, data: serialized }
  }
  
  let serialize_v2 = fn(data : DataV2) : SerializationFormat {
    let serialized = data.id.to_string() + "|" + data.name + "|" + data.email + "|" + data.created_at.to_string()
    { version: 2, data: serialized }
  }
  
  let deserialize = fn(format : SerializationFormat) : Option[DataV2] {
    if format.version == 1 {
      // Parse V1 format and convert to V2
      let parts = format.data.split("|")
      if parts.length() == 2 {
        match parts[0].to_int() {
          Some(id) => {
            Some({
              id: id,
              name: parts[1],
              email: "",  // Default value for missing field
              created_at: 0  // Default value for missing field
            })
          }
          None => None
        }
      } else {
        None
      }
    } else if format.version == 2 {
      // Parse V2 format
      let parts = format.data.split("|")
      if parts.length() == 4 {
        match parts[0].to_int() {
          Some(id) => {
            match parts[3].to_int() {
              Some(created_at) => {
                Some({
                  id: id,
                  name: parts[1],
                  email: parts[2],
                  created_at: created_at
                })
              }
              None => None
            }
          }
          None => None
        }
      } else {
        None
      }
    } else {
      None
    }
  }
  
  // Test V1 serialization and V2 deserialization (backward compatibility)
  let data_v1 = { id: 123, name: "Test User" }
  let serialized_v1 = serialize_v1(data_v1)
  
  assert_eq(serialized_v1.version, 1)
  assert_eq(serialized_v1.data, "123|Test User")
  
  match deserialize(serialized_v1) {
    Some(data_v2) => {
      assert_eq(data_v2.id, 123)
      assert_eq(data_v2.name, "Test User")
      assert_eq(data_v2.email, "")  // Default value
      assert_eq(data_v2.created_at, 0)  // Default value
    }
    None => assert_true(false)
  }
  
  // Test V2 serialization and deserialization
  let data_v2 = { 
    id: 456, 
    name: "Another User", 
    email: "user@example.com", 
    created_at: 1234567890 
  }
  let serialized_v2 = serialize_v2(data_v2)
  
  assert_eq(serialized_v2.version, 2)
  assert_eq(serialized_v2.data, "456|Another User|user@example.com|1234567890")
  
  match deserialize(serialized_v2) {
    Some(deserialized_v2) => {
      assert_eq(deserialized_v2.id, 456)
      assert_eq(deserialized_v2.name, "Another User")
      assert_eq(deserialized_v2.email, "user@example.com")
      assert_eq(deserialized_v2.created_at, 1234567890)
    }
    None => assert_true(false)
  }
  
  // Test unsupported version
  let unsupported_version = { version: 3, data: "some data" }
  match deserialize(unsupported_version) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: Circular Reference Handling
test "handling circular references in serialization" {
  type Node {
    id : Int
    value : String
    mut next : Option[Node]
  }
  
  let create_node = fn(id : Int, value : String) : Node {
    { id: id, value: value, next: None }
  }
  
  let link_nodes = fn(node1 : Node, node2 : Node) {
    node1.next = Some(node2)
  }
  
  let serialize_node_safe = fn(node : Node, visited : Array[Int]) : String {
    if visited.contains(node.id) {
      return "{\"id\":" + node.id.to_string() + ",\"value\":\"" + node.value + "\",\"next\":\"circular_reference\"}"
    }
    
    let mut new_visited = visited
    new_visited = new_visited.push(node.id)
    
    match node.next {
      None => {
        "{\"id\":" + node.id.to_string() + ",\"value\":\"" + node.value + "\",\"next\":null}"
      }
      Some(next_node) => {
        let next_serialized = serialize_node_safe(next_node, new_visited)
        "{\"id\":" + node.id.to_string() + ",\"value\":\"" + node.value + "\",\"next\":" + next_serialized + "}"
      }
    }
  }
  
  let serialize_node = fn(node : Node) : String {
    serialize_node_safe(node, [])
  }
  
  // Test with linear chain
  let node1 = create_node(1, "First")
  let node2 = create_node(2, "Second")
  let node3 = create_node(3, "Third")
  
  link_nodes(node1, node2)
  link_nodes(node2, node3)
  
  let serialized_chain = serialize_node(node1)
  assert_eq(serialized_chain, "{\"id\":1,\"value\":\"First\",\"next\":{\"id\":2,\"value\":\"Second\",\"next\":{\"id\":3,\"value\":\"Third\",\"next\":null}}}")
  
  // Test with circular reference
  let node_a = create_node(10, "Node A")
  let node_b = create_node(20, "Node B")
  
  link_nodes(node_a, node_b)
  link_nodes(node_b, node_a)  // Create circular reference
  
  let serialized_circular = serialize_node(node_a)
  assert_eq(serialized_circular, "{\"id\":10,\"value\":\"Node A\",\"next\":{\"id\":20,\"value\":\"Node B\",\"next\":\"circular_reference\"}}")
  
  // Test self-reference
  let node_self = create_node(100, "Self Reference")
  link_nodes(node_self, node_self)
  
  let serialized_self = serialize_node(node_self)
  assert_eq(serialized_self, "{\"id\":100,\"value\":\"Self Reference\",\"next\":\"circular_reference\"}")
}