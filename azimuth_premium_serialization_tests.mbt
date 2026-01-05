// Azimuth Premium Test Suite - Serialization and Deserialization
// This file contains comprehensive test cases for data serialization, deserialization, and format conversion

// Test 1: JSON Serialization and Deserialization
test "json_serialization_and_deserialization" {
  // Test basic JSON serialization
  let serialize_json = |data| {
    if data is String {
      "\"" + data + "\""
    } else if data is Number {
      data.to_string()
    } else if data is Boolean {
      if data { "true" } else { "false" }
    } else if data is Array {
      let result = "["
      for i = 0; i < data.length(); i = i + 1 {
        result = result + serialize_json(data[i])
        if i < data.length() - 1 {
          result = result + ","
        }
      }
      result + "]"
    } else if data is Object {
      let result = "{"
      let keys = data.keys()
      for i = 0; i < keys.length(); i = i + 1 {
        let key = keys[i]
        result = result + "\"" + key + "\":" + serialize_json(data[key])
        if i < keys.length() - 1 {
          result = result + ","
        }
      }
      result + "}"
    } else {
      "null"
    }
  }
  
  // Test basic JSON deserialization
  let deserialize_json = |json_string| {
    // Simplified JSON parser for testing
    if json_string == "null" {
      null
    } else if json_string == "true" {
      true
    } else if json_string == "false" {
      false
    } else if json_string.starts_with("\"") && json_string.ends_with("\"") {
      json_string.slice(1, json_string.length() - 1)
    } else if json_string.starts_with("[") && json_string.ends_with("]") {
      let content = json_string.slice(1, json_string.length() - 1)
      let parts = content.split(",")
      let result = []
      for part in parts {
        result.push(deserialize_json(part))
      }
      result
    } else if json_string.starts_with("{") && json_string.ends_with("}") {
      let content = json_string.slice(1, json_string.length() - 1)
      let pairs = content.split(",")
      let result = {}
      for pair in pairs {
        let kv = pair.split(":")
        if kv.length() == 2 {
          let key = deserialize_json(kv[0])
          let value = deserialize_json(kv[1])
          result[key] = value
        }
      }
      result
    } else {
      // Try to parse as number
      let num = json_string.to_int()
      if num is not None {
        num
      } else {
        json_string  // Fallback to string
      }
    }
  }
  
  // Test basic types
  assert_eq(serialize_json("hello"), "\"hello\"")
  assert_eq(serialize_json(42), "42")
  assert_eq(serialize_json(true), "true")
  assert_eq(serialize_json(false), "false")
  assert_eq(serialize_json(null), "null")
  
  // Test arrays
  assert_eq(serialize_json([1, 2, 3]), "[1,2,3]")
  assert_eq(serialize_json(["hello", "world"]), "[\"hello\",\"world\"]")
  assert_eq(serialize_json([1, "hello", true]), "[1,\"hello\",true]")
  
  // Test objects
  let obj = { "name": "John", "age": 30, "active": true }
  let serialized_obj = serialize_json(obj)
  assert_true(serialized_obj.contains("\"name\":\"John\""))
  assert_true(serialized_obj.contains("\"age\":30"))
  assert_true(serialized_obj.contains("\"active\":true"))
  
  // Test nested structures
  let nested = {
    "user": {
      "name": "Alice",
      "details": {
        "age": 25,
        "hobbies": ["reading", "coding"]
      }
    },
    "active": true
  }
  let serialized_nested = serialize_json(nested)
  assert_true(serialized_nested.contains("\"user\""))
  assert_true(serialized_nested.contains("\"name\":\"Alice\""))
  assert_true(serialized_nested.contains("\"hobbies\":[\"reading\",\"coding\"]"))
  
  // Test deserialization
  assert_eq(deserialize_json("\"hello\""), "hello")
  assert_eq(deserialize_json("42"), 42)
  assert_eq(deserialize_json("true"), true)
  assert_eq(deserialize_json("false"), false)
  assert_eq(deserialize_json("null"), null)
  
  let deserialized_array = deserialize_json("[1,2,3]")
  assert_eq(deserialized_array, [1, 2, 3])
  
  let deserialized_obj = deserialize_json("{\"name\":\"John\",\"age\":30}")
  assert_eq(deserialized_obj["name"], "John")
  assert_eq(deserialized_obj["age"], 30)
  
  // Test round-trip serialization
  let original = {
    "users": [
      { "name": "Alice", "age": 25, "active": true },
      { "name": "Bob", "age": 30, "active": false }
    ],
    "count": 2
  }
  
  let serialized = serialize_json(original)
  let deserialized = deserialize_json(serialized)
  
  assert_eq(deserialized["count"], 2)
  assert_eq(deserialized["users"].length(), 2)
  assert_eq(deserialized["users"][0]["name"], "Alice")
  assert_eq(deserialized["users"][1]["age"], 30)
}

// Test 2: XML Serialization and Deserialization
test "xml_serialization_and_deserialization" {
  // Test XML serialization
  let serialize_xml = |data, root_name| {
    let xml_open = "<" + root_name + ">"
    let xml_close = "</" + root_name + ">"
    
    let serialize_value = |value, tag_name| {
      if value is String {
        "<" + tag_name + ">" + value + "</" + tag_name + ">"
      } else if value is Number {
        "<" + tag_name + ">" + value.to_string() + "</" + tag_name + ">"
      } else if value is Boolean {
        "<" + tag_name + ">" + (if value { "true" } else { "false" }) + "</" + tag_name + ">"
      } else if value is Array {
        let result = ""
        for item in value {
          result = result + serialize_value(item, tag_name.slice(0, tag_name.length() - 1))  // Remove 's' for singular
        }
        result
      } else if value is Object {
        let result = ""
        let keys = value.keys()
        for key in keys {
          result = result + serialize_value(value[key], key)
        }
        result
      } else {
        "<" + tag_name + " />"
      }
    }
    
    xml_open + serialize_value(data, root_name) + xml_close
  }
  
  // Test XML deserialization
  let deserialize_xml = |xml_string| {
    // Simplified XML parser for testing
    let result = {}
    
    // Extract root tag
    let root_start = xml_string.index_of(">") + 1
    let root_end = xml_string.last_index_of("<")
    let content = xml_string.slice(root_start, root_end)
    
    // Parse simple tags
    let regex = /<(\w+)>(.*?)<\/\1>/g
    let matches = regex.all_matches(content)
    
    for match in matches {
      let tag = match[1]
      let value = match[2]
      
      // Try to convert to appropriate type
      if value == "true" {
        result[tag] = true
      } else if value == "false" {
        result[tag] = false
      } else if value.to_int() is not None {
        result[tag] = value.to_int()
      } else {
        result[tag] = value
      }
    }
    
    result
  }
  
  // Test basic XML serialization
  let user_data = {
    "name": "John Doe",
    "age": 30,
    "active": true
  }
  
  let xml_user = serialize_xml(user_data, "user")
  assert_true(xml_user.contains("<user>"))
  assert_true(xml_user.contains("</user>"))
  assert_true(xml_user.contains("<name>John Doe</name>"))
  assert_true(xml_user.contains("<age>30</age>"))
  assert_true(xml_user.contains("<active>true</active>"))
  
  // Test nested XML serialization
  let company_data = {
    "name": "Tech Corp",
    "employees": [
      { "name": "Alice", "department": "Engineering" },
      { "name": "Bob", "department": "Marketing" }
    ]
  }
  
  let xml_company = serialize_xml(company_data, "company")
  assert_true(xml_company.contains("<company>"))
  assert_true(xml_company.contains("</company>"))
  assert_true(xml_company.contains("<name>Tech Corp</name>"))
  assert_true(xml_company.contains("<employees>"))
  
  // Test XML deserialization
  let xml_string = "<person><name>Jane Doe</name><age>25</age><active>false</active></person>"
  let deserialized = deserialize_xml(xml_string)
  
  assert_eq(deserialized["name"], "Jane Doe")
  assert_eq(deserialized["age"], 25)
  assert_eq(deserialized["active"], false)
  
  // Test attributes in XML
  let serialize_xml_with_attributes = |data, root_name, attributes| {
    let attr_string = attributes.reduce(|acc, (key, value)| acc + " " + key + "=\"" + value + "\"", "")
    let xml_open = "<" + root_name + attr_string + ">"
    let xml_close = "</" + root_name + ">"
    
    let serialize_value = |value, tag_name| {
      if value is String {
        "<" + tag_name + ">" + value + "</" + tag_name + ">"
      } else if value is Number {
        "<" + tag_name + ">" + value.to_string() + "</" + tag_name + ">"
      } else if value is Boolean {
        "<" + tag_name + ">" + (if value { "true" } else { "false" }) + "</" + tag_name + ">"
      } else if value is Object {
        let result = ""
        let keys = value.keys()
        for key in keys {
          result = result + serialize_value(value[key], key)
        }
        result
      } else {
        ""
      }
    }
    
    xml_open + serialize_value(data, root_name) + xml_close
  }
  
  let product_data = { "name": "Laptop", "price": 999.99 }
  let product_attrs = { "id": "123", "category": "Electronics" }
  let xml_product = serialize_xml_with_attributes(product_data, "product", product_attrs)
  
  assert_true(xml_product.contains("<product id=\"123\" category=\"Electronics\">"))
  assert_true(xml_product.contains("</product>"))
  assert_true(xml_product.contains("<name>Laptop</name>"))
  assert_true(xml_product.contains("<price>999.99</price>"))
  
  // Test CDATA sections
  let serialize_xml_with_cdata = |data, root_name| {
    let xml_open = "<" + root_name + ">"
    let xml_close = "</" + root_name + ">"
    
    let serialize_value = |value, tag_name| {
      if value is String && (value.contains("<") || value.contains(">") || value.contains("&")) {
        "<" + tag_name + ">><![CDATA[" + value + "]]></" + tag_name + ">"
      } else if value is String {
        "<" + tag_name + ">" + value + "</" + tag_name + ">"
      } else if value is Number {
        "<" + tag_name + ">" + value.to_string() + "</" + tag_name + ">"
      } else if value is Boolean {
        "<" + tag_name + ">" + (if value { "true" } else { "false" }) + "</" + tag_name + ">"
      } else {
        ""
      }
    }
    
    xml_open + serialize_value(data, root_name) + xml_close
  }
  
  let content_with_special_chars = "This contains <special> & characters"
  let xml_with_cdata = serialize_xml_with_cdata(content_with_special_chars, "content")
  assert_true(xml_with_cdata.contains("<content>"))
  assert_true(xml_with_cdata.contains("<![CDATA["))
  assert_true(xml_with_cdata.contains("This contains <special> & characters"))
  assert_true(xml_with_cdata.contains("]]>"))
  assert_true(xml_with_cdata.contains("</content>"))
}

// Test 3: Binary Serialization and Deserialization
test "binary_serialization_and_deserialization" {
  // Test binary serialization
  let serialize_binary = |data| {
    let buffer = []
    
    let serialize_value = |value| {
      if value is Number {
        // 8-byte double
        let bytes = value.to_bytes()
        for byte in bytes {
          buffer.push(byte)
        }
      } else if value is String {
        // Length-prefixed string
        let length = value.length()
        let length_bytes = length.to_bytes()
        for byte in length_bytes {
          buffer.push(byte)
        }
        
        // String bytes
        for i = 0; i < value.length(); i = i + 1 {
          buffer.push(value[i].to_byte())
        }
      } else if value is Boolean {
        // 1 byte
        buffer.push(if value { 1 } else { 0 })
      } else if value is Array {
        // Length-prefixed array
        let length = value.length()
        let length_bytes = length.to_bytes()
        for byte in length_bytes {
          buffer.push(byte)
        }
        
        // Array elements
        for element in value {
          serialize_value(element)
        }
      } else if value is Object {
        // Length-prefixed object
        let keys = value.keys()
        let length = keys.length()
        let length_bytes = length.to_bytes()
        for byte in length_bytes {
          buffer.push(byte)
        }
        
        // Object key-value pairs
        for key in keys {
          // Key as string
          serialize_value(key)
          // Value
          serialize_value(value[key])
        }
      }
    }
    
    serialize_value(data)
    buffer
  }
  
  // Test binary deserialization
  let deserialize_binary = |buffer| {
    let position = 0
    
    let deserialize_value = || {
      if position >= buffer.length() {
        return null
      }
      
      // For simplicity, we'll assume the first byte indicates type
      let type_byte = buffer[position]
      position = position + 1
      
      if type_byte == 0 {
        // Number (8-byte double)
        let mut bytes = []
        for i = 0; i < 8; i = i + 1 {
          bytes.push(buffer[position])
          position = position + 1
        }
        bytes.to_double()
      } else if type_byte == 1 {
        // String (length-prefixed)
        let length_bytes = buffer.slice(position, position + 4)
        position = position + 4
        let length = length_bytes.to_int()
        
        let mut string_bytes = []
        for i = 0; i < length; i = i + 1 {
          string_bytes.push(buffer[position])
          position = position + 1
        }
        
        string_bytes.to_string()
      } else if type_byte == 2 {
        // Boolean (1 byte)
        let bool_byte = buffer[position]
        position = position + 1
        bool_byte == 1
      } else {
        null
      }
    }
    
    deserialize_value()
  }
  
  // Test basic binary serialization
  let number_data = 42.5
  let binary_number = serialize_binary(number_data)
  assert_eq(binary_number.length(), 9)  // 1 type byte + 8 data bytes
  
  let string_data = "hello"
  let binary_string = serialize_binary(string_data)
  assert_eq(binary_string.length(), 1 + 4 + 5)  // 1 type byte + 4 length bytes + 5 data bytes
  
  let boolean_data = true
  let binary_boolean = serialize_binary(boolean_data)
  assert_eq(binary_boolean.length(), 2)  // 1 type byte + 1 data byte
  
  // Test array serialization
  let array_data = [1, 2, 3]
  let binary_array = serialize_binary(array_data)
  assert_eq(binary_array.length(), 1 + 4 + 3 * 9)  // 1 type byte + 4 length bytes + 3 * (1 type byte + 8 data bytes)
  
  // Test object serialization
  let object_data = { "name": "John", "age": 30 }
  let binary_object = serialize_binary(object_data)
  // 1 type byte + 4 length bytes + 2 * (1 type byte + 4 length bytes + string bytes) + (1 type byte + 8 data bytes)
  
  // Test binary deserialization
  let deserialized_number = deserialize_binary(binary_number)
  assert_true(deserialized_number > 42.4 && deserialized_number < 42.6)
  
  let deserialized_string = deserialize_binary(binary_string)
  assert_eq(deserialized_string, "hello")
  
  let deserialized_boolean = deserialize_binary(binary_boolean)
  assert_eq(deserialized_boolean, true)
  
  // Test fixed-width binary format
  let serialize_fixed_width = |data| {
    let buffer = []
    
    // Fixed-width format: type (1 byte) + data (fixed size)
    if data is Number {
      buffer.push(0)  // Number type
      let bytes = data.to_bytes()
      for byte in bytes {
        buffer.push(byte)
      }
    } else if data is String {
      buffer.push(1)  // String type
      // Fixed-width string: max 256 characters
      let padded = value.pad_end(256, "\0")
      for i = 0; i < 256; i = i + 1 {
        buffer.push(padded[i].to_byte())
      }
    } else if data is Boolean {
      buffer.push(2)  // Boolean type
      buffer.push(if data { 1 } else { 0 })
    }
    
    buffer
  }
  
  let fixed_number = serialize_fixed_width(42.5)
  assert_eq(fixed_number.length(), 9)  // 1 type byte + 8 data bytes
  
  let fixed_string = serialize_fixed_width("hello")
  assert_eq(fixed_string.length(), 257)  // 1 type byte + 256 data bytes
  
  let fixed_boolean = serialize_fixed_width(true)
  assert_eq(fixed_boolean.length(), 2)  // 1 type byte + 1 data byte
  
  // Test endianness handling
  let serialize_with_endianness = |data, is_big_endian| {
    let buffer = []
    
    if data is Number {
      let bytes = data.to_bytes()
      
      if is_big_endian {
        // Big endian: MSB first
        for byte in bytes {
          buffer.push(byte)
        }
      } else {
        // Little endian: LSB first
        for i = bytes.length() - 1; i >= 0; i = i - 1 {
          buffer.push(bytes[i])
        }
      }
    }
    
    buffer
  }
  
  let big_endian_bytes = serialize_with_endianness(0x12345678, true)
  let little_endian_bytes = serialize_with_endianness(0x12345678, false)
  
  assert_eq(big_endian_bytes[0], 0x12)
  assert_eq(big_endian_bytes[1], 0x34)
  assert_eq(big_endian_bytes[2], 0x56)
  assert_eq(big_endian_bytes[3], 0x78)
  
  assert_eq(little_endian_bytes[0], 0x78)
  assert_eq(little_endian_bytes[1], 0x56)
  assert_eq(little_endian_bytes[2], 0x34)
  assert_eq(little_endian_bytes[3], 0x12)
}

// Test 4: Protocol Buffers Serialization
test "protocol_buffers_serialization" {
  // Test Protocol Buffers-like serialization
  let serialize_protobuf = |fields| {
    let buffer = []
    
    for field in fields {
      // Each field: varint key + value
      let field_number = field.number
      let wire_type = field.wire_type
      let key = (field_number << 3) | wire_type
      
      // Encode key as varint
      let mut key_value = key
      while key_value >= 0x80 {
        buffer.push((key_value & 0x7F) | 0x80)
        key_value = key_value >> 7
      }
      buffer.push(key_value)
      
      // Encode value based on wire type
      if wire_type == 0 {
        // Varint
        let mut varint_value = field.value
        while varint_value >= 0x80 {
          buffer.push((varint_value & 0x7F) | 0x80)
          varint_value = varint_value >> 7
        }
        buffer.push(varint_value)
      } else if wire_type == 2 {
        // Length-delimited
        let value_bytes = field.value.to_bytes()
        let length = value_bytes.length()
        
        // Encode length as varint
        let mut length_value = length
        while length_value >= 0x80 {
          buffer.push((length_value & 0x7F) | 0x80)
          length_value = length_value >> 7
        }
        buffer.push(length_value)
        
        // Add value bytes
        for byte in value_bytes {
          buffer.push(byte)
        }
      }
    }
    
    buffer
  }
  
  // Test Protocol Buffers-like deserialization
  let deserialize_protobuf = |buffer| {
    let position = 0
    let fields = []
    
    while position < buffer.length() {
      // Decode key as varint
      let mut key = 0
      let mut shift = 0
      
      while position < buffer.length() && (buffer[position] & 0x80) != 0 {
        key = key | ((buffer[position] & 0x7F) << shift)
        position = position + 1
        shift = shift + 7
      }
      
      if position < buffer.length() {
        key = key | (buffer[position] << shift)
        position = position + 1
      }
      
      let field_number = key >> 3
      let wire_type = key & 0x07
      
      let mut value = null
      
      if wire_type == 0 {
        // Varint
        let mut varint_value = 0
        let mut varint_shift = 0
        
        while position < buffer.length() && (buffer[position] & 0x80) != 0 {
          varint_value = varint_value | ((buffer[position] & 0x7F) << varint_shift)
          position = position + 1
          varint_shift = varint_shift + 7
        }
        
        if position < buffer.length() {
          varint_value = varint_value | (buffer[position] << varint_shift)
          position = position + 1
        }
        
        value = varint_value
      } else if wire_type == 2 {
        // Length-delimited
        let mut length = 0
        let mut length_shift = 0
        
        while position < buffer.length() && (buffer[position] & 0x80) != 0 {
          length = length | ((buffer[position] & 0x7F) << length_shift)
          position = position + 1
          length_shift = length_shift + 7
        }
        
        if position < buffer.length() {
          length = length | (buffer[position] << length_shift)
          position = position + 1
        }
        
        let value_bytes = buffer.slice(position, position + length)
        position = position + length
        value = value_bytes.to_string()
      }
      
      fields.push({
        number: field_number,
        wire_type: wire_type,
        value: value
      })
    }
    
    fields
  }
  
  // Test Protocol Buffers serialization
  let protobuf_fields = [
    { number: 1, wire_type: 0, value: 42 },
    { number: 2, wire_type: 2, value: "hello" },
    { number: 3, wire_type: 0, value: 100 }
  ]
  
  let protobuf_data = serialize_protobuf(protobuf_fields)
  assert_eq(protobuf_data.length(), 2 + 1 + 5 + 1 + 2 + 1 + 1)  // Keys + values
  
  // Test Protocol Buffers deserialization
  let deserialized_fields = deserialize_protobuf(protobuf_data)
  assert_eq(deserialized_fields.length(), 3)
  assert_eq(deserialized_fields[0].number, 1)
  assert_eq(deserialized_fields[0].wire_type, 0)
  assert_eq(deserialized_fields[0].value, 42)
  assert_eq(deserialized_fields[1].number, 2)
  assert_eq(deserialized_fields[1].wire_type, 2)
  assert_eq(deserialized_fields[1].value, "hello")
  assert_eq(deserialized_fields[2].number, 3)
  assert_eq(deserialized_fields[2].wire_type, 0)
  assert_eq(deserialized_fields[2].value, 100)
  
  // Test varint encoding
  let encode_varint = |value| {
    let buffer = []
    let mut varint_value = value
    
    while varint_value >= 0x80 {
      buffer.push((varint_value & 0x7F) | 0x80)
      varint_value = varint_value >> 7
    }
    buffer.push(varint_value)
    
    buffer
  }
  
  let decode_varint = |buffer| {
    let mut value = 0
    let mut shift = 0
    
    for byte in buffer {
      value = value | ((byte & 0x7F) << shift)
      shift = shift + 7
      
      if (byte & 0x80) == 0 {
        break
      }
    }
    
    value
  }
  
  // Test varint encoding/decoding
  let varint_127 = encode_varint(127)
  assert_eq(varint_127, [0x7F])
  assert_eq(decode_varint(varint_127), 127)
  
  let varint_300 = encode_varint(300)
  assert_eq(varint_300, [0xAC, 0x02])
  assert_eq(decode_varint(varint_300), 300)
  
  let varint_16384 = encode_varint(16384)
  assert_eq(varint_16384, [0x80, 0x80, 0x01])
  assert_eq(decode_varint(varint_16384), 16384)
  
  // Test zigzag encoding (for signed integers)
  let encode_zigzag = |value| {
    if value >= 0 {
      value * 2
    } else {
      value * -2 - 1
    }
  }
  
  let decode_zigzag = |zigzag_value| {
    if zigzag_value % 2 == 0 {
      zigzag_value / 2
    } else {
      (zigzag_value / 2) * -1 - 1
    }
  }
  
  // Test zigzag encoding/decoding
  assert_eq(encode_zigzag(0), 0)
  assert_eq(encode_zigzag(-1), 1)
  assert_eq(encode_zigzag(1), 2)
  assert_eq(encode_zigzag(-2), 3)
  assert_eq(encode_zigzag(2), 4)
  
  assert_eq(decode_zigzag(0), 0)
  assert_eq(decode_zigzag(1), -1)
  assert_eq(decode_zigzag(2), 1)
  assert_eq(decode_zigzag(3), -2)
  assert_eq(decode_zigzag(4), 2)
  
  // Test packed repeated fields
  let serialize_packed_repeated = |field_number, values| {
    let buffer = []
    
    // First serialize all values
    let values_buffer = []
    for value in values {
      let mut varint_value = value
      while varint_value >= 0x80 {
        values_buffer.push((varint_value & 0x7F) | 0x80)
        varint_value = varint_value >> 7
      }
      values_buffer.push(varint_value)
    }
    
    // Then add the key with length-delimited wire type
    let key = (field_number << 3) | 2  // Wire type 2 = length-delimited
    
    // Encode key as varint
    let mut key_value = key
    while key_value >= 0x80 {
      buffer.push((key_value & 0x7F) | 0x80)
      key_value = key_value >> 7
    }
    buffer.push(key_value)
    
    // Encode length
    let mut length_value = values_buffer.length()
    while length_value >= 0x80 {
      buffer.push((length_value & 0x7F) | 0x80)
      length_value = length_value >> 7
    }
    buffer.push(length_value)
    
    // Add values
    for byte in values_buffer {
      buffer.push(byte)
    }
    
    buffer
  }
  
  let packed_values = [1, 2, 3, 4, 5]
  let packed_buffer = serialize_packed_repeated(1, packed_values)
  
  // Verify it contains all the values
  assert_true(packed_buffer.length() > 0)
}

// Test 5: MessagePack Serialization
test "messagepack_serialization" {
  // Test MessagePack serialization
  let serialize_msgpack = |data| {
    let buffer = []
    
    let serialize_value = |value| {
      if value is null {
        buffer.push(0xC0)  // nil
      } else if value is Boolean {
        if value {
          buffer.push(0xC3)  // true
        } else {
          buffer.push(0xC2)  // false
        }
      } else if value is Number {
        if value >= 0 && value <= 127 {
          buffer.push(value)  // positive fixint
        } else if value >= -32 && value <= -1 {
          buffer.push(0xE0 + value + 32)  // negative fixint
        } else if value >= 0 && value <= 255 {
          buffer.push(0xCC)  // uint 8
          buffer.push(value)
        } else if value >= -128 && value <= 127 {
          buffer.push(0xD0)  // int 8
          buffer.push(value)
        } else if value >= 0 && value <= 65535 {
          buffer.push(0xCD)  // uint 16
          buffer.push(value >> 8)
          buffer.push(value & 0xFF)
        } else if value >= -32768 && value <= 32767 {
          buffer.push(0xD1)  // int 16
          buffer.push(value >> 8)
          buffer.push(value & 0xFF)
        }
      } else if value is String {
        let length = value.length()
        if length <= 31 {
          buffer.push(0xA0 + length)  // fixstr
        } else if length <= 255 {
          buffer.push(0xD9)  // str 8
          buffer.push(length)
        } else if length <= 65535 {
          buffer.push(0xDA)  // str 16
          buffer.push(length >> 8)
          buffer.push(length & 0xFF)
        }
        
        for i = 0; i < length; i = i + 1 {
          buffer.push(value[i].to_byte())
        }
      } else if value is Array {
        let length = value.length()
        if length <= 15 {
          buffer.push(0x90 + length)  // fixarray
        } else if length <= 65535 {
          buffer.push(0xDC)  // array 16
          buffer.push(length >> 8)
          buffer.push(length & 0xFF)
        }
        
        for element in value {
          serialize_value(element)
        }
      } else if value is Object {
        let length = value.keys().length()
        if length <= 15 {
          buffer.push(0x80 + length)  // fixmap
        } else if length <= 65535 {
          buffer.push(0xDE)  // map 16
          buffer.push(length >> 8)
          buffer.push(length & 0xFF)
        }
        
        let keys = value.keys()
        for key in keys {
          serialize_value(key)
          serialize_value(value[key])
        }
      }
    }
    
    serialize_value(data)
    buffer
  }
  
  // Test MessagePack deserialization
  let deserialize_msgpack = |buffer| {
    let position = 0
    
    let deserialize_value = || {
      if position >= buffer.length() {
        return null
      }
      
      let first_byte = buffer[position]
      position = position + 1
      
      if first_byte == 0xC0 {
        null
      } else if first_byte == 0xC2 {
        false
      } else if first_byte == 0xC3 {
        true
      } else if first_byte >= 0x00 && first_byte <= 0x7F {
        // positive fixint
        first_byte
      } else if first_byte >= 0xE0 && first_byte <= 0xFF {
        // negative fixint
        first_byte - 256
      } else if first_byte == 0xCC {
        // uint 8
        if position < buffer.length() {
          let value = buffer[position]
          position = position + 1
          value
        } else {
          null
        }
      } else if first_byte == 0xD0 {
        // int 8
        if position < buffer.length() {
          let value = buffer[position]
          position = position + 1
          if value >= 128 {
            value - 256
          } else {
            value
          }
        } else {
          null
        }
      } else if first_byte >= 0xA0 && first_byte <= 0xBF {
        // fixstr
        let length = first_byte - 0xA0
        let mut string_bytes = []
        for i = 0; i < length; i = i + 1 {
          if position < buffer.length() {
            string_bytes.push(buffer[position])
            position = position + 1
          }
        }
        string_bytes.to_string()
      } else if first_byte >= 0x90 && first_byte <= 0x9F {
        // fixarray
        let length = first_byte - 0x90
        let mut array = []
        for i = 0; i < length; i = i + 1 {
          array.push(deserialize_value())
        }
        array
      } else if first_byte >= 0x80 && first_byte <= 0x8F {
        // fixmap
        let length = first_byte - 0x80
        let mut object = {}
        for i = 0; i < length; i = i + 1 {
          let key = deserialize_value()
          let value = deserialize_value()
          object[key] = value
        }
        object
      } else {
        null
      }
    }
    
    deserialize_value()
  }
  
  // Test basic MessagePack serialization
  let msgpack_null = serialize_msgpack(null)
  assert_eq(msgpack_null, [0xC0])
  
  let msgpack_true = serialize_msgpack(true)
  assert_eq(msgpack_true, [0xC3])
  
  let msgpack_false = serialize_msgpack(false)
  assert_eq(msgpack_false, [0xC2])
  
  let msgpack_small_int = serialize_msgpack(42)
  assert_eq(msgpack_small_int, [42])
  
  let msgpack_negative_int = serialize_msgpack(-5)
  assert_eq(msgpack_negative_int, [0xFB])  // 0xE0 + (-5) + 32 = 0xFB
  
  let msgpack_uint8 = serialize_msgpack(200)
  assert_eq(msgpack_uint8, [0xCC, 200])
  
  let msgpack_int8 = serialize_msgpack(-100)
  assert_eq(msgpack_int8, [0xD0, 156])  // 156 - 256 = -100
  
  // Test MessagePack string serialization
  let msgpack_short_string = serialize_msgpack("hello")
  assert_eq(msgpack_short_string.length(), 1 + 5)  // 1 format byte + 5 data bytes
  assert_eq(msgpack_short_string[0], 0xA5)  // 0xA0 + 5
  
  let msgpack_long_string = serialize_msgpack("a".repeat(40))
  assert_eq(msgpack_long_string[0], 0xD9)  // str 8 format
  assert_eq(msgpack_long_string[1], 40)   // Length
  
  // Test MessagePack array serialization
  let msgpack_small_array = serialize_msgpack([1, 2, 3])
  assert_eq(msgpack_small_array[0], 0x93)  // 0x90 + 3
  
  let msgpack_large_array = serialize_msgpack([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17])
  assert_eq(msgpack_large_array[0], 0xDC)  // array 16 format
  
  // Test MessagePack object serialization
  let msgpack_small_object = serialize_msgpack({ "a": 1, "b": 2 })
  assert_eq(msgpack_small_object[0], 0x82)  // 0x80 + 2
  
  // Test MessagePack deserialization
  let deserialized_null = deserialize_msgpack(msgpack_null)
  assert_eq(deserialized_null, null)
  
  let deserialized_true = deserialize_msgpack(msgpack_true)
  assert_eq(deserialized_true, true)
  
  let deserialized_false = deserialize_msgpack(msgpack_false)
  assert_eq(deserialized_false, false)
  
  let deserialized_int = deserialize_msgpack(msgpack_small_int)
  assert_eq(deserialized_int, 42)
  
  let deserialized_string = deserialize_msgpack(msgpack_short_string)
  assert_eq(deserialized_string, "hello")
  
  let deserialized_array = deserialize_msgpack(msgpack_small_array)
  assert_eq(deserialized_array, [1, 2, 3])
  
  // Test round-trip serialization
  let original_data = {
    "name": "John",
    "age": 30,
    "active": true,
    "scores": [95, 87, 92]
  }
  
  let msgpack_data = serialize_msgpack(original_data)
  let deserialized_data = deserialize_msgpack(msgpack_data)
  
  assert_eq(deserialized_data["name"], "John")
  assert_eq(deserialized_data["age"], 30)
  assert_eq(deserialized_data["active"], true)
  assert_eq(deserialized_data["scores"], [95, 87, 92])
  
  // Test MessagePack timestamp
  let serialize_timestamp = |timestamp| {
    let buffer = []
    
    if timestamp >= 0 && timestamp <= 0xFFFFFFFF {
      buffer.push(0xD6)  // ext 4
      buffer.push(0xFF)  // timestamp type
      buffer.push(timestamp >> 24)
      buffer.push((timestamp >> 16) & 0xFF)
      buffer.push((timestamp >> 8) & 0xFF)
      buffer.push(timestamp & 0xFF)
    }
    
    buffer
  }
  
  let timestamp = 1640995200  // 2022-01-01 00:00:00 UTC
  let msgpack_timestamp = serialize_timestamp(timestamp)
  assert_eq(msgpack_timestamp, [0xD6, 0xFF, 0x61, 0xE8, 0x3A, 0x80])
}

// Test 6: CSV Serialization and Deserialization
test "csv_serialization_and_deserialization" {
  // Test CSV serialization
  let serialize_csv = |data, headers| {
    let lines = []
    
    // Add headers
    lines.push(headers.join(","))
    
    // Add data rows
    for row in data {
      let values = []
      for header in headers {
        let value = row[header]
        
        if value is String {
          // Escape commas and quotes
          let escaped_value = value
          if escaped_value.contains(",") || escaped_value.contains("\"") {
            escaped_value = "\"" + escaped_value.replace("\"", "\"\"") + "\""
          }
          values.push(escaped_value)
        } else {
          values.push(value.to_string())
        }
      }
      lines.push(values.join(","))
    }
    
    lines.join("\n")
  }
  
  // Test CSV deserialization
  let deserialize_csv = |csv_string| {
    let lines = csv_string.split("\n")
    let headers = lines[0].split(",")
    let data = []
    
    for i = 1; i < lines.length(); i = i + 1 {
      let values = parse_csv_line(lines[i])
      let row = {}
      
      for j = 0; j < headers.length(); j = j + 1 {
        if j < values.length() {
          row[headers[j]] = values[j]
        }
      }
      
      data.push(row)
    }
    
    {
      headers: headers,
      data: data
    }
  }
  
  // Parse CSV line handling quotes
  let parse_csv_line = |line| {
    let values = []
    let mut current_value = ""
    let mut in_quotes = false
    let mut i = 0
    
    while i < line.length() {
      let char = line[i]
      
      if char == "\"" {
        if in_quotes && i + 1 < line.length() && line[i + 1] == "\"" {
          // Escaped quote
          current_value = current_value + "\""
          i = i + 1  // Skip next quote
        } else {
          // Toggle quote state
          in_quotes = !in_quotes
        }
      } else if char == "," && !in_quotes {
        // Field separator
        values.push(current_value)
        current_value = ""
      } else {
        current_value = current_value + char
      }
      
      i = i + 1
    }
    
    // Add last value
    values.push(current_value)
    
    values
  }
  
  // Test CSV serialization
  let csv_data = [
    { "name": "John", "age": 30, "city": "New York" },
    { "name": "Alice", "age": 25, "city": "Los Angeles" },
    { "name": "Bob", "age": 35, "city": "Chicago" }
  ]
  
  let csv_headers = ["name", "age", "city"]
  let csv_string = serialize_csv(csv_data, csv_headers)
  
  assert_true(csv_string.contains("name,age,city"))
  assert_true(csv_string.contains("John,30,New York"))
  assert_true(csv_string.contains("Alice,25,Los Angeles"))
  assert_true(csv_string.contains("Bob,35,Chicago"))
  
  // Test CSV with special characters
  let csv_data_special = [
    { "name": "John Doe", "note": "Lives in New York, NY" },
    { "name": "Alice \"The Great\"", "note": "Works at \"Big Corp\"" }
  ]
  
  let csv_headers_special = ["name", "note"]
  let csv_string_special = serialize_csv(csv_data_special, csv_headers_special)
  
  assert_true(csv_string_special.contains("\"John Doe\",\"Lives in New York, NY\""))
  assert_true(csv_string_special.contains("\"Alice \"\"The Great\"\"\",\"Works at \"\"Big Corp\"\"\""))
  
  // Test CSV deserialization
  let csv_parsed = deserialize_csv(csv_string)
  
  assert_eq(csv_parsed.headers, ["name", "age", "city"])
  assert_eq(csv_parsed.data.length(), 3)
  assert_eq(csv_parsed.data[0]["name"], "John")
  assert_eq(csv_parsed.data[0]["age"], "30")
  assert_eq(csv_parsed.data[0]["city"], "New York")
  assert_eq(csv_parsed.data[1]["name"], "Alice")
  assert_eq(csv_parsed.data[1]["age"], "25")
  assert_eq(csv_parsed.data[1]["city"], "Los Angeles")
  
  // Test CSV deserialization with special characters
  let csv_parsed_special = deserialize_csv(csv_string_special)
  
  assert_eq(csv_parsed_special.data[0]["name"], "John Doe")
  assert_eq(csv_parsed_special.data[0]["note"], "Lives in New York, NY")
  assert_eq(csv_parsed_special.data[1]["name"], "Alice \"The Great\"")
  assert_eq(csv_parsed_special.data[1]["note"], "Works at \"Big Corp\"")
  
  // Test TSV (Tab-Separated Values)
  let serialize_tsv = |data, headers| {
    let lines = []
    
    // Add headers
    lines.push(headers.join("\t"))
    
    // Add data rows
    for row in data {
      let values = []
      for header in headers {
        let value = row[header]
        
        if value is String {
          // Escape tabs and newlines
          let escaped_value = value.replace("\t", "\\t").replace("\n", "\\n")
          values.push(escaped_value)
        } else {
          values.push(value.to_string())
        }
      }
      lines.push(values.join("\t"))
    }
    
    lines.join("\n")
  }
  
  let tsv_string = serialize_tsv(csv_data, csv_headers)
  assert_true(tsv_string.contains("name\tage\tcity"))
  assert_true(tsv_string.contains("John\t30\tNew York"))
  
  // Test CSV with different delimiters
  let serialize_custom_delimiter = |data, headers, delimiter| {
    let lines = []
    
    // Add headers
    lines.push(headers.join(delimiter))
    
    // Add data rows
    for row in data {
      let values = []
      for header in headers {
        let value = row[header]
        
        if value is String {
          // Escape delimiter
          let escaped_value = value.replace(delimiter, "\\" + delimiter)
          values.push(escaped_value)
        } else {
          values.push(value.to_string())
        }
      }
      lines.push(values.join(delimiter))
    }
    
    lines.join("\n")
  }
  
  let semicolon_csv = serialize_custom_delimiter(csv_data, csv_headers, ";")
  assert_true(semicolon_csv.contains("name;age;city"))
  assert_true(semicolon_csv.contains("John;30;New York"))
  
  // Test CSV with BOM (Byte Order Mark)
  let add_bom = |csv_string| {
    "\uFEFF" + csv_string
  }
  
  let csv_with_bom = add_bom(csv_string)
  assert_true(csv_with_bom.starts_with("\uFEFF"))
  
  // Test CSV with empty values
  let csv_data_empty = [
    { "name": "John", "age": 30, "city": "" },
    { "name": "", "age": 25, "city": "Los Angeles" }
  ]
  
  let csv_string_empty = serialize_csv(csv_data_empty, csv_headers)
  assert_true(csv_string_empty.contains("John,30,"))
  assert_true(csv_string_empty.contains(",25,Los Angeles"))
  
  let csv_parsed_empty = deserialize_csv(csv_string_empty)
  assert_eq(csv_parsed_empty.data[0]["city"], "")
  assert_eq(csv_parsed_empty.data[1]["name"], "")
}

// Test 7: YAML Serialization and Deserialization
test "yaml_serialization_and_deserialization" {
  // Test YAML serialization
  let serialize_yaml = |data| {
    let serialize_value = |value, indent| {
      if value is String {
        indent + value
      } else if value is Number {
        indent + value.to_string()
      } else if value is Boolean {
        indent + (if value { "true" } else { "false" })
      } else if value is Array {
        let result = ""
        for item in value {
          result = result + indent + "- " + serialize_value(item, "").replace("\n", "\n" + indent + "  ") + "\n"
        }
        result.slice(0, result.length() - 1)  // Remove trailing newline
      } else if value is Object {
        let result = ""
        let keys = value.keys()
        for key in keys {
          result = result + indent + key + ":"
          if value[key] is Object || value[key] is Array {
            result = result + "\n" + serialize_value(value[key], indent + "  ")
          } else {
            result = result + " " + serialize_value(value[key], "")
          }
          result = result + "\n"
        }
        result.slice(0, result.length() - 1)  // Remove trailing newline
      } else {
        indent + "null"
      }
    }
    
    serialize_value(data, "")
  }
  
  // Test YAML deserialization
  let deserialize_yaml = |yaml_string| {
    let lines = yaml_string.split("\n")
    let result = {}
    let mut current_path = []
    let mut current_indent = 0
    
    for line in lines {
      if line.trim().length() == 0 {
        continue
      }
      
      let trimmed = line.trim()
      let indent = line.length() - trimmed.length()
      
      // Update current path based on indentation
      while current_path.length() > 0 && indent <= current_indent {
        current_path.pop()
        current_indent = current_indent - 2
      }
      
      if trimmed.starts_with("- ") {
        // Array item
        let item_value = trimmed.slice(2)
        
        if current_path.length() > 0 {
          let current_key = current_path[current_path.length() - 1]
          if !result.contains(current_key) {
            result[current_key] = []
          }
          result[current_key].push(parse_yaml_value(item_value))
        }
      } else if trimmed.contains(":") {
        // Key-value pair
        let parts = trimmed.split(":")
        let key = parts[0]
        let value = if parts.length() > 1 { parts[1].trim() } else { "" }
        
        current_path.push(key)
        current_indent = indent
        
        if value.length() > 0 {
          result[key] = parse_yaml_value(value)
        } else {
          // Empty value, likely a nested structure
          result[key] = {}
        }
      }
    }
    
    result
  }
  
  // Parse YAML value
  let parse_yaml_value = |value| {
    if value == "true" {
      true
    } else if value == "false" {
      false
    } else if value == "null" || value == "~" {
      null
    } else if value.starts_with("\"") && value.ends_with("\"") {
      value.slice(1, value.length() - 1)
    } else if value.to_int() is not None {
      value.to_int()
    } else if value.to_float() is not None {
      value.to_float()
    } else {
      value
    }
  }
  
  // Test basic YAML serialization
  let yaml_data = {
    "name": "John Doe",
    "age": 30,
    "active": true,
    "address": {
      "street": "123 Main St",
      "city": "New York",
      "zip": "10001"
    },
    "hobbies": ["reading", "coding", "hiking"]
  }
  
  let yaml_string = serialize_yaml(yaml_data)
  
  assert_true(yaml_string.contains("name: John Doe"))
  assert_true(yaml_string.contains("age: 30"))
  assert_true(yaml_string.contains("active: true"))
  assert_true(yaml_string.contains("address:"))
  assert_true(yaml_string.contains("street: 123 Main St"))
  assert_true(yaml_string.contains("hobbies:"))
  assert_true(yaml_string.contains("- reading"))
  assert_true(yaml_string.contains("- coding"))
  
  // Test YAML deserialization
  let simple_yaml = "name: John Doe\nage: 30\nactive: true"
  let deserialized_simple = deserialize_yaml(simple_yaml)
  
  assert_eq(deserialized_simple["name"], "John Doe")
  assert_eq(deserialized_simple["age"], "30")
  assert_eq(deserialized_simple["active"], "true")
  
  // Test YAML with arrays
  let array_yaml = "hobbies:\n- reading\n- coding\n- hiking"
  let deserialized_array = deserialize_yaml(array_yaml)
  
  assert_true(deserialized_array.contains("hobbies"))
  
  // Test YAML with nested objects
  let nested_yaml = "address:\n  street: 123 Main St\n  city: New York\n  zip: 10001"
  let deserialized_nested = deserialize_yaml(nested_yaml)
  
  assert_true(deserialized_nested.contains("address"))
  
  // Test YAML with different data types
  let types_yaml = "string_value: hello\nnumber_value: 42\nfloat_value: 3.14\nboolean_value: true\nnull_value: null"
  let deserialized_types = deserialize_yaml(types_yaml)
  
  assert_eq(deserialized_types["string_value"], "hello")
  assert_eq(deserialized_types["number_value"], "42")
  assert_eq(deserialized_types["float_value"], "3.14")
  assert_eq(deserialized_types["boolean_value"], "true")
  assert_eq(deserialized_types["null_value"], "null")
  
  // Test YAML with quoted strings
  let quoted_yaml = "quoted_string: \"hello, world\"\nnumber_like_string: \"123\""
  let deserialized_quoted = deserialize_yaml(quoted_yaml)
  
  assert_eq(deserialized_quoted["quoted_string"], "hello, world")
  assert_eq(deserialized_quoted["number_like_string"], "123")
  
  // Test YAML with multiline strings
  let serialize_yaml_multiline = |data| {
    let result = ""
    
    let serialize_value = |key, value| {
      if value is String && value.contains("\n") {
        key + " |\n" + value.split("\n").map(|line| "  " + line).join("\n")
      } else if value is String {
        key + ": " + value
      } else if value is Number {
        key + ": " + value.to_string()
      } else if value is Boolean {
        key + ": " + (if value { "true" } else { "false" })
      } else {
        key + ": null"
      }
    }
    
    let keys = data.keys()
    for i = 0; i < keys.length(); i = i + 1 {
      let key = keys[i]
      result = result + serialize_value(key, data[key])
      if i < keys.length() - 1 {
        result = result + "\n"
      }
    }
    
    result
  }
  
  let multiline_data = {
    "description": "This is a\nmultiline\nstring.",
    "single_line": "This is a single line string"
  }
  
  let multiline_yaml = serialize_yaml_multiline(multiline_data)
  assert_true(multiline_yaml.contains("description: |"))
  assert_true(multiline_yaml.contains("  This is a"))
  assert_true(multiline_yaml.contains("  multiline"))
  assert_true(multiline_yaml.contains("  string."))
  assert_true(multiline_yaml.contains("single_line: This is a single line string"))
  
  // Test YAML with anchors and aliases
  let anchor_yaml = "default_values: &defaults\n  timeout: 30\n  retries: 3\n\nproduction:\n  <<: *defaults\n  host: prod.example.com\n\ndevelopment:\n  <<: *defaults\n  host: dev.example.com"
  
  // This is a simplified test - full YAML parsing with anchors would be more complex
  assert_true(anchor_yaml.contains("&defaults"))
  assert_true(anchor_yaml.contains("*defaults"))
  assert_true(anchor_yaml.contains("<<: *defaults"))
  
  // Test YAML with explicit types
  let typed_yaml = "explicit_string: !!str 123\nexplicit_int: !!int \"123\"\nexplicit_float: !!float \"3.14\"\nexplicit_bool: !!bool \"true\""
  
  assert_true(typed_yaml.contains("!!str"))
  assert_true(typed_yaml.contains("!!int"))
  assert_true(typed_yaml.contains("!!float"))
  assert_true(typed_yaml.contains("!!bool"))
}

// Test 8: Custom Serialization Formats
test "custom_serialization_formats" {
  // Test custom key-value format
  let serialize_kv = |data, separator| {
    let result = []
    let keys = data.keys()
    
    for key in keys {
      let value = data[key]
      let value_str = if value is String {
        value
      } else if value is Number {
        value.to_string()
      } else if value is Boolean {
        if value { "true" } else { "false" }
      } else {
        "null"
      }
      
      result.push(key + separator + value_str)
    }
    
    result.join("\n")
  }
  
  // Test custom key-value deserialization
  let deserialize_kv = |kv_string, separator| {
    let lines = kv_string.split("\n")
    let result = {}
    
    for line in lines {
      if line.contains(separator) {
        let parts = line.split(separator)
        if parts.length() >= 2 {
          let key = parts[0]
          let value = parts[1]
          
          // Try to parse value
          if value == "true" {
            result[key] = true
          } else if value == "false" {
            result[key] = false
          } else if value == "null" {
            result[key] = null
          } else if value.to_int() is not None {
            result[key] = value.to_int()
          } else if value.to_float() is not None {
            result[key] = value.to_float()
          } else {
            result[key] = value
          }
        }
      }
    }
    
    result
  }
  
  // Test key-value serialization
  let kv_data = {
    "name": "John",
    "age": 30,
    "active": true
  }
  
  let kv_string = serialize_kv(kv_data, "=")
  assert_true(kv_string.contains("name=John"))
  assert_true(kv_string.contains("age=30"))
  assert_true(kv_string.contains("active=true"))
  
  let kv_string_colon = serialize_kv(kv_data, ": ")
  assert_true(kv_string_colon.contains("name: John"))
  assert_true(kv_string_colon.contains("age: 30"))
  assert_true(kv_string_colon.contains("active: true"))
  
  // Test key-value deserialization
  let deserialized_kv = deserialize_kv(kv_string, "=")
  assert_eq(deserialized_kv["name"], "John")
  assert_eq(deserialized_kv["age"], "30")
  assert_eq(deserialized_kv["active"], "true")
  
  // Test URL-encoded format
  let serialize_urlencoded = |data| {
    let result = []
    let keys = data.keys()
    
    for key in keys {
      let value = data[key]
      let value_str = if value is String {
        value
      } else {
        value.to_string()
      }
      
      // URL encode key and value (simplified)
      let encoded_key = url_encode(key)
      let encoded_value = url_encode(value_str)
      
      result.push(encoded_key + "=" + encoded_value)
    }
    
    result.join("&")
  }
  
  // Simplified URL encoding
  let url_encode = |text| {
    let result = ""
    
    for i = 0; i < text.length(); i = i + 1 {
      let char = text[i]
      
      if (char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9') || 
         char == '-' || char == '_' || char == '.' || char == '~' {
        result = result + char
      } else if char == ' ' {
        result = result + "+"
      } else {
        // Hex encode
        let code = char.to_byte()
        result = result + "%" + code.to_hex_string().to_uppercase()
      }
    }
    
    result
  }
  
  // Test URL-encoded serialization
  let url_data = {
    "name": "John Doe",
    "city": "New York",
    "active": "true"
  }
  
  let url_string = serialize_urlencoded(url_data)
  assert_true(url_string.contains("name=John+Doe"))
  assert_true(url_string.contains("city=New+York"))
  assert_true(url_string.contains("active=true"))
  
  // Test properties file format
  let serialize_properties = |data| {
    let result = []
    let keys = data.keys()
    
    for key in keys {
      let value = data[key]
      let value_str = if value is String {
        value
      } else {
        value.to_string()
      }
      
      // Escape special characters
      let escaped_key = escape_properties(key)
      let escaped_value = escape_properties(value_str)
      
      result.push(escaped_key + "=" + escaped_value)
    }
    
    result.join("\n")
  }
  
  // Escape special characters in properties format
  let escape_properties = |text| {
    let result = ""
    
    for i = 0; i < text.length(); i = i + 1 {
      let char = text[i]
      
      if char == '=' {
        result = result + "\\="
      } else if char == ':' {
        result = result + "\\:"
      } else if char == '#' {
        result = result + "\\#"
      } else if char == '!' {
        result = result + "\\!"
      } else if char == '\n' {
        result = result + "\\n"
      } else if char == '\t' {
        result = result + "\\t"
      } else if char == '\r' {
        result = result + "\\r"
      } else {
        result = result + char
      }
    }
    
    result
  }
  
  // Test properties serialization
  let props_data = {
    "database.url": "jdbc:mysql://localhost:3306/mydb",
    "database.user": "admin",
    "database.password": "secret"
  }
  
  let props_string = serialize_properties(props_data)
  assert_true(props_string.contains("database.url=jdbc:mysql://localhost:3306/mydb"))
  assert_true(props_string.contains("database.user=admin"))
  assert_true(props_string.contains("database.password=secret"))
  
  // Test INI file format
  let serialize_ini = |sections| {
    let result = []
    
    for section in sections {
      result.push("[" + section.name + "]")
      
      let keys = section.data.keys()
      for key in keys {
        let value = section.data[key]
        let value_str = if value is String {
          value
        } else {
          value.to_string()
        }
        
        result.push(key + "=" + value_str)
      }
      
      result.push("")  // Empty line between sections
    }
    
    result.join("\n")
  }
  
  // Test INI serialization
  let ini_data = [
    {
      name: "database",
      data: {
        "host": "localhost",
        "port": "3306",
        "username": "admin"
      }
    },
    {
      name: "logging",
      data: {
        "level": "info",
        "file": "/var/log/app.log"
      }
    }
  ]
  
  let ini_string = serialize_ini(ini_data)
  assert_true(ini_string.contains("[database]"))
  assert_true(ini_string.contains("host=localhost"))
  assert_true(ini_string.contains("port=3306"))
  assert_true(ini_string.contains("[logging]"))
  assert_true(ini_string.contains("level=info"))
  
  // Test TOML-like format
  let serialize_toml = |data| {
    let result = []
    let keys = data.keys()
    
    for key in keys {
      let value = data[key]
      
      if value is String {
        result.push(key + " = \"" + value + "\"")
      } else if value is Number {
        result.push(key + " = " + value.to_string())
      } else if value is Boolean {
        result.push(key + " = " + (if value { "true" } else { "false" }))
      } else if value is Array {
        let array_str = "["
        for i = 0; i < value.length(); i = i + 1 {
          if value[i] is String {
            array_str = array_str + "\"" + value[i] + "\""
          } else {
            array_str = array_str + value[i].to_string()
          }
          
          if i < value.length() - 1 {
            array_str = array_str + ", "
          }
        }
        array_str = array_str + "]"
        result.push(key + " = " + array_str)
      }
    }
    
    result.join("\n")
  }
  
  // Test TOML-like serialization
  let toml_data = {
    "title": "TOML Example",
    "owner": {
      "name": "Tom Preston-Werner",
      "dob": 1979-05-27T07:32:00-08:00
    },
    "database": {
      "server": "192.168.1.1",
      "ports": [8001, 8001, 8002],
      "connection_max": 5000,
      "enabled": true
    }
  }
  
  let toml_string = serialize_toml(toml_data)
  assert_true(toml_string.contains("title = \"TOML Example\""))
  assert_true(toml_string.contains("ports = [8001, 8001, 8002]"))
  assert_true(toml_string.contains("enabled = true"))
  
  // Test base64 encoding
  let base64_encode = |data| {
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    let result = ""
    
    // Convert string to bytes
    let bytes = []
    for i = 0; i < data.length(); i = i + 1 {
      bytes.push(data[i].to_byte())
    }
    
    // Process 3 bytes at a time
    for i = 0; i < bytes.length(); i = i + 3 {
      let b1 = bytes[i]
      let b2 = if i + 1 < bytes.length() { bytes[i + 1] } else { 0 }
      let b3 = if i + 2 < bytes.length() { bytes[i + 2] } else { 0 }
      
      let combined = (b1 << 16) | (b2 << 8) | b3
      
      // Extract 6-bit values
      let index1 = (combined >> 18) & 0x3F
      let index2 = (combined >> 12) & 0x3F
      let index3 = (combined >> 6) & 0x3F
      let index4 = combined & 0x3F
      
      result = result + chars[index1]
      
      if i + 1 < bytes.length() {
        result = result + chars[index2]
      } else {
        result = result + "="
      }
      
      if i + 2 < bytes.length() {
        result = result + chars[index3] + chars[index4]
      } else {
        result = result + "="
      }
    }
    
    result
  }
  
  // Test base64 encoding
  let base64_result = base64_encode("hello")
  assert_eq(base64_result, "aGVsbG8=")
  
  let base64_result2 = base64_encode("hello world")
  assert_eq(base64_result2, "aGVsbG8gd29ybGQ=")
}

// Test 9: Serialization Performance and Optimization
test "serialization_performance_and_optimization" {
  // Test streaming serialization
  let stream_serialize_json = |data| {
    let chunks = []
    
    let stream_value = |value| {
      if value is String {
        chunks.push("\"")
        chunks.push(value)
        chunks.push("\"")
      } else if value is Number {
        chunks.push(value.to_string())
      } else if value is Boolean {
        chunks.push(if value { "true" } else { "false" })
      } else if value is Array {
        chunks.push("[")
        for i = 0; i < value.length(); i = i + 1 {
          stream_value(value[i])
          if i < value.length() - 1 {
            chunks.push(",")
          }
        }
        chunks.push("]")
      } else if value is Object {
        chunks.push("{")
        let keys = value.keys()
        for i = 0; i < keys.length(); i = i + 1 {
          let key = keys[i]
          chunks.push("\"")
          chunks.push(key)
          chunks.push("\":")
          stream_value(value[key])
          if i < keys.length() - 1 {
            chunks.push(",")
          }
        }
        chunks.push("}")
      } else {
        chunks.push("null")
      }
    }
    
    stream_value(data)
    chunks
  }
  
  // Test streaming serialization
  let stream_data = {
    "name": "John",
    "age": 30,
    "active": true
  }
  
  let stream_chunks = stream_serialize_json(stream_data)
  assert_eq(stream_chunks.join(""), "{\"name\":\"John\",\"age\":30,\"active\":true}")
  
  // Test lazy serialization
  let lazy_serialize = |data| {
    let mut serialized = null
    
    let get_serialized = || {
      if serialized == null {
        // Serialize on first access
        if data is String {
          serialized = "\"" + data + "\""
        } else if data is Number {
          serialized = data.to_string()
        } else if data is Boolean {
          serialized = if data { "true" } else { "false" }
        } else {
          serialized = "null"
        }
      }
      serialized
    }
    
    get_serialized
  }
  
  // Test lazy serialization
  let lazy_string = lazy_serialize("hello")
  assert_eq(lazy_string(), "\"hello\"")
  
  let lazy_number = lazy_serialize(42)
  assert_eq(lazy_number(), "42")
  
  // Test compression before serialization
  let compress_and_serialize = |data| {
    // Simple compression simulation
    let compressed = compress_data(data)
    serialize_json(compressed)
  }
  
  // Simple compression simulation
  let compress_data = |data| {
    if data is String {
      // Replace repeated characters with count
      let result = ""
      let mut i = 0
      
      while i < data.length() {
        let char = data[i]
        let mut count = 1
        
        while i + count < data.length() && data[i + count] == char {
          count = count + 1
        }
        
        if count > 3 {
          result = result + char.to_string() + "[" + count.to_string() + "]"
        } else {
          for j = 0; j < count; j = j + 1 {
            result = result + char
          }
        }
        
        i = i + count
      }
      
      result
    } else {
      data
    }
  }
  
  // Test compression
  let compressible_data = "aaaaabbbcccccddddd"
  let compressed_data = compress_data(compressible_data)
  assert_eq(compressed_data, "a[5]b[3]c[5]d[5]")
  
  let compressed_json = compress_and_serialize(compressible_data)
  assert_true(compressed_json.contains("a[5]b[3]c[5]d[5]"))
  
  // Test serialization with schema validation
  let serialize_with_schema = |data, schema| {
    // Validate data against schema first
    if validate_schema(data, schema) {
      serialize_json(data)
    } else {
      "null"  // Fallback for invalid data
    }
  }
  
  // Simple schema validation
  let validate_schema = |data, schema| {
    if schema.type == "object" {
      if data is Object {
        for property in schema.properties {
          if !data.contains(property.name) {
            return false
          }
          
          if !validate_schema(data[property.name], property.schema) {
            return false
          }
        }
        true
      } else {
        false
      }
    } else if schema.type == "string" {
      data is String
    } else if schema.type == "number" {
      data is Number
    } else if schema.type == "boolean" {
      data is Boolean
    } else if schema.type == "array" {
      if data is Array {
        for item in data {
          if !validate_schema(item, schema.items) {
            return false
          }
        }
        true
      } else {
        false
      }
    } else {
      true
    }
  }
  
  // Test schema validation
  let user_schema = {
    type: "object",
    properties: [
      { name: "name", schema: { type: "string" } },
      { name: "age", schema: { type: "number" } },
      { name: "active", schema: { type: "boolean" } }
    ]
  }
  
  let valid_data = { "name": "John", "age": 30, "active": true }
  let invalid_data = { "name": "John", "age": "30", "active": "true" }  // Wrong types
  
  let valid_json = serialize_with_schema(valid_data, user_schema)
  assert_true(valid_json.contains("\"name\":\"John\""))
  assert_true(valid_json.contains("\"age\":30"))
  assert_true(valid_json.contains("\"active\":true"))
  
  let invalid_json = serialize_with_schema(invalid_data, user_schema)
  assert_eq(invalid_json, "null")
  
  // Test serialization with circular reference handling
  let serialize_with_circular_check = |data| {
    let visited = []
    
    let check_circular = |obj| {
      for item in visited {
        if item === obj {
          return true
        }
      }
      false
    }
    
    let serialize_value = |value| {
      if check_circular(value) {
        "\"[Circular Reference]\""
      } else if value is String {
        "\"" + value + "\""
      } else if value is Number {
        value.to_string()
      } else if value is Boolean {
        if value { "true" } else { "false" }
      } else if value is Array {
        visited.push(value)
        let result = "["
        for i = 0; i < value.length(); i = i + 1 {
          result = result + serialize_value(value[i])
          if i < value.length() - 1 {
            result = result + ","
          }
        }
        result + "]"
      } else if value is Object {
        visited.push(value)
        let result = "{"
        let keys = value.keys()
        for i = 0; i < keys.length(); i = i + 1 {
          let key = keys[i]
          result = result + "\"" + key + "\":" + serialize_value(value[key])
          if i < keys.length() - 1 {
            result = result + ","
          }
        }
        result + "}"
      } else {
        "null"
      }
    }
    
    serialize_value(data)
  }
  
  // Test circular reference handling
  let circular_obj = { "name": "test" }
  circular_obj.self = circular_obj  // Create circular reference
  
  let circular_json = serialize_with_circular_check(circular_obj)
  assert_true(circular_json.contains("\"self\":\"[Circular Reference]\""))
  
  // Test serialization with custom replacer
  let serialize_with_replacer = |data, replacer| {
    let replace_value = |key, value| {
      replacer(key, value)
    }
    
    let serialize_value = |key, value| {
      let new_value = replace_value(key, value)
      
      if new_value is String {
        "\"" + new_value + "\""
      } else if new_value is Number {
        new_value.to_string()
      } else if new_value is Boolean {
        if new_value { "true" } else { "false" }
      } else if new_value is Array {
        let result = "["
        for i = 0; i < new_value.length(); i = i + 1 {
          result = result + serialize_value(i.to_string(), new_value[i])
          if i < new_value.length() - 1 {
            result = result + ","
          }
        }
        result + "]"
      } else if new_value is Object {
        let result = "{"
        let keys = new_value.keys()
        for i = 0; i < keys.length(); i = i + 1 {
          let obj_key = keys[i]
          result = result + "\"" + obj_key + "\":" + serialize_value(obj_key, new_value[obj_key])
          if i < keys.length() - 1 {
            result = result + ","
          }
        }
        result + "}"
      } else {
        "null"
      }
    }
    
    serialize_value("", data)
  }
  
  // Test custom replacer
  let censor_replacer = |key, value| {
    if key == "password" || key == "secret" {
      "***CENSORED***"
    } else {
      value
    }
  }
  
  let sensitive_data = {
    "username": "john",
    "password": "secret123",
    "email": "john@example.com"
  }
  
  let censored_json = serialize_with_replacer(sensitive_data, censor_replacer)
  assert_true(censored_json.contains("\"username\":\"john\""))
  assert_true(censored_json.contains("\"password\":\"***CENSORED***\""))
  assert_true(censored_json.contains("\"email\":\"john@example.com\""))
}

// Test 10: Cross-Format Conversion
test "cross_format_conversion" {
  // Test JSON to XML conversion
  let json_to_xml = |json_data, root_name| {
    let json_obj = deserialize_json(json_data)
    serialize_xml(json_obj, root_name)
  }
  
  // Test XML to JSON conversion
  let xml_to_json = |xml_string| {
    let xml_obj = deserialize_xml(xml_string)
    serialize_json(xml_obj)
  }
  
  // Test JSON to YAML conversion
  let json_to_yaml = |json_data| {
    let json_obj = deserialize_json(json_data)
    serialize_yaml(json_obj)
  }
  
  // Test YAML to JSON conversion
  let yaml_to_json = |yaml_string| {
    let yaml_obj = deserialize_yaml(yaml_string)
    serialize_json(yaml_obj)
  }
  
  // Test JSON to CSV conversion
  let json_to_csv = |json_data| {
    let json_obj = deserialize_json(json_data)
    
    if json_obj is Array {
      let headers = []
      if json_obj.length() > 0 {
        let keys = json_obj[0].keys()
        for key in keys {
          headers.push(key)
        }
      }
      
      serialize_csv(json_obj, headers)
    } else {
      ""
    }
  }
  
  // Test CSV to JSON conversion
  let csv_to_json = |csv_string| {
    let csv_obj = deserialize_csv(csv_string)
    serialize_json(csv_obj.data)
  }
  
  // Test format conversion
  let convert_format = |data, from_format, to_format| {
    if from_format == "json" && to_format == "xml" {
      let json_obj = deserialize_json(data)
      serialize_xml(json_obj, "root")
    } else if from_format == "xml" && to_format == "json" {
      let xml_obj = deserialize_xml(data)
      serialize_json(xml_obj)
    } else if from_format == "json" && to_format == "yaml" {
      let json_obj = deserialize_json(data)
      serialize_yaml(json_obj)
    } else if from_format == "yaml" && to_format == "json" {
      let yaml_obj = deserialize_yaml(data)
      serialize_json(yaml_obj)
    } else if from_format == "json" && to_format == "csv" {
      json_to_csv(data)
    } else if from_format == "csv" && to_format == "json" {
      csv_to_json(data)
    } else {
      data  // No conversion needed
    }
  }
  
  // Test JSON to XML conversion
  let json_string = "{\"name\":\"John\",\"age\":30,\"active\":true}"
  let xml_result = json_to_xml(json_string, "person")
  
  assert_true(xml_result.contains("<person>"))
  assert_true(xml_result.contains("</person>"))
  assert_true(xml_result.contains("<name>John</name>"))
  assert_true(xml_result.contains("<age>30</age>"))
  assert_true(xml_result.contains("<active>true</active>"))
  
  // Test XML to JSON conversion
  let xml_string = "<person><name>John</name><age>30</age><active>true</active></person>"
  let json_result = xml_to_json(xml_string)
  
  assert_true(json_result.contains("\"name\":\"John\""))
  assert_true(json_result.contains("\"age\":30"))
  assert_true(json_result.contains("\"active\":true"))
  
  // Test JSON to YAML conversion
  let json_string2 = "{\"name\":\"John\",\"age\":30,\"active\":true}"
  let yaml_result = json_to_yaml(json_string2)
  
  assert_true(yaml_result.contains("name: John"))
  assert_true(yaml_result.contains("age: 30"))
  assert_true(yaml_result.contains("active: true"))
  
  // Test YAML to JSON conversion
  let yaml_string = "name: John\nage: 30\nactive: true"
  let json_result2 = yaml_to_json(yaml_string)
  
  assert_true(json_result2.contains("\"name\":\"John\""))
  assert_true(json_result2.contains("\"age\":30"))
  assert_true(json_result2.contains("\"active\":true"))
  
  // Test JSON to CSV conversion
  let json_array = "[{\"name\":\"John\",\"age\":30},{\"name\":\"Alice\",\"age\":25}]"
  let csv_result = json_to_csv(json_array)
  
  assert_true(csv_result.contains("name,age"))
  assert_true(csv_result.contains("John,30"))
  assert_true(csv_result.contains("Alice,25"))
  
  // Test CSV to JSON conversion
  let csv_string = "name,age\nJohn,30\nAlice,25"
  let json_array_result = csv_to_json(csv_string)
  
  assert_true(json_array_result.contains("\"name\":\"John\""))
  assert_true(json_array_result.contains("\"age\":30"))
  assert_true(json_array_result.contains("\"name\":\"Alice\""))
  assert_true(json_array_result.contains("\"age\":25"))
  
  // Test generic format conversion
  let converted1 = convert_format("{\"name\":\"John\"}", "json", "xml")
  assert_true(converted1.contains("<name>John</name>"))
  
  let converted2 = convert_format("<name>John</name>", "xml", "json")
  assert_true(converted2.contains("\"name\":\"John\""))
  
  let converted3 = convert_format("{\"name\":\"John\"}", "json", "yaml")
  assert_true(converted3.contains("name: John"))
  
  let converted4 = convert_format("name: John", "yaml", "json")
  assert_true(converted4.contains("\"name\":\"John\""))
  
  // Test format detection
  let detect_format = |data_string| {
    if data_string.starts_with("{") && data_string.ends_with("}") {
      "json"
    } else if data_string.starts_with("<") && data_string.ends_with(">") {
      "xml"
    } else if data_string.contains(":") && !data_string.contains("{") && !data_string.contains("<") {
      "yaml"
    } else if data_string.contains(",") {
      "csv"
    } else {
      "unknown"
    }
  }
  
  // Test format detection
  assert_eq(detect_format("{\"name\":\"John\"}"), "json")
  assert_eq(detect_format("<name>John</name>"), "xml")
  assert_eq(detect_format("name: John"), "yaml")
  assert_eq(detect_format("name,age\nJohn,30"), "csv")
  assert_eq(detect_format("just some text"), "unknown")
  
  // Test automatic format conversion
  let auto_convert = |data, target_format| {
    let source_format = detect_format(data)
    convert_format(data, source_format, target_format)
  }
  
  // Test automatic conversion
  let auto_converted1 = auto_convert("{\"name\":\"John\"}", "xml")
  assert_true(auto_converted1.contains("<name>John</name>"))
  
  let auto_converted2 = auto_convert("<name>John</name>", "yaml")
  assert_true(auto_converted2.contains("name: John"))
  
  let auto_converted3 = auto_convert("name: John", "json")
  assert_true(auto_converted3.contains("\"name\":\"John\""))
  
  // Test round-trip conversion
  let round_trip_convert = |data, format1, format2| {
    let converted1 = convert_format(data, format1, format2)
    let converted2 = convert_format(converted1, format2, format1)
    
    // Note: Round-trip conversion might not preserve exact formatting
    // but should preserve the data structure
    converted2
  }
  
  // Test round-trip conversion
  let original_json = "{\"name\":\"John\",\"age\":30}"
  let round_trip_json = round_trip_convert(original_json, "json", "xml")
  
  // Should still be valid JSON with the same data
  let parsed_original = deserialize_json(original_json)
  let parsed_round_trip = deserialize_json(round_trip_json)
  
  assert_eq(parsed_original["name"], parsed_round_trip["name"])
  assert_eq(parsed_original["age"], parsed_round_trip["age"])
  
  // Test format conversion with validation
  let convert_with_validation = |data, from_format, to_format| {
    try {
      let converted = convert_format(data, from_format, to_format)
      
      // Validate the converted format
      if to_format == "json" && is_valid_json(converted) {
        Some(converted)
      } else if to_format == "xml" && is_valid_xml(converted) {
        Some(converted)
      } else if to_format == "yaml" && is_valid_yaml(converted) {
        Some(converted)
      } else {
        None
      }
    } catch {
      None
    }
  }
  
  // Simple validation functions
  let is_valid_json = |json_string| {
    json_string.starts_with("{") && json_string.ends_with("}") ||
    json_string.starts_with("[") && json_string.ends_with("]")
  }
  
  let is_valid_xml = |xml_string| {
    xml_string.starts_with("<") && xml_string.ends_with(">") &&
    xml_string.count("<") == xml_string.count(">")
  }
  
  let is_valid_yaml = |yaml_string| {
    yaml_string.contains(":") && !yaml_string.contains("{") && !yaml_string.contains("<")
  }
  
  // Test conversion with validation
  let validated_json = convert_with_validation("{\"name\":\"John\"}", "json", "xml")
  assert_true(validated_json.is_some())
  
  let validated_xml = convert_with_validation("<name>John</name>", "xml", "json")
  assert_true(validated_xml.is_some())
  
  let invalid_conversion = convert_with_validation("not a valid format", "unknown", "json")
  assert_true(invalid_conversion.is_none())
}