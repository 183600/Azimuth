// Azimuth Serialization and Deserialization Tests
// This file contains test cases for serialization and deserialization functionality

// Test 1: JSON Serialization and Deserialization
test "JSON serialization and deserialization" {
  // Define telemetry data structure
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    service_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // Serialize to JSON (simplified implementation)
  let serialize_to_json = fn(data: TelemetryData) {
    let parent_span_json = match data.parent_span_id {
      Some(id) => "\"" + id + "\""
      None => "null"
    }
    
    let tags_json = data.tags.map(fn(tag) {
      "\"" + tag.0 + "\":\"" + tag.1 + "\""
    }).reduce(fn(acc, tag_json) { 
      if acc.length() > 0 { acc + "," + tag_json } else { tag_json }
    }, "")
    
    "{"
      + "\"trace_id\":\"" + data.trace_id + "\"," 
      + "\"span_id\":\"" + data.span_id + "\"," 
      + "\"parent_span_id\":" + parent_span_json + "," 
      + "\"operation_name\":\"" + data.operation_name + "\"," 
      + "\"service_name\":\"" + data.service_name + "\"," 
      + "\"start_time\":" + data.start_time.to_string() + "," 
      + "\"end_time\":" + data.end_time.to_string() + "," 
      + "\"status\":\"" + data.status + "\"," 
      + "\"tags\":{" + tags_json + "}"
      + "}"
  }
  
  // Parse JSON string (simplified implementation)
  let parse_json_string = fn(json: String) {
    if json.starts_with("\"") and json.ends_with("\"") {
      json.substring(1, json.length() - 2)
    } else {
      json
    }
  }
  
  // Parse JSON number
  let parse_json_number = fn(json: String) {
    json.to_int()
  }
  
  // Parse JSON object (simplified implementation)
  let parse_json_object = fn(json: String) {
    let trimmed = json.trim()
    if trimmed.starts_with("{") and trimmed.ends_with("}") {
      let content = trimmed.substring(1, trimmed.length() - 2)
      let pairs = content.split(",")
      
      let mut result = []
      for pair in pairs {
        let key_value = pair.split(":")
        if key_value.length() == 2 {
          let key = parse_json_string(key_value[0].trim())
          let value = parse_json_string(key_value[1].trim())
          result = result.push((key, value))
        }
      }
      result
    } else {
      []
    }
  }
  
  // Deserialize from JSON (simplified implementation)
  let deserialize_from_json = fn(json: String) {
    // This is a very simplified implementation
    // In a real implementation, we would use a proper JSON parser
    
    // Extract trace_id
    let trace_id_start = json.index_of("\"trace_id\":\"") + 12
    let trace_id_end = json.index_of("\"", trace_id_start)
    let trace_id = json.substring(trace_id_start, trace_id_end - trace_id_start)
    
    // Extract span_id
    let span_id_start = json.index_of("\"span_id\":\"") + 11
    let span_id_end = json.index_of("\"", span_id_start)
    let span_id = json.substring(span_id_start, span_id_end - span_id_start)
    
    // Extract parent_span_id
    let parent_span_start = json.index_of("\"parent_span_id\":") + 18
    let parent_span_end = json.index_of(",", parent_span_start)
    let parent_span_json = json.substring(parent_span_start, parent_span_end - parent_span_start)
    let parent_span_id = if parent_span_json == "null" { None } else { Some(parse_json_string(parent_span_json)) }
    
    // Extract operation_name
    let operation_start = json.index_of("\"operation_name\":\"") + 18
    let operation_end = json.index_of("\"", operation_start)
    let operation_name = json.substring(operation_start, operation_end - operation_start)
    
    // Extract service_name
    let service_start = json.index_of("\"service_name\":\"") + 16
    let service_end = json.index_of("\"", service_start)
    let service_name = json.substring(service_start, service_end - service_start)
    
    // Extract start_time
    let start_time_start = json.index_of("\"start_time\":") + 13
    let start_time_end = json.index_of(",", start_time_start)
    let start_time = parse_json_number(json.substring(start_time_start, start_time_end - start_time_start))
    
    // Extract end_time
    let end_time_start = json.index_of("\"end_time\":") + 11
    let end_time_end = json.index_of(",", end_time_start)
    let end_time = parse_json_number(json.substring(end_time_start, end_time_end - end_time_start))
    
    // Extract status
    let status_start = json.index_of("\"status\":\"") + 10
    let status_end = json.index_of("\"", status_start)
    let status = json.substring(status_start, status_end - status_start)
    
    // Extract tags
    let tags_start = json.index_of("\"tags\":{") + 8
    let tags_end = json.index_of("}", tags_start)
    let tags_json = json.substring(tags_start, tags_end - tags_start)
    let tags = parse_json_object("{" + tags_json + "}")
    
    {
      trace_id,
      span_id,
      parent_span_id,
      operation_name,
      service_name,
      start_time,
      end_time,
      status,
      tags
    }
  }
  
  // Create test data
  let telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "http.request",
    service_name: "user-service",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "success",
    tags: [
      ("http.method", "GET"),
      ("http.url", "/api/users/123"),
      ("user.id", "123")
    ]
  }
  
  // Test serialization
  let json = serialize_to_json(telemetry_data)
  assert_true(json.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json.contains("\"span_id\":\"span-67890\""))
  assert_true(json.contains("\"parent_span_id\":\"span-11111\""))
  assert_true(json.contains("\"operation_name\":\"http.request\""))
  assert_true(json.contains("\"service_name\":\"user-service\""))
  assert_true(json.contains("\"start_time\":1640995200"))
  assert_true(json.contains("\"end_time\":1640995250"))
  assert_true(json.contains("\"status\":\"success\""))
  assert_true(json.contains("\"tags\":{"))
  assert_true(json.contains("\"http.method\":\"GET\""))
  
  // Test deserialization
  let deserialized_data = deserialize_from_json(json)
  assert_eq(deserialized_data.trace_id, telemetry_data.trace_id)
  assert_eq(deserialized_data.span_id, telemetry_data.span_id)
  assert_eq(deserialized_data.parent_span_id, telemetry_data.parent_span_id)
  assert_eq(deserialized_data.operation_name, telemetry_data.operation_name)
  assert_eq(deserialized_data.service_name, telemetry_data.service_name)
  assert_eq(deserialized_data.start_time, telemetry_data.start_time)
  assert_eq(deserialized_data.end_time, telemetry_data.end_time)
  assert_eq(deserialized_data.status, telemetry_data.status)
  
  // Test data with null parent_span_id
  let telemetry_data_no_parent = { telemetry_data | parent_span_id: None }
  let json_no_parent = serialize_to_json(telemetry_data_no_parent)
  assert_true(json_no_parent.contains("\"parent_span_id\":null"))
  
  let deserialized_no_parent = deserialize_from_json(json_no_parent)
  assert_eq(deserialized_no_parent.parent_span_id, None)
}

// Test 2: Binary Serialization and Deserialization
test "binary serialization and deserialization" {
  // Define binary format types
  enum BinaryType {
    Int8
    Int16
    Int32
    Int64
    Float32
    Float64
    String
    Bool
    Array(BinaryType)
    Object(Array[(String, BinaryType)])
  }
  
  // Define binary value
  type BinaryValue = 
    Int8Value(Int)
    | Int16Value(Int)
    | Int32Value(Int)
    | Int64Value(Int)
    | Float32Value(Float)
    | Float64Value(Float)
    | StringValue(String)
    | BoolValue(Bool)
    | ArrayValue(Array[BinaryValue], BinaryType)
    | ObjectValue(Array[(String, BinaryValue)], Array[(String, BinaryType)])
  
  // Define binary serializer state
  type BinarySerializer = {
    buffer: Array[Int],
    position: Int
  }
  
  // Create binary serializer
  let create_serializer = fn() {
    { buffer: [], position: 0 }
  }
  
  // Write byte to buffer
  let write_byte = fn(serializer: BinarySerializer, value: Int) {
    { buffer: serializer.buffer.push(value), position: serializer.position + 1 }
  }
  
  // Write 32-bit integer
  let write_int32 = fn(serializer: BinarySerializer, value: Int) {
    let bytes = [
      (value >> 24) & 0xFF,
      (value >> 16) & 0xFF,
      (value >> 8) & 0xFF,
      value & 0xFF
    ]
    
    let mut updated_serializer = serializer
    for byte in bytes {
      updated_serializer = write_byte(updated_serializer, byte)
    }
    updated_serializer
  }
  
  // Write string
  let write_string = fn(serializer: BinarySerializer, value: String) {
    // Write string length
    let length_serializer = write_int32(serializer, value.length())
    
    // Write string bytes
    let mut updated_serializer = length_serializer
    for i in 0 .. value.length() {
      updated_serializer = write_byte(updated_serializer, value.to_char_array()[i].to_int())
    }
    updated_serializer
  }
  
  // Write boolean
  let write_bool = fn(serializer: BinarySerializer, value: Bool) {
    write_byte(serializer, if value { 1 } else { 0 })
  }
  
  // Write array
  let write_array = fn(serializer: BinarySerializer, array: Array[BinaryValue], element_type: BinaryType) {
    // Write array length
    let length_serializer = write_int32(serializer, array.length())
    
    // Write array elements
    let mut updated_serializer = length_serializer
    for element in array {
      updated_serializer = write_binary_value(updated_serializer, element)
    }
    updated_serializer
  }
  
  // Write object
  let write_object = fn(
    serializer: BinarySerializer, 
    object: Array[(String, BinaryValue)], 
    schema: Array[(String, BinaryType)]
  ) {
    // Write object length
    let length_serializer = write_int32(serializer, object.length())
    
    // Write object properties
    let mut updated_serializer = length_serializer
    for (key, value) in object {
      updated_serializer = write_string(updated_serializer, key)
      updated_serializer = write_binary_value(updated_serializer, value)
    }
    updated_serializer
  }
  
  // Write binary value
  and write_binary_value = fn(serializer: BinarySerializer, value: BinaryValue) {
    match value {
      Int8Value(v) => write_byte(serializer, v & 0xFF)
      Int16Value(v) => {
        let bytes = [(v >> 8) & 0xFF, v & 0xFF]
        let mut updated_serializer = serializer
        for byte in bytes {
          updated_serializer = write_byte(updated_serializer, byte)
        }
        updated_serializer
      }
      Int32Value(v) => write_int32(serializer, v)
      Int64Value(v) => {
        let bytes = [
          (v >> 56) & 0xFF,
          (v >> 48) & 0xFF,
          (v >> 40) & 0xFF,
          (v >> 32) & 0xFF,
          (v >> 24) & 0xFF,
          (v >> 16) & 0xFF,
          (v >> 8) & 0xFF,
          v & 0xFF
        ]
        let mut updated_serializer = serializer
        for byte in bytes {
          updated_serializer = write_byte(updated_serializer, byte)
        }
        updated_serializer
      }
      Float32Value(v) => {
        // Simplified: just write as string
        write_string(serializer, v.to_string())
      }
      Float64Value(v) => {
        // Simplified: just write as string
        write_string(serializer, v.to_string())
      }
      StringValue(v) => write_string(serializer, v)
      BoolValue(v) => write_bool(serializer, v)
      ArrayValue(arr, element_type) => write_array(serializer, arr, element_type)
      ObjectValue(obj, schema) => write_object(serializer, obj, schema)
    }
  }
  
  // Define binary deserializer state
  type BinaryDeserializer = {
    buffer: Array[Int],
    position: Int
  }
  
  // Create binary deserializer
  let create_deserializer = fn(buffer: Array[Int]) {
    { buffer, position: 0 }
  }
  
  // Read byte from buffer
  let read_byte = fn(deserializer: BinaryDeserializer) {
    if deserializer.position < deserializer.buffer.length() {
      let value = deserializer.buffer[deserializer.position]
      Some((value, { deserializer | position: deserializer.position + 1 }))
    } else {
      None
    }
  }
  
  // Read 32-bit integer
  let read_int32 = fn(deserializer: BinaryDeserializer) {
    match read_byte(deserializer) {
      Some((b1, d1)) => {
        match read_byte(d1) {
          Some((b2, d2)) => {
            match read_byte(d2) {
              Some((b3, d3)) => {
                match read_byte(d3) {
                  Some((b4, d4)) => {
                    let value = (b1 << 24) | (b2 << 16) | (b3 << 8) | b4
                    Some((value, d4))
                  }
                  None => None
                }
              }
              None => None
            }
          }
          None => None
        }
      }
      None => None
    }
  }
  
  // Read string
  let read_string = fn(deserializer: BinaryDeserializer) {
    match read_int32(deserializer) {
      Some((length, d1)) => {
        let mut chars = []
        let mut current_deserializer = d1
        let mut success = true
        
        for i in 0 .. length {
          match read_byte(current_deserializer) {
            Some((byte, d)) => {
              chars = chars.push(byte.to_char())
              current_deserializer = d
            }
            None => {
              success = false
              break
            }
          }
        }
        
        if success {
          Some((chars.reduce(fn(acc, c) { acc + c.to_string() }, ""), current_deserializer))
        } else {
          None
        }
      }
      None => None
    }
  }
  
  // Read boolean
  let read_bool = fn(deserializer: BinaryDeserializer) {
    match read_byte(deserializer) {
      Some((value, d)) => Some((value == 1, d))
      None => None
    }
  }
  
  // Read array
  let read_array = fn(deserializer: BinaryDeserializer, element_type: BinaryType) {
    match read_int32(deserializer) {
      Some((length, d1)) => {
        let mut elements = []
        let mut current_deserializer = d1
        let mut success = true
        
        for i in 0 .. length {
          match read_binary_value(current_deserializer, element_type) {
            Some((value, d)) => {
              elements = elements.push(value)
              current_deserializer = d
            }
            None => {
              success = false
              break
            }
          }
        }
        
        if success {
          Some((elements, current_deserializer))
        } else {
          None
        }
      }
      None => None
    }
  }
  
  // Read object
  let read_object = fn(
    deserializer: BinaryDeserializer, 
    schema: Array[(String, BinaryType)]
  ) {
    match read_int32(deserializer) {
      Some((length, d1)) => {
        let mut properties = []
        let mut current_deserializer = d1
        let mut success = true
        
        for i in 0 .. length {
          match read_string(current_deserializer) {
            Some((key, d2)) => {
              // Find the type for this key
              let type_option = schema.find(fn(s) { s.0 == key })
              match type_option {
                Some((_, value_type)) => {
                  match read_binary_value(d2, value_type) {
                    Some((value, d3)) => {
                      properties = properties.push((key, value))
                      current_deserializer = d3
                    }
                    None => {
                      success = false
                      break
                    }
                  }
                }
                None => {
                  success = false
                  break
                }
              }
            }
            None => {
              success = false
              break
            }
          }
        }
        
        if success {
          Some((properties, current_deserializer))
        } else {
          None
        }
      }
      None => None
    }
  }
  
  // Read binary value
  and read_binary_value = fn(deserializer: BinaryDeserializer, value_type: BinaryType) {
    match value_type {
      BinaryType::Int8 => {
        match read_byte(deserializer) {
          Some((value, d)) => Some((Int8Value(value), d))
          None => None
        }
      }
      BinaryType::Int16 => {
        match read_byte(deserializer) {
          Some((b1, d1)) => {
            match read_byte(d1) {
              Some((b2, d2)) => {
                let value = (b1 << 8) | b2
                Some((Int16Value(value), d2))
              }
              None => None
            }
          }
          None => None
        }
      }
      BinaryType::Int32 => {
        match read_int32(deserializer) {
          Some((value, d)) => Some((Int32Value(value), d))
          None => None
        }
      }
      BinaryType::Int64 => {
        match read_byte(deserializer) {
          Some((b1, d1)) => {
            match read_byte(d1) {
              Some((b2, d2)) => {
                match read_byte(d2) {
                  Some((b3, d3)) => {
                    match read_byte(d3) {
                      Some((b4, d4)) => {
                        match read_byte(d4) {
                          Some((b5, d5)) => {
                            match read_byte(d5) {
                              Some((b6, d6)) => {
                                match read_byte(d6) {
                                  Some((b7, d7)) => {
                                    match read_byte(d7) {
                                      Some((b8, d8)) => {
                                        let value = 
                                          (b1.to_int64() << 56) | 
                                          (b2.to_int64() << 48) | 
                                          (b3.to_int64() << 40) | 
                                          (b4.to_int64() << 32) | 
                                          (b5.to_int64() << 24) | 
                                          (b6.to_int64() << 16) | 
                                          (b7.to_int64() << 8) | 
                                          b8.to_int64()
                                        Some((Int64Value(value), d8))
                                      }
                                      None => None
                                    }
                                  }
                                  None => None
                                }
                              }
                              None => None
                            }
                          }
                          None => None
                        }
                      }
                      None => None
                    }
                  }
                  None => None
                }
              }
              None => None
            }
          }
          None => None
        }
      }
      BinaryType::Float32 => {
        match read_string(deserializer) {
          Some((value, d)) => Some((Float32Value(value.to_float()), d))
          None => None
        }
      }
      BinaryType::Float64 => {
        match read_string(deserializer) {
          Some((value, d)) => Some((Float64Value(value.to_float()), d))
          None => None
        }
      }
      BinaryType::String => {
        match read_string(deserializer) {
          Some((value, d)) => Some((StringValue(value), d))
          None => None
        }
      }
      BinaryType::Bool => {
        match read_bool(deserializer) {
          Some((value, d)) => Some((BoolValue(value), d))
          None => None
        }
      }
      BinaryType::Array(element_type) => {
        match read_array(deserializer, element_type) {
          Some((value, d)) => Some((ArrayValue(value, element_type), d))
          None => None
        }
      }
      BinaryType::Object(schema) => {
        match read_object(deserializer, schema) {
          Some((value, d)) => Some((ObjectValue(value, schema), d))
          None => None
        }
      }
    }
  }
  
  // Test binary serialization and deserialization
  let serializer = create_serializer()
  
  // Serialize a complex object
  let test_object = [
    ("trace_id", StringValue("trace-12345")),
    ("span_id", StringValue("span-67890")),
    ("parent_span_id", StringValue("span-11111")),
    ("operation_name", StringValue("http.request")),
    ("service_name", StringValue("user-service")),
    ("start_time", Int32Value(1640995200)),
    ("end_time", Int32Value(1640995250)),
    ("success", BoolValue(true)),
    ("tags", ArrayValue([
      StringValue("http.method:GET"),
      StringValue("http.url:/api/users/123"),
      StringValue("user.id:123")
    ], BinaryType::String))
  ]
  
  let schema = [
    ("trace_id", BinaryType::String),
    ("span_id", BinaryType::String),
    ("parent_span_id", BinaryType::String),
    ("operation_name", BinaryType::String),
    ("service_name", BinaryType::String),
    ("start_time", BinaryType::Int32),
    ("end_time", BinaryType::Int32),
    ("success", BinaryType::Bool),
    ("tags", BinaryType::Array(BinaryType::String))
  ]
  
  let serialized_serializer = write_object(serializer, test_object, schema)
  let buffer = serialized_serializer.buffer
  
  // Verify buffer is not empty
  assert_true(buffer.length() > 0)
  
  // Test deserialization
  let deserializer = create_deserializer(buffer)
  match read_object(deserializer, schema) {
    Some((deserialized_object, _)) => {
      assert_eq(deserialized_object.length(), 9)
      
      // Check trace_id
      match deserialized_object.filter(fn(p) { p.0 == "trace_id" })[0].1 {
        StringValue(value) => assert_eq(value, "trace-12345")
        _ => assert_true(false)
      }
      
      // Check span_id
      match deserialized_object.filter(fn(p) { p.0 == "span_id" })[0].1 {
        StringValue(value) => assert_eq(value, "span-67890")
        _ => assert_true(false)
      }
      
      // Check start_time
      match deserialized_object.filter(fn(p) { p.0 == "start_time" })[0].1 {
        Int32Value(value) => assert_eq(value, 1640995200)
        _ => assert_true(false)
      }
      
      // Check success
      match deserialized_object.filter(fn(p) { p.0 == "success" })[0].1 {
        BoolValue(value) => assert_true(value)
        _ => assert_true(false)
      }
      
      // Check tags array
      match deserialized_object.filter(fn(p) { p.0 == "tags" })[0].1 {
        ArrayValue(tags, _) => {
          assert_eq(tags.length(), 3)
          match tags[0] {
            StringValue(value) => assert_eq(value, "http.method:GET")
            _ => assert_true(false)
          }
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: Protocol Buffer Serialization
test "protocol buffer serialization" {
  // Define wire types
  enum WireType {
    Varint = 0
    Fixed64 = 1
    LengthDelimited = 2
    StartGroup = 3
    EndGroup = 4
    Fixed32 = 5
  }
  
  // Define field
  type Field = {
    field_number: Int,
    wire_type: WireType,
    value: String
  }
  
  // Encode varint
  let encode_varint = fn(value: Int) {
    let mut bytes = []
    let mut v = value
    
    while v >= 0x80 {
      bytes = bytes.push((v & 0x7F) | 0x80)
      v = v >> 7
    }
    
    bytes = bytes.push(v & 0x7F)
    bytes
  }
  
  // Decode varint
  let decode_varint = fn(bytes: Array[Int], position: Int) {
    let mut result = 0
    let mut shift = 0
    let mut pos = position
    let mut done = false
    
    while pos < bytes.length() and not(done) {
      let byte = bytes[pos]
      result = result | ((byte & 0x7F) << shift)
      pos = pos + 1
      
      if (byte & 0x80) == 0 {
        done = true
      } else {
        shift = shift + 7
      }
    }
    
    if done {
      Some((result, pos))
    } else {
      None
    }
  }
  
  // Encode key
  let encode_key = fn(field_number: Int, wire_type: WireType) {
    let key = (field_number << 3) | wire_type as Int
    encode_varint(key)
  }
  
  // Encode string
  let encode_string = fn(value: String) {
    let length_bytes = encode_varint(value.length())
    let mut value_bytes = []
    
    for i in 0 .. value.length() {
      value_bytes = value_bytes.push(value.to_char_array()[i].to_int())
    }
    
    length_bytes + value_bytes
  }
  
  // Encode message
  let encode_message = fn(fields: Array[Field]) {
    let mut bytes = []
    
    for field in fields {
      let key_bytes = encode_key(field.field_number, field.wire_type)
      let value_bytes = match field.wire_type {
        WireType::Varint => encode_varint(field.value.to_int())
        WireType::LengthDelimited => encode_string(field.value)
        _ => []  // Simplified
      }
      
      bytes = bytes + key_bytes + value_bytes
    }
    
    bytes
  }
  
  // Decode string
  let decode_string = fn(bytes: Array[Int], position: Int) {
    match decode_varint(bytes, position) {
      Some((length, pos)) => {
        if pos + length <= bytes.length() {
          let string_bytes = bytes.slice(pos, pos + length)
          let mut chars = []
          
          for byte in string_bytes {
            chars = chars.push(byte.to_char())
          }
          
          let value = chars.reduce(fn(acc, c) { acc + c.to_string() }, "")
          Some((value, pos + length))
        } else {
          None
        }
      }
      None => None
    }
  }
  
  // Decode message
  let decode_message = fn(bytes: Array[Int], schema: Array[(Int, WireType)]) {
    let mut position = 0
    let mut fields = []
    let mut success = true
    
    while position < bytes.length() and success {
      match decode_varint(bytes, position) {
        Some((key, pos)) => {
          let field_number = key >> 3
          let wire_type = key & 0x07
          
          match schema.find(fn(s) { s.0 == field_number && s.1 as Int == wire_type }) {
            Some(_) => {
              match wire_type {
                0 => {  // Varint
                  match decode_varint(bytes, pos) {
                    Some((value, new_pos)) => {
                      fields = fields.push({
                        field_number,
                        wire_type: WireType::Varint,
                        value: value.to_string()
                      })
                      position = new_pos
                    }
                    None => {
                      success = false
                    }
                  }
                }
                2 => {  // LengthDelimited
                  match decode_string(bytes, pos) {
                    Some((value, new_pos)) => {
                      fields = fields.push({
                        field_number,
                        wire_type: WireType::LengthDelimited,
                        value
                      })
                      position = new_pos
                    }
                    None => {
                      success = false
                    }
                  }
                }
                _ => {
                  // Unsupported wire type
                  success = false
                }
              }
            }
            None => {
              // Unknown field
              success = false
            }
          }
        }
        None => {
          success = false
        }
      }
    }
    
    if success {
      Some(fields)
    } else {
      None
    }
  }
  
  // Define telemetry message schema
  let telemetry_schema = [
    (1, WireType::LengthDelimited),  // trace_id
    (2, WireType::LengthDelimited),  // span_id
    (3, WireType::LengthDelimited),  // parent_span_id
    (4, WireType::LengthDelimited),  // operation_name
    (5, WireType::LengthDelimited),  // service_name
    (6, WireType::Varint),           // start_time
    (7, WireType::Varint),           // end_time
    (8, WireType::LengthDelimited),  // status
    (9, WireType::LengthDelimited)   // tags (simplified as string)
  ]
  
  // Create telemetry message fields
  let telemetry_fields = [
    { field_number: 1, wire_type: WireType::LengthDelimited, value: "trace-12345" },
    { field_number: 2, wire_type: WireType::LengthDelimited, value: "span-67890" },
    { field_number: 3, wire_type: WireType::LengthDelimited, value: "span-11111" },
    { field_number: 4, wire_type: WireType::LengthDelimited, value: "http.request" },
    { field_number: 5, wire_type: WireType::LengthDelimited, value: "user-service" },
    { field_number: 6, wire_type: WireType::Varint, value: "1640995200" },
    { field_number: 7, wire_type: WireType::Varint, value: "1640995250" },
    { field_number: 8, wire_type: WireType::LengthDelimited, value: "success" },
    { field_number: 9, wire_type: WireType::LengthDelimited, value: "http.method:GET,http.url:/api/users" }
  ]
  
  // Test encoding
  let encoded_bytes = encode_message(telemetry_fields)
  assert_true(encoded_bytes.length() > 0)
  
  // Test decoding
  match decode_message(encoded_bytes, telemetry_schema) {
    Some(decoded_fields) => {
      assert_eq(decoded_fields.length(), 9)
      
      // Check trace_id
      match decoded_fields.find(fn(f) { f.field_number == 1 }) {
        Some(field) => assert_eq(field.value, "trace-12345")
        None => assert_true(false)
      }
      
      // Check span_id
      match decoded_fields.find(fn(f) { f.field_number == 2 }) {
        Some(field) => assert_eq(field.value, "span-67890")
        None => assert_true(false)
      }
      
      // Check start_time
      match decoded_fields.find(fn(f) { f.field_number == 6 }) {
        Some(field) => assert_eq(field.value, "1640995200")
        None => assert_true(false)
      }
      
      // Check status
      match decoded_fields.find(fn(f) { f.field_number == 8 }) {
        Some(field) => assert_eq(field.value, "success")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 4: Serialization Format Comparison
test "serialization format comparison" {
  // Define test data
  type TestData = {
    id: String,
    name: String,
    value: Int,
    active: Bool,
    tags: Array[String]
  }
  
  let test_data = {
    id: "test-12345",
    name: "Test Object",
    value: 42,
    active: true,
    tags: ["tag1", "tag2", "tag3"]
  }
  
  // JSON serialization
  let serialize_to_json = fn(data: TestData) {
    let tags_json = data.tags.map(fn(tag) { "\"" + tag + "\"" }).reduce(fn(acc, tag) { 
      if acc.length() > 0 { acc + "," + tag } else { tag }
    }, "")
    
    "{"
      + "\"id\":\"" + data.id + "\"," 
      + "\"name\":\"" + data.name + "\"," 
      + "\"value\":" + data.value.to_string() + "," 
      + "\"active\":" + (if data.active { "true" } else { "false" }) + "," 
      + "\"tags\":[" + tags_json + "]"
      + "}"
  }
  
  // Binary serialization (simplified)
  let serialize_to_binary = fn(data: TestData) {
    let mut bytes = []
    
    // Write id
    let id_length = data.id.length()
    bytes = bytes + [id_length >> 24 & 0xFF, id_length >> 16 & 0xFF, id_length >> 8 & 0xFF, id_length & 0xFF]
    for i in 0 .. data.id.length() {
      bytes = bytes.push(data.id.to_char_array()[i].to_int())
    }
    
    // Write name
    let name_length = data.name.length()
    bytes = bytes + [name_length >> 24 & 0xFF, name_length >> 16 & 0xFF, name_length >> 8 & 0xFF, name_length & 0xFF]
    for i in 0 .. data.name.length() {
      bytes = bytes.push(data.name.to_char_array()[i].to_int())
    }
    
    // Write value
    bytes = bytes + [
      data.value >> 24 & 0xFF,
      data.value >> 16 & 0xFF,
      data.value >> 8 & 0xFF,
      data.value & 0xFF
    ]
    
    // Write active
    bytes = bytes.push(if data.active { 1 } else { 0 })
    
    // Write tags count
    let tags_count = data.tags.length()
    bytes = bytes + [tags_count >> 24 & 0xFF, tags_count >> 16 & 0xFF, tags_count >> 8 & 0xFF, tags_count & 0xFF]
    
    // Write tags
    for tag in data.tags {
      let tag_length = tag.length()
      bytes = bytes + [tag_length >> 24 & 0xFF, tag_length >> 16 & 0xFF, tag_length >> 8 & 0xFF, tag_length & 0xFF]
      for i in 0 .. tag.length() {
        bytes = bytes.push(tag.to_char_array()[i].to_int())
      }
    }
    
    bytes
  }
  
  // Protocol Buffers serialization (simplified)
  let serialize_to_protobuf = fn(data: TestData) {
    let mut bytes = []
    
    // Field 1: id (string)
    let id_length = data.id.length()
    bytes = bytes + [0x0A]  // Key (field 1, wire type 2)
    bytes = bytes + encode_varint(id_length)
    for i in 0 .. data.id.length() {
      bytes = bytes.push(data.id.to_char_array()[i].to_int())
    }
    
    // Field 2: name (string)
    let name_length = data.name.length()
    bytes = bytes + [0x12]  // Key (field 2, wire type 2)
    bytes = bytes + encode_varint(name_length)
    for i in 0 .. data.name.length() {
      bytes = bytes.push(data.name.to_char_array()[i].to_int())
    }
    
    // Field 3: value (int32)
    bytes = bytes + [0x18]  // Key (field 3, wire type 0)
    bytes = bytes + encode_varint(data.value)
    
    // Field 4: active (bool)
    bytes = bytes + [0x20]  // Key (field 4, wire type 0)
    bytes = bytes.push(if data.active { 0x01 } else { 0x00 })
    
    // Field 5: tags (repeated string)
    for tag in data.tags {
      let tag_length = tag.length()
      bytes = bytes + [0x2A]  // Key (field 5, wire type 2)
      bytes = bytes + encode_varint(tag_length)
      for i in 0 .. tag.length() {
        bytes = bytes.push(tag.to_char_array()[i].to_int())
      }
    }
    
    bytes
  }
  
  and encode_varint = fn(value: Int) {
    let mut bytes = []
    let mut v = value
    
    while v >= 0x80 {
      bytes = bytes.push((v & 0x7F) | 0x80)
      v = v >> 7
    }
    
    bytes = bytes.push(v & 0x7F)
    bytes
  }
  
  // Serialize using different formats
  let json_data = serialize_to_json(test_data)
  let binary_data = serialize_to_binary(test_data)
  let protobuf_data = serialize_to_protobuf(test_data)
  
  // Compare sizes
  let json_size = json_data.length()
  let binary_size = binary_data.length()
  let protobuf_size = protobuf_data.length()
  
  assert_true(json_size > 0)
  assert_true(binary_size > 0)
  assert_true(protobuf_size > 0)
  
  // Protocol Buffers should be more compact than JSON
  assert_true(protobuf_size < json_size)
  
  // Binary format should be more compact than JSON
  assert_true(binary_size < json_size)
  
  // Protocol Buffers should be reasonably compact
  assert_true(protobuf_size <= binary_size + 10)  // Allow some overhead
  
  // Verify JSON contains expected fields
  assert_true(json_data.contains("\"id\":\"test-12345\""))
  assert_true(json_data.contains("\"name\":\"Test Object\""))
  assert_true(json_data.contains("\"value\":42"))
  assert_true(json_data.contains("\"active\":true"))
  assert_true(json_data.contains("\"tag1\""))
  
  // Verify binary format contains expected data (simplified checks)
  assert_eq(binary_data[0], 11)  // Length of "test-12345"
  assert_eq(binary_data[5], 116)  // 't'
  assert_eq(binary_data[16], 11)  // Length of "Test Object"
  assert_eq(binary_data[20], 84)  // 'T'
  
  // Verify Protocol Buffers contains expected data (simplified checks)
  assert_eq(protobuf_data[0], 0x0A)  // Key for field 1
  assert_eq(protobuf_data[1], 11)    // Length of "test-12345"
  assert_eq(protobuf_data[2], 116)  // 't'
}

// Test 5: Serialization Error Handling
test "serialization error handling" {
  // Define serialization error types
  enum SerializationError {
    InvalidFormat(String)
    BufferOverflow
    TypeMismatch
    MissingField(String)
    InvalidValue(String)
  }
  
  // Define serialization result
  type SerializationResult[T] = 
    Success(T)
    | Failure(SerializationError)
  
  // Safe JSON string serialization
  let safe_serialize_string = fn(value: String) {
    if value.length() > 1000 {
      Failure(SerializationError::InvalidValue("String too long"))
    } else {
      Success("\"" + value + "\"")
    }
  }
  
  // Safe JSON number serialization
  let safe_serialize_number = fn(value: Int) {
    if value > 1000000 || value < -1000000 {
      Failure(SerializationError::InvalidValue("Number out of range"))
    } else {
      Success(value.to_string())
    }
  }
  
  // Safe JSON object serialization
  let safe_serialize_object = fn(
    id: String,
    name: String,
    value: Int,
    active: Bool
  ) {
    match safe_serialize_string(id) {
      Success(id_json) => {
        match safe_serialize_string(name) {
          Success(name_json) => {
            match safe_serialize_number(value) {
              Success(value_json) => {
                let active_json = if active { "true" } else { "false" }
                Success(
                  "{"
                  + "\"id\":" + id_json + "," 
                  + "\"name\":" + name_json + "," 
                  + "\"value\":" + value_json + "," 
                  + "\"active\":" + active_json
                  + "}"
                )
              }
              Failure(error) => Failure(error)
            }
          }
          Failure(error) => Failure(error)
        }
      }
      Failure(error) => Failure(error)
    }
  }
  
  // Safe JSON deserialization
  let safe_deserialize_string = fn(json: String) {
    if json.length() < 2 or not(json.starts_with("\"") or json.ends_with("\"")) {
      Failure(SerializationError::InvalidFormat("Not a valid JSON string"))
    } else {
      Success(json.substring(1, json.length() - 2))
    }
  }
  
  let safe_deserialize_number = fn(json: String) {
    if json.length() == 0 {
      Failure(SerializationError::InvalidFormat("Empty number"))
    } else {
      let mut valid = true
      for i in 0 .. json.length() {
        let c = json.to_char_array()[i]
        if not((c >= '0' and c <= '9') or c == '-') {
          valid = false
          break
        }
      }
      
      if valid {
        Success(json.to_int())
      } else {
        Failure(SerializationError::InvalidFormat("Invalid number format"))
      }
    }
  }
  
  // Test successful serialization
  match safe_serialize_object("test-123", "Test Object", 42, true) {
    Success(json) => {
      assert_true(json.contains("\"id\":\"test-123\""))
      assert_true(json.contains("\"name\":\"Test Object\""))
      assert_true(json.contains("\"value\":42"))
      assert_true(json.contains("\"active\":true"))
    }
    Failure(_) => assert_true(false)
  }
  
  // Test serialization error - string too long
  let long_string = "a".repeat(1001)
  match safe_serialize_object(long_string, "Test Object", 42, true) {
    Success(_) => assert_true(false)
    Failure(SerializationError::InvalidValue(message)) => {
      assert_eq(message, "String too long")
    }
    Failure(_) => assert_true(false)
  }
  
  // Test serialization error - number out of range
  match safe_serialize_object("test-123", "Test Object", 2000000, true) {
    Success(_) => assert_true(false)
    Failure(SerializationError::InvalidValue(message)) => {
      assert_eq(message, "Number out of range")
    }
    Failure(_) => assert_true(false)
  }
  
  // Test successful deserialization
  match safe_deserialize_string("\"hello world\"") {
    Success(value) => assert_eq(value, "hello world")
    Failure(_) => assert_true(false)
  }
  
  match safe_deserialize_number("12345") {
    Success(value) => assert_eq(value, 12345)
    Failure(_) => assert_true(false)
  }
  
  // Test deserialization error - invalid string format
  match safe_deserialize_string("hello world") {
    Success(_) => assert_true(false)
    Failure(SerializationError::InvalidFormat(message)) => {
      assert_eq(message, "Not a valid JSON string")
    }
    Failure(_) => assert_true(false)
  }
  
  // Test deserialization error - invalid number format
  match safe_deserialize_number("12a45") {
    Success(_) => assert_true(false)
    Failure(SerializationError::InvalidFormat(message)) => {
      assert_eq(message, "Invalid number format")
    }
    Failure(_) => assert_true(false)
  }
}