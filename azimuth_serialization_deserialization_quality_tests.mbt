// Azimuth Data Serialization/Deserialization Test Suite
// This file contains high-quality test cases for data serialization and deserialization

// Test 1: JSON Serialization/Deserialization
test "json serialization/deserialization" {
  // Define JSON value
  enum JsonValue {
    Null
    Bool(Bool)
    Int(Int)
    Float(Float)
    String(String)
    Array(Array[JsonValue])
    Object(Map[String, JsonValue])
  }
  
  // Define telemetry span
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Map[String, String]
  }
  
  // Convert string to JSON value
  let string_to_json_value = fn(value: String) {
    JsonValue::String(value)
  }
  
  // Convert int to JSON value
  let int_to_json_value = fn(value: Int) {
    JsonValue::Int(value)
  }
  
  // Convert option to JSON value
  let option_to_json_value = fn(opt: Option[String]) {
    match opt {
      Some(value) => JsonValue::String(value)
      None => JsonValue::Null
    }
  }
  
  // Convert map to JSON object
  let map_to_json_object = fn(map: Map[String, String]) {
    let mut json_object = []
    
    for i in 0..map.length() {
      let entry = map[i]
      json_object = json_object.push((entry.0, JsonValue::String(entry.1)))
    }
    
    JsonValue::Object(json_object)
  }
  
  // Serialize telemetry span to JSON
  let serialize_span_to_json = fn(span: TelemetrySpan) {
    let json_object = [
      ("trace_id", string_to_json_value(span.trace_id)),
      ("span_id", string_to_json_value(span.span_id)),
      ("parent_span_id", option_to_json_value(span.parent_span_id)),
      ("operation_name", string_to_json_value(span.operation_name)),
      ("start_time", int_to_json_value(span.start_time)),
      ("end_time", int_to_json_value(span.end_time)),
      ("status", string_to_json_value(span.status)),
      ("tags", map_to_json_object(span.tags))
    ]
    
    JsonValue::Object(json_object)
  }
  
  // Convert JSON value to string (simplified)
  let json_value_to_string = fn(value: JsonValue) {
    match value {
      JsonValue::Null => "null"
      JsonValue::Bool(b) => if b { "true" } else { "false" }
      JsonValue::Int(i) => i.to_string()
      JsonValue::Float(f) => f.to_string()
      JsonValue::String(s) => "\"" + s + "\""
      JsonValue::Array(arr) => {
        let mut elements = []
        for i in 0..arr.length() {
          elements = elements.push(json_value_to_string(arr[i]))
        }
        "[" + elements.join(",") + "]"
      }
      JsonValue::Object(obj) => {
        let mut properties = []
        for i in 0..obj.length() {
          let prop = obj[i]
          properties = properties.push("\"" + prop.0 + "\":" + json_value_to_string(prop.1))
        }
        "{" + properties.join(",") + "}"
      }
    }
  }
  
  // Parse string to JSON value (simplified)
  let parse_json_string = fn(json_str: String) {
    if json_str == "null" {
      JsonValue::Null
    } else if json_str == "true" {
      JsonValue::Bool(true)
    } else if json_str == "false" {
      JsonValue::Bool(false)
    } else if json_str.starts_with("\"") and json_str.ends_with("\"") {
      JsonValue::String(json_str.substring(1, json_str.length() - 2))
    } else if json_str.contains(".") {
      JsonValue::Float(json_str.to_float())
    } else {
      JsonValue::Int(json_str.to_int())
    }
  }
  
  // Extract string from JSON value
  let extract_string_from_json = fn(value: JsonValue) {
    match value {
      JsonValue::String(s) => Some(s)
      _ => None
    }
  }
  
  // Extract int from JSON value
  let extract_int_from_json = fn(value: JsonValue) {
    match value {
      JsonValue::Int(i) => Some(i)
      _ => None
    }
  }
  
  // Extract option from JSON value
  let extract_option_from_json = fn(value: JsonValue) {
    match value {
      JsonValue::Null => None
      JsonValue::String(s) => Some(s)
      _ => None
    }
  }
  
  // Extract map from JSON object
  let extract_map_from_json_object = fn(value: JsonValue) {
    match value {
      JsonValue::Object(obj) => {
        let mut map = []
        for i in 0..obj.length() {
          let prop = obj[i]
          match prop.1 {
            JsonValue::String(s) => map = map.push((prop.0, s))
            _ => {}
          }
        }
        Some(map)
      }
      _ => None
    }
  }
  
  // Deserialize JSON to telemetry span
  let deserialize_json_to_span = fn(json: JsonValue) {
    match json {
      JsonValue::Object(obj) => {
        let trace_id = match obj.find(fn(p) { p.0 == "trace_id" }) {
          Some((_, value)) => extract_string_from_json(value)
          None => None
        }
        
        let span_id = match obj.find(fn(p) { p.0 == "span_id" }) {
          Some((_, value)) => extract_string_from_json(value)
          None => None
        }
        
        let parent_span_id = match obj.find(fn(p) { p.0 == "parent_span_id" }) {
          Some((_, value)) => extract_option_from_json(value)
          None => None
        }
        
        let operation_name = match obj.find(fn(p) { p.0 == "operation_name" }) {
          Some((_, value)) => extract_string_from_json(value)
          None => None
        }
        
        let start_time = match obj.find(fn(p) { p.0 == "start_time" }) {
          Some((_, value)) => extract_int_from_json(value)
          None => None
        }
        
        let end_time = match obj.find(fn(p) { p.0 == "end_time" }) {
          Some((_, value)) => extract_int_from_json(value)
          None => None
        }
        
        let status = match obj.find(fn(p) { p.0 == "status" }) {
          Some((_, value)) => extract_string_from_json(value)
          None => None
        }
        
        let tags = match obj.find(fn(p) { p.0 == "tags" }) {
          Some((_, value)) => extract_map_from_json_object(value)
          None => None
        }
        
        // Check if all required fields are present
        match (trace_id, span_id, operation_name, start_time, end_time, status, tags) {
          (Some(tid), Some(sid), Some(op), Some(st), Some(et), Some(stat), Some(t)) => {
            Some({
              trace_id: tid,
              span_id: sid,
              parent_span_id: parent_span_id,
              operation_name: op,
              start_time: st,
              end_time: et,
              status: stat,
              tags: t
            })
          }
          _ => None
        }
      }
      _ => None
    }
  }
  
  // Test JSON serialization/deserialization
  let original_span = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    parent_span_id: Some("a7ad6b7169203330"),
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    tags: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users"),
      ("service.name", "payment-service")
    ]
  }
  
  // Serialize to JSON
  let json_value = serialize_span_to_json(original_span)
  let json_string = json_value_to_string(json_value)
  
  // Verify JSON contains expected values
  assert_true(json_string.contains("\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\""))
  assert_true(json_string.contains("\"span_id\":\"b7ad6b7169203331\""))
  assert_true(json_string.contains("\"parent_span_id\":\"a7ad6b7169203330\""))
  assert_true(json_string.contains("\"operation_name\":\"database.query\""))
  assert_true(json_string.contains("\"start_time\":1640995200"))
  assert_true(json_string.contains("\"end_time\":1640995250"))
  assert_true(json_string.contains("\"status\":\"ok\""))
  assert_true(json_string.contains("\"db.type\":\"postgresql\""))
  
  // Deserialize from JSON
  let deserialized_span = deserialize_json_to_span(json_value)
  
  match deserialized_span {
    Some(span) => {
      assert_eq(span.trace_id, original_span.trace_id)
      assert_eq(span.span_id, original_span.span_id)
      assert_eq(span.parent_span_id, original_span.parent_span_id)
      assert_eq(span.operation_name, original_span.operation_name)
      assert_eq(span.start_time, original_span.start_time)
      assert_eq(span.end_time, original_span.end_time)
      assert_eq(span.status, original_span.status)
      assert_eq(span.tags, original_span.tags)
    }
    None => assert_true(false)
  }
  
  // Test with null parent_span_id
  let span_without_parent = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    parent_span_id: None,
    operation_name: "root.operation",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    tags: []
  }
  
  let json_without_parent = serialize_span_to_json(span_without_parent)
  let json_string_without_parent = json_value_to_string(json_without_parent)
  
  assert_true(json_string_without_parent.contains("\"parent_span_id\":null"))
  
  let deserialized_without_parent = deserialize_json_to_span(json_without_parent)
  match deserialized_without_parent {
    Some(span) => {
      assert_eq(span.parent_span_id, None)
    }
    None => assert_true(false)
  }
}

// Test 2: Protocol Buffer Serialization/Deserialization
test "protocol buffer serialization/deserialization" {
  // Define wire types
  enum WireType {
    Varint  // 0
    Fixed64 // 1
    LengthDelimited // 2
    StartGroup // 3 (deprecated)
    EndGroup // 4 (deprecated)
    Fixed32 // 5
  }
  
  // Define field
  type Field = {
    field_number: Int,
    wire_type: WireType,
    value: Array[Int]
  }
  
  // Define telemetry metric
  type TelemetryMetric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    labels: Map[String, String]
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
  let decode_varint = fn(bytes: Array[Int], offset: Int) {
    let mut result = 0
    let mut shift = 0
    let mut index = offset
    
    while index < bytes.length() and (bytes[index] & 0x80) != 0 {
      result = result | ((bytes[index] & 0x7F) << shift)
      shift = shift + 7
      index = index + 1
    }
    
    if index < bytes.length() {
      result = result | (bytes[index] << shift)
      index = index + 1
    }
    
    (result, index)
  }
  
  // Encode field key
  let encode_field_key = fn(field_number: Int, wire_type: WireType) {
    let wire_type_value = match wire_type {
      Varint => 0
      Fixed64 => 1
      LengthDelimited => 2
      StartGroup => 3
      EndGroup => 4
      Fixed32 => 5
    }
    
    (field_number << 3) | wire_type_value
  }
  
  // Decode field key
  let decode_field_key = fn(key: Int) {
    let field_number = key >> 3
    let wire_type_value = key & 0x07
    
    let wire_type = match wire_type_value {
      0 => Varint
      1 => Fixed64
      2 => LengthDelimited
      3 => StartGroup
      4 => EndGroup
      5 => Fixed32
      _ => Varint  // Default
    }
    
    (field_number, wire_type)
  }
  
  // Encode string field
  let encode_string_field = fn(field_number: Int, value: String) {
    let key = encode_field_key(field_number, LengthDelimited)
    let key_bytes = encode_varint(key)
    
    let str_bytes = value.to_utf8_array()
    let length_bytes = encode_varint(str_bytes.length())
    
    key_bytes + length_bytes + str_bytes
  }
  
  // Encode double field
  let encode_double_field = fn(field_number: Int, value: Float) {
    let key = encode_field_key(field_number, Fixed64)
    let key_bytes = encode_varint(key)
    
    // Convert float to 64-bit representation (simplified)
    let int_value = value as Int
    let value_bytes = [
      int_value & 0xFF,
      (int_value >> 8) & 0xFF,
      (int_value >> 16) & 0xFF,
      (int_value >> 24) & 0xFF,
      (int_value >> 32) & 0xFF,
      (int_value >> 40) & 0xFF,
      (int_value >> 48) & 0xFF,
      (int_value >> 56) & 0xFF
    ]
    
    key_bytes + value_bytes
  }
  
  // Encode int64 field
  let encode_int64_field = fn(field_number: Int, value: Int) {
    let key = encode_field_key(field_number, Varint)
    let key_bytes = encode_varint(key)
    let value_bytes = encode_varint(value)
    
    key_bytes + value_bytes
  }
  
  // Serialize telemetry metric to protocol buffer
  let serialize_metric_to_protobuf = fn(metric: TelemetryMetric) {
    let mut bytes = []
    
    // Field 1: name (string)
    bytes = bytes + encode_string_field(1, metric.name)
    
    // Field 2: value (double)
    bytes = bytes + encode_double_field(2, metric.value)
    
    // Field 3: unit (string)
    bytes = bytes + encode_string_field(3, metric.unit)
    
    // Field 4: timestamp (int64)
    bytes = bytes + encode_int64_field(4, metric.timestamp)
    
    // Field 5: labels (repeated string message)
    for i in 0..metric.labels.length() {
      let label = metric.labels[i]
      
      // Encode label as a message with key and value
      let mut label_bytes = []
      
      // Label key
      label_bytes = label_bytes + encode_string_field(1, label.0)
      
      // Label value
      label_bytes = label_bytes + encode_string_field(2, label.1)
      
      // Wrap in length-delimited field
      let key = encode_field_key(5, LengthDelimited)
      let key_bytes = encode_varint(key)
      let length_bytes = encode_varint(label_bytes.length())
      
      bytes = bytes + key_bytes + length_bytes + label_bytes
    }
    
    bytes
  }
  
  // Decode string field
  let decode_string_field = fn(bytes: Array[Int], offset: Int) {
    if offset >= bytes.length() {
      (None, offset)
    } else {
      let (length, new_offset) = decode_varint(bytes, offset)
      
      if new_offset + length <= bytes.length() {
        let str_bytes = bytes.slice(new_offset, new_offset + length)
        let str_value = str_bytes.to_ascii_string()
        
        (Some(str_value), new_offset + length)
      } else {
        (None, offset)
      }
    }
  }
  
  // Decode double field
  let decode_double_field = fn(bytes: Array[Int], offset: Int) {
    if offset + 8 <= bytes.length() {
      let b0 = bytes[offset]
      let b1 = bytes[offset + 1]
      let b2 = bytes[offset + 2]
      let b3 = bytes[offset + 3]
      let b4 = bytes[offset + 4]
      let b5 = bytes[offset + 5]
      let b6 = bytes[offset + 6]
      let b7 = bytes[offset + 7]
      
      let int_value = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24) | 
                     (b4 << 32) | (b5 << 40) | (b6 << 48) | (b7 << 56)
      
      (Some(int_value as Float), offset + 8)
    } else {
      (None, offset)
    }
  }
  
  // Decode int64 field
  let decode_int64_field = fn(bytes: Array[Int], offset: Int) {
    decode_varint(bytes, offset)
  }
  
  // Deserialize protocol buffer to telemetry metric
  let deserialize_protobuf_to_metric = fn(bytes: Array[Int]) {
    let mut index = 0
    let mut name = None
    let mut value = None
    let mut unit = None
    let mut timestamp = None
    let mut labels = []
    
    while index < bytes.length() {
      let (key, key_offset) = decode_varint(bytes, index)
      let (field_number, wire_type) = decode_field_key(key)
      index = key_offset
      
      match field_number {
        1 => {  // name
          let (str_value, new_offset) = decode_string_field(bytes, index)
          name = str_value
          index = new_offset
        }
        2 => {  // value
          let (double_value, new_offset) = decode_double_field(bytes, index)
          value = double_value
          index = new_offset
        }
        3 => {  // unit
          let (str_value, new_offset) = decode_string_field(bytes, index)
          unit = str_value
          index = new_offset
        }
        4 => {  // timestamp
          let (int_value, new_offset) = decode_int64_field(bytes, index)
          timestamp = Some(int_value)
          index = new_offset
        }
        5 => {  // labels
          let (length, length_offset) = decode_varint(bytes, index)
          index = length_offset
          
          if index + length <= bytes.length() {
            let label_bytes = bytes.slice(index, index + length)
            let mut label_index = 0
            let mut label_key = None
            let mut label_value = None
            
            while label_index < label_bytes.length() {
              let (label_key, key_offset) = decode_varint(label_bytes, label_index)
              let (label_field_number, label_wire_type) = decode_field_key(label_key)
              label_index = key_offset
              
              match label_field_number {
                1 => {  // label key
                  let (str_value, new_offset) = decode_string_field(label_bytes, label_index)
                  label_key = str_value
                  label_index = new_offset
                }
                2 => {  // label value
                  let (str_value, new_offset) = decode_string_field(label_bytes, label_index)
                  label_value = str_value
                  label_index = new_offset
                }
                _ => {
                  // Skip unknown field
                  label_index = label_index + 1
                }
              }
            }
            
            match (label_key, label_value) {
              (Some(key), Some(val)) => {
                labels = labels.push((key, val))
              }
              _ => {}
            }
            
            index = index + length
          }
        }
        _ => {
          // Skip unknown field
          index = index + 1
        }
      }
    }
    
    match (name, value, unit, timestamp) {
      (Some(n), Some(v), Some(u), Some(t)) => {
        Some({
          name: n,
          value: v,
          unit: u,
          timestamp: t,
          labels: labels
        })
      }
      _ => None
    }
  }
  
  // Test protocol buffer serialization/deserialization
  let original_metric = {
    name: "cpu.usage",
    value: 75.5,
    unit: "percent",
    timestamp: 1640995200,
    labels: [
      ("service.name", "api-gateway"),
      ("host.name", "server-1"),
      ("environment", "production")
    ]
  }
  
  // Serialize to protocol buffer
  let protobuf_bytes = serialize_metric_to_protobuf(original_metric)
  
  assert_true(protobuf_bytes.length() > 0)
  
  // Deserialize from protocol buffer
  let deserialized_metric = deserialize_protobuf_to_metric(protobuf_bytes)
  
  match deserialized_metric {
    Some(metric) => {
      assert_eq(metric.name, original_metric.name)
      assert_eq(metric.value, original_metric.value)
      assert_eq(metric.unit, original_metric.unit)
      assert_eq(metric.timestamp, original_metric.timestamp)
      assert_eq(metric.labels, original_metric.labels)
    }
    None => assert_true(false)
  }
}

// Test 3: Binary Serialization with Versioning
test "binary serialization with versioning" {
  // Define serialization version
  type SerializationVersion = {
    major: Int,
    minor: Int,
    patch: Int
  }
  
  // Define binary serializer
  type BinarySerializer = {
    version: SerializationVersion,
    endianness: String  // "little" or "big"
  }
  
  // Define telemetry event
  type TelemetryEvent = {
    event_id: String,
    event_type: String,
    timestamp: Int,
    data: Map[String, String]
  }
  
  // Create binary serializer
  let create_binary_serializer = fn(major: Int, minor: Int, patch: Int, endianness: String) {
    {
      version: { major: major, minor: minor, patch: patch },
      endianness: endianness
    }
  }
  
  // Write string to bytes
  let write_string = fn(str: String) {
    let str_bytes = str.to_utf8_array()
    let length_bytes = [
      str_bytes.length() & 0xFF,
      (str_bytes.length() >> 8) & 0xFF,
      (str_bytes.length() >> 16) & 0xFF,
      (str_bytes.length() >> 24) & 0xFF
    ]
    
    length_bytes + str_bytes
  }
  
  // Read string from bytes
  let read_string = fn(bytes: Array[Int], offset: Int) {
    if offset + 4 <= bytes.length() {
      let length = bytes[offset] | (bytes[offset + 1] << 8) | 
                  (bytes[offset + 2] << 16) | (bytes[offset + 3] << 24)
      
      if offset + 4 + length <= bytes.length() {
        let str_bytes = bytes.slice(offset + 4, offset + 4 + length)
        let str_value = str_bytes.to_ascii_string()
        
        (Some(str_value), offset + 4 + length)
      } else {
        (None, offset)
      }
    } else {
      (None, offset)
    }
  }
  
  // Write int to bytes
  let write_int = fn(value: Int, endianness: String) {
    let bytes = [
      value & 0xFF,
      (value >> 8) & 0xFF,
      (value >> 16) & 0xFF,
      (value >> 24) & 0xFF
    ]
    
    if endianness == "big" {
      [bytes[3], bytes[2], bytes[1], bytes[0]]
    } else {
      bytes
    }
  }
  
  // Read int from bytes
  let read_int = fn(bytes: Array[Int], offset: Int, endianness: String) {
    if offset + 4 <= bytes.length() {
      let value = if endianness == "big" {
        (bytes[offset] << 24) | (bytes[offset + 1] << 16) | 
        (bytes[offset + 2] << 8) | bytes[offset + 3]
      } else {
        bytes[offset] | (bytes[offset + 1] << 8) | 
        (bytes[offset + 2] << 16) | (bytes[offset + 3] << 24)
      }
      
      (Some(value), offset + 4)
    } else {
      (None, offset)
    }
  }
  
  // Serialize telemetry event to binary
  let serialize_event_to_binary = fn(event: TelemetryEvent, serializer: BinarySerializer) {
    let mut bytes = []
    
    // Write version header
    bytes = bytes.push(serializer.version.major)
    bytes = bytes.push(serializer.version.minor)
    bytes = bytes.push(serializer.version.patch)
    
    // Write event_id
    bytes = bytes + write_string(event.event_id)
    
    // Write event_type
    bytes = bytes + write_string(event.event_type)
    
    // Write timestamp
    bytes = bytes + write_int(event.timestamp, serializer.endianness)
    
    // Write data count
    bytes = bytes.push(event.data.length())
    
    // Write data entries
    for i in 0..event.data.length() {
      let entry = event.data[i]
      bytes = bytes + write_string(entry.0)  // key
      bytes = bytes + write_string(entry.1)  // value
    }
    
    bytes
  }
  
  // Deserialize binary to telemetry event
  let deserialize_binary_to_event = fn(bytes: Array[Int]) {
    if bytes.length() < 3 {
      None
    } else {
      // Read version header
      let major = bytes[0]
      let minor = bytes[1]
      let patch = bytes[2]
      
      // Determine endianness based on version
      let endianness = if major == 1 { "little" } else { "big" }
      
      let mut index = 3
      
      // Read event_id
      let (event_id, new_index1) = read_string(bytes, index)
      index = new_index1
      
      // Read event_type
      let (event_type, new_index2) = read_string(bytes, index)
      index = new_index2
      
      // Read timestamp
      let (timestamp, new_index3) = read_int(bytes, index, endianness)
      index = new_index3
      
      // Read data count
      let data_count = if index < bytes.length() { bytes[index] } else { 0 }
      index = index + 1
      
      // Read data entries
      let mut data = []
      for i in 0..data_count {
        let (key, new_index4) = read_string(bytes, index)
        index = new_index4
        
        let (value, new_index5) = read_string(bytes, index)
        index = new_index5
        
        match (key, value) {
          (Some(k), Some(v)) => data = data.push((k, v))
          _ => {}
        }
      }
      
      match (event_id, event_type, timestamp) {
        (Some(eid), Some(etype), Some(ts)) => {
          Some({
            event_id: eid,
            event_type: etype,
            timestamp: ts,
            data: data
          })
        }
        _ => None
      }
    }
  }
  
  // Test binary serialization with versioning
  let v1_serializer = create_binary_serializer(1, 0, 0, "little")
  let v2_serializer = create_binary_serializer(2, 0, 0, "big")
  
  let original_event = {
    event_id: "event-12345",
    event_type: "user.login",
    timestamp: 1640995200,
    data: [
      ("user.id", "user-67890"),
      ("ip.address", "192.168.1.1"),
      ("user.agent", "Mozilla/5.0")
    ]
  }
  
  // Serialize with v1
  let v1_bytes = serialize_event_to_binary(original_event, v1_serializer)
  
  // Verify version header
  assert_eq(v1_bytes[0], 1)  // major
  assert_eq(v1_bytes[1], 0)  // minor
  assert_eq(v1_bytes[2], 0)  // patch
  
  // Deserialize with v1
  let v1_deserialized = deserialize_binary_to_event(v1_bytes)
  
  match v1_deserialized {
    Some(event) => {
      assert_eq(event.event_id, original_event.event_id)
      assert_eq(event.event_type, original_event.event_type)
      assert_eq(event.timestamp, original_event.timestamp)
      assert_eq(event.data, original_event.data)
    }
    None => assert_true(false)
  }
  
  // Serialize with v2
  let v2_bytes = serialize_event_to_binary(original_event, v2_serializer)
  
  // Verify version header
  assert_eq(v2_bytes[0], 2)  // major
  assert_eq(v2_bytes[1], 0)  // minor
  assert_eq(v2_bytes[2], 0)  // patch
  
  // Deserialize with v2
  let v2_deserialized = deserialize_binary_to_event(v2_bytes)
  
  match v2_deserialized {
    Some(event) => {
      assert_eq(event.event_id, original_event.event_id)
      assert_eq(event.event_type, original_event.event_type)
      assert_eq(event.timestamp, original_event.timestamp)
      assert_eq(event.data, original_event.data)
    }
    None => assert_true(false)
  }
  
  // Test cross-version compatibility
  // v1 should be able to deserialize v2 (with proper version handling)
  let v2_deserialized_by_v1 = deserialize_binary_to_event(v2_bytes)
  
  match v2_deserialized_by_v1 {
    Some(event) => {
      assert_eq(event.event_id, original_event.event_id)
      assert_eq(event.event_type, original_event.event_type)
      assert_eq(event.timestamp, original_event.timestamp)
      assert_eq(event.data, original_event.data)
    }
    None => assert_true(false)
  }
}

// Test 4: Custom Serialization Format
test "custom serialization format" {
  // Define custom format field
  type CustomField = {
    name: String,
    type: String,
    value: String
  }
  
  // Define custom record
  type CustomRecord = {
    record_type: String,
    record_id: String,
    timestamp: Int,
    fields: Array[CustomField]
  }
  
  // Define custom serializer
  type CustomSerializer = {
    field_separator: String,
    record_separator: String,
    escape_character: String
  }
  
  // Create custom serializer
  let create_custom_serializer = fn(field_separator: String, record_separator: String, escape_character: String) {
    {
      field_separator: field_separator,
      record_separator: record_separator,
      escape_character: escape_character
    }
  }
  
  // Escape value for custom format
  let escape_value = fn(value: String, serializer: CustomSerializer) {
    let escaped = value.replace(serializer.field_separator, serializer.escape_character + serializer.field_separator)
    escaped.replace(serializer.record_separator, serializer.escape_character + serializer.record_separator)
  }
  
  // Unescape value from custom format
  let unescape_value = fn(value: String, serializer: CustomSerializer) {
    let unescaped = value.replace(serializer.escape_character + serializer.record_separator, serializer.record_separator)
    unescaped.replace(serializer.escape_character + serializer.field_separator, serializer.field_separator)
  }
  
  // Serialize custom record to string
  let serialize_record_to_custom = fn(record: CustomRecord, serializer: CustomSerializer) {
    let mut parts = []
    
    // Add record_type
    parts = parts.push(escape_value(record.record_type, serializer))
    
    // Add record_id
    parts = parts.push(escape_value(record.record_id, serializer))
    
    // Add timestamp
    parts = parts.push(record.timestamp.to_string())
    
    // Add fields
    for i in 0..record.fields.length() {
      let field = record.fields[i]
      let field_str = field.name + ":" + field.type + ":" + escape_value(field.value, serializer)
      parts = parts.push(field_str)
    }
    
    parts.join(serializer.field_separator)
  }
  
  // Parse custom format string to record
  let parse_custom_to_record = fn(str: String, serializer: CustomSerializer) {
    let parts = str.split(serializer.field_separator)
    
    if parts.length() >= 3 {
      let record_type = unescape_value(parts[0], serializer)
      let record_id = unescape_value(parts[1], serializer)
      let timestamp = parts[2].to_int()
      
      let mut fields = []
      for i in 3..parts.length() {
        let field_str = parts[i]
        let field_parts = field_str.split(":")
        
        if field_parts.length() >= 3 {
          let field_name = field_parts[0]
          let field_type = field_parts[1]
          let field_value = unescape_value(field_parts.slice(1).join(":"), serializer)
          
          fields = fields.push({
            name: field_name,
            type: field_type,
            value: field_value
          })
        }
      }
      
      Some({
        record_type: record_type,
        record_id: record_id,
        timestamp: timestamp,
        fields: fields
      })
    } else {
      None
    }
  }
  
  // Test custom serialization format
  let serializer = create_custom_serializer("|", "\n", "\\")
  
  let original_record = {
    record_type: "telemetry.span",
    record_id: "span-12345",
    timestamp: 1640995200,
    fields: [
      { name: "trace_id", type: "string", value: "0af7651916cd43dd8448eb211c80319c" },
      { name: "operation", type: "string", value: "database.query|SELECT * FROM users" },
      { name: "duration", type: "int", value: "250" },
      { name: "status", type: "string", value: "ok" }
    ]
  }
  
  // Serialize to custom format
  let custom_string = serialize_record_to_custom(original_record, serializer)
  
  // Verify custom string contains expected values
  assert_true(custom_string.starts_with("telemetry.span|span-12345|1640995200"))
  assert_true(custom_string.contains("trace_id:string:0af7651916cd43dd8448eb211c80319c"))
  assert_true(custom_string.contains("operation:string:database.query\\|SELECT * FROM users"))
  assert_true(custom_string.contains("duration:int:250"))
  assert_true(custom_string.contains("status:string:ok"))
  
  // Parse from custom format
  let parsed_record = parse_custom_to_record(custom_string, serializer)
  
  match parsed_record {
    Some(record) => {
      assert_eq(record.record_type, original_record.record_type)
      assert_eq(record.record_id, original_record.record_id)
      assert_eq(record.timestamp, original_record.timestamp)
      assert_eq(record.fields, original_record.fields)
    }
    None => assert_true(false)
  }
  
  // Test with field separator in value
  let record_with_separator = {
    record_type: "telemetry.log",
    record_id: "log-67890",
    timestamp: 1640995250,
    fields: [
      { name: "message", type: "string", value: "User login failed|Invalid password" },
      { name: "level", type: "string", value: "error" }
    ]
  }
  
  let custom_with_separator = serialize_record_to_custom(record_with_separator, serializer)
  
  // Verify the separator is escaped
  assert_true(custom_with_separator.contains("message:string:User login failed\\|Invalid password"))
  
  // Parse back
  let parsed_with_separator = parse_custom_to_record(custom_with_separator, serializer)
  
  match parsed_with_separator {
    Some(record) => {
      assert_eq(record.record_type, record_with_separator.record_type)
      assert_eq(record.record_id, record_with_separator.record_id)
      assert_eq(record.timestamp, record_with_separator.timestamp)
      assert_eq(record.fields, record_with_separator.fields)
    }
    None => assert_true(false)
  }
}

// Test 5: Serialization Performance and Efficiency
test "serialization performance and efficiency" {
  // Define serialization benchmark
  type SerializationBenchmark = {
    format: String,
    data_size: Int,
    serialization_time: Int,
    deserialization_time: Int,
    compressed_size: Int
  }
  
  // Define telemetry data set
  type TelemetryDataSet = {
    spans: Array[String],
    metrics: Array[String],
    logs: Array[String]
  }
  
  // Create test data set
  let create_test_data_set = fn(size: Int) {
    let mut spans = []
    let mut metrics = []
    let mut logs = []
    
    for i in 0..size {
      spans = spans.push("span-" + i.to_string())
      metrics = metrics.push("metric-" + i.to_string())
      logs = logs.push("log-" + i.to_string())
    }
    
    {
      spans: spans,
      metrics: metrics,
      logs: logs
    }
  }
  
  // Simple compression (run-length encoding)
  let simple_compress = fn(data: String) {
    if data.length() == 0 {
      ""
    } else {
      let mut compressed = []
      let mut i = 0
      
      while i < data.length() {
        let current_char = data[i]
        let mut count = 1
        
        while i + count < data.length() and data[i + count] == current_char {
          count = count + 1
        }
        
        if count > 3 {
          compressed = compressed.push(count.to_string() + current_char)
        } else {
          for j in 0..count {
            compressed = compressed.push(current_char.to_string())
          }
        }
        
        i = i + count
      }
      
      compressed.join("")
    }
  }
  
  // Simple decompression
  let simple_decompress = fn(compressed: String) {
    let mut decompressed = []
    let mut i = 0
    
    while i < compressed.length() {
      let char = compressed[i]
      
      if char >= '0' and char <= '9' {
        let mut count_str = ""
        let mut j = i
        
        while j < compressed.length() and compressed[j] >= '0' and compressed[j] <= '9' {
          count_str = count_str + compressed[j]
          j = j + 1
        }
        
        if j < compressed.length() {
          let count = count_str.to_int()
          let repeat_char = compressed[j]
          
          for k in 0..count {
            decompressed = decompressed.push(repeat_char)
          }
          
          i = j + 1
        } else {
          i = j
        }
      } else {
        decompressed = decompressed.push(char)
        i = i + 1
      }
    }
    
    decompressed.join("")
  }
  
  // Simulate serialization time
  let simulate_serialization_time = fn(data_size: Int, format: String) {
    let base_time = match format {
      "json" => 10
      "protobuf" => 5
      "binary" => 3
      "custom" => 15
      _ => 10
    }
    
    base_time * (data_size / 100 + 1)
  }
  
  // Simulate deserialization time
  let simulate_deserialization_time = fn(data_size: Int, format: String) {
    let base_time = match format {
      "json" => 12
      "protobuf" => 6
      "binary" => 4
      "custom" => 18
      _ => 12
    }
    
    base_time * (data_size / 100 + 1)
  }
  
  // Benchmark serialization format
  let benchmark_format = fn(data_set: TelemetryDataSet, format: String) {
    let data_size = data_set.spans.length() + data_set.metrics.length() + data_set.logs.length()
    
    // Simulate serialization
    let serialization_time = simulate_serialization_time(data_size, format)
    
    // Simulate deserialization
    let deserialization_time = simulate_deserialization_time(data_size, format)
    
    // Simulate compression
    let raw_data = format + ":" + data_size.to_string()
    let compressed_data = simple_compress(raw_data)
    let compressed_size = compressed_data.length()
    
    {
      format: format,
      data_size: data_size,
      serialization_time: serialization_time,
      deserialization_time: deserialization_time,
      compressed_size: compressed_size
    }
  }
  
  // Test serialization performance and efficiency
  let data_set = create_test_data_set(1000)
  
  // Benchmark different formats
  let json_benchmark = benchmark_format(data_set, "json")
  let protobuf_benchmark = benchmark_format(data_set, "protobuf")
  let binary_benchmark = benchmark_format(data_set, "binary")
  let custom_benchmark = benchmark_format(data_set, "custom")
  
  let benchmarks = [json_benchmark, protobuf_benchmark, binary_benchmark, custom_benchmark]
  
  // Verify all benchmarks have the same data size
  for i in 0..benchmarks.length() {
    assert_eq(benchmarks[i].data_size, 3000)  // 1000 spans + 1000 metrics + 1000 logs
  }
  
  // Verify binary is fastest for serialization
  assert_true(binary_benchmark.serialization_time < protobuf_benchmark.serialization_time)
  assert_true(protobuf_benchmark.serialization_time < json_benchmark.serialization_time)
  assert_true(json_benchmark.serialization_time < custom_benchmark.serialization_time)
  
  // Verify binary is fastest for deserialization
  assert_true(binary_benchmark.deserialization_time < protobuf_benchmark.deserialization_time)
  assert_true(protobuf_benchmark.deserialization_time < json_benchmark.deserialization_time)
  assert_true(json_benchmark.deserialization_time < custom_benchmark.deserialization_time)
  
  // Find most efficient format (lowest total time + size)
  let mut most_efficient = benchmarks[0]
  let mut lowest_score = benchmarks[0].serialization_time + benchmarks[0].deserialization_time + benchmarks[0].compressed_size
  
  for i in 1..benchmarks.length() {
    let score = benchmarks[i].serialization_time + benchmarks[i].deserialization_time + benchmarks[i].compressed_size
    if score < lowest_score {
      lowest_score = score
      most_efficient = benchmarks[i]
    }
  }
  
  // Binary should be most efficient
  assert_eq(most_efficient.format, "binary")
  
  // Test compression effectiveness
  let original_data = "AAAAABBBBBCCCCCDDDDDEEEEE"
  let compressed_data = simple_compress(original_data)
  let decompressed_data = simple_decompress(compressed_data)
  
  assert_eq(compressed_data, "5A5B5C5D5E")
  assert_eq(decompressed_data, original_data)
  assert_true(compressed_data.length() < original_data.length())
}

// Test 6: Serialization Error Handling
test "serialization error handling" {
  // Define serialization error
  enum SerializationError {
    InvalidFormat
    CorruptedData
    UnsupportedVersion
    MissingField(String)
    TypeMismatch
  }
  
  // Define serialization result
  type SerializationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[SerializationError]
  }
  
  // Define safe serializer
  type SafeSerializer = {
    supported_versions: Array[String],
    max_data_size: Int,
    required_fields: Array[String]
  }
  
  // Create safe serializer
  let create_safe_serializer = fn() {
    {
      supported_versions: ["1.0", "2.0"],
      max_data_size: 1024 * 1024,  // 1MB
      required_fields: ["id", "timestamp", "type"]
    }
  }
  
  // Validate data before serialization
  let validate_data_for_serialization = fn(data: Map[String, String], serializer: SafeSerializer) {
    // Check required fields
    for i in 0..serializer.required_fields.length() {
      let field = serializer.required_fields[i]
      let field_exists = data.some(fn(pair) { pair.0 == field })
      
      if not(field_exists) {
        return SerializationResult({
          success: false,
          data: None,
          error: Some(MissingField(field))
        })
      }
    }
    
    // Check data size
    let mut total_size = 0
    for i in 0..data.length() {
      total_size = total_size + data[i].0.length() + data[i].1.length()
    }
    
    if total_size > serializer.max_data_size {
      return SerializationResult({
        success: false,
        data: None,
        error: Some(CorruptedData)
      })
    }
    
    SerializationResult({
      success: true,
      data: Some(data),
      error: None
    })
  }
  
  // Safe serialize
  let safe_serialize = fn(data: Map[String, String], serializer: SafeSerializer, version: String) {
    // Check if version is supported
    if not(serializer.supported_versions.contains(version)) {
      return SerializationResult({
        success: false,
        data: None,
        error: Some(UnsupportedVersion)
      })
    }
    
    // Validate data
    let validation_result = validate_data_for_serialization(data, serializer)
    
    match validation_result.success {
      true => {
        // Serialize data (simplified)
        let mut serialized = version + ";"
        
        for i in 0..data.length() {
          let entry = data[i]
          serialized = serialized + entry.0 + ":" + entry.1 + ";"
        }
        
        SerializationResult({
          success: true,
          data: Some(serialized),
          error: None
        })
      }
      false => {
        validation_result
      }
    }
  }
  
  // Safe deserialize
  let safe_deserialize = fn(serialized: String, serializer: SafeSerializer) {
    if serialized.length() == 0 {
      return SerializationResult({
        success: false,
        data: None,
        error: Some(CorruptedData)
      })
    }
    
    // Extract version
    let parts = serialized.split(";")
    
    if parts.length() < 1 {
      return SerializationResult({
        success: false,
        data: None,
        error: Some(InvalidFormat)
      })
    }
    
    let version = parts[0]
    
    // Check if version is supported
    if not(serializer.supported_versions.contains(version)) {
      return SerializationResult({
        success: false,
        data: None,
        error: Some(UnsupportedVersion)
      })
    }
    
    // Parse data
    let mut data = []
    
    for i in 1..parts.length() {
      let part = parts[i]
      if part != "" {
        let kv_parts = part.split(":")
        
        if kv_parts.length() == 2 {
          data = data.push((kv_parts[0], kv_parts[1]))
        }
      }
    }
    
    // Validate deserialized data
    let validation_result = validate_data_for_serialization(data, serializer)
    
    match validation_result.success {
      true => {
        SerializationResult({
          success: true,
          data: Some(data),
          error: None
        })
      }
      false => {
        validation_result
      }
    }
  }
  
  // Test serialization error handling
  let serializer = create_safe_serializer()
  
  // Test valid data
  let valid_data = [
    ("id", "12345"),
    ("timestamp", "1640995200"),
    ("type", "span"),
    ("name", "operation")
  ]
  
  let valid_serialize_result = safe_serialize(valid_data, serializer, "1.0")
  assert_true(valid_serialize_result.success)
  
  match valid_serialize_result.data {
    Some(serialized) => {
      // Test valid deserialization
      let valid_deserialize_result = safe_deserialize(serialized, serializer)
      assert_true(valid_deserialize_result.success)
    }
    None => assert_true(false)
  }
  
  // Test missing required field
  let missing_field_data = [
    ("id", "12345"),
    // Missing "timestamp"
    ("type", "span")
  ]
  
  let missing_field_result = safe_serialize(missing_field_data, serializer, "1.0")
  assert_false(missing_field_result.success)
  
  match missing_field_result.error {
    Some(MissingField(field)) => assert_eq(field, "timestamp")
    _ => assert_true(false)
  }
  
  // Test unsupported version
  let unsupported_version_result = safe_serialize(valid_data, serializer, "3.0")
  assert_false(unsupported_version_result.success)
  
  match unsupported_version_result.error {
    Some(UnsupportedVersion) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test corrupted data
  let corrupted_data = "invalid;format"
  let corrupted_result = safe_deserialize(corrupted_data, serializer)
  assert_false(corrupted_result.success)
  
  match corrupted_result.error {
    Some(InvalidFormat) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test unsupported version in deserialization
  let valid_serialized = match valid_serialize_result.data {
    Some(data) => data
    None => ""
  }
  
  let unsupported_version_serialized = valid_serialized.replace("1.0", "3.0")
  let unsupported_deserialize_result = safe_deserialize(unsupported_version_serialized, serializer)
  assert_false(unsupported_deserialize_result.success)
  
  match unsupported_deserialize_result.error {
    Some(UnsupportedVersion) => assert_true(true)
    _ => assert_true(false)
  }
}