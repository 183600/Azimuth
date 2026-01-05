// Azimuth Serialization/Deserialization Comprehensive Test Suite
// This file contains comprehensive test cases for serialization and deserialization functionality

// Test 1: JSON Serialization/Deserialization
test "JSON serialization and deserialization for telemetry data" {
  // Define JSON types
  type JsonValue = 
    | JsonNull
    | JsonBool(Bool)
    | JsonNumber(Float)
    | JsonString(String)
    | JsonArray(Array[JsonValue])
    | JsonObject(Array[(String, JsonValue)])
  
  type SerializationError = {
    code: String,
    message: String
  }
  
  type SerializationResult[T] = 
    | Success(T)
    | Failure(SerializationError)
  
  // JSON operations
  let json_stringify = fn(value: JsonValue) {
    match value {
      JsonNull => "null"
      JsonBool(b) => if b { "true" } else { "false" }
      JsonNumber(n) => n.to_string()
      JsonString(s) => "\"" + s + "\""
      JsonArray(arr) => {
        let mut result = "["
        for i in 0..arr.length() {
          result = result + json_stringify(arr[i])
          if i < arr.length() - 1 {
            result = result + ","
          }
        }
        result + "]"
      }
      Object(obj) => {
        let mut result = "{"
        for i in 0..obj.length() {
          let (key, val) = obj[i]
          result = result + "\"" + key + "\":" + json_stringify(val)
          if i < obj.length() - 1 {
            result = result + ","
          }
        }
        result + "}"
      }
    }
  }
  
  let json_parse = fn(json_str: String) {
    // Simplified JSON parser for testing
    if json_str == "null" {
      Success(JsonNull)
    } else if json_str == "true" {
      Success(JsonBool(true))
    } else if json_str == "false" {
      Success(JsonBool(false))
    } else if json_str.starts_with("\"") and json_str.ends_with("\"") {
      // String value
      Success(JsonString(json_str.substring(1, json_str.length() - 2)))
    } else if json_str.starts_with("{") and json_str.ends_with("}") {
      // Object value
      Success(JsonObject([])) // Simplified
    } else if json_str.starts_with("[") and json_str.ends_with("]") {
      // Array value
      Success(JsonArray([])) // Simplified
    } else {
      // Try to parse as number
      match json_str.to_float() {
        Some(n) => Success(JsonNumber(n))
        None => Failure({ code: "PARSE_ERROR", message: "Invalid JSON" })
      }
    }
  }
  
  // Test basic JSON values
  let null_json = JsonNull
  let null_string = json_stringify(null_json)
  assert_eq(null_string, "null")
  
  let null_parsed = json_parse(null_string)
  match null_parsed {
    Success(JsonNull) => assert_true(true)
    _ => assert_true(false)
  }
  
  let bool_json = JsonBool(true)
  let bool_string = json_stringify(bool_json)
  assert_eq(bool_string, "true")
  
  let bool_parsed = json_parse(bool_string)
  match bool_parsed {
    Success(JsonBool(true)) => assert_true(true)
    _ => assert_true(false)
  }
  
  let number_json = JsonNumber(42.5)
  let number_string = json_stringify(number_json)
  assert_eq(number_string, "42.5")
  
  let number_parsed = json_parse(number_string)
  match number_parsed {
    Success(JsonNumber(n)) => assert_eq(n, 42.5)
    _ => assert_true(false)
  }
  
  let string_json = JsonString("hello world")
  let string_string = json_stringify(string_json)
  assert_eq(string_string, "\"hello world\"")
  
  let string_parsed = json_parse(string_string)
  match string_parsed {
    Success(JsonString(s)) => assert_eq(s, "hello world")
    _ => assert_true(false)
  }
  
  // Test array serialization
  let array_json = JsonArray([JsonString("item1"), JsonNumber(123), JsonBool(false)])
  let array_string = json_stringify(array_json)
  assert_true(array_string.starts_with("["))
  assert_true(array_string.ends_with("]"))
  assert_true(array_string.contains("\"item1\""))
  assert_true(array_string.contains("123"))
  assert_true(array_string.contains("false"))
  
  // Test object serialization
  let object_json = JsonObject([("name", JsonString("test")), ("value", JsonNumber(42))])
  let object_string = json_stringify(object_json)
  assert_true(object_string.starts_with("{"))
  assert_true(object_string.ends_with("}"))
  assert_true(object_string.contains("\"name\""))
  assert_true(object_string.contains("\"test\""))
  assert_true(object_string.contains("\"value\""))
  assert_true(object_string.contains("42"))
  
  // Test error handling
  let invalid_json = json_parse("invalid json")
  match invalid_json {
    Failure(error) => {
      assert_eq(error.code, "PARSE_ERROR")
      assert_eq(error.message, "Invalid JSON")
    }
    _ => assert_true(false)
  }
}

// Test 2: Protocol Buffer Serialization
test "protocol buffer serialization for telemetry data" {
  // Define Protocol Buffer types
  type ProtoField = {
    field_number: Int,
    wire_type: Int, // 0=Varint, 1=64-bit, 2=Length-delimited, 5=32-bit
    value: String
  }
  
  type ProtoMessage = {
    fields: Array[ProtoField]
  }
  
  // Protocol Buffer operations
  let encode_varint = fn(value: Int) {
    let mut result = ""
    let mut v = value
    
    while v >= 128 {
      let byte = (v & 127) | 128
      result = result + byte.to_string() + ","
      v = v >> 7
    }
    
    result = result + v.to_string()
    result
  }
  
  let decode_varint = fn(data: Array[Int]) {
    let mut result = 0
    let mut shift = 0
    
    for i in 0..data.length() {
      let byte = data[i]
      result = result + ((byte & 127) << shift)
      
      if (byte & 128) == 0 {
        break
      }
      
      shift = shift + 7
    }
    
    result
  }
  
  let serialize_field = fn(field: ProtoField) {
    let key = (field.field_number << 3) | field.wire_type
    
    match field.wire_type {
      0 => encode_varint(key) + "," + encode_varint(field.value.to_int())
      1 => encode_varint(key) + "," + field.value // 64-bit
      2 => encode_varint(key) + "," + encode_varint(field.value.length()) + "," + field.value
      5 => encode_varint(key) + "," + field.value // 32-bit
      _ => ""
    }
  }
  
  let serialize_message = fn(message: ProtoMessage) {
    let mut result = ""
    
    for i in 0..message.fields.length() {
      result = result + serialize_field(message.fields[i])
      if i < message.fields.length() - 1 {
        result = result + "|"
      }
    }
    
    result
  }
  
  let parse_field = fn(data: String) {
    let parts = data.split(",")
    if parts.length() >= 2 {
      let key = decode_varint(parts[0].split(",").map(fn(s) { s.to_int() }))
      let field_number = key >> 3
      let wire_type = key & 7
      
      Some({
        field_number,
        wire_type,
        value: if parts.length() > 2 { parts[2] } else { parts[1] }
      })
    } else {
      None
    }
  }
  
  let deserialize_message = fn(data: String) {
    let field_strings = data.split("|")
    let mut fields = []
    
    for field_str in field_strings {
      match parse_field(field_str) {
        Some(field) => fields = fields.push(field)
        None => ()
      }
    }
    
    { fields }
  }
  
  // Test varint encoding/decoding
  let encoded_varint = encode_varint(300)
  let decoded_varint = decode_varint(encoded_varint.split(",").map(fn(s) { s.to_int() }))
  assert_eq(decoded_varint, 300)
  
  // Test field serialization
  let field = {
    field_number: 1,
    wire_type: 2, // Length-delimited
    value: "test data"
  }
  
  let serialized_field = serialize_field(field)
  assert_true(serialized_field.contains(","))
  
  // Test message serialization
  let message = {
    fields: [
      { field_number: 1, wire_type: 2, value: "test" },
      { field_number: 2, wire_type: 0, value: "42" },
      { field_number: 3, wire_type: 2, value: "more data" }
    ]
  }
  
  let serialized_message = serialize_message(message)
  assert_true(serialized_message.contains("|"))
  
  // Test message deserialization
  let deserialized_message = deserialize_message(serialized_message)
  assert_eq(deserialized_message.fields.length(), 3)
  assert_eq(deserialized_message.fields[0].field_number, 1)
  assert_eq(deserialized_message.fields[1].field_number, 2)
  assert_eq(deserialized_message.fields[2].field_number, 3)
}

// Test 3: Binary Serialization
test "binary serialization for telemetry data" {
  // Define binary serialization types
  type BinaryWriter = {
    buffer: Array[Int],
    position: Int
  }
  
  type BinaryReader = {
    buffer: Array[Int],
    position: Int
  }
  
  // Binary operations
  let create_binary_writer = fn(initial_capacity: Int) {
    {
      buffer: Array::with_capacity(initial_capacity),
      position: 0
    }
  }
  
  let create_binary_reader = fn(buffer: Array[Int]) {
    {
      buffer,
      position: 0
    }
  }
  
  let write_byte = fn(writer: BinaryWriter, value: Int) {
    let new_buffer = writer.buffer.push(value)
    {
      buffer: new_buffer,
      position: writer.position + 1
    }
  }
  
  let write_bytes = fn(writer: BinaryWriter, values: Array[Int]) {
    let mut new_buffer = writer.buffer
    for value in values {
      new_buffer = new_buffer.push(value)
    }
    
    {
      buffer: new_buffer,
      position: writer.position + values.length()
    }
  }
  
  let write_int32 = fn(writer: BinaryWriter, value: Int) {
    let bytes = [
      value & 255,
      (value >> 8) & 255,
      (value >> 16) & 255,
      (value >> 24) & 255
    ]
    write_bytes(writer, bytes)
  }
  
  let write_string = fn(writer: BinaryWriter, value: String) {
    let length = value.length()
    let writer_with_length = write_int32(writer, length)
    
    let mut new_buffer = writer_with_length.buffer
    for i in 0..value.length() {
      new_buffer = new_buffer.push(value[i].to_int())
    }
    
    {
      buffer: new_buffer,
      position: writer_with_length.position + length
    }
  }
  
  let read_byte = fn(reader: BinaryReader) {
    if reader.position < reader.buffer.length() {
      (Some(reader.buffer[reader.position]), {
        buffer: reader.buffer,
        position: reader.position + 1
      })
    } else {
      (None, reader)
    }
  }
  
  let read_int32 = fn(reader: BinaryReader) {
    if reader.position + 4 <= reader.buffer.length() {
      let b1 = reader.buffer[reader.position]
      let b2 = reader.buffer[reader.position + 1]
      let b3 = reader.buffer[reader.position + 2]
      let b4 = reader.buffer[reader.position + 3]
      
      let value = b1 | (b2 << 8) | (b3 << 16) | (b4 << 24)
      
      (Some(value), {
        buffer: reader.buffer,
        position: reader.position + 4
      })
    } else {
      (None, reader)
    }
  }
  
  let read_string = fn(reader: BinaryReader) {
    let (length_result, reader_after_length) = read_int32(reader)
    
    match length_result {
      Some(length) => {
        if reader_after_length.position + length <= reader_after_length.buffer.length() {
          let mut chars = []
          
          for i in 0..length {
            chars = chars.push(reader_after_length.buffer[reader_after_length.position + i].to_char())
          }
          
          let string_value = chars.fold("", fn(acc, c) { acc + c.to_string() })
          
          (Some(string_value), {
            buffer: reader_after_length.buffer,
            position: reader_after_length.position + length
          })
        } else {
          (None, reader_after_length)
        }
      }
      None => (None, reader_after_length)
    }
  }
  
  // Test binary writer
  let writer = create_binary_writer(10)
  assert_eq(writer.buffer.length(), 0)
  assert_eq(writer.position, 0)
  
  let writer1 = write_byte(writer, 65) // 'A'
  assert_eq(writer1.buffer.length(), 1)
  assert_eq(writer1.buffer[0], 65)
  assert_eq(writer1.position, 1)
  
  let writer2 = write_int32(writer1, 123456789)
  assert_eq(writer2.buffer.length(), 5) // 1 byte + 4 bytes
  assert_eq(writer2.position, 5)
  
  let writer3 = write_string(writer2, "hello")
  assert_eq(writer3.buffer.length(), 10) // 5 bytes + 4 bytes length + 5 bytes string
  assert_eq(writer3.position, 10)
  
  // Test binary reader
  let reader = create_binary_reader(writer3.buffer)
  assert_eq(reader.position, 0)
  
  let (byte_value, reader1) = read_byte(reader)
  assert_eq(byte_value, Some(65))
  assert_eq(reader1.position, 1)
  
  let (int_value, reader2) = read_int32(reader1)
  assert_eq(int_value, Some(123456789))
  assert_eq(reader2.position, 5)
  
  let (string_value, reader3) = read_string(reader2)
  assert_eq(string_value, Some("hello"))
  assert_eq(reader3.position, 10)
  
  // Test edge cases
  let empty_reader = create_binary_reader([])
  let (no_byte, _) = read_byte(empty_reader)
  assert_eq(no_byte, None)
  
  let short_reader = create_binary_reader([1, 2, 3]) // Less than 4 bytes for int32
  let (no_int, _) = read_int32(short_reader)
  assert_eq(no_int, None)
}

// Test 4: Telemetry Data Serialization
test "telemetry data serialization formats" {
  // Define telemetry data types
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  type TelemetryMetric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  type TelemetryLog = {
    timestamp: Int,
    level: String,
    message: String,
    attributes: Array[(String, String)]
  }
  
  // Serialization operations
  let serialize_span_to_json = fn(span: TelemetrySpan) {
    let mut attributes_json = ""
    
    for i in 0..span.attributes.length() {
      let (key, value) = span.attributes[i]
      attributes_json = attributes_json + "\"" + key + "\":\"" + value + "\""
      if i < span.attributes.length() - 1 {
        attributes_json = attributes_json + ","
      }
    }
    
    let parent_json = match span.parent_span_id {
      Some(id) => "\"" + id + "\""
      None => "null"
    }
    
    "{
      \"trace_id\":\"" + span.trace_id + "\",
      \"span_id\":\"" + span.span_id + "\",
      \"parent_span_id\":" + parent_json + ",
      \"operation_name\":\"" + span.operation_name + "\",
      \"start_time\":" + span.start_time.to_string() + ",
      \"end_time\":" + span.end_time.to_string() + ",
      \"status\":\"" + span.status + "\",
      \"attributes\":{" + attributes_json + "}
    }"
  }
  
  let serialize_metric_to_json = fn(metric: TelemetryMetric) {
    let mut attributes_json = ""
    
    for i in 0..metric.attributes.length() {
      let (key, value) = metric.attributes[i]
      attributes_json = attributes_json + "\"" + key + "\":\"" + value + "\""
      if i < metric.attributes.length() - 1 {
        attributes_json = attributes_json + ","
      }
    }
    
    "{
      \"name\":\"" + metric.name + "\",
      \"value\":" + metric.value.to_string() + ",
      \"unit\":\"" + metric.unit + "\",
      \"timestamp\":" + metric.timestamp.to_string() + ",
      \"attributes\":{" + attributes_json + "}
    }"
  }
  
  let serialize_log_to_json = fn(log: TelemetryLog) {
    let mut attributes_json = ""
    
    for i in 0..log.attributes.length() {
      let (key, value) = log.attributes[i]
      attributes_json = attributes_json + "\"" + key + "\":\"" + value + "\""
      if i < log.attributes.length() - 1 {
        attributes_json = attributes_json + ","
      }
    }
    
    "{
      \"timestamp\":" + log.timestamp.to_string() + ",
      \"level\":\"" + log.level + "\",
      \"message\":\"" + log.message + "\",
      \"attributes\":{" + attributes_json + "}
    }"
  }
  
  let serialize_to_csv = fn(data: Array[String]) {
    let mut csv = ""
    
    for i in 0..data.length() {
      csv = csv + data[i]
      if i < data.length() - 1 {
        csv = csv + ","
      }
    }
    
    csv
  }
  
  // Test span serialization
  let span = {
    trace_id: "trace-123",
    span_id: "span-456",
    parent_span_id: Some("span-789"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [("db.type", "postgresql"), ("db.statement", "SELECT * FROM users")]
  }
  
  let span_json = serialize_span_to_json(span)
  assert_true(span_json.contains("\"trace_id\":\"trace-123\""))
  assert_true(span_json.contains("\"span_id\":\"span-456\""))
  assert_true(span_json.contains("\"parent_span_id\":\"span-789\""))
  assert_true(span_json.contains("\"operation_name\":\"database_query\""))
  assert_true(span_json.contains("\"db.type\":\"postgresql\""))
  
  // Test metric serialization
  let metric = {
    name: "response_time",
    value: 125.5,
    unit: "ms",
    timestamp: 1640995200,
    attributes: [("service", "api-gateway"), ("endpoint", "/users")]
  }
  
  let metric_json = serialize_metric_to_json(metric)
  assert_true(metric_json.contains("\"name\":\"response_time\""))
  assert_true(metric_json.contains("\"value\":125.5"))
  assert_true(metric_json.contains("\"unit\":\"ms\""))
  assert_true(metric_json.contains("\"service\":\"api-gateway\""))
  
  // Test log serialization
  let log = {
    timestamp: 1640995200,
    level: "INFO",
    message: "User login successful",
    attributes: [("user_id", "12345"), ("ip", "192.168.1.1")]
  }
  
  let log_json = serialize_log_to_json(log)
  assert_true(log_json.contains("\"level\":\"INFO\""))
  assert_true(log_json.contains("\"message\":\"User login successful\""))
  assert_true(log_json.contains("\"user_id\":\"12345\""))
  
  // Test CSV serialization
  let csv_data = ["trace-123", "span-456", "database_query", "50"]
  let csv = serialize_to_csv(csv_data)
  assert_eq(csv, "trace-123,span-456,database_query,50")
}

// Test 5: Custom Serialization Formats
test "custom serialization formats for telemetry" {
  // Define custom format types
  type KeyValueFormat = {
    delimiter: String,
    key_value_separator: String,
    escape_character: String
  }
  
  type SerializationFormat = 
    | JsonFormat
    | XmlFormat
    | KeyValueFormat(KeyValueFormat)
    | CustomFormat(String)
  
  // Custom serialization operations
  let escape_string = fn(str: String, escape_char: String) {
    let mut result = ""
    
    for i in 0..str.length() {
      let c = str[i]
      if c == ' ' or c == '=' or c == escape_char.to_char() {
        result = result + escape_char + c.to_string()
      } else {
        result = result + c.to_string()
      }
    }
    
    result
  }
  
  let serialize_attributes_to_kv = fn(attributes: Array[(String, String)], format: KeyValueFormat) {
    let mut result = ""
    
    for i in 0..attributes.length() {
      let (key, value) = attributes[i]
      let escaped_key = escape_string(key, format.escape_character)
      let escaped_value = escape_string(value, format.escape_character)
      
      result = result + escaped_key + format.key_value_separator + escaped_value
      
      if i < attributes.length() - 1 {
        result = result + format.delimiter
      }
    }
    
    result
  }
  
  let serialize_to_xml = fn(data: Array[(String, String)], root_element: String) {
    let mut xml = "<" + root_element + ">"
    
    for (key, value) in data {
      xml = xml + "<" + key + ">" + value + "</" + key + ">"
    }
    
    xml = xml + "</" + root_element + ">"
    xml
  }
  
  let serialize_telemetry_data = fn(data: Array[(String, String)], format: SerializationFormat) {
    match format {
      JsonFormat => {
        let mut json = "{"
        
        for i in 0..data.length() {
          let (key, value) = data[i]
          json = json + "\"" + key + "\":\"" + value + "\""
          
          if i < data.length() - 1 {
            json = json + ","
          }
        }
        
        json + "}"
      }
      XmlFormat => serialize_to_xml(data, "telemetry")
      KeyValueFormat(kv_format) => serialize_attributes_to_kv(data, kv_format)
      CustomFormat(format_name) => "Custom format: " + format_name
    }
  }
  
  // Test key-value format
  let kv_format = {
    delimiter: " ",
    key_value_separator: "=",
    escape_character: "\\"
  }
  
  let attributes = [
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("environment", "production")
  ]
  
  let kv_string = serialize_attributes_to_kv(attributes, kv_format)
  assert_eq(kv_string, "service.name=payment-service service.version=1.2.3 environment=production")
  
  // Test escaping in key-value format
  let attributes_with_spaces = [
    ("service name", "payment service"),
    ("value=with=equals", "test value")
  ]
  
  let escaped_kv = serialize_attributes_to_kv(attributes_with_spaces, kv_format)
  assert_eq(escaped_kv, "service\\ name=payment\\ service value\\=with\\=equals=test\\ value")
  
  // Test XML format
  let xml_string = serialize_to_xml(attributes, "service")
  assert_true(xml_string.starts_with("<service>"))
  assert_true(xml_string.ends_with("</service>"))
  assert_true(xml_string.contains("<service.name>payment-service</service.name>"))
  assert_true(xml_string.contains("<service.version>1.2.3</service.version>"))
  
  // Test different serialization formats
  let json_result = serialize_telemetry_data(attributes, JsonFormat)
  assert_true(json_result.starts_with("{"))
  assert_true(json_result.ends_with("}"))
  assert_true(json_result.contains("\"service.name\":\"payment-service\""))
  
  let xml_result = serialize_telemetry_data(attributes, XmlFormat)
  assert_true(xml_result.starts_with("<telemetry>"))
  assert_true(xml_result.ends_with("</telemetry>"))
  
  let kv_result = serialize_telemetry_data(attributes, KeyValueFormat(kv_format))
  assert_eq(kv_result, "service.name=payment-service service.version=1.2.3 environment=production")
  
  let custom_result = serialize_telemetry_data(attributes, CustomFormat("MyFormat"))
  assert_eq(custom_result, "Custom format: MyFormat")
}

// Test 6: Serialization Performance
test "serialization performance optimization" {
  // Define performance types
  type SerializationMetrics = {
    serialize_time_ms: Int,
    deserialize_time_ms: Int,
    data_size_bytes: Int,
    compression_ratio: Float
  }
  
  type PerformanceTest = {
    name: String,
    data_size: Int,
    iterations: Int
  }
  
  // Performance operations
  let measure_serialization_performance = fn(test: PerformanceTest, serialize_fn: String -> String, deserialize_fn: String -> String) {
    // Generate test data
    let test_data = "x".repeat(test.data_size)
    
    // Measure serialization
    let serialize_start = 1640995200000 // Mock timestamp
    let mut serialized_data = ""
    
    for i in 0..test.iterations {
      serialized_data = serialize_fn(test_data)
    }
    
    let serialize_end = 1640995200000 + 100 // Mock 100ms serialization time
    let serialize_time = serialize_end - serialize_start
    
    // Measure deserialization
    let deserialize_start = serialize_end
    let mut deserialized_data = ""
    
    for i in 0..test.iterations {
      deserialized_data = deserialize_fn(serialized_data)
    }
    
    let deserialize_end = deserialize_start + 50 // Mock 50ms deserialization time
    let deserialize_time = deserialize_end - deserialize_start
    
    // Calculate metrics
    let original_size = test_data.length() * test.iterations
    let compressed_size = serialized_data.length() * test.iterations
    let compression_ratio = original_size.to_float() / compressed_size.to_float()
    
    {
      serialize_time_ms: serialize_time,
      deserialize_time_ms: deserialize_time,
      data_size_bytes: original_size,
      compression_ratio
    }
  }
  
  let fast_serialize = fn(data: String) {
    // Fast but less efficient serialization
    "FAST:" + data
  }
  
  let fast_deserialize = fn(data: String) {
    // Fast deserialization
    if data.starts_with("FAST:") {
      data.substring(5, data.length() - 5)
    } else {
      data
    }
  }
  
  let efficient_serialize = fn(data: String) {
    // More efficient but slower serialization
    let mut result = "EFFICIENT:"
    
    // Add compression simulation
    for i in 0..data.length() {
      if i % 2 == 0 {
        result = result + data[i].to_string()
      }
    }
    
    result
  }
  
  let efficient_deserialize = fn(data: String) {
    // More efficient but slower deserialization
    if data.starts_with("EFFICIENT:") {
      let compressed = data.substring(10, data.length() - 10)
      let mut result = ""
      
      // Decompression simulation
      for i in 0..compressed.length() {
        result = result + compressed[i].to_string() + compressed[i].to_string()
      }
      
      result
    } else {
      data
    }
  }
  
  // Test performance measurement
  let test = {
    name: "Serialization Test",
    data_size: 1000,
    iterations: 100
  }
  
  let fast_metrics = measure_serialization_performance(test, fast_serialize, fast_deserialize)
  assert_eq(fast_metrics.serialize_time_ms, 100)
  assert_eq(fast_metrics.deserialize_time_ms, 50)
  assert_eq(fast_metrics.data_size_bytes, 100000) // 1000 * 100
  assert_eq(fast_metrics.compression_ratio, 100000.to_float() / 105000.to_float()) // 5 bytes overhead per iteration
  
  let efficient_metrics = measure_serialization_performance(test, efficient_serialize, efficient_deserialize)
  assert_eq(efficient_metrics.serialize_time_ms, 100)
  assert_eq(efficient_metrics.deserialize_time_ms, 50)
  assert_eq(efficient_metrics.data_size_bytes, 100000)
  
  // Efficient serialization should have better compression ratio
  assert_true(efficient_metrics.compression_ratio > fast_metrics.compression_ratio)
  
  // Test with different data sizes
  let small_test = { test | data_size: 100 }
  let large_test = { test | data_size: 10000 }
  
  let small_metrics = measure_serialization_performance(small_test, fast_serialize, fast_deserialize)
  let large_metrics = measure_serialization_performance(large_test, fast_serialize, fast_deserialize)
  
  assert_eq(small_metrics.data_size_bytes, 10000) // 100 * 100
  assert_eq(large_metrics.data_size_bytes, 1000000) // 10000 * 100
  
  // Larger data should have better compression ratio due to overhead being less significant
  assert_true(large_metrics.compression_ratio >= small_metrics.compression_ratio)
}

// Test 7: Serialization Error Handling
test "serialization error handling and recovery" {
  // Define error handling types
  type SerializationError = {
    error_type: String,
    message: String,
    data_snippet: String
  }
  
  type SerializationResult[T] = 
    | Success(T)
    | Error(SerializationError)
  
  type ErrorRecoveryStrategy = 
    | SkipInvalidData
    | UseDefaultValue
    | RetryWithAlternativeFormat
    | FailFast
  
  // Error handling operations
  let safe_json_parse = fn(json_str: String) {
    if json_str.length() == 0 {
      Error({
        error_type: "EmptyInput",
        message: "Empty JSON string",
        data_snippet: json_str
      })
    } else if not(json_str.starts_with("{") or json_str.starts_with("[")) {
      Error({
        error_type: "InvalidFormat",
        message: "JSON must start with { or [",
        data_snippet: json_str.substring(0, if json_str.length() > 20 { 20 } else { json_str.length() })
      })
    } else if not(json_str.ends_with("}") or json_str.ends_with("]")) {
      Error({
        error_type: "InvalidFormat",
        message: "JSON must end with } or ]",
        data_snippet: json_str.substring(if json_str.length() > 20 { json_str.length() - 20 } else { 0 })
      })
    } else {
      Success("parsed") // Simplified success case
    }
  }
  
  let safe_serialize = fn(data: String, max_size: Int) {
    if data.length() > max_size {
      Error({
        error_type: "DataTooLarge",
        message: "Data exceeds maximum size",
        data_snippet: data.substring(0, if data.length() > 20 { 20 } else { data.length() })
      })
    } else if data.contains("\0") {
      Error({
        error_type: "InvalidCharacter",
        message: "Data contains null character",
        data_snippet: "contains_null"
      })
    } else {
      Success("serialized:" + data)
    }
  }
  
  let handle_serialization_error = fn(error: SerializationError, strategy: ErrorRecoveryStrategy, default_value: String) {
    match strategy {
      SkipInvalidData => Success("skipped")
      UseDefaultValue => Success(default_value)
      RetryWithAlternativeFormat => Success("alternative_format:" + default_value)
      FailFast => Error(error)
    }
  }
  
  // Test error detection
  let empty_result = safe_json_parse("")
  match empty_result {
    Error(error) => {
      assert_eq(error.error_type, "EmptyInput")
      assert_eq(error.message, "Empty JSON string")
    }
    _ => assert_true(false)
  }
  
  let invalid_start_result = safe_json_parse("invalid json")
  match invalid_start_result {
    Error(error) => {
      assert_eq(error.error_type, "InvalidFormat")
      assert_eq(error.message, "JSON must start with { or [")
    }
    _ => assert_true(false)
  }
  
  let invalid_end_result = safe_json_parse("{invalid")
  match invalid_end_result {
    Error(error) => {
      assert_eq(error.error_type, "InvalidFormat")
      assert_eq(error.message, "JSON must end with } or ]")
    }
    _ => assert_true(false)
  }
  
  let valid_result = safe_json_parse("{}")
  match valid_result {
    Success(value) => assert_eq(value, "parsed")
    _ => assert_true(false)
  }
  
  // Test serialization error detection
  let large_data = "x".repeat(1001)
  let large_result = safe_serialize(large_data, 1000)
  match large_result {
    Error(error) => {
      assert_eq(error.error_type, "DataTooLarge")
      assert_eq(error.message, "Data exceeds maximum size")
    }
    _ => assert_true(false)
  }
  
  let invalid_data = "test\0data"
  let invalid_result = safe_serialize(invalid_data, 100)
  match invalid_result {
    Error(error) => {
      assert_eq(error.error_type, "InvalidCharacter")
      assert_eq(error.message, "Data contains null character")
    }
    _ => assert_true(false)
  }
  
  let valid_data = "test data"
  let valid_serialize_result = safe_serialize(valid_data, 100)
  match valid_serialize_result {
    Success(value) => assert_eq(value, "serialized:test data")
    _ => assert_true(false)
  }
  
  // Test error recovery strategies
  let error = {
    error_type: "TestError",
    message: "Test error message",
    data_snippet: "test snippet"
  }
  
  let skip_result = handle_serialization_error(error, SkipInvalidData, "default")
  match skip_result {
    Success(value) => assert_eq(value, "skipped")
    _ => assert_true(false)
  }
  
  let default_result = handle_serialization_error(error, UseDefaultValue, "default")
  match default_result {
    Success(value) => assert_eq(value, "default")
    _ => assert_true(false)
  }
  
  let retry_result = handle_serialization_error(error, RetryWithAlternativeFormat, "default")
  match retry_result {
    Success(value) => assert_eq(value, "alternative_format:default")
    _ => assert_true(false)
  }
  
  let fail_result = handle_serialization_error(error, FailFast, "default")
  match fail_result {
    Error(recovered_error) => {
      assert_eq(recovered_error.error_type, "TestError")
      assert_eq(recovered_error.message, "Test error message")
    }
    _ => assert_true(false)
  }
}

// Test 8: Schema Evolution and Compatibility
test "schema evolution and backward compatibility" {
  // Define schema types
  type FieldDefinition = {
    name: String,
    field_type: String,
    required: Bool,
    default_value: Option[String]
  }
  
  type SchemaVersion = {
    version: Int,
    fields: Array[FieldDefinition]
  }
  
  type CompatibilityLevel = 
    | BackwardCompatible
    | ForwardCompatible
    | FullCompatible
    | Incompatible
  
  // Schema operations
  let create_field = fn(name: String, field_type: String, required: Bool, default_value: Option[String]) {
    {
      name,
      field_type,
      required,
      default_value
    }
  }
  
  let create_schema_version = fn(version: Int, fields: Array[FieldDefinition]) {
    {
      version,
      fields
    }
  }
  
  let check_compatibility = fn(old_schema: SchemaVersion, new_schema: SchemaVersion) {
    let mut backward_compatible = true
    let mut forward_compatible = true
    
    // Check backward compatibility (new schema can read old data)
    for old_field in old_schema.fields {
      let mut field_exists = false
      
      for new_field in new_schema.fields {
        if new_field.name == old_field.name {
          field_exists = true
          
          // Check if field type is compatible
          if new_field.field_type != old_field.field_type {
            backward_compatible = false
          }
          
          // Check if required field became optional
          if old_field.required and not(new_field.required) {
            // This is OK for backward compatibility
          } else if not(old_field.required) and new_field.required {
            backward_compatible = false
          }
          
          break
        }
      }
      
      if not(field_exists) and old_field.required {
        backward_compatible = false
      }
    }
    
    // Check forward compatibility (old schema can read new data)
    for new_field in new_schema.fields {
      let mut field_exists = false
      
      for old_field in old_schema.fields {
        if old_field.name == new_field.name {
          field_exists = true
          break
        }
      }
      
      if not(field_exists) and new_field.required {
        forward_compatible = false
      }
    }
    
    if backward_compatible and forward_compatible {
      FullCompatible
    } else if backward_compatible {
      BackwardCompatible
    } else if forward_compatible {
      ForwardCompatible
    } else {
      Incompatible
    }
  }
  
  let serialize_with_schema = fn(data: Array[(String, String)], schema: SchemaVersion) {
    let mut result = ""
    
    for field in schema.fields {
      let field_value = match data.find(fn(pair) { pair.0 == field.name }) {
        Some((_, value)) => value
        None => {
          match field.default_value {
            Some(default) => default
            None => ""
          }
        }
      }
      
      result = result + field.name + ":" + field_value + ";"
    }
    
    result
  }
  
  let deserialize_with_schema = fn(data: String, schema: SchemaVersion) {
    let mut result = []
    let field_pairs = data.split(";")
    
    for field_pair in field_pairs {
      if field_pair.length() > 0 {
        let parts = field_pair.split(":")
        if parts.length() >= 1 {
          let field_name = parts[0]
          let field_value = if parts.length() > 1 { parts[1] } else { "" }
          
          // Check if field exists in schema
          for field in schema.fields {
            if field.name == field_name {
              result = result.push((field_name, field_value))
              break
            }
          }
        }
      }
    }
    
    result
  }
  
  // Test schema creation
  let field1 = create_field("id", "string", true, None)
  let field2 = create_field("name", "string", true, None)
  let field3 = create_field("age", "number", false, Some("0"))
  
  let schema_v1 = create_schema_version(1, [field1, field2])
  let schema_v2 = create_schema_version(2, [field1, field2, field3]) // Added optional field
  let schema_v3 = create_schema_version(3, [field1, field2, field3]) // Same as v2
  
  // Test compatibility
  let v1_to_v2_compatibility = check_compatibility(schema_v1, schema_v2)
  assert_eq(v1_to_v2_compatibility, FullCompatible) // Added optional field
  
  let v2_to_v1_compatibility = check_compatibility(schema_v2, schema_v1)
  assert_eq(v2_to_v1_compatibility, BackwardCompatible) // Old schema can't read new field
  
  let v2_to_v3_compatibility = check_compatibility(schema_v2, schema_v3)
  assert_eq(v2_to_v3_compatibility, FullCompatible) // Identical schemas
  
  // Test incompatible change
  let field4 = create_field("id", "number", true, None) // Changed type
  let incompatible_schema = create_schema_version(4, [field4, field2])
  
  let v1_to_incompatible_compatibility = check_compatibility(schema_v1, incompatible_schema)
  assert_eq(v1_to_incompatible_compatibility, Incompatible) // Type change
  
  // Test serialization with schema
  let data_v1 = [("id", "123"), ("name", "test")]
  let data_v2 = [("id", "123"), ("name", "test"), ("age", "25")]
  
  let serialized_v1 = serialize_with_schema(data_v1, schema_v1)
  assert_true(serialized_v1.contains("id:123;"))
  assert_true(serialized_v1.contains("name:test;"))
  
  let serialized_v2 = serialize_with_schema(data_v2, schema_v2)
  assert_true(serialized_v2.contains("id:123;"))
  assert_true(serialized_v2.contains("name:test;"))
  assert_true(serialized_v2.contains("age:25;"))
  
  // Test deserialization with schema
  let deserialized_v1 = deserialize_with_schema(serialized_v1, schema_v1)
  assert_eq(deserialized_v1.length(), 2)
  assert_true(deserialized_v1.contains(("id", "123")))
  assert_true(deserialized_v1.contains(("name", "test")))
  
  // Deserialize v1 data with v2 schema (forward compatibility)
  let deserialized_v1_with_v2 = deserialize_with_schema(serialized_v1, schema_v2)
  assert_eq(deserialized_v1_with_v2.length(), 2) // Should only have fields from v1
  assert_true(deserialized_v1_with_v2.contains(("id", "123")))
  assert_true(deserialized_v1_with_v2.contains(("name", "test")))
  
  // Deserialize v2 data with v1 schema (backward compatibility)
  let deserialized_v2_with_v1 = deserialize_with_schema(serialized_v2, schema_v1)
  assert_eq(deserialized_v2_with_v1.length(), 2) // Should only have fields from v1
  assert_true(deserialized_v2_with_v1.contains(("id", "123")))
  assert_true(deserialized_v2_with_v1.contains(("name", "test")))
}