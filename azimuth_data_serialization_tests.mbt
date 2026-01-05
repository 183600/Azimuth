// Azimuth Data Serialization Tests
// This file contains test cases for data serialization functionality

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Define basic data types for serialization
  type JsonValue = 
    | JsonString(String)
    | JsonNumber(Float)
    | JsonBool(Bool)
    | JsonNull
    | JsonObject(Array[(String, JsonValue)])
    | JsonArray(Array[JsonValue])
  
  // Telemetry span for serialization
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // Convert string to JSON value
  let string_to_json = fn(s: String) { JsonString(s) }
  
  // Convert int to JSON value
  let int_to_json = fn(i: Int) { JsonNumber(i.to_float()) }
  
  // Convert bool to JSON value
  let bool_to_json = fn(b: Bool) { 
    if b { JsonBool(true) } else { JsonBool(false) } 
  }
  
  // Convert option to JSON value
  let option_to_json = fn(opt: Option[String], converter: String -> JsonValue) {
    match opt {
      Some(value) => converter(value)
      None => JsonNull
    }
  }
  
  // Convert array to JSON value
  let array_to_json = fn(arr: Array[String], converter: String -> JsonValue) {
    let json_values = arr.map(converter)
    JsonArray(json_values)
  }
  
  // Convert tuple to JSON object field
  let tuple_to_json_field = fn(tuple: (String, String)) {
    (tuple.0, string_to_json(tuple.1))
  }
  
  // Convert telemetry span to JSON
  let span_to_json = fn(span: TelemetrySpan) {
    let fields = [
      ("trace_id", string_to_json(span.trace_id)),
      ("span_id", string_to_json(span.span_id)),
      ("parent_span_id", option_to_json(span.parent_span_id, string_to_json)),
      ("operation_name", string_to_json(span.operation_name)),
      ("start_time", int_to_json(span.start_time)),
      ("end_time", int_to_json(span.end_time)),
      ("status", string_to_json(span.status)),
      ("tags", array_to_json(span.tags, fn(tag) { 
        // Convert tag tuple to JSON object
        let parts = tag.split("=")
        if parts.length() == 2 {
          JsonObject([
            ("key", string_to_json(parts[0])),
            ("value", string_to_json(parts[1]))
          ])
        } else {
          string_to_json(tag)
        }
      }))
    ]
    
    JsonObject(fields)
  }
  
  // Convert JSON value to string (simplified)
  let json_to_string = fn(json: JsonValue) {
    match json {
      JsonString(s) => "\"" + s + "\""
      JsonNumber(n) => n.to_string()
      JsonBool(b) => if b { "true" } else { "false" }
      JsonNull => "null"
      JsonObject(fields) => {
        let mut result = "{"
        let mut first = true
        
        for (key, value) in fields {
          if not(first) {
            result = result + ","
          }
          result = result + "\"" + key + "\":" + json_to_string(value)
          first = false
        }
        
        result = result + "}"
        result
      }
      JsonArray(values) => {
        let mut result = "["
        let mut first = true
        
        for value in values {
          if not(first) {
            result = result + ","
          }
          result = result + json_to_string(value)
          first = false
        }
        
        result = result + "]"
        result
      }
    }
  }
  
  // Parse JSON string to JSON value (simplified)
  let parse_json = fn(json_str: String) {
    // Very simplified JSON parser for testing
    if json_str.starts_with("\"") and json_str.ends_with("\"") {
      JsonString(json_str.substring(1, json_str.length() - 2))
    } else if json_str == "true" {
      JsonBool(true)
    } else if json_str == "false" {
      JsonBool(false)
    } else if json_str == "null" {
      JsonNull
    } else if json_str.starts_with("{") and json_str.ends_with("}") {
      // Simplified object parsing
      JsonObject([
        ("trace_id", JsonString("trace-123")),
        ("span_id", JsonString("span-456"))
      ])
    } else if json_str.starts_with("[") and json_str.ends_with("]") {
      // Simplified array parsing
      JsonArray([
        JsonString("item1"),
        JsonString("item2")
      ])
    } else {
      // Try to parse as number
      let mut i = 0
      let mut valid = true
      let mut dot_seen = false
      
      while i < json_str.length() and valid {
        let c = json_str[i]
        if c == '.' {
          if dot_seen {
            valid = false
          } else {
            dot_seen = true
          }
        } else if (c < '0' or c > '9') and c != '-' {
          valid = false
        }
        i = i + 1
      }
      
      if valid {
        JsonNumber(json_str.to_float())
      } else {
        JsonString(json_str)  // Fallback
      }
    }
  }
  
  // Create test span
  let test_span = {
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    parent_span_id: Some("span-123456789"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    tags: [
      ("service", "payment-service"),
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users")
    ]
  }
  
  // Serialize span to JSON
  let json_span = span_to_json(test_span)
  let json_string = json_to_string(json_span)
  
  // Verify JSON string contains expected fields
  assert_true(json_string.contains("trace_id"))
  assert_true(json_string.contains("trace-123456789"))
  assert_true(json_string.contains("span_id"))
  assert_true(json_string.contains("span-987654321"))
  assert_true(json_string.contains("operation_name"))
  assert_true(json_string.contains("database_query"))
  assert_true(json_string.contains("start_time"))
  assert_true(json_string.contains("1640995200"))
  assert_true(json_string.contains("end_time"))
  assert_true(json_string.contains("1640995250"))
  assert_true(json_string.contains("status"))
  assert_true(json_string.contains("ok"))
  
  // Test basic JSON parsing
  let parsed_string = parse_json("\"test_string\"")
  match parsed_string {
    JsonString(s) => assert_eq(s, "test_string")
    _ => assert_true(false)
  }
  
  let parsed_number = parse_json("42.5")
  match parsed_number {
    JsonNumber(n) => assert_eq(n, 42.5)
    _ => assert_true(false)
  }
  
  let parsed_bool = parse_json("true")
  match parsed_bool {
    JsonBool(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  let parsed_null = parse_json("null")
  match parsed_null {
    JsonNull => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 2: Binary Serialization
test "binary serialization" {
  // Define binary format constants
  let TYPE_STRING = 1
  let TYPE_INT = 2
  let TYPE_FLOAT = 3
  let TYPE_BOOL = 4
  let TYPE_ARRAY = 5
  let TYPE_OBJECT = 6
  
  // Binary writer
  type BinaryWriter = {
    mut buffer: Array[Byte]
  }
  
  // Create binary writer
  let create_writer = fn() {
    { mut buffer: [] }
  }
  
  // Write byte to buffer
  let write_byte = fn(writer: BinaryWriter, value: Byte) {
    writer.buffer = writer.buffer.push(value)
  }
  
  // Write bytes to buffer
  let write_bytes = fn(writer: BinaryWriter, bytes: Array[Byte]) {
    for byte in bytes {
      writer.buffer = writer.buffer.push(byte)
    }
  }
  
  // Write string to buffer
  let write_string = fn(writer: BinaryWriter, value: String) {
    write_byte(writer, TYPE_STRING)
    let bytes = value.to_bytes()
    write_byte(writer, bytes.length().to_byte())
    write_bytes(writer, bytes)
  }
  
  // Write int to buffer
  let write_int = fn(writer: BinaryWriter, value: Int) {
    write_byte(writer, TYPE_INT)
    // Write 4 bytes for int (big-endian)
    write_byte(writer, (value >> 24).to_byte())
    write_byte(writer, (value >> 16).to_byte())
    write_byte(writer, (value >> 8).to_byte())
    write_byte(writer, value.to_byte())
  }
  
  // Write float to buffer
  let write_float = fn(writer: BinaryWriter, value: Float) {
    write_byte(writer, TYPE_FLOAT)
    // Simplified: convert to string and write as string
    write_string(writer, value.to_string())
  }
  
  // Write bool to buffer
  let write_bool = fn(writer: BinaryWriter, value: Bool) {
    write_byte(writer, TYPE_BOOL)
    write_byte(writer, if value { 1 } else { 0 })
  }
  
  // Write array to buffer
  let write_array = fn(writer: BinaryWriter, values: Array[String]) {
    write_byte(writer, TYPE_ARRAY)
    write_byte(writer, values.length().to_byte())
    for value in values {
      write_string(writer, value)
    }
  }
  
  // Get buffer as array
  let get_buffer = fn(writer: BinaryWriter) {
    writer.buffer
  }
  
  // Binary reader
  type BinaryReader = {
    buffer: Array[Byte],
    mut position: Int
  }
  
  // Create binary reader
  let create_reader = fn(buffer: Array[Byte]) {
    {
      buffer: buffer,
      mut position: 0
    }
  }
  
  // Read byte from buffer
  let read_byte = fn(reader: BinaryReader) {
    if reader.position < reader.buffer.length() {
      let value = reader.buffer[reader.position]
      reader.position = reader.position + 1
      Some(value)
    } else {
      None
    }
  }
  
  // Read bytes from buffer
  let read_bytes = fn(reader: BinaryReader, count: Int) {
    if reader.position + count <= reader.buffer.length() {
      let mut bytes = []
      let mut i = 0
      while i < count {
        bytes = bytes.push(reader.buffer[reader.position + i])
        i = i + 1
      }
      reader.position = reader.position + count
      Some(bytes)
    } else {
      None
    }
  }
  
  // Read string from buffer
  let read_string = fn(reader: BinaryReader) {
    match read_byte(reader) {
      Some(TYPE_STRING) => {
        match read_byte(reader) {
          Some(length) => {
            match read_bytes(reader, length.to_int()) {
              Some(bytes) => Some(bytes.to_string())
              None => None
            }
          }
          _ => None
        }
      }
      _ => None
    }
  }
  
  // Read int from buffer
  let read_int = fn(reader: BinaryReader) {
    match read_byte(reader) {
      Some(TYPE_INT) => {
        match read_bytes(reader, 4) {
          Some(bytes) => {
            let value = (bytes[0].to_int() << 24) | 
                       (bytes[1].to_int() << 16) | 
                       (bytes[2].to_int() << 8) | 
                       bytes[3].to_int()
            Some(value)
          }
          None => None
        }
      }
      _ => None
    }
  }
  
  // Read float from buffer
  let read_float = fn(reader: BinaryReader) {
    match read_byte(reader) {
      Some(TYPE_FLOAT) => {
        match read_string(reader) {
          Some(str) => Some(str.to_float())
          None => None
        }
      }
      _ => None
    }
  }
  
  // Read bool from buffer
  let read_bool = fn(reader: BinaryReader) {
    match read_byte(reader) {
      Some(TYPE_BOOL) => {
        match read_byte(reader) {
          Some(value) => Some(value == 1)
          None => None
        }
      }
      _ => None
    }
  }
  
  // Read array from buffer
  let read_array = fn(reader: BinaryReader) {
    match read_byte(reader) {
      Some(TYPE_ARRAY) => {
        match read_byte(reader) {
          Some(length) => {
            let mut values = []
            let mut i = 0
            let mut success = true
            
            while i < length.to_int() and success {
              match read_string(reader) {
                Some(value) => {
                  values = values.push(value)
                  i = i + 1
                }
                None => {
                  success = false
                }
              }
            }
            
            if success {
              Some(values)
            } else {
              None
            }
          }
          None => None
        }
      }
      _ => None
    }
  }
  
  // Test binary serialization
  let writer = create_writer()
  
  // Write different types
  write_string(writer, "test_string")
  write_int(writer, 12345)
  write_float(writer, 3.14159)
  write_bool(writer, true)
  write_array(writer, ["item1", "item2", "item3"])
  
  let buffer = get_buffer(writer)
  assert_true(buffer.length() > 0)
  
  // Test binary deserialization
  let reader = create_reader(buffer)
  
  let read_str = read_string(reader)
  match read_str {
    Some(s) => assert_eq(s, "test_string")
    None => assert_true(false)
  }
  
  let read_int_val = read_int(reader)
  match read_int_val {
    Some(i) => assert_eq(i, 12345)
    None => assert_true(false)
  }
  
  let read_float_val = read_float(reader)
  match read_float_val {
    Some(f) => assert_true(f > 3.14 and f < 3.15)  // Approximate equality
    None => assert_true(false)
  }
  
  let read_bool_val = read_bool(reader)
  match read_bool_val {
    Some(b) => assert_true(b)
    None => assert_true(false)
  }
  
  let read_array_val = read_array(reader)
  match read_array_val {
    Some(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    None => assert_true(false)
  }
}

// Test 3: Protocol Buffer Serialization (Simplified)
test "protocol buffer serialization" {
  // Simplified Protocol Buffer implementation
  type PbField = {
    field_number: Int,
    wire_type: Int,
    value: String
  }
  
  type PbMessage = {
    fields: Array[PbField]
  }
  
  // Wire types
  let WIRE_TYPE_VARINT = 0
  let WIRE_TYPE_64BIT = 1
  let WIRE_TYPE_LENGTH_DELIMITED = 2
  let WIRE_TYPE_32BIT = 5
  
  // Create PB field
  let create_field = fn(field_number: Int, wire_type: Int, value: String) {
    {
      field_number: field_number,
      wire_type: wire_type,
      value: value
    }
  }
  
  // Create PB message
  let create_message = fn(fields: Array[PbField]) {
    { fields: fields }
  }
  
  // Encode varint
  let encode_varint = fn(value: Int) {
    let mut bytes = []
    let mut v = value
    
    while v >= 128 {
      bytes = bytes.push(((v & 0x7F) | 0x80).to_byte())
      v = v >> 7
    }
    
    bytes = bytes.push(v.to_byte())
    bytes
  }
  
  // Decode varint
  let decode_varint = fn(bytes: Array[Byte], mut position: Int) {
    let mut result = 0
    let mut shift = 0
    let mut done = false
    
    while position < bytes.length() and not(done) {
      let byte = bytes[position]
      position = position + 1
      
      result = result | ((byte & 0x7F).to_int() << shift)
      
      if (byte & 0x80) == 0 {
        done = true
      } else {
        shift = shift + 7
      }
    }
    
    (result, position)
  }
  
  // Encode field key
  let encode_key = fn(field_number: Int, wire_type: Int) {
    let key = (field_number << 3) | wire_type
    encode_varint(key)
  }
  
  // Serialize PB message to bytes
  let serialize_pb_message = fn(message: PbMessage) {
    let mut bytes = []
    
    for field in message.fields {
      // Encode key
      let key_bytes = encode_key(field.field_number, field.wire_type)
      bytes = bytes.concat(key_bytes)
      
      // Encode value based on wire type
      match field.wire_type {
        WIRE_TYPE_VARINT => {
          let value = field.value.to_int()
          let value_bytes = encode_varint(value)
          bytes = bytes.concat(value_bytes)
        }
        WIRE_TYPE_LENGTH_DELIMITED => {
          let value_bytes = field.value.to_bytes()
          let length_bytes = encode_varint(value_bytes.length())
          bytes = bytes.concat(length_bytes)
          bytes = bytes.concat(value_bytes)
        }
        _ => {}  // Simplified
      }
    }
    
    bytes
  }
  
  // Deserialize bytes to PB message
  let deserialize_pb_message = fn(bytes: Array[Byte]) {
    let mut fields = []
    let mut position = 0
    
    while position < bytes.length() {
      // Decode key
      let (key, new_pos) = decode_varint(bytes, position)
      position = new_pos
      
      let field_number = key >> 3
      let wire_type = key & 0x07
      
      // Decode value based on wire type
      match wire_type {
        WIRE_TYPE_VARINT => {
          let (value, new_pos) = decode_varint(bytes, position)
          position = new_pos
          fields = fields.push(create_field(field_number, wire_type, value.to_string()))
        }
        WIRE_TYPE_LENGTH_DELIMITED => {
          let (length, new_pos) = decode_varint(bytes, position)
          position = new_pos
          
          let mut value_bytes = []
          let mut i = 0
          while i < length {
            if position + i < bytes.length() {
              value_bytes = value_bytes.push(bytes[position + i])
            }
            i = i + 1
          }
          
          position = position + length
          fields = fields.push(create_field(field_number, wire_type, value_bytes.to_string()))
        }
        _ => {
          // Skip unknown wire types
          position = bytes.length()  // End parsing
        }
      }
    }
    
    create_message(fields)
  }
  
  // Create test message
  let fields = [
    create_field(1, WIRE_TYPE_LENGTH_DELIMITED, "trace-123"),  // trace_id
    create_field(2, WIRE_TYPE_LENGTH_DELIMITED, "span-456"),   // span_id
    create_field(3, WIRE_TYPE_VARINT, "1640995200"),           // start_time
    create_field(4, WIRE_TYPE_VARINT, "1640995250"),           // end_time
    create_field(5, WIRE_TYPE_LENGTH_DELIMITED, "database_query")  // operation_name
  ]
  
  let message = create_message(fields)
  
  // Serialize message
  let serialized = serialize_pb_message(message)
  assert_true(serialized.length() > 0)
  
  // Deserialize message
  let deserialized = deserialize_pb_message(serialized)
  assert_eq(deserialized.fields.length(), 5)
  
  // Check fields
  let trace_id_field = deserialized.fields.find_fn(f) { f.field_number == 1 }
  match trace_id_field {
    Some(field) => assert_eq(field.value, "trace-123")
    None => assert_true(false)
  }
  
  let span_id_field = deserialized.fields.find_fn(f) { f.field_number == 2 }
  match span_id_field {
    Some(field) => assert_eq(field.value, "span-456")
    None => assert_true(false)
  }
  
  let start_time_field = deserialized.fields.find_fn(f) { f.field_number == 3 }
  match start_time_field {
    Some(field) => assert_eq(field.value, "1640995200")
    None => assert_true(false)
  }
  
  let end_time_field = deserialized.fields.find_fn(f) { f.field_number == 4 }
  match end_time_field {
    Some(field) => assert_eq(field.value, "1640995250")
    None => assert_true(false)
  }
  
  let operation_field = deserialized.fields.find_fn(f) { f.field_number == 5 }
  match operation_field {
    Some(field) => assert_eq(field.value, "database_query")
    None => assert_true(false)
  }
}

// Test 4: Custom Serialization Format
test "custom serialization format" {
  // Define custom format structure
  type CustomFormat = {
    format_type: String,
    version: Int,
    data: Array[(String, String)]
  }
  
  // Create custom format
  let create_custom_format = fn(format_type: String, version: Int) {
    {
      format_type: format_type,
      version: version,
      data: []
    }
  }
  
  // Add data to custom format
  let add_data = fn(format: CustomFormat, key: String, value: String) {
    { format | data: format.data.push((key, value)) }
  }
  
  // Serialize custom format to string
  let serialize_custom_format = fn(format: CustomFormat) {
    let mut result = ""
    
    // Add header
    result = result + "FORMAT:" + format.format_type + "\n"
    result = result + "VERSION:" + format.version.to_string() + "\n"
    result = result + "DATA_COUNT:" + format.data.length().to_string() + "\n"
    
    // Add data
    for (key, value) in format.data {
      result = result + key + ":" + value + "\n"
    }
    
    result
  }
  
  // Parse custom format from string
  let parse_custom_format = fn(str: String) {
    let lines = str.split("\n")
    
    if lines.length() < 3 {
      None
    } else {
      // Parse header
      let format_line = lines[0]
      let version_line = lines[1]
      let count_line = lines[2]
      
      if format_line.starts_with("FORMAT:") and 
         version_line.starts_with("VERSION:") and 
         count_line.starts_with("DATA_COUNT:") {
        
        let format_type = format_line.substring(7, format_line.length() - 7)
        let version = version_line.substring(8, version_line.length() - 8).to_int()
        let data_count = count_line.substring(11, count_line.length() - 11).to_int()
        
        // Parse data
        let mut data = []
        let mut i = 3
        while i < lines.length() and i < 3 + data_count {
          let line = lines[i]
          let parts = line.split(":")
          if parts.length() >= 2 {
            let key = parts[0]
            let value = line.substring(key.length() + 1, line.length() - (key.length() + 1))
            data = data.push((key, value))
          }
          i = i + 1
        }
        
        Some({
          format_type: format_type,
          version: version,
          data: data
        })
      } else {
        None
      }
    }
  }
  
  // Create test custom format
  let format = create_custom_format("TELEMETRY_SPAN", 1)
  let format_with_data = add_data(
    add_data(
      add_data(
        add_data(
          add_data(format, "trace_id", "trace-123"),
          "span_id", "span-456"
        ),
        "operation_name", "database_query"
      ),
      "start_time", "1640995200"
    ),
    "end_time", "1640995250"
  )
  
  // Serialize format
  let serialized = serialize_custom_format(format_with_data)
  
  // Verify serialized string
  assert_true(serialized.contains("FORMAT:TELEMETRY_SPAN"))
  assert_true(serialized.contains("VERSION:1"))
  assert_true(serialized.contains("DATA_COUNT:5"))
  assert_true(serialized.contains("trace_id:trace-123"))
  assert_true(serialized.contains("span_id:span-456"))
  assert_true(serialized.contains("operation_name:database_query"))
  assert_true(serialized.contains("start_time:1640995200"))
  assert_true(serialized.contains("end_time:1640995250"))
  
  // Parse format
  let parsed = parse_custom_format(serialized)
  match parsed {
    Some(parsed_format) => {
      assert_eq(parsed_format.format_type, "TELEMETRY_SPAN")
      assert_eq(parsed_format.version, 1)
      assert_eq(parsed_format.data.length(), 5)
      
      // Check data
      let trace_id = parsed_format.data.find_fn(d) { d.0 == "trace_id" }
      match trace_id {
        Some(pair) => assert_eq(pair.1, "trace-123")
        None => assert_true(false)
      }
      
      let span_id = parsed_format.data.find_fn(d) { d.0 == "span_id" }
      match span_id {
        Some(pair) => assert_eq(pair.1, "span-456")
        None => assert_true(false)
      }
      
      let operation_name = parsed_format.data.find_fn(d) { d.0 == "operation_name" }
      match operation_name {
        Some(pair) => assert_eq(pair.1, "database_query")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test invalid format
  let invalid_format = "INVALID FORMAT"
  let invalid_parsed = parse_custom_format(invalid_format)
  assert_eq(invalid_parsed, None)
}

// Test 5: Compression and Serialization
test "compression and serialization" {
  // Simple run-length encoding compression
  let compress = fn(data: Array[Byte]) {
    if data.length() == 0 {
      []
    } else {
      let mut compressed = []
      let mut i = 0
      
      while i < data.length() {
        let current = data[i]
        let mut count = 1
        
        while i + count < data.length() and data[i + count] == current {
          count = count + 1
        }
        
        // Write count and value
        compressed = compressed.push(count.to_byte())
        compressed = compressed.push(current)
        
        i = i + count
      }
      
      compressed
    }
  }
  
  // Decompress run-length encoded data
  let decompress = fn(compressed: Array[Byte]) {
    if compressed.length() % 2 != 0 {
      []  // Invalid compressed data
    } else {
      let mut decompressed = []
      let mut i = 0
      
      while i < compressed.length() {
        let count = compressed[i].to_int()
        let value = compressed[i + 1]
        
        let mut j = 0
        while j < count {
          decompressed = decompressed.push(value)
          j = j + 1
        }
        
        i = i + 2
      }
      
      decompressed
    }
  }
  
  // Serialize with compression
  let serialize_with_compression = fn(data: Array[Byte]) {
    let compressed = compress(data)
    
    // Add header
    let mut result = []
    result = result.push('C'.to_byte())  // Compression marker
    result = result.push((compressed.length() >> 8).to_byte())
    result = result.push((compressed.length() & 0xFF).to_byte())
    
    // Add compressed data
    result = result.concat(compressed)
    
    result
  }
  
  // Deserialize with decompression
  let deserialize_with_compression = fn(data: Array[Byte]) {
    if data.length() < 3 or data[0] != 'C'.to_byte() {
      None  // Invalid format
    } else {
      let compressed_length = (data[1].to_int() << 8) | data[2].to_int()
      
      if data.length() < 3 + compressed_length {
        None  // Incomplete data
      } else {
        let compressed = data.slice(3, 3 + compressed_length)
        let decompressed = decompress(compressed)
        Some(decompressed)
      }
    }
  }
  
  // Test compression
  let original_data = [
    'A'.to_byte(), 'A'.to_byte(), 'A'.to_byte(),  // 3 A's
    'B'.to_byte(), 'B'.to_byte(),                  // 2 B's
    'C'.to_byte(),                                 // 1 C
    'D'.to_byte(), 'D'.to_byte(), 'D'.to_byte(), 'D'.to_byte(),  // 4 D's
    'E'.to_byte()                                  // 1 E
  ]
  
  let compressed = compress(original_data)
  
  // Verify compression
  assert_eq(compressed.length(), 10)  // 5 pairs of (count, value)
  assert_eq(compressed[0], 3.to_byte())  // 3 A's
  assert_eq(compressed[1], 'A'.to_byte())
  assert_eq(compressed[2], 2.to_byte())  // 2 B's
  assert_eq(compressed[3], 'B'.to_byte())
  assert_eq(compressed[4], 1.to_byte())  // 1 C
  assert_eq(compressed[5], 'C'.to_byte())
  assert_eq(compressed[6], 4.to_byte())  // 4 D's
  assert_eq(compressed[7], 'D'.to_byte())
  assert_eq(compressed[8], 1.to_byte())  // 1 E
  assert_eq(compressed[9], 'E'.to_byte())
  
  // Test decompression
  let decompressed = decompress(compressed)
  assert_eq(decompressed.length(), original_data.length())
  
  let mut i = 0
  while i < original_data.length() {
    assert_eq(decompressed[i], original_data[i])
    i = i + 1
  }
  
  // Test serialize with compression
  let serialized = serialize_with_compression(original_data)
  
  // Verify header
  assert_eq(serialized[0], 'C'.to_byte())
  assert_eq(serialized[1], (compressed.length() >> 8).to_byte())
  assert_eq(serialized[2], (compressed.length() & 0xFF).to_byte())
  
  // Verify compressed data
  let mut i = 0
  while i < compressed.length() {
    assert_eq(serialized[3 + i], compressed[i])
    i = i + 1
  }
  
  // Test deserialize with decompression
  let deserialized = deserialize_with_compression(serialized)
  match deserialized {
    Some(data) => {
      assert_eq(data.length(), original_data.length())
      
      let mut i = 0
      while i < original_data.length() {
        assert_eq(data[i], original_data[i])
        i = i + 1
      }
    }
    None => assert_true(false)
  }
  
  // Test invalid serialized data
  let invalid_data = ['X'.to_byte()]  // Invalid marker
  let invalid_result = deserialize_with_compression(invalid_data)
  assert_eq(invalid_result, None)
  
  // Test incomplete serialized data
  let incomplete_data = ['C'.to_byte(), 0.to_byte()]  // Missing length byte
  let incomplete_result = deserialize_with_compression(incomplete_data)
  assert_eq(incomplete_result, None)
}