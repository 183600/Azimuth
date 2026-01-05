// Azimuth Telemetry System - Comprehensive Serialization Format Tests
// This file contains comprehensive test cases for serialization and deserialization

// Test 1: Binary Serialization
test "binary serialization for telemetry data" {
  type BinaryWriter = {
    buffer: Array[Byte],
    position: Int
  }
  
  let binary_writer_new = fn() -> BinaryWriter {
    { buffer: [], position: 0 }
  }
  
  let binary_writer_write_byte = fn(writer: BinaryWriter, value: Byte) -> BinaryWriter {
    let new_buffer = writer.buffer + [value]
    { buffer: new_buffer, position: writer.position + 1 }
  }
  
  let binary_writer_write_int = fn(writer: BinaryWriter, value: Int) -> BinaryWriter {
    // Write 4 bytes in little-endian format
    let byte1 = Byte::from_int(value & 0xFF)
    let byte2 = Byte::from_int((value >> 8) & 0xFF)
    let byte3 = Byte::from_int((value >> 16) & 0xFF)
    let byte4 = Byte::from_int((value >> 24) & 0xFF)
    
    let mut new_writer = binary_writer_write_byte(writer, byte1)
    new_writer = binary_writer_write_byte(new_writer, byte2)
    new_writer = binary_writer_write_byte(new_writer, byte3)
    new_writer = binary_writer_write_byte(new_writer, byte4)
    
    new_writer
  }
  
  let binary_writer_write_string = fn(writer: BinaryWriter, value: String) -> BinaryWriter {
    // Write length first, then string bytes
    let new_writer = binary_writer_write_int(writer, value.length())
    
    let mut final_writer = new_writer
    for char in value.to_char_array() {
      final_writer = binary_writer_write_byte(final_writer, Byte::from_int(char.to_int()))
    }
    
    final_writer
  }
  
  type BinaryReader = {
    buffer: Array[Byte],
    position: Int
  }
  
  let binary_reader_new = fn(buffer: Array[Byte]) -> BinaryReader {
    { buffer, position: 0 }
  }
  
  let binary_reader_read_byte = fn(reader: BinaryReader) -> (BinaryReader, Byte) {
    if reader.position >= reader.buffer.length() {
      panic("End of buffer")
    }
    
    let byte = reader.buffer[reader.position]
    let new_reader = { buffer: reader.buffer, position: reader.position + 1 }
    (new_reader, byte)
  }
  
  let binary_reader_read_int = fn(reader: BinaryReader) -> (BinaryReader, Int) {
    let (reader1, byte1) = binary_reader_read_byte(reader)
    let (reader2, byte2) = binary_reader_read_byte(reader1)
    let (reader3, byte3) = binary_reader_read_byte(reader2)
    let (reader4, byte4) = binary_reader_read_byte(reader3)
    
    let value = byte1.to_int() | 
               (byte2.to_int() << 8) | 
               (byte3.to_int() << 16) | 
               (byte4.to_int() << 24)
    
    (reader4, value)
  }
  
  let binary_reader_read_string = fn(reader: BinaryReader) -> (BinaryReader, String) {
    let (reader1, length) = binary_reader_read_int(reader)
    
    let mut chars = []
    let mut current_reader = reader1
    
    for i in 0..=length-1 {
      let (next_reader, byte) = binary_reader_read_byte(current_reader)
      chars = chars + [byte.to_int().to_char()]
      current_reader = next_reader
    }
    
    // Convert chars to string (simplified)
    let mut result = ""
    for char in chars {
      result = result + char.to_string()
    }
    
    (current_reader, result)
  }
  
  // Test binary serialization
  let writer = binary_writer_new()
  
  // Write different data types
  let writer1 = binary_writer_write_int(writer, 42)
  let writer2 = binary_writer_write_string(writer1, "hello")
  let writer3 = binary_writer_write_int(writer2, 100)
  let writer4 = binary_writer_write_string(writer3, "world")
  
  // Read back the data
  let reader = binary_reader_new(writer4.buffer)
  
  let (reader1, int1) = binary_reader_read_int(reader)
  assert_eq(int1, 42)
  
  let (reader2, str1) = binary_reader_read_string(reader1)
  assert_eq(str1, "hello")
  
  let (reader3, int2) = binary_reader_read_int(reader2)
  assert_eq(int2, 100)
  
  let (reader4, str2) = binary_reader_read_string(reader3)
  assert_eq(str2, "world")
}

// Test 2: JSON Serialization
test "json serialization for telemetry data" {
  type JsonValue = {
    type: String,
    string_value: String,
    number_value: Float,
    bool_value: Bool,
    array_value: Array[JsonValue],
    object_value: Array[(String, JsonValue)]
  }
  
  let json_string = fn(value: String) -> JsonValue {
    {
      type: "string",
      string_value: value,
      number_value: 0.0,
      bool_value: false,
      array_value: [],
      object_value: []
    }
  }
  
  let json_number = fn(value: Float) -> JsonValue {
    {
      type: "number",
      string_value: "",
      number_value: value,
      bool_value: false,
      array_value: [],
      object_value: []
    }
  }
  
  let json_bool = fn(value: Bool) -> JsonValue {
    {
      type: "bool",
      string_value: "",
      number_value: 0.0,
      bool_value: value,
      array_value: [],
      object_value: []
    }
  }
  
  let json_array = fn(values: Array[JsonValue]) -> JsonValue {
    {
      type: "array",
      string_value: "",
      number_value: 0.0,
      bool_value: false,
      array_value: values,
      object_value: []
    }
  }
  
  let json_object = fn(pairs: Array[(String, JsonValue)]) -> JsonValue {
    {
      type: "object",
      string_value: "",
      number_value: 0.0,
      bool_value: false,
      array_value: [],
      object_value: pairs
    }
  }
  
  let json_to_string = fn(value: JsonValue) -> String {
    match value.type {
      "string" => "\"" + value.string_value + "\""
      "number" => value.number_value.to_string()
      "bool" => if value.bool_value { "true" } else { "false" }
      "array" => {
        let mut result = "["
        for i in 0..=value.array_value.length()-1 {
          if i > 0 {
            result = result + ", "
          }
          result = result + json_to_string(value.array_value[i])
        }
        result = result + "]"
        result
      }
      "object" => {
        let mut result = "{"
        for i in 0..=value.object_value.length()-1 {
          if i > 0 {
            result = result + ", "
          }
          let (key, val) = value.object_value[i]
          result = result + "\"" + key + "\": " + json_to_string(val)
        }
        result = result + "}"
        result
      }
      _ => "null"
    }
  }
  
  // Test JSON serialization
  let telemetry_data = json_object([
    ("trace_id", json_string("0af7651916cd43dd8448eb211c80319c")),
    ("span_id", json_string("b7ad6b7169203331")),
    ("name", json_string("HTTP GET /api/users")),
    ("duration", json_number(150.5)),
    ("success", json_bool(true)),
    ("tags", json_array([
      json_string("http"),
      json_string("api"),
      json_string("users")
    ])),
    ("attributes", json_object([
      ("http.method", json_string("GET")),
      ("http.status_code", json_number(200.0)),
      ("user.id", json_string("12345"))
    ]))
  ])
  
  let json_string = json_to_string(telemetry_data)
  
  // Verify JSON structure
  assert_true(json_string.contains("\"trace_id\": \"0af7651916cd43dd8448eb211c80319c\""))
  assert_true(json_string.contains("\"span_id\": \"b7ad6b7169203331\""))
  assert_true(json_string.contains("\"name\": \"HTTP GET /api/users\""))
  assert_true(json_string.contains("\"duration\": 150.5"))
  assert_true(json_string.contains("\"success\": true"))
  assert_true(json_string.contains("\"tags\": [\"http\", \"api\", \"users\"]"))
  assert_true(json_string.contains("\"http.method\": \"GET\""))
  assert_true(json_string.contains("\"http.status_code\": 200"))
}

// Test 3: Protocol Buffers Serialization
test "protocol buffers serialization for telemetry data" {
  type ProtoField = {
    field_number: Int,
    wire_type: Int,
    value: String
  }
  
  type ProtoMessage = {
    fields: Array[ProtoField]
  }
  
  let proto_message_new = fn() -> ProtoMessage {
    { fields: [] }
  }
  
  let proto_add_varint = fn(msg: ProtoMessage, field_number: Int, value: Int) -> ProtoMessage {
    // Wire type 0: Varint
    let field = {
      field_number,
      wire_type: 0,
      value: value.to_string()
    }
    { fields: msg.fields + [field] }
  }
  
  let proto_add_string = fn(msg: ProtoMessage, field_number: Int, value: String) -> ProtoMessage {
    // Wire type 2: Length-delimited
    let field = {
      field_number,
      wire_type: 2,
      value: value
    }
    { fields: msg.fields + [field] }
  }
  
  let proto_serialize = fn(msg: ProtoMessage) -> String {
    let mut result = ""
    
    for field in msg.fields {
      let key = (field.field_number << 3) | field.wire_type
      result = result + "key:" + key.to_string() + ","
      
      match field.wire_type {
        0 => result = result + "value:" + field.value + ";"  // Varint
        2 => result = result + "len:" + field.value.length().to_string() + ",value:" + field.value + ";"  // Length-delimited
        _ => result = result + "unknown;"
      }
    }
    
    result
  }
  
  // Test protocol buffers serialization
  let msg1 = proto_message_new()
  let msg2 = proto_add_string(msg1, 1, "0af7651916cd43dd8448eb211c80319c")  // trace_id
  let msg3 = proto_add_string(msg2, 2, "b7ad6b7169203331")  // span_id
  let msg4 = proto_add_string(msg3, 3, "HTTP GET /api/users")  // name
  let msg5 = proto_add_varint(msg4, 4, 150)  // duration
  let msg6 = proto_add_varint(msg5, 5, 1)  // success (1 = true)
  
  let serialized = proto_serialize(msg6)
  
  // Verify serialized format
  assert_true(serialized.contains("key:11"))  // Field 1, wire type 2
  assert_true(serialized.contains("key:22"))  // Field 2, wire type 2
  assert_true(serialized.contains("key:26"))  // Field 3, wire type 2
  assert_true(serialized.contains("key:32"))  // Field 4, wire type 0
  assert_true(serialized.contains("key:40"))  // Field 5, wire type 0
}

// Test 4: Avro Serialization
test "avro serialization for telemetry data" {
  type AvroSchema = {
    type: String,
    name: String,
    fields: Array[(String, String)]
  }
  
  type AvroRecord = {
    schema_name: String,
    values: Array[(String, String)]
  }
  
  let avro_schema_new = fn(name: String) -> AvroSchema {
    { type: "record", name, fields: [] }
  }
  
  let avro_schema_add_field = fn(schema: AvroSchema, name: String, field_type: String) -> AvroSchema {
    { 
      type: schema.type, 
      name: schema.name, 
      fields: schema.fields + [(name, field_type)] 
    }
  }
  
  let avro_record_new = fn(schema_name: String) -> AvroRecord {
    { schema_name, values: [] }
  }
  
  let avro_record_add_field = fn(record: AvroRecord, name: String, value: String) -> AvroRecord {
    { schema_name: record.schema_name, values: record.values + [(name, value)] }
  }
  
  let avro_serialize = fn(schema: AvroSchema, record: AvroRecord) -> String {
    let mut result = "schema:" + schema.name + "{"
    
    for i in 0..=schema.fields.length()-1 {
      if i > 0 {
        result = result + ","
      }
      let (field_name, field_type) = schema.fields[i]
      result = result + field_name + ":" + field_type
    }
    
    result = result + "}|values:"
    
    for i in 0..=record.values.length()-1 {
      if i > 0 {
        result = result + ","
      }
      let (field_name, field_value) = record.values[i]
      result = result + field_name + "=" + field_value
    }
    
    result
  }
  
  // Test Avro serialization
  let schema1 = avro_schema_new("TelemetrySpan")
  let schema2 = avro_schema_add_field(schema1, "trace_id", "string")
  let schema3 = avro_schema_add_field(schema2, "span_id", "string")
  let schema4 = avro_schema_add_field(schema3, "name", "string")
  let schema5 = avro_schema_add_field(schema4, "duration", "long")
  let schema6 = avro_schema_add_field(schema5, "success", "boolean")
  
  let record1 = avro_record_new("TelemetrySpan")
  let record2 = avro_record_add_field(record1, "trace_id", "0af7651916cd43dd8448eb211c80319c")
  let record3 = avro_record_add_field(record2, "span_id", "b7ad6b7169203331")
  let record4 = avro_record_add_field(record3, "name", "HTTP GET /api/users")
  let record5 = avro_record_add_field(record4, "duration", "150")
  let record6 = avro_record_add_field(record5, "success", "true")
  
  let serialized = avro_serialize(schema6, record6)
  
  // Verify serialized format
  assert_true(serialized.contains("schema:TelemetrySpan"))
  assert_true(serialized.contains("trace_id:string"))
  assert_true(serialized.contains("span_id:string"))
  assert_true(serialized.contains("name:string"))
  assert_true(serialized.contains("duration:long"))
  assert_true(serialized.contains("success:boolean"))
  assert_true(serialized.contains("trace_id=0af7651916cd43dd8448eb211c80319c"))
  assert_true(serialized.contains("span_id=b7ad6b7169203331"))
  assert_true(serialized.contains("name=HTTP GET /api/users"))
  assert_true(serialized.contains("duration=150"))
  assert_true(serialized.contains("success=true"))
}

// Test 5: MessagePack Serialization
test "messagepack serialization for telemetry data" {
  type MessagePackValue = {
    type: Int,  // MessagePack type code
    data: String
  }
  
  let msgpack_nil = fn() -> MessagePackValue {
    { type: 0xC0, data: "" }
  }
  
  let msgpack_bool = fn(value: Bool) -> MessagePackValue {
    if value {
      { type: 0xC3, data: "" }
    } else {
      { type: 0xC2, data: "" }
    }
  }
  
  let msgpack_int = fn(value: Int) -> MessagePackValue {
    if value >= 0 && value <= 127 {
      { type: value, data: "" }  // Positive fixint
    } else if value >= -32 && value < 0 {
      { type: 0xE0 + (value + 32), data: "" }  // Negative fixint
    } else {
      { type: 0xCC, data: value.to_string() }  // uint8 (simplified)
    }
  }
  
  let msgpack_string = fn(value: String) -> MessagePackValue {
    if value.length() <= 31 {
      { type: 0xA0 + value.length(), data: value }  // Fixstr
    } else {
      { type: 0xD9, data: value.length().to_string() + ":" + value }  // str8 (simplified)
    }
  }
  
  let msgpack_array = fn(values: Array[MessagePackValue]) -> MessagePackValue {
    if values.length() <= 15 {
      { type: 0x90 + values.length(), data: "" }  // Fixarray
    } else {
      { type: 0xDC, data: values.length().to_string() }  // array16 (simplified)
    }
  }
  
  let msgpack_map = fn(pairs: Array[(String, MessagePackValue)]) -> MessagePackValue {
    if pairs.length() <= 15 {
      { type: 0x80 + pairs.length(), data: "" }  // Fixmap
    } else {
      { type: 0xDE, data: pairs.length().to_string() }  // map16 (simplified)
    }
  }
  
  let msgpack_serialize = fn(value: MessagePackValue) -> String {
    let mut result = "type:0x" + value.type.to_string(16)
    
    if value.data.length() > 0 {
      result = result + ",data:" + value.data
    }
    
    result
  }
  
  // Test MessagePack serialization
  let trace_id = msgpack_string("0af7651916cd43dd8448eb211c80319c")
  let span_id = msgpack_string("b7ad6b7169203331")
  let name = msgpack_string("HTTP GET /api/users")
  let duration = msgpack_int(150)
  let success = msgpack_bool(true)
  
  let telemetry_map = msgpack_map([
    ("trace_id", trace_id),
    ("span_id", span_id),
    ("name", name),
    ("duration", duration),
    ("success", success)
  ])
  
  let serialized = msgpack_serialize(telemetry_map)
  
  // Verify serialized format
  assert_true(serialized.contains("type:0x8"))  // Fixmap with 5 elements (0x80 + 5)
}

// Test 6: CSV Serialization
test "csv serialization for telemetry metrics" {
  type CsvRow = {
    fields: Array[String]
  }
  
  let csv_row_new = fn() -> CsvRow {
    { fields: [] }
  }
  
  let csv_row_add_field = fn(row: CsvRow, value: String) -> CsvRow {
    { fields: row.fields + [value] }
  }
  
  let csv_escape_field = fn(field: String) -> String {
    if field.contains(",") || field.contains("\"") || field.contains("\n") {
      let escaped = field.replace("\"", "\"\"")
      "\"" + escaped + "\""
    } else {
      field
    }
  }
  
  let csv_serialize_row = fn(row: CsvRow) -> String {
    let mut result = ""
    
    for i in 0..=row.fields.length()-1 {
      if i > 0 {
        result = result + ","
      }
      result = result + csv_escape_field(row.fields[i])
    }
    
    result
  }
  
  let csv_serialize = fn(header: CsvRow, rows: Array[CsvRow]) -> String {
    let mut result = csv_serialize_row(header) + "\n"
    
    for row in rows {
      result = result + csv_serialize_row(row) + "\n"
    }
    
    result
  }
  
  // Test CSV serialization
  let header = csv_row_new()
  let header1 = csv_row_add_field(header, "timestamp")
  let header2 = csv_row_add_field(header1, "metric_name")
  let header3 = csv_row_add_field(header2, "value")
  let header4 = csv_row_add_field(header3, "tags")
  
  let row1 = csv_row_new()
  let row1_1 = csv_row_add_field(row1, "2023-01-01T00:00:00Z")
  let row1_2 = csv_row_add_field(row1_1, "http_requests_total")
  let row1_3 = csv_row_add_field(row1_2, "42")
  let row1_4 = csv_row_add_field(row1_3, "method=GET,endpoint=/api/users")
  
  let row2 = csv_row_new()
  let row2_1 = csv_row_add_field(row2, "2023-01-01T00:01:00Z")
  let row2_2 = csv_row_add_field(row2_1, "http_requests_total")
  let row2_3 = csv_row_add_field(row2_2, "38")
  let row2_4 = csv_row_add_field(row2_3, "method=POST,endpoint=/api/orders")
  
  let row3 = csv_row_new()
  let row3_1 = csv_row_add_field(row3, "2023-01-01T00:02:00Z")
  let row3_2 = csv_row_add_field(row3_1, "database_connections")
  let row3_3 = csv_row_add_field(row3_2, "5")
  let row3_4 = csv_row_add_field(row3_3, "pool=primary")
  
  let csv_data = csv_serialize(header4, [row1_4, row2_4, row3_4])
  
  // Verify CSV format
  assert_true(csv_data.contains("timestamp,metric_name,value,tags"))
  assert_true(csv_data.contains("2023-01-01T00:00:00Z,http_requests_total,42,method=GET,endpoint=/api/users"))
  assert_true(csv_data.contains("2023-01-01T00:01:00Z,http_requests_total,38,method=POST,endpoint=/api/orders"))
  assert_true(csv_data.contains("2023-01-01T00:02:00Z,database_connections,5,pool=primary"))
}

// Test 7: XML Serialization
test "xml serialization for telemetry data" {
  type XmlElement = {
    name: String,
    attributes: Array[(String, String)],
    children: Array[XmlElement],
    text_content: String
  }
  
  let xml_element_new = fn(name: String) -> XmlElement {
    {
      name,
      attributes: [],
      children: [],
      text_content: ""
    }
  }
  
  let xml_element_add_attribute = fn(element: XmlElement, name: String, value: String) -> XmlElement {
    {
      name: element.name,
      attributes: element.attributes + [(name, value)],
      children: element.children,
      text_content: element.text_content
    }
  }
  
  let xml_element_add_child = fn(element: XmlElement, child: XmlElement) -> XmlElement {
    {
      name: element.name,
      attributes: element.attributes,
      children: element.children + [child],
      text_content: element.text_content
    }
  }
  
  let xml_element_set_text = fn(element: XmlElement, text: String) -> XmlElement {
    {
      name: element.name,
      attributes: element.attributes,
      children: element.children,
      text_content: text
    }
  }
  
  let xml_escape = fn(text: String) -> String {
    text
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&apos;")
  }
  
  let xml_serialize = fn(element: XmlElement, indent: Int) -> String {
    let mut result = ""
    let mut spaces = ""
    for i in 0..=indent-1 {
      spaces = spaces + "  "
    }
    
    result = result + spaces + "<" + element.name
    
    for (name, value) in element.attributes {
      result = result + " " + name + "=\"" + xml_escape(value) + "\""
    }
    
    if element.children.length() == 0 && element.text_content.length() == 0 {
      result = result + " />"
    } else {
      result = result + ">"
      
      if element.text_content.length() > 0 {
        result = result + xml_escape(element.text_content)
      }
      
      if element.children.length() > 0 {
        result = result + "\n"
        for child in element.children {
          result = result + xml_serialize(child, indent + 1) + "\n"
        }
        result = result + spaces
      }
      
      result = result + "</" + element.name + ">"
    }
    
    result
  }
  
  // Test XML serialization
  let span_element = xml_element_new("span")
  let span1 = xml_element_add_attribute(span_element, "trace_id", "0af7651916cd43dd8448eb211c80319c")
  let span2 = xml_element_add_attribute(span1, "span_id", "b7ad6b7169203331")
  let span3 = xml_element_add_attribute(span2, "name", "HTTP GET /api/users")
  let span4 = xml_element_add_attribute(span3, "duration", "150")
  let span5 = xml_element_add_attribute(span4, "success", "true")
  
  let tags_element = xml_element_new("tags")
  let tag1 = xml_element_new("tag")
  let tag1_with_attr = xml_element_add_attribute(tag1, "key", "http.method")
  let tag1_with_text = xml_element_set_text(tag1_with_attr, "GET")
  
  let tag2 = xml_element_new("tag")
  let tag2_with_attr = xml_element_add_attribute(tag2, "key", "http.status_code")
  let tag2_with_text = xml_element_set_text(tag2_with_attr, "200")
  
  let tags_with_children = xml_element_add_child(tags_element, tag1_with_text)
  let tags_final = xml_element_add_child(tags_with_children, tag2_with_text)
  
  let span_final = xml_element_add_child(span5, tags_final)
  
  let xml_data = xml_serialize(span_final, 0)
  
  // Verify XML format
  assert_true(xml_data.contains("<span trace_id=\"0af7651916cd43dd8448eb211c80319c\""))
  assert_true(xml_data.contains("span_id=\"b7ad6b7169203331\""))
  assert_true(xml_data.contains("name=\"HTTP GET /api/users\""))
  assert_true(xml_data.contains("duration=\"150\""))
  assert_true(xml_data.contains("success=\"true\""))
  assert_true(xml_data.contains("<tags>"))
  assert_true(xml_data.contains("<tag key=\"http.method\">GET</tag>"))
  assert_true(xml_data.contains("<tag key=\"http.status_code\">200</tag>"))
}

// Test 8: YAML Serialization
test "yaml serialization for telemetry configuration" {
  type YamlValue = {
    type: String,
    string_value: String,
    number_value: Float,
    bool_value: Bool,
    array_value: Array[YamlValue],
    object_value: Array[(String, YamlValue)]
  }
  
  let yaml_string = fn(value: String) -> YamlValue {
    {
      type: "string",
      string_value: value,
      number_value: 0.0,
      bool_value: false,
      array_value: [],
      object_value: []
    }
  }
  
  let yaml_number = fn(value: Float) -> YamlValue {
    {
      type: "number",
      string_value: "",
      number_value: value,
      bool_value: false,
      array_value: [],
      object_value: []
    }
  }
  
  let yaml_bool = fn(value: Bool) -> YamlValue {
    {
      type: "bool",
      string_value: "",
      number_value: 0.0,
      bool_value: value,
      array_value: [],
      object_value: []
    }
  }
  
  let yaml_array = fn(values: Array[YamlValue]) -> YamlValue {
    {
      type: "array",
      string_value: "",
      number_value: 0.0,
      bool_value: false,
      array_value: values,
      object_value: []
    }
  }
  
  let yaml_object = fn(pairs: Array[(String, YamlValue)]) -> YamlValue {
    {
      type: "object",
      string_value: "",
      number_value: 0.0,
      bool_value: false,
      array_value: [],
      object_value: pairs
    }
  }
  
  let yaml_to_string = fn(value: YamlValue, indent: Int) -> String {
    let mut spaces = ""
    for i in 0..=indent-1 {
      spaces = spaces + "  "
    }
    
    match value.type {
      "string" => {
        if value.string_value.contains("\n") {
          spaces + "|\n" + spaces + "  " + value.string_value.replace("\n", "\n" + spaces + "  ")
        } else {
          spaces + value.string_value
        }
      }
      "number" => spaces + value.number_value.to_string()
      "bool" => spaces + if value.bool_value { "true" } else { "false" }
      "array" => {
        let mut result = ""
        for item in value.array_value {
          if result.length() > 0 {
            result = result + "\n"
          }
          result = result + spaces + "- " + yaml_to_string(item, 0)
        }
        result
      }
      "object" => {
        let mut result = ""
        for i in 0..=value.object_value.length()-1 {
          if i > 0 {
            result = result + "\n"
          }
          let (key, val) = value.object_value[i]
          result = result + spaces + key + ": " + yaml_to_string(val, indent + 2)
        }
        result
      }
      _ => spaces + "null"
    }
  }
  
  // Test YAML serialization
  let telemetry_config = yaml_object([
    ("service", yaml_object([
      ("name", yaml_string("telemetry-service")),
      ("version", yaml_string("1.0.0")),
      ("environment", yaml_string("production"))
    ])),
    ("sampling", yaml_object([
      ("enabled", yaml_bool(true)),
      ("rate", yaml_number(0.1))
    ])),
    ("exporters", yaml_array([
      yaml_object([
        ("type", yaml_string("jaeger")),
        ("endpoint", yaml_string("http://jaeger:14268/api/traces"))
      ]),
      yaml_object([
        ("type", yaml_string("prometheus")),
        ("port", yaml_number(9090))
      ])
    ]))
  ])
  
  let yaml_data = yaml_to_string(telemetry_config, 0)
  
  // Verify YAML format
  assert_true(yaml_data.contains("service:"))
  assert_true(yaml_data.contains("name: telemetry-service"))
  assert_true(yaml_data.contains("version: 1.0.0"))
  assert_true(yaml_data.contains("environment: production"))
  assert_true(yaml_data.contains("sampling:"))
  assert_true(yaml_data.contains("enabled: true"))
  assert_true(yaml_data.contains("rate: 0.1"))
  assert_true(yaml_data.contains("exporters:"))
  assert_true(yaml_data.contains("- type: jaeger"))
  assert_true(yaml_data.contains("endpoint: http://jaeger:14268/api/traces"))
  assert_true(yaml_data.contains("- type: prometheus"))
  assert_true(yaml_data.contains("port: 9090"))
}

// Test 9: Custom Binary Format
test "custom binary format for telemetry data" {
  type CustomFormat = {
    magic_bytes: Array[Byte],
    version: Int,
    entries: Array[CustomEntry]
  }
  
  type CustomEntry = {
    type_id: Int,
    data: Array[Byte]
  }
  
  let custom_format_new = fn(version: Int) -> CustomFormat {
    {
      magic_bytes: [Byte::from_int(0x54), Byte::from_int(0x45), Byte::from_int(0x4C)],  // "TEL"
      version,
      entries: []
    }
  }
  
  let custom_add_string_entry = fn(format: CustomFormat, type_id: Int, value: String) -> CustomFormat {
    let mut data = []
    for char in value.to_char_array() {
      data = data + [Byte::from_int(char.to_int())]
    }
    
    {
      magic_bytes: format.magic_bytes,
      version: format.version,
      entries: format.entries + [{ type_id, data }]
    }
  }
  
  let custom_add_int_entry = fn(format: CustomFormat, type_id: Int, value: Int) -> CustomFormat {
    let data = [
      Byte::from_int(value & 0xFF),
      Byte::from_int((value >> 8) & 0xFF),
      Byte::from_int((value >> 16) & 0xFF),
      Byte::from_int((value >> 24) & 0xFF)
    ]
    
    {
      magic_bytes: format.magic_bytes,
      version: format.version,
      entries: format.entries + [{ type_id, data }]
    }
  }
  
  let custom_serialize = fn(format: CustomFormat) -> String {
    let mut result = "magic:"
    
    for byte in format.magic_bytes {
      result = result + byte.to_string(16)
    }
    
    result = result + ",version:" + format.version.to_string() + ",entries:"
    
    for entry in format.entries {
      result = result + "type:" + entry.type_id.to_string() + ",data:"
      
      for byte in entry.data {
        result = result + byte.to_string(16)
      }
      
      result = result + ";"
    }
    
    result
  }
  
  // Test custom format
  let format1 = custom_format_new(1)
  let format2 = custom_add_string_entry(format1, 1, "0af7651916cd43dd8448eb211c80319c")  // trace_id
  let format3 = custom_add_string_entry(format2, 2, "b7ad6b7169203331")  // span_id
  let format4 = custom_add_string_entry(format3, 3, "HTTP GET /api/users")  // name
  let format5 = custom_add_int_entry(format4, 4, 150)  // duration
  let format6 = custom_add_int_entry(format5, 5, 1)  // success (1 = true)
  
  let serialized = custom_serialize(format6)
  
  // Verify custom format
  assert_true(serialized.contains("magic:54454c"))  // "TEL" in hex
  assert_true(serialized.contains("version:1"))
  assert_true(serialized.contains("type:1"))  // trace_id
  assert_true(serialized.contains("type:2"))  // span_id
  assert_true(serialized.contains("type:3"))  // name
  assert_true(serialized.contains("type:4"))  // duration
  assert_true(serialized.contains("type:5"))  // success
}

// Test 10: Format Conversion
test "format conversion between serialization formats" {
  // Convert JSON to XML
  let json_to_xml = fn(json_string: String) -> String {
    // Very simplified conversion
    let mut xml = "<root>\n"
    
    // Parse key-value pairs from simplified JSON
    let pairs = json_string.split(",")
    
    for pair in pairs {
      if pair.contains(":") {
        let parts = pair.split(":")
        if parts.length() >= 2 {
          let key = parts[0].replace("\"", "").replace(" ", "")
          let value = parts[1].replace("\"", "").replace(" ", "").replace("}", "")
          
          if key.length() > 0 && value.length() > 0 {
            xml = xml + "  <" + key + ">" + value + "</" + key + ">\n"
          }
        }
      }
    }
    
    xml = xml + "</root>"
    xml
  }
  
  // Convert XML to JSON
  let xml_to_json = fn(xml_string: String) -> String {
    // Very simplified conversion
    let mut json = "{\n"
    let mut first = true
    
    // Parse elements from simplified XML
    let lines = xml_string.split("\n")
    
    for line in lines {
      if line.contains("<") && line.contains(">") && !line.contains("</") {
        let start_tag = line.split("<")[1].split(">")[0]
        let end_tag_index = line.split("</").length()
        
        if end_tag_index > 1 {
          let value = line.split(">")[1].split("<")[0]
          
          if !first {
            json = json + ",\n"
          }
          
          json = json + "  \"" + start_tag + "\": \"" + value + "\""
          first = false
        }
      }
    }
    
    json = json + "\n}"
    json
  }
  
  // Test format conversion
  let simple_json = "{\"name\":\"test\",\"value\":42,\"enabled\":true}"
  let converted_xml = json_to_xml(simple_json)
  
  // Verify XML conversion
  assert_true(converted_xml.contains("<root>"))
  assert_true(converted_xml.contains("<name>test</name>"))
  assert_true(converted_xml.contains("<value>42</value>"))
  assert_true(converted_xml.contains("<enabled>true</enabled>"))
  assert_true(converted_xml.contains("</root>"))
  
  // Convert back to JSON
  let converted_back_json = xml_to_json(converted_xml)
  
  // Verify JSON conversion
  assert_true(converted_back_json.contains("\"name\": \"test\""))
  assert_true(converted_back_json.contains("\"value\": \"42\""))
  assert_true(converted_back_json.contains("\"enabled\": \"true\""))
}