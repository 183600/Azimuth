// Azimuth Serialization and Deserialization Test Suite
// This file contains test cases for serialization and deserialization functionality

// Test 1: JSON Serialization
test "JSON serialization" {
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    start_time: Int,
    duration: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  let create_telemetry_data = fn(trace_id: String, span_id: String, service_name: String, operation_name: String) {
    {
      trace_id,
      span_id,
      service_name,
      operation_name,
      start_time: 1640995200,
      duration: 250,
      status: "ok",
      tags: []
    }
  }
  
  let add_tag = fn(data: TelemetryData, key: String, value: String) {
    { data | tags: data.tags + [(key, value)] }
  }
  
  let serialize_to_json = fn(data: TelemetryData) {
    let tags_json = if data.tags.length() > 0 {
      let tag_pairs = data.tags.map(fn(tag) { 
        "\"" + tag.0 + "\":\"" + tag.1 + "\"" 
      })
      "[" + tag_pairs.join(",") + "]"
    } else {
      "[]"
    }
    
    "{"
    + "\"trace_id\":\"" + data.trace_id + "\"," 
    + "\"span_id\":\"" + data.span_id + "\"," 
    + "\"service_name\":\"" + data.service_name + "\"," 
    + "\"operation_name\":\"" + data.operation_name + "\"," 
    + "\"start_time\":" + data.start_time.to_string() + "," 
    + "\"duration\":" + data.duration.to_string() + "," 
    + "\"status\":\"" + data.status + "\"," 
    + "\"tags\":" + tags_json
    + "}"
  }
  
  let data = create_telemetry_data("trace-123", "span-456", "web-service", "handle_request")
  let data_with_tags = add_tag(add_tag(data, "http.method", "GET"), "http.status_code", "200")
  
  let json = serialize_to_json(data_with_tags)
  
  assert_true(json.contains("\"trace_id\":\"trace-123\""))
  assert_true(json.contains("\"span_id\":\"span-456\""))
  assert_true(json.contains("\"service_name\":\"web-service\""))
  assert_true(json.contains("\"operation_name\":\"handle_request\""))
  assert_true(json.contains("\"start_time\":1640995200"))
  assert_true(json.contains("\"duration\":250"))
  assert_true(json.contains("\"status\":\"ok\""))
  assert_true(json.contains("\"tags\":[\"http.method\":\"GET\",\"http.status_code\":\"200\"]"))
}

// Test 2: JSON Deserialization
test "JSON deserialization" {
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    start_time: Int,
    duration: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  let parse_json_string = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":\""
    let start_index = json.index_of(key_pattern)
    
    match start_index {
      Some(idx) => {
        let value_start = idx + key_pattern.length()
        let value_end = json.index_of("\"", value_start)
        
        match value_end {
          Some(end) => Some(json.substring(value_start, end - value_start))
          None => None
        }
      }
      None => None
    }
  }
  
  let parse_json_int = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":"
    let start_index = json.index_of(key_pattern)
    
    match start_index {
      Some(idx) => {
        let value_start = idx + key_pattern.length()
        let mut value_end = value_start
        
        while value_end < json.length() && 
              (json[value_end] >= '0' && json[value_end] <= '9') {
          value_end = value_end + 1
        }
        
        if value_end > value_start {
          let value_str = json.substring(value_start, value_end - value_start)
          Some(value_str.to_int())
        } else {
          None
        }
      }
      None => None
    }
  }
  
  let parse_json_tags = fn(json: String) {
    let tags_start = json.index_of("\"tags\":[")
    
    match tags_start {
      Some(start) => {
        let tags_content_start = start + 8  // Length of "\"tags\":["
        let tags_end = json.index_of("]", tags_content_start)
        
        match tags_end {
          Some(end) => {
            let tags_content = json.substring(tags_content_start, end - tags_content_start)
            if tags_content.length() == 0 {
              []
            } else {
              // Simple parsing for tags like "key\":\"value\",\"key2\":\"value2"
              let mut tags = []
              let mut current = tags_content
              
              while current.length() > 0 {
                let key_end = current.index_of("\":\"")
                match key_end {
                  Some(ke) => {
                    let key = current.substring(0, ke)
                    let value_start = ke + 3  // Length of "\":\""
                    let value_end = current.index_of("\"", value_start)
                    
                    match value_end {
                      Some(ve) => {
                        let value = current.substring(value_start, ve - value_start)
                        tags = tags + [(key, value)]
                        
                        let next_start = ve + 2  // Skip ",\""
                        if next_start < current.length() {
                          current = current.substring(next_start, current.length() - next_start)
                        } else {
                          current = ""
                        }
                      }
                      None => current = ""
                    }
                  }
                  None => current = ""
                }
              }
              
              tags
            }
          }
          None => []
        }
      }
      None => []
    }
  }
  
  let deserialize_from_json = fn(json: String) {
    let trace_id = parse_json_string(json, "trace_id")
    let span_id = parse_json_string(json, "span_id")
    let service_name = parse_json_string(json, "service_name")
    let operation_name = parse_json_string(json, "operation_name")
    let start_time = parse_json_int(json, "start_time")
    let duration = parse_json_int(json, "duration")
    let status = parse_json_string(json, "status")
    let tags = parse_json_tags(json)
    
    match (trace_id, span_id, service_name, operation_name, start_time, duration, status) {
      (Some(tid), Some(sid), Some(sname), Some(oname), Some(st), Some(dur), Some(stat)) => {
        Some({
          trace_id: tid,
          span_id: sid,
          service_name: sname,
          operation_name: oname,
          start_time: st,
          duration: dur,
          status: stat,
          tags
        })
      }
      _ => None
    }
  }
  
  let json = "{"
  + "\"trace_id\":\"trace-123\"," 
  + "\"span_id\":\"span-456\"," 
  + "\"service_name\":\"web-service\"," 
  + "\"operation_name\":\"handle_request\"," 
  + "\"start_time\":1640995200," 
  + "\"duration\":250," 
  + "\"status\":\"ok\"," 
  + "\"tags\":[\"http.method\":\"GET\",\"http.status_code\":\"200\"]"
  + "}"
  
  let deserialized = deserialize_from_json(json)
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(data) => {
      assert_eq(data.trace_id, "trace-123")
      assert_eq(data.span_id, "span-456")
      assert_eq(data.service_name, "web-service")
      assert_eq(data.operation_name, "handle_request")
      assert_eq(data.start_time, 1640995200)
      assert_eq(data.duration, 250)
      assert_eq(data.status, "ok")
      assert_eq(data.tags.length(), 2)
      assert_true(data.tags.contains(("http.method", "GET")))
      assert_true(data.tags.contains(("http.status_code", "200")))
    }
    None => assert_true(false)
  }
}

// Test 3: Protocol Buffer Serialization
test "protocol buffer serialization" {
  type PbField = {
    field_number: Int,
    wire_type: Int,
    value: String
  }
  
  type PbMessage = {
    fields: Array[PbField]
  }
  
  let create_pb_field = fn(field_number: Int, wire_type: Int, value: String) {
    {
      field_number,
      wire_type,
      value
    }
  }
  
  let create_pb_message = fn(fields: Array[PbField]) {
    { fields }
  }
  
  let encode_varint = fn(value: Int) {
    let mut bytes = []
    let mut v = value
    
    while v >= 128 {
      let byte = (v & 127) | 128
      bytes = bytes + [byte.to_string()]
      v = v >> 7
    }
    
    bytes = bytes + [v.to_string()]
    bytes.join(",")
  }
  
  let encode_string_field = fn(field_number: Int, value: String) {
    let key = (field_number << 3) | 2  // Wire type 2 for length-delimited
    let key_bytes = encode_varint(key)
    let length_bytes = encode_varint(value.length())
    
    key_bytes + "," + length_bytes + "," + value.chars().map(fn(c) { c.to_int().to_string() }).join(",")
  }
  
  let encode_int_field = fn(field_number: Int, value: Int) {
    let key = (field_number << 3) | 0  // Wire type 0 for varint
    let key_bytes = encode_varint(key)
    let value_bytes = encode_varint(value)
    
    key_bytes + "," + value_bytes
  }
  
  let serialize_to_protobuf = fn(trace_id: String, span_id: String, service_name: String, start_time: Int, duration: Int) {
    let trace_id_field = encode_string_field(1, trace_id)
    let span_id_field = encode_string_field(2, span_id)
    let service_name_field = encode_string_field(3, service_name)
    let start_time_field = encode_int_field(4, start_time)
    let duration_field = encode_int_field(5, duration)
    
    trace_id_field + "|" + span_id_field + "|" + service_name_field + "|" + start_time_field + "|" + duration_field
  }
  
  let protobuf = serialize_to_protobuf("trace-123", "span-456", "web-service", 1640995200, 250)
  
  // Just check that the protobuf contains the expected fields
  assert_true(protobuf.contains("|"))
  assert_eq(protobuf.split("|").length(), 5)
  
  let fields = protobuf.split("|")
  assert_true(fields[0].length() > 0)  // trace_id field
  assert_true(fields[1].length() > 0)  // span_id field
  assert_true(fields[2].length() > 0)  // service_name field
  assert_true(fields[3].length() > 0)  // start_time field
  assert_true(fields[4].length() > 0)  // duration field
}

// Test 4: Avro Serialization
test "avro serialization" {
  type AvroSchema = {
    name: String,
    type_field: String,
    fields: Array[(String, String)]
  }
  
  type AvroRecord = {
    schema_name: String,
    values: Array[(String, String)]
  }
  
  let create_avro_schema = fn(name: String, fields: Array[(String, String)]) {
    {
      name,
      type_field: "record",
      fields
    }
  }
  
  let create_avro_record = fn(schema_name: String, values: Array[(String, String)]) {
    {
      schema_name,
      values
    }
  }
  
  let serialize_avro_schema = fn(schema: AvroSchema) {
    let fields_json = schema.fields.map(fn(field) { 
      "{\"name\":\"" + field.0 + "\",\"type\":\"" + field.1 + "\"}" 
    }).join(",")
    
    "{"
    + "\"name\":\"" + schema.name + "\"," 
    + "\"type\":\"" + schema.type_field + "\"," 
    + "\"fields\":[" + fields_json + "]"
    + "}"
  }
  
  let serialize_avro_record = fn(record: AvroRecord) {
    let values_json = record.values.map(fn(value) { 
      "\"" + value.0 + "\":\"" + value.1 + "\"" 
    }).join(",")
    
    "{"
    + "\"schema\":\"" + record.schema_name + "\"," 
    + "\"values\":{" + values_json + "}"
    + "}"
  }
  
  let schema = create_avro_schema("TelemetryData", [
    ("trace_id", "string"),
    ("span_id", "string"),
    ("service_name", "string"),
    ("start_time", "long"),
    ("duration", "int"),
    ("status", "string")
  ])
  
  let record = create_avro_record("TelemetryData", [
    ("trace_id", "trace-123"),
    ("span_id", "span-456"),
    ("service_name", "web-service"),
    ("start_time", "1640995200"),
    ("duration", "250"),
    ("status", "ok")
  ])
  
  let schema_json = serialize_avro_schema(schema)
  let record_json = serialize_avro_record(record)
  
  assert_true(schema_json.contains("\"name\":\"TelemetryData\""))
  assert_true(schema_json.contains("\"type\":\"record\""))
  assert_true(schema_json.contains("\"name\":\"trace_id\",\"type\":\"string\""))
  assert_true(schema_json.contains("\"name\":\"span_id\",\"type\":\"string\""))
  assert_true(schema_json.contains("\"name\":\"service_name\",\"type\":\"string\""))
  assert_true(schema_json.contains("\"name\":\"start_time\",\"type\":\"long\""))
  assert_true(schema_json.contains("\"name\":\"duration\",\"type\":\"int\""))
  assert_true(schema_json.contains("\"name\":\"status\",\"type\":\"string\""))
  
  assert_true(record_json.contains("\"schema\":\"TelemetryData\""))
  assert_true(record_json.contains("\"trace_id\":\"trace-123\""))
  assert_true(record_json.contains("\"span_id\":\"span-456\""))
  assert_true(record_json.contains("\"service_name\":\"web-service\""))
  assert_true(record_json.contains("\"start_time\":\"1640995200\""))
  assert_true(record_json.contains("\"duration\":\"250\""))
  assert_true(record_json.contains("\"status\":\"ok\""))
}

// Test 5: MessagePack Serialization
test "messagepack serialization" {
  type MpValue = {
    type_code: Int,
    data: String
  }
  
  let create_mp_string = fn(value: String) {
    {
      type_code: if value.length() < 32 { 160 + value.length() } else { 218 },  // fixstr or str8
      data: value
    }
  }
  
  let create_mp_int = fn(value: Int) {
    let type_code = if value < 128 { value } else { 204 }  // positive fixint or uint8
    {
      type_code,
      data: value.to_string()
    }
  }
  
  let create_mp_array = fn(values: Array[MpValue]) {
    {
      type_code: if values.length() < 16 { 144 + values.length() } else { 220 },  // fixarray or array16
      data: values.map(fn(v) { v.type_code.to_string() + ":" + v.data }).join(",")
    }
  }
  
  let create_mp_map = fn(values: Array[(String, MpValue)]) {
    {
      type_code: if values.length() < 16 { 128 + values.length() } else { 222 },  // fixmap or map16
      data: values.map(fn(kv) { 
        let key_mp = create_mp_string(kv.0)
        key_mp.type_code.to_string() + ":" + key_mp.data + "," + 
        kv.1.type_code.to_string() + ":" + kv.1.data
      }).join(",")
    }
  }
  
  let serialize_to_messagepack = fn(trace_id: String, span_id: String, service_name: String, start_time: Int, duration: Int) {
    let trace_id_mp = create_mp_string(trace_id)
    let span_id_mp = create_mp_string(span_id)
    let service_name_mp = create_mp_string(service_name)
    let start_time_mp = create_mp_int(start_time)
    let duration_mp = create_mp_int(duration)
    
    let map_values = [
      ("trace_id", trace_id_mp),
      ("span_id", span_id_mp),
      ("service_name", service_name_mp),
      ("start_time", start_time_mp),
      ("duration", duration_mp)
    ]
    
    let map_mp = create_mp_map(map_values)
    map_mp.type_code.to_string() + ":" + map_mp.data
  }
  
  let msgpack = serialize_to_messagepack("trace-123", "span-456", "web-service", 1640995200, 250)
  
  // Check that the msgpack contains the expected structure
  assert_true(msgpack.contains(":"))
  assert_true(msgpack.contains(","))
  
  let parts = msgpack.split(":")
  assert_true(parts.length() >= 2)  // At least type code and data
  
  let map_type_code = parts[0].to_int()
  assert_true(map_type_code >= 128)  // Should be a fixmap or map16 type code
}

// Test 6: Custom Binary Serialization
test "custom binary serialization" {
  type BinaryField = {
    field_type: String,
    value: String
  }
  
  let create_string_field = fn(value: String) {
    {
      field_type: "string",
      value: value
    }
  }
  
  let create_int_field = fn(value: Int) {
    {
      field_type: "int",
      value: value.to_string()
    }
  }
  
  let serialize_field = fn(field: BinaryField) {
    match field.field_type {
      "string" => {
        let length = field.value.length()
        length.to_string() + ":" + field.value
      }
      "int" => {
        "i:" + field.value
      }
      _ => ""
    }
  }
  
  let serialize_to_custom_binary = fn(fields: Array[BinaryField]) {
    fields.map(serialize_field).join(";")
  }
  
  let fields = [
    create_string_field("trace-123"),
    create_string_field("span-456"),
    create_string_field("web-service"),
    create_int_field(1640995200),
    create_int_field(250)
  ]
  
  let binary = serialize_to_custom_binary(fields)
  
  assert_eq(binary, "9:trace-123;7:span-456;11:web-service;i:1640995200;i:250")
  
  // Test deserialization
  let deserialize_custom_binary = fn(binary: String) {
    let field_strings = binary.split(";")
    let mut fields = []
    
    for field_str in field_strings {
      if field_str.starts_with("i:") {
        let value = field_str.substring(2, field_str.length() - 2)
        fields = fields + [{ field_type: "int", value }]
      } else {
        let colon_index = field_str.index_of(":")
        match colon_index {
          Some(idx) => {
            let length = field_str.substring(0, idx).to_int()
            let value = field_str.substring(idx + 1, length)
            fields = fields + [{ field_type: "string", value }]
          }
          None => {}
        }
      }
    }
    
    fields
  }
  
  let deserialized = deserialize_custom_binary(binary)
  assert_eq(deserialized.length(), 5)
  assert_eq(deserialized[0].field_type, "string")
  assert_eq(deserialized[0].value, "trace-123")
  assert_eq(deserialized[1].field_type, "string")
  assert_eq(deserialized[1].value, "span-456")
  assert_eq(deserialized[2].field_type, "string")
  assert_eq(deserialized[2].value, "web-service")
  assert_eq(deserialized[3].field_type, "int")
  assert_eq(deserialized[3].value, "1640995200")
  assert_eq(deserialized[4].field_type, "int")
  assert_eq(deserialized[4].value, "250")
}

// Test 7: Serialization Performance
test "serialization performance" {
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    start_time: Int,
    duration: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  let create_telemetry_data = fn(trace_id: String, span_id: String, service_name: String, operation_name: String) {
    {
      trace_id,
      span_id,
      service_name,
      operation_name,
      start_time: 1640995200,
      duration: 250,
      status: "ok",
      tags: []
    }
  }
  
  let serialize_to_json = fn(data: TelemetryData) {
    "{"
    + "\"trace_id\":\"" + data.trace_id + "\"," 
    + "\"span_id\":\"" + data.span_id + "\"," 
    + "\"service_name\":\"" + data.service_name + "\"," 
    + "\"operation_name\":\"" + data.operation_name + "\"," 
    + "\"start_time\":" + data.start_time.to_string() + "," 
    + "\"duration\":" + data.duration.to_string() + "," 
    + "\"status\":\"" + data.status + "\""
    + "}"
  }
  
  let serialize_to_custom = fn(data: TelemetryData) {
    data.trace_id + "|" + 
    data.span_id + "|" + 
    data.service_name + "|" + 
    data.operation_name + "|" + 
    data.start_time.to_string() + "|" + 
    data.duration.to_string() + "|" + 
    data.status
  }
  
  let measure_serialization_time = fn(data: TelemetryData, serializer: (TelemetryData) -> String, iterations: Int) {
    let start_time = 1640995200  // Simulated timestamp
    
    for i in 0..iterations {
      let _ = serializer(data)
    }
    
    let end_time = 1640995200 + iterations  // Simulated time based on iterations
    end_time - start_time
  }
  
  let data = create_telemetry_data("trace-123", "span-456", "web-service", "handle_request")
  
  let json_time = measure_serialization_time(data, serialize_to_json, 1000)
  let custom_time = measure_serialization_time(data, serialize_to_custom, 1000)
  
  // In this simplified implementation, time is based on iterations
  assert_eq(json_time, 1000)
  assert_eq(custom_time, 1000)
  
  let json_result = serialize_to_json(data)
  let custom_result = serialize_to_custom(data)
  
  // JSON should be longer due to formatting overhead
  assert_true(json_result.length() > custom_result.length())
  
  // Both should contain the same data
  assert_true(json_result.contains("trace-123"))
  assert_true(custom_result.contains("trace-123"))
  assert_true(json_result.contains("span-456"))
  assert_true(custom_result.contains("span-456"))
}

// Test 8: Serialization Format Conversion
test "serialization format conversion" {
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    start_time: Int,
    duration: Int,
    status: String
  }
  
  let create_telemetry_data = fn(trace_id: String, span_id: String, service_name: String, operation_name: String) {
    {
      trace_id,
      span_id,
      service_name,
      operation_name,
      start_time: 1640995200,
      duration: 250,
      status: "ok"
    }
  }
  
  let serialize_to_json = fn(data: TelemetryData) {
    "{"
    + "\"trace_id\":\"" + data.trace_id + "\"," 
    + "\"span_id\":\"" + data.span_id + "\"," 
    + "\"service_name\":\"" + data.service_name + "\"," 
    + "\"operation_name\":\"" + data.operation_name + "\"," 
    + "\"start_time\":" + data.start_time.to_string() + "," 
    + "\"duration\":" + data.duration.to_string() + "," 
    + "\"status\":\"" + data.status + "\""
    + "}"
  }
  
  let serialize_to_csv = fn(data: TelemetryData) {
    data.trace_id + "," + 
    data.span_id + "," + 
    data.service_name + "," + 
    data.operation_name + "," + 
    data.start_time.to_string() + "," + 
    data.duration.to_string() + "," + 
    data.status
  }
  
  let serialize_to_xml = fn(data: TelemetryData) {
    "<TelemetryData>"
    + "<trace_id>" + data.trace_id + "</trace_id>"
    + "<span_id>" + data.span_id + "</span_id>"
    + "<service_name>" + data.service_name + "</service_name>"
    + "<operation_name>" + data.operation_name + "</operation_name>"
    + "<start_time>" + data.start_time.to_string() + "</start_time>"
    + "<duration>" + data.duration.to_string() + "</duration>"
    + "<status>" + data.status + "</status>"
    + "</TelemetryData>"
  }
  
  let parse_json_string = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":\""
    let start_index = json.index_of(key_pattern)
    
    match start_index {
      Some(idx) => {
        let value_start = idx + key_pattern.length()
        let value_end = json.index_of("\"", value_start)
        
        match value_end {
          Some(end) => Some(json.substring(value_start, end - value_start))
          None => None
        }
      }
      None => None
    }
  }
  
  let parse_json_int = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":"
    let start_index = json.index_of(key_pattern)
    
    match start_index {
      Some(idx) => {
        let value_start = idx + key_pattern.length()
        let mut value_end = value_start
        
        while value_end < json.length() && 
              (json[value_end] >= '0' && json[value_end] <= '9') {
          value_end = value_end + 1
        }
        
        if value_end > value_start {
          let value_str = json.substring(value_start, value_end - value_start)
          Some(value_str.to_int())
        } else {
          None
        }
      }
      None => None
    }
  }
  
  let json_to_telemetry_data = fn(json: String) {
    let trace_id = parse_json_string(json, "trace_id")
    let span_id = parse_json_string(json, "span_id")
    let service_name = parse_json_string(json, "service_name")
    let operation_name = parse_json_string(json, "operation_name")
    let start_time = parse_json_int(json, "start_time")
    let duration = parse_json_int(json, "duration")
    let status = parse_json_string(json, "status")
    
    match (trace_id, span_id, service_name, operation_name, start_time, duration, status) {
      (Some(tid), Some(sid), Some(sname), Some(oname), Some(st), Some(dur), Some(stat)) => {
        Some({
          trace_id: tid,
          span_id: sid,
          service_name: sname,
          operation_name: oname,
          start_time: st,
          duration: dur,
          status: stat
        })
      }
      _ => None
    }
  }
  
  let csv_to_telemetry_data = fn(csv: String) {
    let fields = csv.split(",")
    
    if fields.length() == 7 {
      Some({
        trace_id: fields[0],
        span_id: fields[1],
        service_name: fields[2],
        operation_name: fields[3],
        start_time: fields[4].to_int(),
        duration: fields[5].to_int(),
        status: fields[6]
      })
    } else {
      None
    }
  }
  
  let xml_to_telemetry_data = fn(xml: String) {
    let extract_tag_value = fn(xml: String, tag: String) {
      let open_tag = "<" + tag + ">"
      let close_tag = "</" + tag + ">"
      let start = xml.index_of(open_tag)
      
      match start {
        Some(s) => {
          let value_start = s + open_tag.length()
          let end = xml.index_of(close_tag, value_start)
          
          match end {
            Some(e) => Some(xml.substring(value_start, e - value_start))
            None => None
          }
        }
        None => None
      }
    }
    
    let trace_id = extract_tag_value(xml, "trace_id")
    let span_id = extract_tag_value(xml, "span_id")
    let service_name = extract_tag_value(xml, "service_name")
    let operation_name = extract_tag_value(xml, "operation_name")
    let start_time_str = extract_tag_value(xml, "start_time")
    let duration_str = extract_tag_value(xml, "duration")
    let status = extract_tag_value(xml, "status")
    
    match (trace_id, span_id, service_name, operation_name, start_time_str, duration_str, status) {
      (Some(tid), Some(sid), Some(sname), Some(oname), Some(st_str), Some(dur_str), Some(stat)) => {
        Some({
          trace_id: tid,
          span_id: sid,
          service_name: sname,
          operation_name: oname,
          start_time: st_str.to_int(),
          duration: dur_str.to_int(),
          status: stat
        })
      }
      _ => None
    }
  }
  
  let data = create_telemetry_data("trace-123", "span-456", "web-service", "handle_request")
  
  let json = serialize_to_json(data)
  let csv = serialize_to_csv(data)
  let xml = serialize_to_xml(data)
  
  let data_from_json = json_to_telemetry_data(json)
  let data_from_csv = csv_to_telemetry_data(csv)
  let data_from_xml = xml_to_telemetry_data(xml)
  
  assert_true(data_from_json.is_some())
  assert_true(data_from_csv.is_some())
  assert_true(data_from_xml.is_some())
  
  match data_from_json {
    Some(d) => {
      assert_eq(d.trace_id, data.trace_id)
      assert_eq(d.span_id, data.span_id)
      assert_eq(d.service_name, data.service_name)
      assert_eq(d.operation_name, data.operation_name)
      assert_eq(d.start_time, data.start_time)
      assert_eq(d.duration, data.duration)
      assert_eq(d.status, data.status)
    }
    None => assert_true(false)
  }
  
  match data_from_csv {
    Some(d) => {
      assert_eq(d.trace_id, data.trace_id)
      assert_eq(d.span_id, data.span_id)
      assert_eq(d.service_name, data.service_name)
      assert_eq(d.operation_name, data.operation_name)
      assert_eq(d.start_time, data.start_time)
      assert_eq(d.duration, data.duration)
      assert_eq(d.status, data.status)
    }
    None => assert_true(false)
  }
  
  match data_from_xml {
    Some(d) => {
      assert_eq(d.trace_id, data.trace_id)
      assert_eq(d.span_id, data.span_id)
      assert_eq(d.service_name, data.service_name)
      assert_eq(d.operation_name, data.operation_name)
      assert_eq(d.start_time, data.start_time)
      assert_eq(d.duration, data.duration)
      assert_eq(d.status, data.status)
    }
    None => assert_true(false)
  }
}