// Azimuth Serialization and Deserialization Test Suite
// This file contains test cases for serialization and deserialization of telemetry data

// Test 1: JSON Serialization for Spans
test "JSON serialization for span data" {
  // Define span structure
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // Serialize span to JSON
  let serialize_span_to_json = fn(span: Span) {
    let parent_span_json = match span.parent_span_id {
      Some(id) => "\"" + id + "\"",
      None => "null"
    }
    
    let tags_json = span.tags.map(fn(tag) {
      "\"" + tag.0 + "\":\"" + tag.1 + "\""
    }).join(",")
    
    "{"
      + "\"trace_id\":\"" + span.trace_id + "\"," 
      + "\"span_id\":\"" + span.span_id + "\"," 
      + "\"parent_span_id\":" + parent_span_json + "," 
      + "\"operation_name\":\"" + span.operation_name + "\"," 
      + "\"start_time\":" + span.start_time.to_string() + "," 
      + "\"end_time\":" + span.end_time.to_string() + "," 
      + "\"status\":\"" + span.status + "\"," 
      + "\"tags\":{" + tags_json + "}"
      + "}"
  }
  
  // Deserialize span from JSON
  let deserialize_span_from_json = fn(json: String) {
    // Simplified JSON parsing for demonstration
    // In a real implementation, this would use a proper JSON parser
    
    // Extract trace_id
    let trace_id_start = json.index_of("\"trace_id\":\"") + 12
    let trace_id_end = json.index_of("\"", trace_id_start)
    let trace_id = json.substring(trace_id_start, trace_id_end - trace_id_start)
    
    // Extract span_id
    let span_id_start = json.index_of("\"span_id\":\"") + 11
    let span_id_end = json.index_of("\"", span_id_start)
    let span_id = json.substring(span_id_start, span_id_end - span_id_start)
    
    // Extract operation_name
    let operation_start = json.index_of("\"operation_name\":\"") + 18
    let operation_end = json.index_of("\"", operation_start)
    let operation_name = json.substring(operation_start, operation_end - operation_start)
    
    // Extract start_time
    let start_time_start = json.index_of("\"start_time\":") + 13
    let start_time_end = json.index_of(",", start_time_start)
    let start_time = json.substring(start_time_start, start_time_end - start_time_start).to_int()
    
    // Extract end_time
    let end_time_start = json.index_of("\"end_time\":") + 11
    let end_time_end = json.index_of(",", end_time_start)
    let end_time = json.substring(end_time_start, end_time_end - end_time_start).to_int()
    
    // Extract status
    let status_start = json.index_of("\"status\":\"") + 10
    let status_end = json.index_of("\"", status_start)
    let status = json.substring(status_start, status_end - status_start)
    
    // Extract parent_span_id
    let parent_span_id = if json.contains("\"parent_span_id\":null") {
      None
    } else {
      let parent_start = json.index_of("\"parent_span_id\":\"") + 17
      let parent_end = json.index_of("\"", parent_start)
      Some(json.substring(parent_start, parent_end - parent_start))
    }
    
    // Extract tags (simplified)
    let tags_start = json.index_of("\"tags\":{") + 8
    let tags_end = json.index_of("}", tags_start)
    let tags_section = json.substring(tags_start, tags_end - tags_start + 1)
    
    let mut tags = []
    if tags_section.length() > 2 {  // More than just "{}"
      let tag_pairs = tags_section.split(",")
      for i in 1..tag_pairs.length() - 1 {  // Skip first and last
        let pair = tag_pairs[i]
        let colon_pos = pair.index_of("\":\"")
        if colon_pos > 0 {
          let key = pair.substring(1, colon_pos - 1)
          let value_start = colon_pos + 2
          let value_end = pair.length() - 1
          let value = pair.substring(value_start, value_end - value_start)
          tags = tags.push((key, value))
        }
      }
    }
    
    {
      trace_id,
      span_id,
      parent_span_id,
      operation_name,
      start_time,
      end_time,
      status,
      tags
    }
  }
  
  // Create test span
  let test_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    tags: [
      ("service.name", "payment-service"),
      ("service.version", "1.2.3"),
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM orders")
    ]
  }
  
  // Serialize span
  let json = serialize_span_to_json(test_span)
  
  // Test JSON structure
  assert_true(json.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json.contains("\"span_id\":\"span-67890\""))
  assert_true(json.contains("\"parent_span_id\":\"span-11111\""))
  assert_true(json.contains("\"operation_name\":\"database_query\""))
  assert_true(json.contains("\"start_time\":1640995200"))
  assert_true(json.contains("\"end_time\":1640995250"))
  assert_true(json.contains("\"status\":\"ok\""))
  assert_true(json.contains("\"service.name\":\"payment-service\""))
  assert_true(json.contains("\"service.version\":\"1.2.3\""))
  
  // Deserialize span
  let deserialized_span = deserialize_span_from_json(json)
  
  // Test deserialized span
  assert_eq(deserialized_span.trace_id, test_span.trace_id)
  assert_eq(deserialized_span.span_id, test_span.span_id)
  assert_eq(deserialized_span.parent_span_id, test_span.parent_span_id)
  assert_eq(deserialized_span.operation_name, test_span.operation_name)
  assert_eq(deserialized_span.start_time, test_span.start_time)
  assert_eq(deserialized_span.end_time, test_span.end_time)
  assert_eq(deserialized_span.status, test_span.status)
  assert_eq(deserialized_span.tags.length(), test_span.tags.length())
  
  // Test span without parent
  let span_without_parent = { test_span | parent_span_id: None }
  let json_without_parent = serialize_span_to_json(span_without_parent)
  assert_true(json_without_parent.contains("\"parent_span_id\":null"))
  
  let deserialized_without_parent = deserialize_span_from_json(json_without_parent)
  assert_eq(deserialized_without_parent.parent_span_id, None)
}

// Test 2: Binary Serialization for Metrics
test "binary serialization for metric data" {
  // Define metric structure
  type Metric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // Serialize metric to binary (simulated)
  let serialize_metric_to_binary = fn(metric: Metric) {
    // In a real implementation, this would convert to actual binary format
    // For simulation, we'll create a structured string representation
    
    let name_length = metric.name.length()
    let unit_length = metric.unit.length()
    let tags_count = metric.tags.length()
    
    // Convert value to string representation
    let value_str = metric.value.to_string()
    let value_parts = value_str.split(".")
    let integer_part = value_parts[0]
    let decimal_part = if value_parts.length() > 1 { value_parts[1] } else { "0" }
    
    // Build binary representation (simulated as structured string)
    let binary = "MTRC"
      + integer_part.to_string().pad_left(10, '0')  // Integer part (10 chars)
      + decimal_part.pad_left(10, '0')               // Decimal part (10 chars)
      + metric.timestamp.to_string().pad_left(12, '0')  // Timestamp (12 chars)
      + name_length.to_string().pad_left(3, '0')     // Name length (3 chars)
      + unit_length.to_string().pad_left(3, '0')     // Unit length (3 chars)
      + tags_count.to_string().pad_left(3, '0')      // Tags count (3 chars)
      + metric.name                                   // Name
      + metric.unit                                   // Unit
      + metric.tags.map(fn(tag) {
          let key_length = tag.0.length()
          let value_length = tag.1.length()
          key_length.to_string().pad_left(3, '0')
          + value_length.to_string().pad_left(3, '0')
          + tag.0 + tag.1
        }).join("")
    
    binary
  }
  
  // Deserialize metric from binary (simulated)
  let deserialize_metric_from_binary = fn(binary: String) {
    // Extract header
    assert_eq(binary.substring(0, 4), "MTRC")
    
    // Extract integer part
    let integer_part = binary.substring(4, 14).to_int()
    
    // Extract decimal part
    let decimal_part_str = binary.substring(14, 24)
    let decimal_part = decimal_part_str.to_int()
    
    // Reconstruct float value
    let value = integer_part.to_float() + (decimal_part.to_float() / 10000000000.0)
    
    // Extract timestamp
    let timestamp = binary.substring(24, 36).to_int()
    
    // Extract name length
    let name_length = binary.substring(36, 39).to_int()
    
    // Extract unit length
    let unit_length = binary.substring(39, 42).to_int()
    
    // Extract tags count
    let tags_count = binary.substring(42, 45).to_int()
    
    // Extract name
    let name_start = 45
    let name_end = name_start + name_length
    let name = binary.substring(name_start, name_length)
    
    // Extract unit
    let unit_start = name_end
    let unit_end = unit_start + unit_length
    let unit = binary.substring(unit_start, unit_length)
    
    // Extract tags
    let mut tags = []
    let mut current_pos = unit_end
    
    for i in 0..tags_count {
      // Extract key length
      let key_length = binary.substring(current_pos, current_pos + 3).to_int()
      current_pos = current_pos + 3
      
      // Extract value length
      let value_length = binary.substring(current_pos, current_pos + 3).to_int()
      current_pos = current_pos + 3
      
      // Extract key
      let key = binary.substring(current_pos, key_length)
      current_pos = current_pos + key_length
      
      // Extract value
      let value = binary.substring(current_pos, value_length)
      current_pos = current_pos + value_length
      
      tags = tags.push((key, value))
    }
    
    {
      name,
      value,
      unit,
      timestamp,
      tags
    }
  }
  
  // Create test metric
  let test_metric = {
    name: "http_request_duration",
    value: 123.456,
    unit: "milliseconds",
    timestamp: 1640995200,
    tags: [
      ("method", "GET"),
      ("status", "200"),
      ("endpoint", "/api/payments")
    ]
  }
  
  // Serialize metric
  let binary = serialize_metric_to_binary(test_metric)
  
  // Test binary structure
  assert_eq(binary.length(), 4 + 10 + 10 + 12 + 3 + 3 + 3 + test_metric.name.length() + test_metric.unit.length() + 54)  // 54 for tags
  assert_eq(binary.substring(0, 4), "MTRC")
  assert_eq(binary.substring(4, 14), "0000000123")  // Integer part
  assert_eq(binary.substring(14, 24), "0456000000")  // Decimal part
  assert_eq(binary.substring(24, 36), "001640995200")  // Timestamp
  assert_eq(binary.substring(36, 39), "023")  // Name length
  assert_eq(binary.substring(39, 42), "011")  // Unit length
  assert_eq(binary.substring(42, 45), "003")  // Tags count
  
  // Deserialize metric
  let deserialized_metric = deserialize_metric_from_binary(binary)
  
  // Test deserialized metric
  assert_eq(deserialized_metric.name, test_metric.name)
  assert_eq(deserialized_metric.value, test_metric.value)
  assert_eq(deserialized_metric.unit, test_metric.unit)
  assert_eq(deserialized_metric.timestamp, test_metric.timestamp)
  assert_eq(deserialized_metric.tags.length(), test_metric.tags.length())
  
  // Test specific tags
  assert_eq(deserialized_metric.tags[0], ("method", "GET"))
  assert_eq(deserialized_metric.tags[1], ("status", "200"))
  assert_eq(deserialized_metric.tags[2], ("endpoint", "/api/payments"))
}

// Test 3: Protocol Buffers Serialization
test "protocol buffers serialization for telemetry data" {
  // Define telemetry data structure
  type TelemetryData = {
    trace_id: String,
    spans: Array[SpanInfo],
    metrics: Array[MetricInfo],
    resource: ResourceInfo
  }
  
  type SpanInfo = {
    span_id: String,
    parent_span_id: Option[String],
    name: String,
    kind: String,
    start_time_unix_nano: Int,
    end_time_unix_nano: Int,
    status: StatusInfo
  }
  
  type MetricInfo = {
    name: String,
    unit: String,
    description: String,
    data: MetricData
  }
  
  type StatusInfo = {
    code: Int,
    message: String
  }
  
  type MetricData = {
    type: String,  // "gauge", "counter", "histogram"
    value: Float
  }
  
  type ResourceInfo = {
    service_name: String,
    service_version: String,
    host_name: String
  }
  
  // Serialize to Protocol Buffers (simulated)
  let serialize_to_protobuf = fn(data: TelemetryData) {
    // In a real implementation, this would use actual Protocol Buffers library
    // For simulation, we'll create a structured binary representation
    
    // Header with field numbers and types (simplified)
    let header = "TELE"
    
    // Trace ID field (field 1, string)
    let trace_id_field = "1" + data.trace_id.length().to_string().pad_left(4, '0') + data.trace_id
    
    // Spans field (field 2, repeated)
    let spans_field = "2" + data.spans.length().to_string().pad_left(4, '0')
    let spans_data = data.spans.map(fn(span) {
      let parent_id = match span.parent_span_id {
        Some(id) => id,
        None => ""
      }
      
      "a" + span.span_id.length().to_string().pad_left(4, '0') + span.span_id
      + "b" + parent_id.length().to_string().pad_left(4, '0') + parent_id
      + "c" + span.name.length().to_string().pad_left(4, '0') + span.name
      + "d" + span.kind.length().to_string().pad_left(4, '0') + span.kind
      + "e" + span.start_time_unix_nano.to_string().pad_left(12, '0')
      + "f" + span.end_time_unix_nano.to_string().pad_left(12, '0')
      + "g" + span.status.code.to_string().pad_left(3, '0')
      + "h" + span.status.message.length().to_string().pad_left(4, '0') + span.status.message
    }).join("")
    
    // Metrics field (field 3, repeated)
    let metrics_field = "3" + data.metrics.length().to_string().pad_left(4, '0')
    let metrics_data = data.metrics.map(fn(metric) {
      "a" + metric.name.length().to_string().pad_left(4, '0') + metric.name
      + "b" + metric.unit.length().to_string().pad_left(4, '0') + metric.unit
      + "c" + metric.description.length().to_string().pad_left(4, '0') + metric.description
      + "d" + metric.data.type.length().to_string().pad_left(4, '0') + metric.data.type
      + "e" + metric.data.value.to_string().pad_left(10, '0')
    }).join("")
    
    // Resource field (field 4, message)
    let resource_field = "4"
      + "a" + data.resource.service_name.length().to_string().pad_left(4, '0') + data.resource.service_name
      + "b" + data.resource.service_version.length().to_string().pad_left(4, '0') + data.resource.service_version
      + "c" + data.resource.host_name.length().to_string().pad_left(4, '0') + data.resource.host_name
    
    header + trace_id_field + spans_field + spans_data + metrics_field + metrics_data + resource_field
  }
  
  // Deserialize from Protocol Buffers (simulated)
  let deserialize_from_protobuf = fn(protobuf: String) {
    // Extract header
    assert_eq(protobuf.substring(0, 4), "TELE")
    let mut pos = 4
    
    // Extract trace ID (field 1)
    assert_eq(protobuf.substring(pos, pos + 1), "1")
    let trace_id_length = protobuf.substring(pos + 1, pos + 5).to_int()
    pos = pos + 5
    let trace_id = protobuf.substring(pos, trace_id_length)
    pos = pos + trace_id_length
    
    // Extract spans (field 2)
    assert_eq(protobuf.substring(pos, pos + 1), "2")
    let spans_count = protobuf.substring(pos + 1, pos + 5).to_int()
    pos = pos + 5
    
    let mut spans = []
    for i in 0..spans_count {
      // Extract span_id
      assert_eq(protobuf.substring(pos, pos + 1), "a")
      let span_id_length = protobuf.substring(pos + 1, pos + 5).to_int()
      pos = pos + 5
      let span_id = protobuf.substring(pos, span_id_length)
      pos = pos + span_id_length
      
      // Extract parent_span_id
      assert_eq(protobuf.substring(pos, pos + 1), "b")
      let parent_id_length = protobuf.substring(pos + 1, pos + 5).to_int()
      pos = pos + 5
      let parent_span_id = if parent_id_length > 0 {
        let parent_id = protobuf.substring(pos, parent_id_length)
        pos = pos + parent_id_length
        Some(parent_id)
      } else {
        pos = pos + parent_id_length
        None
      }
      
      // Extract name
      assert_eq(protobuf.substring(pos, pos + 1), "c")
      let name_length = protobuf.substring(pos + 1, pos + 5).to_int()
      pos = pos + 5
      let name = protobuf.substring(pos, name_length)
      pos = pos + name_length
      
      // Extract kind
      assert_eq(protobuf.substring(pos, pos + 1), "d")
      let kind_length = protobuf.substring(pos + 1, pos + 5).to_int()
      pos = pos + 5
      let kind = protobuf.substring(pos, kind_length)
      pos = pos + kind_length
      
      // Extract start_time
      assert_eq(protobuf.substring(pos, pos + 1), "e")
      let start_time = protobuf.substring(pos + 1, pos + 13).to_int()
      pos = pos + 13
      
      // Extract end_time
      assert_eq(protobuf.substring(pos, pos + 1), "f")
      let end_time = protobuf.substring(pos + 1, pos + 13).to_int()
      pos = pos + 13
      
      // Extract status code
      assert_eq(protobuf.substring(pos, pos + 1), "g")
      let status_code = protobuf.substring(pos + 1, pos + 4).to_int()
      pos = pos + 4
      
      // Extract status message
      assert_eq(protobuf.substring(pos, pos + 1), "h")
      let message_length = protobuf.substring(pos + 1, pos + 5).to_int()
      pos = pos + 5
      let message = protobuf.substring(pos, message_length)
      pos = pos + message_length
      
      spans = spans.push({
        span_id,
        parent_span_id,
        name,
        kind,
        start_time_unix_nano: start_time,
        end_time_unix_nano: end_time,
        status: {
          code: status_code,
          message
        }
      })
    }
    
    // Extract metrics (field 3)
    assert_eq(protobuf.substring(pos, pos + 1), "3")
    let metrics_count = protobuf.substring(pos + 1, pos + 5).to_int()
    pos = pos + 5
    
    let mut metrics = []
    for i in 0..metrics_count {
      // Extract name
      assert_eq(protobuf.substring(pos, pos + 1), "a")
      let name_length = protobuf.substring(pos + 1, pos + 5).to_int()
      pos = pos + 5
      let name = protobuf.substring(pos, name_length)
      pos = pos + name_length
      
      // Extract unit
      assert_eq(protobuf.substring(pos, pos + 1), "b")
      let unit_length = protobuf.substring(pos + 1, pos + 5).to_int()
      pos = pos + 5
      let unit = protobuf.substring(pos, unit_length)
      pos = pos + unit_length
      
      // Extract description
      assert_eq(protobuf.substring(pos, pos + 1), "c")
      let description_length = protobuf.substring(pos + 1, pos + 5).to_int()
      pos = pos + 5
      let description = protobuf.substring(pos, description_length)
      pos = pos + description_length
      
      // Extract type
      assert_eq(protobuf.substring(pos, pos + 1), "d")
      let type_length = protobuf.substring(pos + 1, pos + 5).to_int()
      pos = pos + 5
      let type_str = protobuf.substring(pos, type_length)
      pos = pos + type_length
      
      // Extract value
      assert_eq(protobuf.substring(pos, pos + 1), "e")
      let value_str = protobuf.substring(pos + 1, pos + 11)
      let value = value_str.to_int().to_float()
      pos = pos + 11
      
      metrics = metrics.push({
        name,
        unit,
        description,
        data: {
          type: type_str,
          value
        }
      })
    }
    
    // Extract resource (field 4)
    assert_eq(protobuf.substring(pos, pos + 1), "4")
    pos = pos + 1
    
    // Extract service_name
    assert_eq(protobuf.substring(pos, pos + 1), "a")
    let service_name_length = protobuf.substring(pos + 1, pos + 5).to_int()
    pos = pos + 5
    let service_name = protobuf.substring(pos, service_name_length)
    pos = pos + service_name_length
    
    // Extract service_version
    assert_eq(protobuf.substring(pos, pos + 1), "b")
    let service_version_length = protobuf.substring(pos + 1, pos + 5).to_int()
    pos = pos + 5
    let service_version = protobuf.substring(pos, service_version_length)
    pos = pos + service_version_length
    
    // Extract host_name
    assert_eq(protobuf.substring(pos, pos + 1), "c")
    let host_name_length = protobuf.substring(pos + 1, pos + 5).to_int()
    pos = pos + 5
    let host_name = protobuf.substring(pos, host_name_length)
    pos = pos + host_name_length
    
    {
      trace_id,
      spans,
      metrics,
      resource: {
        service_name,
        service_version,
        host_name
      }
    }
  }
  
  // Create test telemetry data
  let test_data = {
    trace_id: "trace-abcdef123456",
    spans: [
      {
        span_id: "span-111111",
        parent_span_id: None,
        name: "HTTP GET /api/users",
        kind: "SERVER",
        start_time_unix_nano: 1640995200000000000,
        end_time_unix_nano: 1640995200250000000,
        status: {
          code: 0,
          message: "OK"
        }
      },
      {
        span_id: "span-222222",
        parent_span_id: Some("span-111111"),
        name: "database query",
        kind: "CLIENT",
        start_time_unix_nano: 1640995200050000000,
        end_time_unix_nano: 1640995200200000000,
        status: {
          code: 0,
          message: "OK"
        }
      }
    ],
    metrics: [
      {
        name: "http.server.request_count",
        unit: "1",
        description: "Total number of HTTP requests",
        data: {
          type: "counter",
          value: 12345.0
        }
      },
      {
        name: "http.server.duration",
        unit: "ms",
        description: "Duration of HTTP requests",
        data: {
          type: "histogram",
          value: 25.5
        }
      }
    ],
    resource: {
      service_name: "user-service",
      service_version: "1.2.3",
      host_name: "server-01"
    }
  }
  
  // Serialize to Protocol Buffers
  let protobuf = serialize_to_protobuf(test_data)
  
  // Test protobuf structure
  assert_eq(protobuf.substring(0, 4), "TELE")
  assert_eq(protobuf.substring(4, 5), "1")  // Field 1 (trace_id)
  assert_eq(protobuf.substring(9, 10), "2")  // Field 2 (spans)
  assert_eq(protobuf.substring(18, 19), "3")  // Field 3 (metrics)
  assert_true(protobuf.contains("4a"))  // Field 4 (resource)
  
  // Deserialize from Protocol Buffers
  let deserialized_data = deserialize_from_protobuf(protobuf)
  
  // Test deserialized data
  assert_eq(deserialized_data.trace_id, test_data.trace_id)
  assert_eq(deserialized_data.spans.length(), test_data.spans.length())
  assert_eq(deserialized_data.metrics.length(), test_data.metrics.length())
  assert_eq(deserialized_data.resource.service_name, test_data.resource.service_name)
  assert_eq(deserialized_data.resource.service_version, test_data.resource.service_version)
  assert_eq(deserialized_data.resource.host_name, test_data.resource.host_name)
  
  // Test specific span
  let first_span = deserialized_data.spans[0]
  assert_eq(first_span.span_id, "span-111111")
  assert_eq(first_span.parent_span_id, None)
  assert_eq(first_span.name, "HTTP GET /api/users")
  assert_eq(first_span.kind, "SERVER")
  assert_eq(first_span.status.code, 0)
  assert_eq(first_span.status.message, "OK")
  
  // Test specific metric
  let first_metric = deserialized_data.metrics[0]
  assert_eq(first_metric.name, "http.server.request_count")
  assert_eq(first_metric.unit, "1")
  assert_eq(first_metric.description, "Total number of HTTP requests")
  assert_eq(first_metric.data.type, "counter")
  assert_eq(first_metric.data.value, 12345.0)
}

// Test 4: Avro Serialization
test "Avro serialization for log data" {
  // Define log entry structure
  type LogEntry = {
    timestamp: Int,
    level: String,
    message: String,
    logger_name: String,
    thread_name: String,
    exception: Option[ExceptionInfo]
  }
  
  type ExceptionInfo = {
    type: String,
    message: String,
    stack_trace: Array[String]
  }
  
  // Define Avro schema (simulated)
  let avro_schema = {
    "type": "record",
    "name": "LogEntry",
    "fields": [
      { "name": "timestamp", "type": "long" },
      { "name": "level", "type": "string" },
      { "name": "message", "type": "string" },
      { "name": "logger_name", "type": "string" },
      { "name": "thread_name", "type": "string" },
      {
        "name": "exception",
        "type": [
          "null",
          {
            "type": "record",
            "name": "ExceptionInfo",
            "fields": [
              { "name": "type", "type": "string" },
              { "name": "message", "type": "string" },
              { "name": "stack_trace", "type": { "type": "array", "items": "string" } }
            ]
          }
        ]
      }
    ]
  }
  
  // Serialize to Avro (simulated)
  let serialize_to_avro = fn(entry: LogEntry) {
    // In a real implementation, this would use actual Avro library
    // For simulation, we'll create a structured binary representation
    
    // Header with schema fingerprint
    let header = "AVRO"
    
    // Timestamp (long, 8 bytes)
    let timestamp_bytes = entry.timestamp.to_string().pad_left(20, '0')
    
    // Level (string with length prefix)
    let level_bytes = entry.level.length().to_string().pad_left(4, '0') + entry.level
    
    // Message (string with length prefix)
    let message_bytes = entry.message.length().to_string().pad_left(4, '0') + entry.message
    
    // Logger name (string with length prefix)
    let logger_name_bytes = entry.logger_name.length().to_string().pad_left(4, '0') + entry.logger_name
    
    // Thread name (string with length prefix)
    let thread_name_bytes = entry.thread_name.length().to_string().pad_left(4, '0') + entry.thread_name
    
    // Exception (union with null)
    let exception_bytes = match entry.exception {
      Some(exception) => {
        "1"  // Not null
        + exception.type.length().to_string().pad_left(4, '0') + exception.type
        + exception.message.length().to_string().pad_left(4, '0') + exception.message
        + exception.stack_trace.length().to_string().pad_left(4, '0')
        + exception.stack_trace.map(fn(trace) {
            trace.length().to_string().pad_left(4, '0') + trace
          }).join("")
      }
      None => "0"  // Null
    }
    
    header + timestamp_bytes + level_bytes + message_bytes + logger_name_bytes + thread_name_bytes + exception_bytes
  }
  
  // Deserialize from Avro (simulated)
  let deserialize_from_avro = fn(avro: String) {
    // Extract header
    assert_eq(avro.substring(0, 4), "AVRO")
    let mut pos = 4
    
    // Extract timestamp
    let timestamp = avro.substring(pos, pos + 20).to_int()
    pos = pos + 20
    
    // Extract level
    let level_length = avro.substring(pos, pos + 4).to_int()
    pos = pos + 4
    let level = avro.substring(pos, level_length)
    pos = pos + level_length
    
    // Extract message
    let message_length = avro.substring(pos, pos + 4).to_int()
    pos = pos + 4
    let message = avro.substring(pos, message_length)
    pos = pos + message_length
    
    // Extract logger name
    let logger_name_length = avro.substring(pos, pos + 4).to_int()
    pos = pos + 4
    let logger_name = avro.substring(pos, logger_name_length)
    pos = pos + logger_name_length
    
    // Extract thread name
    let thread_name_length = avro.substring(pos, pos + 4).to_int()
    pos = pos + 4
    let thread_name = avro.substring(pos, thread_name_length)
    pos = pos + thread_name_length
    
    // Extract exception
    let exception_flag = avro.substring(pos, pos + 1)
    pos = pos + 1
    
    let exception = if exception_flag == "1" {
      // Extract exception type
      let exception_type_length = avro.substring(pos, pos + 4).to_int()
      pos = pos + 4
      let exception_type = avro.substring(pos, exception_type_length)
      pos = pos + exception_type_length
      
      // Extract exception message
      let exception_message_length = avro.substring(pos, pos + 4).to_int()
      pos = pos + 4
      let exception_message = avro.substring(pos, exception_message_length)
      pos = pos + exception_message_length
      
      // Extract stack trace
      let stack_trace_count = avro.substring(pos, pos + 4).to_int()
      pos = pos + 4
      
      let mut stack_trace = []
      for i in 0..stack_trace_count {
        let trace_length = avro.substring(pos, pos + 4).to_int()
        pos = pos + 4
        let trace = avro.substring(pos, trace_length)
        pos = pos + trace_length
        stack_trace = stack_trace.push(trace)
      }
      
      Some({
        type: exception_type,
        message: exception_message,
        stack_trace
      })
    } else {
      None
    }
    
    {
      timestamp,
      level,
      message,
      logger_name,
      thread_name,
      exception
    }
  }
  
  // Create test log entry without exception
  let test_log_without_exception = {
    timestamp: 1640995200123,
    level: "INFO",
    message: "User login successful",
    logger_name: "com.example.auth.AuthenticationService",
    thread_name: "main",
    exception: None
  }
  
  // Serialize log entry without exception
  let avro_without_exception = serialize_to_avro(test_log_without_exception)
  
  // Test Avro structure
  assert_eq(avro_without_exception.substring(0, 4), "AVRO")
  assert_eq(avro_without_exception.substring(24, 28), "0004")  // Level length (4)
  assert_eq(avro_without_exception.substring(32, 36), "0023")  // Message length (35)
  assert_eq(avro_without_exception.substring(61, 65), "0039")  // Logger name length (57)
  assert_eq(avro_without_exception.substring(124, 128), "0004")  // Thread name length (4)
  assert_eq(avro_without_exception.substring(132, 133), "0")    // Exception null flag
  
  // Deserialize log entry without exception
  let deserialized_without_exception = deserialize_from_avro(avro_without_exception)
  
  // Test deserialized log entry
  assert_eq(deserialized_without_exception.timestamp, test_log_without_exception.timestamp)
  assert_eq(deserialized_without_exception.level, test_log_without_exception.level)
  assert_eq(deserialized_without_exception.message, test_log_without_exception.message)
  assert_eq(deserialized_without_exception.logger_name, test_log_without_exception.logger_name)
  assert_eq(deserialized_without_exception.thread_name, test_log_without_exception.thread_name)
  assert_eq(deserialized_without_exception.exception, None)
  
  // Create test log entry with exception
  let test_log_with_exception = {
    timestamp: 1640995200456,
    level: "ERROR",
    message: "Database connection failed",
    logger_name: "com.example.db.DatabaseService",
    thread_name: "worker-1",
    exception: Some({
      type: "java.sql.SQLException",
      message: "Connection timeout",
      stack_trace: [
        "com.example.db.DatabaseService.getConnection(DatabaseService.java:42)",
        "com.example.auth.AuthenticationService.authenticate(AuthenticationService.java:123)",
        "com.example.api.AuthController.login(AuthController.java:67)"
      ]
    })
  }
  
  // Serialize log entry with exception
  let avro_with_exception = serialize_to_avro(test_log_with_exception)
  
  // Test Avro structure with exception
  assert_eq(avro_with_exception.substring(0, 4), "AVRO")
  assert_eq(avro_with_exception.substring(24, 28), "0005")  // Level length (5)
  assert_eq(avro_with_exception.substring(33, 37), "0027")  // Message length (39)
  assert_eq(avro_with_exception.substring(76, 80), "0034")  // Logger name length (52)
  assert_eq(avro_with_exception.substring(132, 136), "0007")  // Thread name length (7)
  assert_eq(avro_with_exception.substring(143, 144), "1")    // Exception not null flag
  
  // Deserialize log entry with exception
  let deserialized_with_exception = deserialize_from_avro(avro_with_exception)
  
  // Test deserialized log entry with exception
  assert_eq(deserialized_with_exception.timestamp, test_log_with_exception.timestamp)
  assert_eq(deserialized_with_exception.level, test_log_with_exception.level)
  assert_eq(deserialized_with_exception.message, test_log_with_exception.message)
  assert_eq(deserialized_with_exception.logger_name, test_log_with_exception.logger_name)
  assert_eq(deserialized_with_exception.thread_name, test_log_with_exception.thread_name)
  
  match deserialized_with_exception.exception {
    Some(exception) => {
      assert_eq(exception.type, "java.sql.SQLException")
      assert_eq(exception.message, "Connection timeout")
      assert_eq(exception.stack_trace.length(), 3)
      assert_eq(exception.stack_trace[0], "com.example.db.DatabaseService.getConnection(DatabaseService.java:42)")
      assert_eq(exception.stack_trace[1], "com.example.auth.AuthenticationService.authenticate(AuthenticationService.java:123)")
      assert_eq(exception.stack_trace[2], "com.example.api.AuthController.login(AuthController.java:67)")
    }
    None => assert_true(false)
  }
}

// Test 5: MessagePack Serialization
test "MessagePack serialization for configuration data" {
  // Define configuration structure
  type Configuration = {
    service_name: String,
    version: String,
    environment: String,
    settings: Array[Setting],
    metadata: Metadata
  }
  
  type Setting = {
    key: String,
    value: ConfigValue
  }
  
  type ConfigValue = {
    type: String,  // "string", "number", "boolean", "array", "object"
    data: String   // Serialized value
  }
  
  type Metadata = {
    created_at: Int,
    updated_at: Int,
    created_by: String,
    tags: Array[String]
  }
  
  // Serialize to MessagePack (simulated)
  let serialize_to_msgpack = fn(config: Configuration) {
    // In a real implementation, this would use actual MessagePack library
    // For simulation, we'll create a structured binary representation
    
    // Array marker with 4 elements
    let header = "94"  // MessagePack array with 4 elements
    
    // Service name (string)
    let service_name_bytes = "a" + config.service_name.length().to_string(16) + config.service_name
    
    // Version (string)
    let version_bytes = "a" + config.version.length().to_string(16) + config.version
    
    // Environment (string)
    let environment_bytes = "a" + config.environment.length().to_string(16) + config.environment
    
    // Settings (array)
    let settings_header = "9" + config.settings.length().to_string(16)  // Array with N elements
    let settings_bytes = config.settings.map(fn(setting) {
      // Setting (array with 2 elements)
      "92"  // Array with 2 elements
      // Key (string)
      + "a" + setting.key.length().to_string(16) + setting.key
      // Value (object)
      + "83"  // Map with 3 elements
      + "a4typea" + setting.value.type.length().to_string(16) + setting.value.type
      + "a4dataa" + setting.value.data.length().to_string(16) + setting.value.data
    }).join("")
    
    // Metadata (object)
    let metadata_bytes = "84"  // Map with 4 elements
      + "a9created_at" + config.metadata.created_at.to_string(16)
      + "a9updated_at" + config.metadata.updated_at.to_string(16)
      + "aacreated_bya" + config.metadata.created_by.length().to_string(16) + config.metadata.created_by
      + "a3tags" + "9" + config.metadata.tags.length().to_string(16)
      + config.metadata.tags.map(fn(tag) {
          "a" + tag.length().to_string(16) + tag
        }).join("")
    
    header + service_name_bytes + version_bytes + environment_bytes + settings_header + settings_bytes + metadata_bytes
  }
  
  // Deserialize from MessagePack (simulated)
  let deserialize_from_msgpack = fn(msgpack: String) {
    // Extract array header
    assert_eq(msgpack.substring(0, 2), "94")  // Array with 4 elements
    let mut pos = 2
    
    // Extract service name
    assert_eq(msgpack.substring(pos, pos + 1), "a")  // String marker
    let service_name_length = msgpack.substring(pos + 1, pos + 2).to_int(16)
    pos = pos + 2
    let service_name = msgpack.substring(pos, service_name_length)
    pos = pos + service_name_length
    
    // Extract version
    assert_eq(msgpack.substring(pos, pos + 1), "a")  // String marker
    let version_length = msgpack.substring(pos + 1, pos + 2).to_int(16)
    pos = pos + 2
    let version = msgpack.substring(pos, version_length)
    pos = pos + version_length
    
    // Extract environment
    assert_eq(msgpack.substring(pos, pos + 1), "a")  // String marker
    let environment_length = msgpack.substring(pos + 1, pos + 2).to_int(16)
    pos = pos + 2
    let environment = msgpack.substring(pos, environment_length)
    pos = pos + environment_length
    
    // Extract settings
    let settings_count = msgpack.substring(pos, pos + 1).to_int(16)
    pos = pos + 1
    
    let mut settings = []
    for i in 0..settings_count {
      // Extract setting (array with 2 elements)
      assert_eq(msgpack.substring(pos, pos + 2), "92")  // Array with 2 elements
      pos = pos + 2
      
      // Extract key
      assert_eq(msgpack.substring(pos, pos + 1), "a")  // String marker
      let key_length = msgpack.substring(pos + 1, pos + 2).to_int(16)
      pos = pos + 2
      let key = msgpack.substring(pos, key_length)
      pos = pos + key_length
      
      // Extract value (object with 3 elements)
      assert_eq(msgpack.substring(pos, pos + 2), "83")  // Map with 3 elements
      pos = pos + 2
      
      // Extract type
      assert_eq(msgpack.substring(pos, pos + 5), "a4typea")  // "type" key
      let type_length = msgpack.substring(pos + 5, pos + 6).to_int(16)
      pos = pos + 6
      let type_str = msgpack.substring(pos, type_length)
      pos = pos + type_length
      
      // Extract data
      assert_eq(msgpack.substring(pos, pos + 5), "a4dataa")  // "data" key
      let data_length = msgpack.substring(pos + 5, pos + 6).to_int(16)
      pos = pos + 6
      let data = msgpack.substring(pos, data_length)
      pos = pos + data_length
      
      settings = settings.push({
        key,
        value: {
          type: type_str,
          data
        }
      })
    }
    
    // Extract metadata (object with 4 elements)
    assert_eq(msgpack.substring(pos, pos + 2), "84")  // Map with 4 elements
    pos = pos + 2
    
    // Extract created_at
    assert_eq(msgpack.substring(pos, pos + 11), "a9created_at")  // "created_at" key
    pos = pos + 11
    let created_at = msgpack.substring(pos, pos + 8).to_int(16)
    pos = pos + 8
    
    // Extract updated_at
    assert_eq(msgpack.substring(pos, pos + 11), "a9updated_at")  // "updated_at" key
    pos = pos + 11
    let updated_at = msgpack.substring(pos, pos + 8).to_int(16)
    pos = pos + 8
    
    // Extract created_by
    assert_eq(msgpack.substring(pos, pos + 12), "aacreated_by")  // "created_by" key
    pos = pos + 12
    assert_eq(msgpack.substring(pos, pos + 1), "a")  // String marker
    let created_by_length = msgpack.substring(pos + 1, pos + 2).to_int(16)
    pos = pos + 2
    let created_by = msgpack.substring(pos, created_by_length)
    pos = pos + created_by_length
    
    // Extract tags
    assert_eq(msgpack.substring(pos, pos + 5), "a3tags")  // "tags" key
    pos = pos + 5
    let tags_count = msgpack.substring(pos, pos + 1).to_int(16)
    pos = pos + 1
    
    let mut tags = []
    for i in 0..tags_count {
      assert_eq(msgpack.substring(pos, pos + 1), "a")  // String marker
      let tag_length = msgpack.substring(pos + 1, pos + 2).to_int(16)
      pos = pos + 2
      let tag = msgpack.substring(pos, tag_length)
      pos = pos + tag_length
      tags = tags.push(tag)
    }
    
    {
      service_name,
      version,
      environment,
      settings,
      metadata: {
        created_at,
        updated_at,
        created_by,
        tags
      }
    }
  }
  
  // Create test configuration
  let test_config = {
    service_name: "payment-service",
    version: "1.2.3",
    environment: "production",
    settings: [
      {
        key: "database.url",
        value: {
          type: "string",
          data: "postgresql://localhost:5432/payments"
        }
      },
      {
        key: "database.max_connections",
        value: {
          type: "number",
          data: "20"
        }
      },
      {
        key: "cache.enabled",
        value: {
          type: "boolean",
          data: "true"
        }
      }
    ],
    metadata: {
      created_at: 1640995200,
      updated_at: 1640995300,
      created_by: "admin",
      tags: ["payment", "database", "cache"]
    }
  }
  
  // Serialize to MessagePack
  let msgpack = serialize_to_msgpack(test_config)
  
  // Test MessagePack structure
  assert_eq(msgpack.substring(0, 2), "94")  // Array with 4 elements
  assert_eq(msgpack.substring(2, 3), "a")    // String marker
  assert_eq(msgpack.substring(3, 4), "f")    // Length 15 in hex
  assert_eq(msgpack.substring(4, 19), "payment-service")
  
  // Deserialize from MessagePack
  let deserialized_config = deserialize_from_msgpack(msgpack)
  
  // Test deserialized configuration
  assert_eq(deserialized_config.service_name, test_config.service_name)
  assert_eq(deserialized_config.version, test_config.version)
  assert_eq(deserialized_config.environment, test_config.environment)
  assert_eq(deserialized_config.settings.length(), test_config.settings.length())
  assert_eq(deserialized_config.metadata.created_at, test_config.metadata.created_at)
  assert_eq(deserialized_config.metadata.updated_at, test_config.metadata.updated_at)
  assert_eq(deserialized_config.metadata.created_by, test_config.metadata.created_by)
  assert_eq(deserialized_config.metadata.tags.length(), test_config.metadata.tags.length())
  
  // Test specific setting
  let first_setting = deserialized_config.settings[0]
  assert_eq(first_setting.key, "database.url")
  assert_eq(first_setting.value.type, "string")
  assert_eq(first_setting.value.data, "postgresql://localhost:5432/payments")
  
  // Test tags
  assert_eq(deserialized_config.metadata.tags[0], "payment")
  assert_eq(deserialized_config.metadata.tags[1], "database")
  assert_eq(deserialized_config.metadata.tags[2], "cache")
}

// Test 6: CBOR Serialization
test "CBOR serialization for event data" {
  // Define event structure
  type Event = {
    id: String,
    source: String,
    type: String,
    timestamp: Int,
    data: EventData
  }
  
  type EventData = {
    attributes: Array[(String, String)],
    payload: Option[String]
  }
  
  // Serialize to CBOR (simulated)
  let serialize_to_cbor = fn(event: Event) {
    // In a real implementation, this would use actual CBOR library
    // For simulation, we'll create a structured binary representation
    
    // Map with 5 elements
    let header = "A5"  // CBOR map with 5 elements
    
    // ID (text string)
    let id_bytes = "65" + event.id.length().to_string(16) + event.id
    
    // Source (text string)
    let source_bytes = "66" + event.source.length().to_string(16) + event.source
    
    // Type (text string)
    let type_bytes = "64" + event.type.length().to_string(16) + event.type
    
    // Timestamp (unsigned integer)
    let timestamp_bytes = "69" + event.timestamp.to_string(16)
    
    // Data (map)
    let data_header = "64"  // Map with 4 elements
    
    // Attributes (array)
    let attributes_header = "69" + "61" + event.data.attributes.length().to_string(16)  // "attributes" key + array
    let attributes_bytes = event.data.attributes.map(fn(attr) {
      "82"  // Array with 2 elements
      + "61" + attr.0.length().to_string(16) + attr.0  // Key
      + "61" + attr.1.length().to_string(16) + attr.1  // Value
    }).join("")
    
    // Payload (optional)
    let payload_bytes = match event.data.payload {
      Some(payload) => "67" + "67" + payload.length().to_string(16) + payload,  // "payload" key + value
      None => "67" + "F6"  // "payload" key + null
    }
    
    header + id_bytes + source_bytes + type_bytes + timestamp_bytes + data_header + attributes_header + attributes_bytes + payload_bytes
  }
  
  // Deserialize from CBOR (simulated)
  let deserialize_from_cbor = fn(cbor: String) {
    // Extract map header
    assert_eq(cbor.substring(0, 2), "A5")  // Map with 5 elements
    let mut pos = 2
    
    // Extract ID
    assert_eq(cbor.substring(pos, pos + 2), "65")  // "id" key
    pos = pos + 2
    let id_length = cbor.substring(pos, pos + 1).to_int(16)
    pos = pos + 1
    let id = cbor.substring(pos, id_length)
    pos = pos + id_length
    
    // Extract source
    assert_eq(cbor.substring(pos, pos + 2), "66")  // "source" key
    pos = pos + 2
    let source_length = cbor.substring(pos, pos + 1).to_int(16)
    pos = pos + 1
    let source = cbor.substring(pos, source_length)
    pos = pos + source_length
    
    // Extract type
    assert_eq(cbor.substring(pos, pos + 2), "64")  // "type" key
    pos = pos + 2
    let type_length = cbor.substring(pos, pos + 1).to_int(16)
    pos = pos + 1
    let type_str = cbor.substring(pos, type_length)
    pos = pos + type_length
    
    // Extract timestamp
    assert_eq(cbor.substring(pos, pos + 2), "69")  // "timestamp" key
    pos = pos + 2
    let timestamp = cbor.substring(pos, pos + 8).to_int(16)
    pos = pos + 8
    
    // Extract data
    assert_eq(cbor.substring(pos, pos + 2), "64")  // "data" key
    pos = pos + 2
    
    // Extract attributes
    assert_eq(cbor.substring(pos, pos + 2), "69")  // "attributes" key
    pos = pos + 2
    assert_eq(cbor.substring(pos, pos + 2), "61")  // Array marker
    let attributes_count = cbor.substring(pos + 2, pos + 3).to_int(16)
    pos = pos + 3
    
    let mut attributes = []
    for i in 0..attributes_count {
      assert_eq(cbor.substring(pos, pos + 2), "82")  // Array with 2 elements
      pos = pos + 2
      
      // Extract key
      assert_eq(cbor.substring(pos, pos + 2), "61")  // String marker
      let key_length = cbor.substring(pos + 2, pos + 3).to_int(16)
      pos = pos + 3
      let key = cbor.substring(pos, key_length)
      pos = pos + key_length
      
      // Extract value
      assert_eq(cbor.substring(pos, pos + 2), "61")  // String marker
      let value_length = cbor.substring(pos + 2, pos + 3).to_int(16)
      pos = pos + 3
      let value = cbor.substring(pos, value_length)
      pos = pos + value_length
      
      attributes = attributes.push((key, value))
    }
    
    // Extract payload
    assert_eq(cbor.substring(pos, pos + 2), "67")  // "payload" key
    pos = pos + 2
    
    let payload = if cbor.substring(pos, pos + 2) == "F6" {
      pos = pos + 2
      None
    } else {
      let payload_length = cbor.substring(pos + 2, pos + 3).to_int(16)
      pos = pos + 3
      let payload = cbor.substring(pos, payload_length)
      pos = pos + payload_length
      Some(payload)
    }
    
    {
      id,
      source,
      type: type_str,
      timestamp,
      data: {
        attributes,
        payload
      }
    }
  }
  
  // Create test event with payload
  let test_event_with_payload = {
    id: "event-12345",
    source: "payment-service",
    type: "transaction.completed",
    timestamp: 1640995200,
    data: {
      attributes: [
        ("user.id", "user-67890"),
        ("transaction.id", "txn-abcdef"),
        ("amount", "99.99"),
        ("currency", "USD")
      ],
      payload: "{\"status\":\"success\",\"payment_method\":\"credit_card\"}"
    }
  }
  
  // Serialize to CBOR
  let cbor_with_payload = serialize_to_cbor(test_event_with_payload)
  
  // Test CBOR structure
  assert_eq(cbor_with_payload.substring(0, 2), "A5")  // Map with 5 elements
  assert_eq(cbor_with_payload.substring(2, 4), "65")  // "id" key
  assert_eq(cbor_with_payload.substring(6, 8), "66")  // "source" key
  assert_eq(cbor_with_payload.substring(28, 30), "64")  // "type" key
  assert_eq(cbor_with_payload.substring(49, 51), "69")  // "timestamp" key
  assert_eq(cbor_with_payload.substring(61, 63), "64")  // "data" key
  assert_eq(cbor_with_payload.substring(65, 67), "69")  // "attributes" key
  assert_eq(cbor_with_payload.substring(69, 71), "61")  // Array marker
  assert_eq(cbor_with_payload.substring(71, 73), "64")  // Array with 4 elements
  assert_eq(cbor_with_payload.substring(155, 157), "67")  // "payload" key
  
  // Deserialize from CBOR
  let deserialized_with_payload = deserialize_from_cbor(cbor_with_payload)
  
  // Test deserialized event
  assert_eq(deserialized_with_payload.id, test_event_with_payload.id)
  assert_eq(deserialized_with_payload.source, test_event_with_payload.source)
  assert_eq(deserialized_with_payload.type, test_event_with_payload.type)
  assert_eq(deserialized_with_payload.timestamp, test_event_with_payload.timestamp)
  assert_eq(deserialized_with_payload.data.attributes.length(), test_event_with_payload.data.attributes.length())
  
  match deserialized_with_payload.data.payload {
    Some(payload) => {
      assert_eq(payload, "{\"status\":\"success\",\"payment_method\":\"credit_card\"}")
    }
    None => assert_true(false)
  }
  
  // Create test event without payload
  let test_event_without_payload = {
    id: "event-54321",
    source: "user-service",
    type: "user.created",
    timestamp: 1640995250,
    data: {
      attributes: [
        ("user.id", "user-11111"),
        ("email", "user@example.com")
      ],
      payload: None
    }
  }
  
  // Serialize to CBOR
  let cbor_without_payload = serialize_to_cbor(test_event_without_payload)
  
  // Deserialize from CBOR
  let deserialized_without_payload = deserialize_from_cbor(cbor_without_payload)
  
  // Test deserialized event without payload
  assert_eq(deserialized_without_payload.id, test_event_without_payload.id)
  assert_eq(deserialized_without_payload.source, test_event_without_payload.source)
  assert_eq(deserialized_without_payload.type, test_event_without_payload.type)
  assert_eq(deserialized_without_payload.timestamp, test_event_without_payload.timestamp)
  assert_eq(deserialized_without_payload.data.attributes.length(), test_event_without_payload.data.attributes.length())
  assert_eq(deserialized_without_payload.data.payload, None)
}

// Test 7: XML Serialization
test "XML serialization for trace data" {
  // Define trace structure
  type Trace = {
    trace_id: String,
    spans: Array[XMLSpan],
    resources: Array[XMLResource]
  }
  
  type XMLSpan = {
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status_code: Int,
    attributes: Array[XMLAttribute]
  }
  
  type XMLResource = {
    service_name: String,
    attributes: Array[XMLAttribute]
  }
  
  type XMLAttribute = {
    key: String,
    value: String,
    type: String
  }
  
  // Escape XML special characters
  let escape_xml = fn(text: String) {
    let mut escaped = text
    escaped = escaped.replace("&", "&amp;")
    escaped = escaped.replace("<", "&lt;")
    escaped = escaped.replace(">", "&gt;")
    escaped = escaped.replace("\"", "&quot;")
    escaped = escaped.replace("'", "&apos;")
    escaped
  }
  
  // Serialize to XML
  let serialize_to_xml = fn(trace: Trace) {
    let xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    
    // Start trace element
    let trace_start = "<trace id=\"" + escape_xml(trace.trace_id) + "\">"
    
    // Serialize spans
    let spans_xml = trace.spans.map(fn(span) {
      let parent_attr = match span.parent_span_id {
        Some(id) => " parent_span_id=\"" + escape_xml(id) + "\"",
        None => ""
      }
      
      let span_start = "<span span_id=\"" + escape_xml(span.span_id) + "\"" + parent_attr + ">"
      let operation = "<operation_name>" + escape_xml(span.operation_name) + "</operation_name>"
      let start = "<start_time>" + span.start_time.to_string() + "</start_time>"
      let end = "<end_time>" + span.end_time.to_string() + "</end_time>"
      let status = "<status_code>" + span.status_code.to_string() + "</status_code>"
      
      let attributes_xml = if span.attributes.length() > 0 {
        let attrs_start = "<attributes>"
        let attrs = span.attributes.map(fn(attr) {
          "<attribute key=\"" + escape_xml(attr.key) + "\" type=\"" + escape_xml(attr.type) + "\">" + escape_xml(attr.value) + "</attribute>"
        }).join("")
        let attrs_end = "</attributes>"
        attrs_start + attrs + attrs_end
      } else {
        ""
      }
      
      let span_end = "</span>"
      
      span_start + operation + start + end + status + attributes_xml + span_end
    }).join("")
    
    // Serialize resources
    let resources_xml = if trace.resources.length() > 0 {
      let resources_start = "<resources>"
      let resources = trace.resources.map(fn(resource) {
        let resource_start = "<resource service_name=\"" + escape_xml(resource.service_name) + "\">"
        
        let attributes_xml = if resource.attributes.length() > 0 {
          let attrs_start = "<attributes>"
          let attrs = resource.attributes.map(fn(attr) {
            "<attribute key=\"" + escape_xml(attr.key) + "\" type=\"" + escape_xml(attr.type) + "\">" + escape_xml(attr.value) + "</attribute>"
          }).join("")
          let attrs_end = "</attributes>"
          attrs_start + attrs + attrs_end
        } else {
          ""
        }
        
        let resource_end = "</resource>"
        
        resource_start + attributes_xml + resource_end
      }).join("")
      let resources_end = "</resources>"
      resources_start + resources + resources_end
    } else {
      ""
    }
    
    let trace_end = "</trace>"
    
    xml_header + trace_start + spans_xml + resources_xml + trace_end
  }
  
  // Deserialize from XML (simulated)
  let deserialize_from_xml = fn(xml: String) {
    // Extract trace_id
    let trace_start = xml.index_of("<trace id=\"") + 12
    let trace_end = xml.index_of("\">", trace_start)
    let trace_id = xml.substring(trace_start, trace_end - trace_start)
    
    // Extract spans
    let mut spans = []
    let mut pos = xml.index_of("<trace id=\"") + 1
    
    while true {
      let span_start = xml.index_of("<span span_id=\"", pos)
      if span_start < 0 {
        break
      }
      
      // Extract span_id
      let span_id_start = span_start + 15
      let span_id_end = xml.index_of("\"", span_id_start)
      let span_id = xml.substring(span_id_start, span_id_end - span_id_start)
      
      // Extract parent_span_id (optional)
      let parent_start = xml.index_of(" parent_span_id=\"", span_id_end)
      let parent_span_id = if parent_start > 0 && parent_start < xml.index_of(">", span_id_end) {
        let parent_id_start = parent_start + 17
        let parent_id_end = xml.index_of("\"", parent_id_start)
        Some(xml.substring(parent_id_start, parent_id_end - parent_id_start))
      } else {
        None
      }
      
      // Extract operation_name
      let op_start = xml.index_of("<operation_name>", span_id_end) + 16
      let op_end = xml.index_of("</operation_name>", op_start)
      let operation_name = xml.substring(op_start, op_end - op_start)
      
      // Extract start_time
      let start_start = xml.index_of("<start_time>", op_end) + 12
      let start_end = xml.index_of("</start_time>", start_start)
      let start_time = xml.substring(start_start, start_end - start_start).to_int()
      
      // Extract end_time
      let end_start = xml.index_of("<end_time>", start_end) + 10
      let end_end = xml.index_of("</end_time>", end_start)
      let end_time = xml.substring(end_start, end_end - end_start).to_int()
      
      // Extract status_code
      let status_start = xml.index_of("<status_code>", end_end) + 13
      let status_end = xml.index_of("</status_code>", status_start)
      let status_code = xml.substring(status_start, status_end - status_start).to_int()
      
      // Extract attributes (optional)
      let mut attributes = []
      let attrs_start = xml.index_of("<attributes>", status_end)
      
      if attrs_start > 0 && attrs_start < xml.index_of("</span>", status_end) {
        let attrs_content_start = attrs_start + 12
        let attrs_content_end = xml.index_of("</attributes>", attrs_content_start)
        let attrs_content = xml.substring(attrs_content_start, attrs_content_end - attrs_content_start)
        
        let mut attr_pos = 0
        while true {
          let attr_start = attrs_content.index_of("<attribute key=\"", attr_pos)
          if attr_start < 0 {
            break
          }
          
          // Extract key
          let key_start = attr_start + 16
          let key_end = attrs_content.index_of("\"", key_start)
          let key = attrs_content.substring(key_start, key_end - key_start)
          
          // Extract type
          let type_start = attrs_content.index_of(" type=\"", key_end) + 7
          let type_end = attrs_content.index_of("\">", type_start)
          let type_str = attrs_content.substring(type_start, type_end - type_start)
          
          // Extract value
          let value_start = type_end + 2
          let value_end = attrs_content.index_of("</attribute>", value_start)
          let value = attrs_content.substring(value_start, value_end - value_start)
          
          attributes = attributes.push({
            key,
            value,
            type: type_str
          })
          
          attr_pos = value_end + 13
        }
      }
      
      spans = spans.push({
        span_id,
        parent_span_id,
        operation_name,
        start_time,
        end_time,
        status_code,
        attributes
      })
      
      pos = xml.index_of("</span>", status_end) + 7
    }
    
    // Extract resources
    let mut resources = []
    let resources_start = xml.index_of("<resources>")
    
    if resources_start > 0 {
      let resources_content_start = resources_start + 11
      let resources_content_end = xml.index_of("</resources>")
      let resources_content = xml.substring(resources_content_start, resources_content_end - resources_content_start)
      
      let mut res_pos = 0
      while true {
        let res_start = resources_content.index_of("<resource service_name=\"", res_pos)
        if res_start < 0 {
          break
        }
        
        // Extract service_name
        let service_start = res_start + 22
        let service_end = resources_content.index_of("\">", service_start)
        let service_name = resources_content.substring(service_start, service_end - service_start)
        
        // Extract attributes (optional)
        let mut res_attributes = []
        let attrs_start = resources_content.index_of("<attributes>", service_end)
        
        if attrs_start > 0 && attrs_start < resources_content.index_of("</resource>", service_end) {
          let attrs_content_start = attrs_start + 12
          let attrs_content_end = resources_content.index_of("</attributes>", attrs_content_start)
          let attrs_content = resources_content.substring(attrs_content_start, attrs_content_end - attrs_content_start)
          
          let mut attr_pos = 0
          while true {
            let attr_start = attrs_content.index_of("<attribute key=\"", attr_pos)
            if attr_start < 0 {
              break
            }
            
            // Extract key
            let key_start = attr_start + 16
            let key_end = attrs_content.index_of("\"", key_start)
            let key = attrs_content.substring(key_start, key_end - key_start)
            
            // Extract type
            let type_start = attrs_content.index_of(" type=\"", key_end) + 7
            let type_end = attrs_content.index_of("\">", type_start)
            let type_str = attrs_content.substring(type_start, type_end - type_start)
            
            // Extract value
            let value_start = type_end + 2
            let value_end = attrs_content.index_of("</attribute>", value_start)
            let value = attrs_content.substring(value_start, value_end - value_start)
            
            res_attributes = res_attributes.push({
              key,
              value,
              type: type_str
            })
            
            attr_pos = value_end + 13
          }
        }
        
        resources = resources.push({
          service_name,
          attributes: res_attributes
        })
        
        res_pos = resources_content.index_of("</resource>", service_end) + 11
      }
    }
    
    {
      trace_id,
      spans,
      resources
    }
  }
  
  // Create test trace
  let test_trace = {
    trace_id: "trace-xml-12345",
    spans: [
      {
        span_id: "span-xml-11111",
        parent_span_id: None,
        operation_name: "HTTP POST /api/orders",
        start_time: 1640995200000,
        end_time: 1640995200500,
        status_code: 1,
        attributes: [
          {
            key: "http.method",
            value: "POST",
            type: "string"
          },
          {
            key: "http.status_code",
            value: "201",
            type: "int"
          }
        ]
      },
      {
        span_id: "span-xml-22222",
        parent_span_id: Some("span-xml-11111"),
        operation_name: "database.insert",
        start_time: 1640995200100,
        end_time: 1640995200300,
        status_code: 1,
        attributes: [
          {
            key: "db.statement",
            value: "INSERT INTO orders (id, user_id, amount) VALUES (?, ?, ?)",
            type: "string"
          },
          {
            key: "db.rows_affected",
            value: "1",
            type: "int"
          }
        ]
      }
    ],
    resources: [
      {
        service_name: "order-service",
        attributes: [
          {
            key: "service.version",
            value: "1.0.0",
            type: "string"
          },
          {
            key: "service.instance.id",
            value: "instance-123",
            type: "string"
          }
        ]
      }
    ]
  }
  
  // Serialize to XML
  let xml = serialize_to_xml(test_trace)
  
  // Test XML structure
  assert_true(xml.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
  assert_true(xml.contains("<trace id=\"trace-xml-12345\">"))
  assert_true(xml.contains("<span span_id=\"span-xml-11111\">"))
  assert_true(xml.contains("<operation_name>HTTP POST /api/orders</operation_name>"))
  assert_true(xml.contains("<start_time>1640995200000</start_time>"))
  assert_true(xml.contains("<end_time>1640995200500</end_time>"))
  assert_true(xml.contains("<status_code>1</status_code>"))
  assert_true(xml.contains("<attribute key=\"http.method\" type=\"string\">POST</attribute>"))
  assert_true(xml.contains("<span span_id=\"span-xml-22222\" parent_span_id=\"span-xml-11111\">"))
  assert_true(xml.contains("<resource service_name=\"order-service\">"))
  assert_true(xml.contains("</trace>"))
  
  // Deserialize from XML
  let deserialized_trace = deserialize_from_xml(xml)
  
  // Test deserialized trace
  assert_eq(deserialized_trace.trace_id, test_trace.trace_id)
  assert_eq(deserialized_trace.spans.length(), test_trace.spans.length())
  assert_eq(deserialized_trace.resources.length(), test_trace.resources.length())
  
  // Test specific span
  let first_span = deserialized_trace.spans[0]
  assert_eq(first_span.span_id, "span-xml-11111")
  assert_eq(first_span.parent_span_id, None)
  assert_eq(first_span.operation_name, "HTTP POST /api/orders")
  assert_eq(first_span.start_time, 1640995200000)
  assert_eq(first_span.end_time, 1640995200500)
  assert_eq(first_span.status_code, 1)
  assert_eq(first_span.attributes.length(), 2)
  assert_eq(first_span.attributes[0].key, "http.method")
  assert_eq(first_span.attributes[0].value, "POST")
  assert_eq(first_span.attributes[0].type, "string")
  
  // Test second span with parent
  let second_span = deserialized_trace.spans[1]
  assert_eq(second_span.span_id, "span-xml-22222")
  assert_eq(second_span.parent_span_id, Some("span-xml-11111"))
  assert_eq(second_span.operation_name, "database.insert")
  
  // Test resource
  let first_resource = deserialized_trace.resources[0]
  assert_eq(first_resource.service_name, "order-service")
  assert_eq(first_resource.attributes.length(), 2)
  assert_eq(first_resource.attributes[0].key, "service.version")
  assert_eq(first_resource.attributes[0].value, "1.0.0")
  assert_eq(first_resource.attributes[0].type, "string")
}

// Test 8: YAML Serialization
test "YAML serialization for configuration data" {
  // Define configuration structure
  type YAMLConfiguration = {
    application: ApplicationConfig,
    database: DatabaseConfig,
    cache: CacheConfig,
    logging: LoggingConfig
  }
  
  type ApplicationConfig = {
    name: String,
    version: String,
    environment: String,
    port: Int
  }
  
  type DatabaseConfig = {
    host: String,
    port: Int,
    name: String,
    username: String,
    ssl_enabled: Bool
  }
  
  type CacheConfig = {
    type: String,
    ttl: Int,
    max_size: Int
  }
  
  type LoggingConfig = {
    level: String,
    format: String,
    outputs: Array[String]
  }
  
  // Serialize to YAML
  let serialize_to_yaml = fn(config: YAMLConfiguration) {
    let yaml = 
      "application:\n"
      + "  name: " + config.application.name + "\n"
      + "  version: " + config.application.version + "\n"
      + "  environment: " + config.application.environment + "\n"
      + "  port: " + config.application.port.to_string() + "\n"
      + "database:\n"
      + "  host: " + config.database.host + "\n"
      + "  port: " + config.database.port.to_string() + "\n"
      + "  name: " + config.database.name + "\n"
      + "  username: " + config.database.username + "\n"
      + "  ssl_enabled: " + (if config.database.ssl_enabled { "true" } else { "false" }) + "\n"
      + "cache:\n"
      + "  type: " + config.cache.type + "\n"
      + "  ttl: " + config.cache.ttl.to_string() + "\n"
      + "  max_size: " + config.cache.max_size.to_string() + "\n"
      + "logging:\n"
      + "  level: " + config.logging.level + "\n"
      + "  format: " + config.logging.format + "\n"
      + "  outputs:\n"
      + config.logging.outputs.map(fn(output) {
          "    - " + output + "\n"
        }).join("")
    
    yaml
  }
  
  // Deserialize from YAML (simulated)
  let deserialize_from_yaml = fn(yaml: String) {
    // Split YAML into lines
    let lines = yaml.split("\n")
    
    // Parse application section
    let mut app_name = ""
    let mut app_version = ""
    let mut app_environment = ""
    let mut app_port = 0
    
    // Parse database section
    let mut db_host = ""
    let mut db_port = 0
    let mut db_name = ""
    let mut db_username = ""
    let mut db_ssl_enabled = false
    
    // Parse cache section
    let mut cache_type = ""
    let mut cache_ttl = 0
    let mut cache_max_size = 0
    
    // Parse logging section
    let mut log_level = ""
    let mut log_format = ""
    let mut log_outputs = []
    
    let mut current_section = ""
    
    for line in lines {
      if line.starts_with("application:") {
        current_section = "application"
      } else if line.starts_with("database:") {
        current_section = "database"
      } else if line.starts_with("cache:") {
        current_section = "cache"
      } else if line.starts_with("logging:") {
        current_section = "logging"
      } else if line.starts_with("  ") && not(line.trim() == "") {
        let key_value = line.trim().split(":")
        if key_value.length() >= 2 {
          let key = key_value[0]
          let value = key_value[1].trim()
          
          match current_section {
            "application" => {
              match key {
                "name" => app_name = value
                "version" => app_version = value
                "environment" => app_environment = value
                "port" => app_port = value.to_int()
                _ => ()
              }
            }
            "database" => {
              match key {
                "host" => db_host = value
                "port" => db_port = value.to_int()
                "name" => db_name = value
                "username" => db_username = value
                "ssl_enabled" => db_ssl_enabled = value == "true"
                _ => ()
              }
            }
            "cache" => {
              match key {
                "type" => cache_type = value
                "ttl" => cache_ttl = value.to_int()
                "max_size" => cache_max_size = value.to_int()
                _ => ()
              }
            }
            "logging" => {
              match key {
                "level" => log_level = value
                "format" => log_format = value
                "outputs" => ()  // Handled separately
                _ => ()
              }
            }
            _ => ()
          }
        }
      } else if line.starts_with("    - ") && current_section == "logging" {
        let output = line.substring(6, line.length())
        log_outputs = log_outputs.push(output)
      }
    }
    
    {
      application: {
        name: app_name,
        version: app_version,
        environment: app_environment,
        port: app_port
      },
      database: {
        host: db_host,
        port: db_port,
        name: db_name,
        username: db_username,
        ssl_enabled: db_ssl_enabled
      },
      cache: {
        type: cache_type,
        ttl: cache_ttl,
        max_size: cache_max_size
      },
      logging: {
        level: log_level,
        format: log_format,
        outputs: log_outputs
      }
    }
  }
  
  // Create test configuration
  let test_config = {
    application: {
      name: "payment-service",
      version: "2.1.0",
      environment: "production",
      port: 8080
    },
    database: {
      host: "db.example.com",
      port: 5432,
      name: "payments",
      username: "payment_user",
      ssl_enabled: true
    },
    cache: {
      type: "redis",
      ttl: 300,
      max_size: 1000
    },
    logging: {
      level: "INFO",
      format: "json",
      outputs: ["stdout", "file"]
    }
  }
  
  // Serialize to YAML
  let yaml = serialize_to_yaml(test_config)
  
  // Test YAML structure
  assert_true(yaml.contains("application:"))
  assert_true(yaml.contains("  name: payment-service"))
  assert_true(yaml.contains("  version: 2.1.0"))
  assert_true(yaml.contains("  environment: production"))
  assert_true(yaml.contains("  port: 8080"))
  assert_true(yaml.contains("database:"))
  assert_true(yaml.contains("  host: db.example.com"))
  assert_true(yaml.contains("  port: 5432"))
  assert_true(yaml.contains("  name: payments"))
  assert_true(yaml.contains("  username: payment_user"))
  assert_true(yaml.contains("  ssl_enabled: true"))
  assert_true(yaml.contains("cache:"))
  assert_true(yaml.contains("  type: redis"))
  assert_true(yaml.contains("  ttl: 300"))
  assert_true(yaml.contains("  max_size: 1000"))
  assert_true(yaml.contains("logging:"))
  assert_true(yaml.contains("  level: INFO"))
  assert_true(yaml.contains("  format: json"))
  assert_true(yaml.contains("  outputs:"))
  assert_true(yaml.contains("    - stdout"))
  assert_true(yaml.contains("    - file"))
  
  // Deserialize from YAML
  let deserialized_config = deserialize_from_yaml(yaml)
  
  // Test deserialized configuration
  assert_eq(deserialized_config.application.name, test_config.application.name)
  assert_eq(deserialized_config.application.version, test_config.application.version)
  assert_eq(deserialized_config.application.environment, test_config.application.environment)
  assert_eq(deserialized_config.application.port, test_config.application.port)
  
  assert_eq(deserialized_config.database.host, test_config.database.host)
  assert_eq(deserialized_config.database.port, test_config.database.port)
  assert_eq(deserialized_config.database.name, test_config.database.name)
  assert_eq(deserialized_config.database.username, test_config.database.username)
  assert_eq(deserialized_config.database.ssl_enabled, test_config.database.ssl_enabled)
  
  assert_eq(deserialized_config.cache.type, test_config.cache.type)
  assert_eq(deserialized_config.cache.ttl, test_config.cache.ttl)
  assert_eq(deserialized_config.cache.max_size, test_config.cache.max_size)
  
  assert_eq(deserialized_config.logging.level, test_config.logging.level)
  assert_eq(deserialized_config.logging.format, test_config.logging.format)
  assert_eq(deserialized_config.logging.outputs.length(), test_config.logging.outputs.length())
  assert_eq(deserialized_config.logging.outputs[0], "stdout")
  assert_eq(deserialized_config.logging.outputs[1], "file")
}

// Test 9: CSV Serialization
test "CSV serialization for metrics data" {
  // Define metric record
  type MetricRecord = {
    timestamp: Int,
    metric_name: String,
    metric_value: Float,
    metric_unit: String,
    tags: Array[(String, String)]
  }
  
  // Serialize to CSV
  let serialize_to_csv = fn(records: Array[MetricRecord]) {
    // CSV header
    let header = "timestamp,metric_name,metric_value,metric_unit,tags\n"
    
    // CSV rows
    let rows = records.map(fn(record) {
      let tags_json = "{" + record.tags.map(fn(tag) {
        "\"" + tag.0 + "\":\"" + tag.1 + "\""
      }).join(",") + "}"
      
      record.timestamp.to_string() + ","
      + record.metric_name + ","
      + record.metric_value.to_string() + ","
      + record.metric_unit + ","
      + tags_json
    }).join("\n")
    
    header + rows
  }
  
  // Deserialize from CSV (simulated)
  let deserialize_from_csv = fn(csv: String) {
    // Split into lines
    let lines = csv.split("\n")
    
    // Skip header line
    let mut records = []
    
    for i in 1..lines.length() {
      let line = lines[i]
      if line.length() > 0 {
        let fields = line.split(",")
        
        if fields.length() >= 5 {
          let timestamp = fields[0].to_int()
          let metric_name = fields[1]
          let metric_value = fields[2].to_float()
          let metric_unit = fields[3]
          let tags_json = fields[4]
          
          // Parse tags JSON (simplified)
          let mut tags = []
          if tags_json.length() > 2 {  // More than just "{}"
            let inner = tags_json.substring(1, tags_json.length() - 1)  // Remove braces
            let pairs = inner.split(",")
            
            for pair in pairs {
              let kv = pair.split(":")
              if kv.length() >= 2 {
                let key = kv[0].substring(1, kv[0].length() - 1)  // Remove quotes
                let value = kv[1].substring(1, kv[1].length() - 1)  // Remove quotes
                tags = tags.push((key, value))
              }
            }
          }
          
          records = records.push({
            timestamp,
            metric_name,
            metric_value,
            metric_unit,
            tags
          })
        }
      }
    }
    
    records
  }
  
  // Create test records
  let test_records = [
    {
      timestamp: 1640995200,
      metric_name: "http_requests_total",
      metric_value: 12345.0,
      metric_unit: "count",
      tags: [
        ("method", "GET"),
        ("status", "200"),
        ("endpoint", "/api/users")
      ]
    },
    {
      timestamp: 1640995260,
      metric_name: "http_request_duration_seconds",
      metric_value: 0.125,
      metric_unit: "seconds",
      tags: [
        ("method", "POST"),
        ("status", "201"),
        ("endpoint", "/api/orders")
      ]
    },
    {
      timestamp: 1640995320,
      metric_name: "database_connections_active",
      metric_value: 15.0,
      metric_unit: "connections",
      tags: [
        ("database", "postgresql"),
        ("pool", "main")
      ]
    }
  ]
  
  // Serialize to CSV
  let csv = serialize_to_csv(test_records)
  
  // Test CSV structure
  assert_true(csv.contains("timestamp,metric_name,metric_value,metric_unit,tags"))
  assert_true(csv.contains("1640995200,http_requests_total,12345,count,{\"method\":\"GET\",\"status\":\"200\",\"endpoint\":\"/api/users\"}"))
  assert_true(csv.contains("1640995260,http_request_duration_seconds,0.125,seconds,{\"method\":\"POST\",\"status\":\"201\",\"endpoint\":\"/api/orders\"}"))
  assert_true(csv.contains("1640995320,database_connections_active,15,connections,{\"database\":\"postgresql\",\"pool\":\"main\"}"))
  
  // Deserialize from CSV
  let deserialized_records = deserialize_from_csv(csv)
  
  // Test deserialized records
  assert_eq(deserialized_records.length(), test_records.length())
  
  // Test first record
  let first_record = deserialized_records[0]
  assert_eq(first_record.timestamp, test_records[0].timestamp)
  assert_eq(first_record.metric_name, test_records[0].metric_name)
  assert_eq(first_record.metric_value, test_records[0].metric_value)
  assert_eq(first_record.metric_unit, test_records[0].metric_unit)
  assert_eq(first_record.tags.length(), test_records[0].tags.length())
  assert_eq(first_record.tags[0], ("method", "GET"))
  assert_eq(first_record.tags[1], ("status", "200"))
  assert_eq(first_record.tags[2], ("endpoint", "/api/users"))
  
  // Test second record
  let second_record = deserialized_records[1]
  assert_eq(second_record.timestamp, test_records[1].timestamp)
  assert_eq(second_record.metric_name, test_records[1].metric_name)
  assert_eq(second_record.metric_value, test_records[1].metric_value)
  assert_eq(second_record.metric_unit, test_records[1].metric_unit)
  assert_eq(second_record.tags.length(), test_records[1].tags.length())
  assert_eq(second_record.tags[0], ("method", "POST"))
  assert_eq(second_record.tags[1], ("status", "201"))
  assert_eq(second_record.tags[2], ("endpoint", "/api/orders"))
  
  // Test third record
  let third_record = deserialized_records[2]
  assert_eq(third_record.timestamp, test_records[2].timestamp)
  assert_eq(third_record.metric_name, test_records[2].metric_name)
  assert_eq(third_record.metric_value, test_records[2].metric_value)
  assert_eq(third_record.metric_unit, test_records[2].metric_unit)
  assert_eq(third_record.tags.length(), test_records[2].tags.length())
  assert_eq(third_record.tags[0], ("database", "postgresql"))
  assert_eq(third_record.tags[1], ("pool", "main"))
}

// Test 10: Custom Binary Format Serialization
test "custom binary format serialization for high-performance telemetry" {
  // Define custom binary format header
  type BinaryHeader = {
    magic: Array[Int],  // 4 bytes
    version: Int,      // 1 byte
    flags: Int,        // 1 byte
    reserved: Array[Int]  // 2 bytes
  }
  
  // Define telemetry packet
  type TelemetryPacket = {
    header: BinaryHeader,
    timestamp: Int,
    node_id: String,
    data_type: String,  // "span", "metric", "log"
    data_size: Int,
    data: String
  }
  
  // Serialize to custom binary format
  let serialize_to_binary = fn(packet: TelemetryPacket) {
    // Convert header to bytes (simulated as hex string)
    let header_bytes = packet.header.magic.map(fn(b) { b.to_string(16).pad_left(2, '0') }).join("")
      + packet.header.version.to_string(16).pad_left(2, '0')
      + packet.header.flags.to_string(16).pad_left(2, '0')
      + packet.header.reserved.map(fn(b) { b.to_string(16).pad_left(2, '0') }).join("")
    
    // Convert timestamp to 8-byte big-endian
    let timestamp_bytes = packet.timestamp.to_string(16).pad_left(16, '0')
    
    // Convert node_id (length-prefixed)
    let node_id_length = packet.node_id.length().to_string(16).pad_left(4, '0')
    let node_id_bytes = node_id_length + packet.node_id
    
    // Convert data_type (fixed 4 bytes, padded)
    let data_type_bytes = packet.data_type.pad_right(4, '0')
    
    // Convert data_size (4-byte big-endian)
    let data_size_bytes = packet.data_size.to_string(16).pad_left(8, '0')
    
    // Convert data (raw bytes)
    let data_bytes = packet.data
    
    header_bytes + timestamp_bytes + node_id_bytes + data_type_bytes + data_size_bytes + data_bytes
  }
  
  // Deserialize from custom binary format
  let deserialize_from_binary = fn(binary: String) {
    // Extract header (8 bytes)
    let magic1 = binary.substring(0, 2).to_int(16)
    let magic2 = binary.substring(2, 4).to_int(16)
    let magic3 = binary.substring(4, 6).to_int(16)
    let magic4 = binary.substring(6, 8).to_int(16)
    let version = binary.substring(8, 10).to_int(16)
    let flags = binary.substring(10, 12).to_int(16)
    let reserved1 = binary.substring(12, 14).to_int(16)
    let reserved2 = binary.substring(14, 16).to_int(16)
    
    let header = {
      magic: [magic1, magic2, magic3, magic4],
      version,
      flags,
      reserved: [reserved1, reserved2]
    }
    
    // Extract timestamp (8 bytes)
    let timestamp = binary.substring(16, 24).to_int(16)
    
    // Extract node_id
    let node_id_length = binary.substring(24, 28).to_int(16)
    let node_id = binary.substring(28, 28 + node_id_length)
    
    // Extract data_type (4 bytes)
    let data_type = binary.substring(28 + node_id_length, 32 + node_id_length)
    
    // Extract data_size (4 bytes)
    let data_size = binary.substring(32 + node_id_length, 40 + node_id_length).to_int(16)
    
    // Extract data
    let data = binary.substring(40 + node_id_length, 40 + node_id_length + data_size)
    
    {
      header,
      timestamp,
      node_id,
      data_type,
      data_size,
      data
    }
  }
  
  // Create test packet
  let test_packet = {
    header: {
      magic: [0xAB, 0xCD, 0xEF, 0x01],
      version: 1,
      flags: 0x80,
      reserved: [0x00, 0x00]
    },
    timestamp: 1640995200,
    node_id: "node-12345",
    data_type: "span",
    data_size: 50,
    data: "trace-abcdef,span-12345,operation,duration,100"
  }
  
  // Serialize to binary
  let binary = serialize_to_binary(test_packet)
  
  // Test binary structure
  assert_eq(binary.substring(0, 2), "ab")  // magic1
  assert_eq(binary.substring(2, 4), "cd")  // magic2
  assert_eq(binary.substring(4, 6), "ef")  // magic3
  assert_eq(binary.substring(6, 8), "01")  // magic4
  assert_eq(binary.substring(8, 10), "01") // version
  assert_eq(binary.substring(10, 12), "80") // flags
  assert_eq(binary.substring(12, 14), "00") // reserved1
  assert_eq(binary.substring(14, 16), "00") // reserved2
  assert_eq(binary.substring(16, 24), "00000061a8c9e000") // timestamp
  
  // Deserialize from binary
  let deserialized_packet = deserialize_from_binary(binary)
  
  // Test deserialized packet
  assert_eq(deserialized_packet.header.magic, test_packet.header.magic)
  assert_eq(deserialized_packet.header.version, test_packet.header.version)
  assert_eq(deserialized_packet.header.flags, test_packet.header.flags)
  assert_eq(deserialized_packet.header.reserved, test_packet.header.reserved)
  assert_eq(deserialized_packet.timestamp, test_packet.timestamp)
  assert_eq(deserialized_packet.node_id, test_packet.node_id)
  assert_eq(deserialized_packet.data_type, test_packet.data_type)
  assert_eq(deserialized_packet.data_size, test_packet.data_size)
  assert_eq(deserialized_packet.data, test_packet.data)
}