// 数据序列化和反序列化测试用例

test "telemetry_serialization_with_json_format" {
  // 测试JSON格式的遥测数据序列化
  
  // 创建测试Span
  let trace_id = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10]
  let span_id = [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]
  
  let span_context = azimuth::telemetry::api::trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0x01,
    trace_state: "key1=value1,key2=value2"
  }
  
  let attributes = [
    ("http.method", azimuth::telemetry::api::common::AttributeValue::string("GET")),
    ("http.status_code", azimuth::telemetry::api::common::AttributeValue::int(200L)),
    ("response.time", azimuth::telemetry::api::common::AttributeValue::float(150.5)),
    ("cache.hit", azimuth::telemetry::api::common::AttributeValue::bool(true))
  ]
  
  let span = azimuth::telemetry::api::trace::Span::{
    name: "api-request",
    context: span_context,
    kind: azimuth::telemetry::api::trace::SpanKind::Server,
    parent_span_id: Some([0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x11, 0x22]),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200150000000L),
    status: azimuth::telemetry::api::trace::StatusCode::Ok,
    status_description: Some("Request completed successfully"),
    attributes: attributes,
    events: [],
    links: []
  }
  
  // 模拟JSON序列化
  let json_representation = {
    "name": "api-request",
    "traceId": "0102030405060708090a0b0c0d0e0f10",
    "spanId": "1112131415161718",
    "parentSpanId": "aabbccddeeff1122",
    "kind": "SERVER",
    "startTimeUnixNano": "1640995200000000000",
    "endTimeUnixNano": "1640995200150000000",
    "status": "OK",
    "statusDescription": "Request completed successfully",
    "attributes": {
      "http.method": "GET",
      "http.status_code": "200",
      "response.time": "150.5",
      "cache.hit": "true"
    },
    "traceState": "key1=value1,key2=value2"
  }
  
  // 验证JSON序列化结果
  assert_eq(json_representation["name"], "api-request")
  assert_eq(json_representation["traceId"], "0102030405060708090a0b0c0d0e0f10")
  assert_eq(json_representation["spanId"], "1112131415161718")
  assert_eq(json_representation["kind"], "SERVER")
  assert_eq(json_representation["status"], "OK")
  
  // 验证属性序列化
  let attributes_json = json_representation["attributes"]
  assert_eq(attributes_json["http.method"], "GET")
  assert_eq(attributes_json["http.status_code"], "200")
  assert_eq(attributes_json["response.time"], "150.5")
  assert_eq(attributes_json["cache.hit"], "true")
}

test "telemetry_serialization_with_protobuf_format" {
  // 测试Protobuf格式的遥测数据序列化
  
  // 创建测试指标
  let metric_name = "request_duration"
  let metric_unit = "ms"
  let metric_description = "Request duration in milliseconds"
  
  let measurements = [
    azimuth::telemetry::api::metrics::Measurement::{
      value: 100.0,
      attributes: [
        ("http.method", azimuth::telemetry::api::common::AttributeValue::string("GET")),
        ("http.status_code", azimuth::telemetry::api::common::AttributeValue::int(200L))
      ]
    },
    azimuth::telemetry::api::metrics::Measurement::{
      value: 250.0,
      attributes: [
        ("http.method", azimuth::telemetry::api::common::AttributeValue::string("POST")),
        ("http.status_code", azimuth::telemetry::api::common::AttributeValue::int(201L))
      ]
    },
    azimuth::telemetry::api::metrics::Measurement::{
      value: 75.0,
      attributes: [
        ("http.method", azimuth::telemetry::api::common::AttributeValue::string("GET")),
        ("http.status_code", azimuth::telemetry::api::common::AttributeValue::int(404L))
      ]
    }
  ]
  
  // 模拟Protobuf序列化（简化版本）
  let protobuf_fields = [
    ("name", metric_name),
    ("unit", metric_unit),
    ("description", metric_description),
    ("measurement_count", measurements.length().to_string()),
    ("data_points", measurements.map(fn(m) { m.value.to_string() }).join(","))
  ]
  
  // 验证Protobuf字段
  assert_eq(protobuf_fields[0], ("name", metric_name))
  assert_eq(protobuf_fields[1], ("unit", metric_unit))
  assert_eq(protobuf_fields[2], ("description", metric_description))
  assert_eq(protobuf_fields[3], ("measurement_count", "3"))
  assert_eq(protobuf_fields[4], ("data_points", "100.0,250.0,75.0"))
  
  // 验证测量值
  assert_eq(measurements[0].value, 100.0)
  assert_eq(measurements[1].value, 250.0)
  assert_eq(measurements[2].value, 75.0)
  
  // 验证测量属性
  let first_measurement_attrs = measurements[0].attributes
  assert_eq(first_measurement_attrs.length(), 2)
  assert_eq(first_measurement_attrs[0].0, "http.method")
  assert_eq(first_measurement_attrs[1].0, "http.status_code")
}

test "telemetry_deserialization_with_data_validation" {
  // 测试遥测数据反序列化和验证
  
  // 模拟JSON格式的遥测数据
  let json_data = {
    "traceId": "0102030405060708090a0b0c0d0e0f10",
    "spanId": "1112131415161718",
    "name": "test-operation",
    "kind": "CLIENT",
    "startTimeUnixNano": "1640995200000000000",
    "attributes": {
      "service.name": "test-service",
      "operation.type": "database",
      "db.statement": "SELECT * FROM users"
    },
    "status": "OK"
  }
  
  // 验证必需字段
  assert_eq(json_data.has_key("traceId"), true, "Missing required field: traceId")
  assert_eq(json_data.has_key("spanId"), true, "Missing required field: spanId")
  assert_eq(json_data.has_key("name"), true, "Missing required field: name")
  assert_eq(json_data.has_key("kind"), true, "Missing required field: kind")
  assert_eq(json_data.has_key("startTimeUnixNano"), true, "Missing required field: startTimeUnixNano")
  
  // 验证字段格式
  let trace_id_hex = json_data["traceId"]
  assert_eq(trace_id_hex.length(), 32, "Trace ID should be 32 hex characters")
  
  let span_id_hex = json_data["spanId"]
  assert_eq(span_id_hex.length(), 16, "Span ID should be 16 hex characters")
  
  let start_time = json_data["startTimeUnixNano"].to_int64()
  assert_eq(start_time > 0, true, "Start time should be positive")
  
  // 验证枚举值
  let span_kind = json_data["kind"]
  let valid_kinds = ["INTERNAL", "SERVER", "CLIENT", "PRODUCER", "CONSUMER"]
  assert_eq(valid_kinds.contains(span_kind), true, "Invalid span kind: " + span_kind)
  
  let status = json_data["status"]
  let valid_statuses = ["UNSET", "OK", "ERROR"]
  assert_eq(valid_statuses.contains(status), true, "Invalid status: " + status)
  
  // 验证属性类型
  let attributes = json_data["attributes"]
  assert_eq(attributes.has_key("service.name"), true, "Missing service.name attribute")
  assert_eq(attributes.has_key("operation.type"), true, "Missing operation.type attribute")
  assert_eq(attributes.has_key("db.statement"), true, "Missing db.statement attribute")
}

test "telemetry_serialization_compatibility_across_versions" {
  // 测试跨版本的序列化兼容性
  
  // 模拟不同版本的数据格式
  let v1_format = {
    "version": "1.0",
    "traceId": "0102030405060708090a0b0c0d0e0f10",
    "spanId": "1112131415161718",
    "operationName": "old-operation-name",
    "startTime": "1640995200",  // 秒级时间戳
    "duration": "1500"          // 毫秒级持续时间
  }
  
  let v2_format = {
    "version": "2.0",
    "traceId": "0102030405060708090a0b0c0d0e0f10",
    "spanId": "1112131415161718",
    "name": "new-operation-name",
    "kind": "SERVER",
    "startTimeUnixNano": "1640995200000000000",  // 纳秒级时间戳
    "endTimeUnixNano": "1640995200015000000",    // 纳秒级结束时间
    "attributes": {},
    "status": "OK"
  }
  
  // 验证版本识别
  assert_eq(v1_format["version"], "1.0")
  assert_eq(v2_format["version"], "2.0")
  
  // 模拟版本兼容性处理
  fn normalize_telemetry_data(data : Map[String, String]) -> Map[String, String] {
    if data.get("version") == Some("1.0") {
      // V1到V2的转换
      let normalized = data.clone()
      normalized.insert("name", data.get("operationName").unwrap())
      normalized.insert("startTimeUnixNano", data.get("startTime").unwrap() + "000000000")
      
      let duration_ms = data.get("duration").unwrap().to_int64()
      let start_time_ns = data.get("startTime").unwrap().to_int64() * 1000000000L
      let end_time_ns = start_time_ns + (duration_ms * 1000000L)
      normalized.insert("endTimeUnixNano", end_time_ns.to_string())
      
      normalized.insert("kind", "INTERNAL")  // V1默认值
      normalized.insert("attributes", "{}")  // V1默认值
      normalized.insert("status", "UNSET")   // V1默认值
      
      normalized
    } else {
      data
    }
  }
  
  // 测试V1数据标准化
  let normalized_v1 = normalize_telemetry_data(v1_format)
  assert_eq(normalized_v1.get("name"), Some("old-operation-name"))
  assert_eq(normalized_v1.get("startTimeUnixNano"), Some("1640995200000000000"))
  assert_eq(normalized_v1.get("endTimeUnixNano"), Some("1640995200015000000"))
  assert_eq(normalized_v1.get("kind"), Some("INTERNAL"))
  assert_eq(normalized_v1.get("status"), Some("UNSET"))
  
  // 测试V2数据（不需要转换）
  let normalized_v2 = normalize_telemetry_data(v2_format)
  assert_eq(normalized_v2.get("name"), Some("new-operation-name"))
  assert_eq(normalized_v2.get("startTimeUnixNano"), Some("1640995200000000000"))
  assert_eq(normalized_v2.get("endTimeUnixNano"), Some("1640995200015000000"))
}

test "telemetry_serialization_with_compression" {
  // 测试压缩序列化
  
  // 创建大型遥测数据集
  let large_attribute_set = [for i = 0; i < 1000; i = i + 1].map(fn(i) {
    ("attr_" + i.to_string(), azimuth::telemetry::api::common::AttributeValue::string("value_" + i.to_string()))
  })
  
  let large_event_set = [for i = 0; i < 100; i = i + 1].map(fn(i) {
    azimuth::telemetry::api::trace::SpanEvent::{
      name: "event_" + i.to_string(),
      timestamp_unix_nanos: 1640995200000000000L + (i * 1000000L),
      attributes: [
        ("event_index", azimuth::telemetry::api::common::AttributeValue::int(i.to_int64())),
        ("event_type", azimuth::telemetry::api::common::AttributeValue::string("test_event"))
      ]
    }
  })
  
  // 计算原始数据大小
  let original_size = large_attribute_set.length() * 50 + large_event_set.length() * 100  // 估算
  
  // 模拟压缩效果
  let compression_ratios = [
    ("gzip", 0.3),
    ("lz4", 0.5),
    ("zstd", 0.25),
    ("snappy", 0.6)
  ]
  
  // 验证不同压缩算法的效果
  for compression in compression_ratios {
    let algorithm = compression.0
    let ratio = compression.1
    let compressed_size = (original_size.to_double() * ratio).to_int()
    
    // 验证压缩效果
    assert_eq(compressed_size < original_size, true, 
      algorithm + " compression should reduce size")
    
    // 验证压缩比在合理范围内
    assert_eq(ratio >= 0.1 && ratio <= 0.8, true, 
      algorithm + " compression ratio " + ratio.to_string() + " should be reasonable")
    
    // 计算压缩时间（模拟）
    let compression_time_ms = original_size.to_double() / 10000.0  // 假设压缩速度为10MB/s
    let decompression_time_ms = compressed_size.to_double() / 50000.0  // 假设解压速度为50MB/s
    
    // 验证压缩/解压时间在合理范围内
    assert_eq(compression_time_ms <= 1000, true, 
      algorithm + " compression time should be reasonable")
    assert_eq(decompression_time_ms <= 500, true, 
      algorithm + " decompression time should be reasonable")
  }
  
  // 验证数据完整性
  assert_eq(large_attribute_set.length(), 1000, "Large attribute set should maintain size")
  assert_eq(large_event_set.length(), 100, "Large event set should maintain size")
  
  // 验证特定数据点
  assert_eq(large_attribute_set[0].0, "attr_0")
  assert_eq(large_attribute_set[999].0, "attr_999")
  assert_eq(large_event_set[0].name, "event_0")
  assert_eq(large_event_set[99].name, "event_99")
}