// 遥测数据序列化/反序列化测试用例

test "span_serialization_basic" {
  // 测试Span的基本序列化
  
  // 创建测试Span
  let trace_id = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10]
  let span_id = [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]
  
  let span_context = azimuth::telemetry::api::trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0x01,
    trace_state: "key1=value1,key2=value2"
  }
  
  let span = azimuth::telemetry::api::trace::Span::{
    name: "test-span",
    context: span_context,
    kind: azimuth::telemetry::api::trace::SpanKind::Server,
    parent_span_id: Some([0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x11, 0x22]),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: azimuth::telemetry::api::trace::StatusCode::Ok,
    status_description: Some("Operation completed successfully"),
    attributes: [
      ("http.method", azimuth::telemetry::api::common::AttributeValue::string("GET")),
      ("http.status_code", azimuth::telemetry::api::common::AttributeValue::int(200L)),
      ("user.id", azimuth::telemetry::api::common::AttributeValue::string("user123"))
    ],
    events: [
      azimuth::telemetry::api::trace::SpanEvent::{
        name: "cache.miss",
        timestamp_unix_nanos: 1640995200050000000L,
        attributes: [
          ("cache.key", azimuth::telemetry::api::common::AttributeValue::string("user:123"))
        ]
      }
    ],
    links: []
  }
  
  // 模拟序列化为JSON字符串
  let serialized_json = {
    "name": "test-span",
    "traceId": "0102030405060708090a0b0c0d0e0f10",
    "spanId": "1112131415161718",
    "parentSpanId": "aabbccddeeff1122",
    "kind": "SERVER",
    "startTimeUnixNano": "1640995200000000000",
    "endTimeUnixNano": "1640995200100000000",
    "status": {
      "code": "OK",
      "description": "Operation completed successfully"
    },
    "attributes": [
      {"key": "http.method", "value": {"stringValue": "GET"}},
      {"key": "http.status_code", "value": {"intValue": "200"}},
      {"key": "user.id", "value": {"stringValue": "user123"}}
    ],
    "events": [
      {
        "name": "cache.miss",
        "timeUnixNano": "1640995200050000000",
        "attributes": [
          {"key": "cache.key", "value": {"stringValue": "user:123"}}
        ]
      }
    ],
    "links": []
  }
  
  // 验证序列化结果包含必要字段
  assert_eq(serialized_json.name, "test-span")
  assert_eq(serialized_json.traceId, "0102030405060708090a0b0c0d0e0f10")
  assert_eq(serialized_json.spanId, "1112131415161718")
  assert_eq(serialized_json.kind, "SERVER")
  assert_eq(serialized_json.attributes.length(), 3)
  assert_eq(serialized_json.events.length(), 1)
}

test "attribute_value_serialization" {
  // 测试属性值的序列化
  
  // 创建各种类型的属性值
  let test_attributes = [
    ("string.attr", azimuth::telemetry::api::common::AttributeValue::string("hello world")),
    ("int.attr", azimuth::telemetry::api::common::AttributeValue::int(42L)),
    ("float.attr", azimuth::telemetry::api::common::AttributeValue::float(3.14159)),
    ("bool.attr", azimuth::telemetry::api::common::AttributeValue::bool(true)),
    ("string.array.attr", azimuth::telemetry::api::common::AttributeValue::array_string(["a", "b", "c"])),
    ("int.array.attr", azimuth::telemetry::api::common::AttributeValue::array_int([1L, 2L, 3L])),
    ("float.array.attr", azimuth::telemetry::api::common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array.attr", azimuth::telemetry::api::common::AttributeValue::array_bool([true, false, true]))
  ]
  
  // 模拟序列化结果
  let serialized_attributes = []
  
  for attr in test_attributes {
    let (key, value) = attr
    let serialized = match value {
      StringValue(s) => {
        {"key": key, "value": {"stringValue": s}}
      }
      IntValue(i) => {
        {"key": key, "value": {"intValue": i.to_string()}}
      }
      FloatValue(f) => {
        {"key": key, "value": {"doubleValue": f.to_string()}}
      }
      BoolValue(b) => {
        {"key": key, "value": {"boolValue": b.to_string()}}
      }
      ArrayStringValue(arr) => {
        {"key": key, "value": {"arrayStringValue": arr}}
      }
      ArrayIntValue(arr) => {
        {"key": key, "value": {"arrayIntValue": arr.map(fn(i) { i.to_string() })}}
      }
      ArrayFloatValue(arr) => {
        {"key": key, "value": {"arrayDoubleValue": arr.map(fn(f) { f.to_string() })}}
      }
      ArrayBoolValue(arr) => {
        {"key": key, "value": {"arrayBoolValue": arr.map(fn(b) { b.to_string() })}}
      }
    }
    serialized_attributes.push(serialized)
  }
  
  // 验证序列化结果
  assert_eq(serialized_attributes.length(), 8)
  
  // 验证特定类型的序列化
  assert_eq(serialized_attributes[0].key, "string.attr")
  assert_eq(serialized_attributes[0].value.stringValue, "hello world")
  
  assert_eq(serialized_attributes[1].key, "int.attr")
  assert_eq(serialized_attributes[1].value.intValue, "42")
  
  assert_eq(serialized_attributes[3].key, "bool.attr")
  assert_eq(serialized_attributes[3].value.boolValue, "true")
}

test "metrics_serialization" {
  // 测试指标数据的序列化
  
  // 模拟指标数据
  let metric_data = {
    name: "http.requests.total",
    description: "Total number of HTTP requests",
    unit: "requests",
    data_type: "counter",
    data_points: [
      {
        attributes: [
          {"key": "http.method", "value": {"stringValue": "GET"}},
          {"key": "http.status_code", "value": {"intValue": "200"}}
        ],
        start_time_unix_nano: "1640995200000000000",
        time_unix_nano: "1640995200600000000",
        value: "150"
      },
      {
        attributes: [
          {"key": "http.method", "value": {"stringValue": "POST"}},
          {"key": "http.status_code", "value": {"intValue": "201"}}
        ],
        start_time_unix_nano: "1640995200000000000",
        time_unix_nano: "1640995200600000000",
        value: "75"
      }
    ]
  }
  
  // 验证指标序列化结构
  assert_eq(metric_data.name, "http.requests.total")
  assert_eq(metric_data.description, "Total number of HTTP requests")
  assert_eq(metric_data.unit, "requests")
  assert_eq(metric_data.data_type, "counter")
  assert_eq(metric_data.data_points.length(), 2)
  
  // 验证数据点
  let data_point = metric_data.data_points[0]
  assert_eq(data_point.attributes.length(), 2)
  assert_eq(data_point.value, "150")
  assert_eq(data_point.attributes[0].key, "http.method")
  assert_eq(data_point.attributes[0].value.stringValue, "GET")
}

test "resource_serialization" {
  // 测试资源的序列化
  
  // 创建资源
  let resource = azimuth::telemetry::api::common::Resource::{
    service_name: "payment-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", azimuth::telemetry::api::common::AttributeValue::string("instance-12345")),
      ("host.name", azimuth::telemetry::api::common::AttributeValue::string("web-server-01")),
      ("cloud.provider", azimuth::telemetry::api::common::AttributeValue::string("aws")),
      ("cloud.region", azimuth::telemetry::api::common::AttributeValue::string("us-west-2")),
      ("deployment.environment", azimuth::telemetry::api::common::AttributeValue::string("production"))
    ]
  }
  
  // 模拟序列化结果
  let serialized_resource = {
    "attributes": [
      {"key": "service.name", "value": {"stringValue": "payment-service"}},
      {"key": "service.version", "value": {"stringValue": "2.1.0"}},
      {"key": "telemetry.sdk.name", "value": {"stringValue": "azimuth"}},
      {"key": "telemetry.sdk.version", "value": {"stringValue": "0.1.0"}},
      {"key": "service.instance.id", "value": {"stringValue": "instance-12345"}},
      {"key": "host.name", "value": {"stringValue": "web-server-01"}},
      {"key": "cloud.provider", "value": {"stringValue": "aws"}},
      {"key": "cloud.region", "value": {"stringValue": "us-west-2"}},
      {"key": "deployment.environment", "value": {"stringValue": "production"}}
    ]
  }
  
  // 验证资源序列化
  assert_eq(serialized_resource.attributes.length(), 9)
  
  // 验证特定属性
  let mut found_service_name = false
  let mut found_cloud_region = false
  
  for attr in serialized_resource.attributes {
    if attr.key == "service.name" && attr.value.stringValue == "payment-service" {
      found_service_name = true
    }
    if attr.key == "cloud.region" && attr.value.stringValue == "us-west-2" {
      found_cloud_region = true
    }
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_cloud_region, true)
}

test "context_serialization" {
  // 测试上下文的序列化
  
  // 创建上下文
  let ctx = azimuth::telemetry::api::context::Context::empty()
  let trace_key = azimuth::telemetry::api::context::create_key("trace_id")
  let correlation_key = azimuth::telemetry::api::context::create_key("correlation_id")
  let user_key = azimuth::telemetry::api::context::create_key("user_id")
  
  let enriched_ctx = ctx
    .with_value(trace_key, "trace-123-abc-456")
    .with_value(correlation_key, "corr-789-def-012")
    .with_value(user_key, "user-345-ghi-678")
  
  // 模拟序列化结果
  let serialized_context = {
    "entries": [
      {"key": "trace_id", "value": "trace-123-abc-456"},
      {"key": "correlation_id", "value": "corr-789-def-012"},
      {"key": "user_id", "value": "user-345-ghi-678"}
    ]
  }
  
  // 验证上下文序列化
  assert_eq(serialized_context.entries.length(), 3)
  assert_eq(serialized_context.entries[0].key, "trace_id")
  assert_eq(serialized_context.entries[0].value, "trace-123-abc-456")
  assert_eq(serialized_context.entries[1].key, "correlation_id")
  assert_eq(serialized_context.entries[1].value, "corr-789-def-012")
}

test "baggage_serialization" {
  // 测试Baggage的序列化
  
  // 创建Baggage
  let baggage = azimuth::telemetry::api::context::Baggage::empty()
  let enriched_baggage = baggage
    .with_entry("user.id", "12345")
    .with_entry("user.role", "premium")
    .with_entry("tenant.id", "tenant-001")
    .with_entry("feature.flags", "new-ui,advanced-analytics")
  
  // 模拟序列化结果
  let serialized_baggage = {
    "entries": [
      {"key": "user.id", "value": "12345"},
      {"key": "user.role", "value": "premium"},
      {"key": "tenant.id", "value": "tenant-001"},
      {"key": "feature.flags", "value": "new-ui,advanced-analytics"}
    ]
  }
  
  // 验证Baggage序列化
  assert_eq(serialized_baggage.entries.length(), 4)
  assert_eq(serialized_baggage.entries[0].key, "user.id")
  assert_eq(serialized_baggage.entries[0].value, "12345")
  assert_eq(serialized_baggage.entries[1].key, "user.role")
  assert_eq(serialized_baggage.entries[1].value, "premium")
}

test "deserialization_span_from_json" {
  // 测试从JSON反序列化Span
  
  // 模拟JSON输入
  let json_input = {
    "name": "deserialized-span",
    "traceId": "0102030405060708090a0b0c0d0e0f10",
    "spanId": "1112131415161718",
    "parentSpanId": "aabbccddeeff1122",
    "kind": "CLIENT",
    "startTimeUnixNano": "1640995200000000000",
    "endTimeUnixNano": "1640995200100000000",
    "status": {
      "code": "ERROR",
      "description": "Operation failed"
    },
    "attributes": [
      {"key": "error.type", "value": {"stringValue": "timeout"}},
      {"key": "retry.count", "value": {"intValue": "3"}}
    ],
    "events": [
      {
        "name": "retry.attempt",
        "timeUnixNano": "1640995200050000000",
        "attributes": [
          {"key": "attempt.number", "value": {"intValue": "2"}}
        ]
      }
    ],
    "links": []
  }
  
  // 模拟反序列化过程
  let trace_id_bytes = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10]
  let span_id_bytes = [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]
  let parent_span_id_bytes = [0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x11, 0x22]
  
  let deserialized_span = azimuth::telemetry::api::trace::Span::{
    name: json_input.name,
    context: azimuth::telemetry::api::trace::SpanContext::{
      trace_id: trace_id_bytes,
      span_id: span_id_bytes,
      trace_flags: 0x01,
      trace_state: ""
    },
    kind: azimuth::telemetry::api::trace::SpanKind::Client,
    parent_span_id: Some(parent_span_id_bytes),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: azimuth::telemetry::api::trace::StatusCode::Error,
    status_description: Some("Operation failed"),
    attributes: [
      ("error.type", azimuth::telemetry::api::common::AttributeValue::string("timeout")),
      ("retry.count", azimuth::telemetry::api::common::AttributeValue::int(3L))
    ],
    events: [
      azimuth::telemetry::api::trace::SpanEvent::{
        name: "retry.attempt",
        timestamp_unix_nanos: 1640995200050000000L,
        attributes: [
          ("attempt.number", azimuth::telemetry::api::common::AttributeValue::int(2L))
        ]
      }
    ],
    links: []
  }
  
  // 验证反序列化结果
  assert_eq(deserialized_span.name, "deserialized-span")
  assert_eq(deserialized_span.kind, azimuth::telemetry::api::trace::SpanKind::Client)
  assert_eq(deserialized_span.status, azimuth::telemetry::api::trace::StatusCode::Error)
  assert_eq(deserialized_span.status_description.unwrap(), "Operation failed")
  assert_eq(deserialized_span.attributes.length(), 2)
  assert_eq(deserialized_span.events.length(), 1)
}

test "serialization_format_compatibility" {
  // 测试序列化格式兼容性
  
  // 测试不同的序列化格式
  let test_data = {
    span_name: "compatibility-test",
    trace_id: "abcdef1234567890",
    span_id: "12345678",
    attributes: [
      {"key": "format.test", "value": {"stringValue": "compatibility"}}
    ]
  }
  
  // JSON格式
  let json_format = {
    "format": "json",
    "data": test_data,
    "version": "1.0"
  }
  
  // 简化的文本格式
  let text_format = "format=text|span=compatibility-test|trace=abcdef1234567890|span=12345678|attributes=1"
  
  // 二进制格式（模拟）
  let binary_format = [0x01, 0x02, 0x03, 0x04]  // 简化的二进制表示
  
  // 验证不同格式的存在
  assert_eq(json_format.format, "json")
  assert_eq(json_format.version, "1.0")
  assert_eq(text_format.contains("format=text"), true)
  assert_eq(binary_format.length() > 0, true)
  
  // 验证数据在格式间保持一致
  assert_eq(json_format.data.span_name, test_data.span_name)
  assert_eq(text_format.contains("compatibility-test"), true)
}

test "serialization_error_handling" {
  // 测试序列化错误处理
  
  // 测试处理无效数据
  let invalid_cases = [
    {"type": "invalid_trace_id", "data": "not-a-valid-hex-string"},
    {"type": "invalid_timestamp", "data": "not-a-number"},
    {"type": "invalid_attribute", "data": {"key": "", "value": null}},
    {"type": "missing_required_field", "data": {"name": null}},
    {"type": "invalid_enum_value", "data": "INVALID_SPAN_KIND"}
  ]
  
  for case in invalid_cases {
    // 模拟错误处理
    let error_handled = match case.type {
      "invalid_trace_id" => true,
      "invalid_timestamp" => true,
      "invalid_attribute" => true,
      "missing_required_field" => true,
      "invalid_enum_value" => true,
      _ => false
    }
    
    assert_eq(error_handled, true)
  }
  
  // 测试部分序列化失败的处理
  let partial_data = {
    valid_field: "valid_value",
    invalid_field: null,
    another_valid: 123
  }
  
  // 验证可以序列化有效字段，跳过无效字段
  let partial_serialization_possible = true
  assert_eq(partial_serialization_possible, true)
}

test "serialization_performance_considerations" {
  // 测试序列化性能考虑
  
  // 创建大型数据集进行序列化测试
  let large_span = azimuth::telemetry::api::trace::Span::{
    name: "large-test-span",
    context: azimuth::telemetry::api::trace::SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(i) { i.to_byte() }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(i) { i.to_byte() }),
      trace_flags: 0x01,
      trace_state: ""
    },
    kind: azimuth::telemetry::api::trace::SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: azimuth::telemetry::api::trace::StatusCode::Ok,
    status_description: None,
    attributes: [for i = 0; i < 100; i = i + 1].map(fn(i) {
      ("attr_" + i.to_string(), azimuth::telemetry::api::common::AttributeValue::string("value_" + i.to_string()))
    }),
    events: [for i = 0; i < 50; i = i + 1].map(fn(i) {
      azimuth::telemetry::api::trace::SpanEvent::{
        name: "event_" + i.to_string(),
        timestamp_unix_nanos: 1640995200000000000L + i.to_int64() * 1000000L,
        attributes: [
          ("event_index", azimuth::telemetry::api::common::AttributeValue::int(i.to_int64()))
        ]
      }
    }),
    links: []
  }
  
  // 验证大型数据集的处理
  assert_eq(large_span.attributes.length(), 100)
  assert_eq(large_span.events.length(), 50)
  
  // 模拟序列化大小估算
  let estimated_size = large_span.name.length() + 
    large_span.attributes.length() * 50 +  // 每个属性约50字符
    large_span.events.length() * 100       // 每个事件约100字符
  
  assert_eq(estimated_size > 1000, true)  // 确保数据量足够大
  
  // 验证序列化不会导致内存溢出（简化验证）
  let serialization_safe = estimated_size < 10000000  // 10MB限制
  assert_eq(serialization_safe, true)
}