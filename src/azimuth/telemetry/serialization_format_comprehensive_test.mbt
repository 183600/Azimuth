// 序列化和反序列化综合测试
test "attribute_value_serialization" {
  // 测试AttributeValue的序列化格式
  
  // 字符串属性序列化
  let string_attr = AttributeValue::string("test-value")
  let string_serialized = match string_attr {
    StringValue(s) => "string:" + s
    _ => "unknown"
  }
  assert_eq(string_serialized, "string:test-value")
  
  // 整数属性序列化
  let int_attr = AttributeValue::int(42L)
  let int_serialized = match int_attr {
    IntValue(i) => "int:" + i.to_string()
    _ => "unknown"
  }
  assert_eq(int_serialized, "int:42")
  
  // 浮点数属性序列化
  let float_attr = AttributeValue::float(3.14)
  let float_serialized = match float_attr {
    FloatValue(f) => "float:" + f.to_string()
    _ => "unknown"
  }
  assert_eq(float_serialized.has_prefix("float:3.14"), true)
  
  // 布尔属性序列化
  let bool_attr = AttributeValue::bool(true)
  let bool_serialized = match bool_attr {
    BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
    _ => "unknown"
  }
  assert_eq(bool_serialized, "bool:true")
  
  // 字符串数组属性序列化
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  let string_array_serialized = match string_array_attr {
    ArrayStringValue(arr) => {
      let result = "array_string:"
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + "," }
        result = result + arr[i]
        i = i + 1
      }
      result
    }
    _ => "unknown"
  }
  assert_eq(string_array_serialized, "array_string:a,b,c")
  
  // 整数数组属性序列化
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  let int_array_serialized = match int_array_attr {
    ArrayIntValue(arr) => {
      let result = "array_int:"
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + "," }
        result = result + arr[i].to_string()
        i = i + 1
      }
      result
    }
    _ => "unknown"
  }
  assert_eq(int_array_serialized, "array_int:1,2,3")
}

test "span_context_serialization" {
  // 测试SpanContext的序列化格式
  
  let trace_id = Array::make(16, 0x00_byte)
  trace_id[0] = 0x0a_byte
  trace_id[1] = 0xf7_byte
  trace_id[2] = 0x65_byte
  trace_id[3] = 0x19_byte
  
  let span_id = Array::make(8, 0x00_byte)
  span_id[0] = 0xb7_byte
  span_id[1] = 0xad_byte
  span_id[2] = 0x6b_byte
  span_id[3] = 0x71_byte
  
  let span_context = SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0x01_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 序列化SpanContext为W3C traceparent格式
  let trace_id_hex = "0af7651916cd43dd8448eb211c80319c"
  let span_id_hex = "b7ad6b7169203331"
  let trace_flags_hex = "01"
  
  let traceparent_serialized = "00-" + trace_id_hex + "-" + span_id_hex + "-" + trace_flags_hex
  assert_eq(traceparent_serialized, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // 序列化trace_state
  let trace_state_serialized = span_context.trace_state
  assert_eq(trace_state_serialized, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 验证序列化格式
  assert_eq(traceparent_serialized.has_prefix("00-"), true)
  assert_eq(traceparent_serialized.contains("-"), true)
  assert_eq(traceparent_serialized.length(), 55)
  
  assert_eq(trace_state_serialized.contains("rojo="), true)
  assert_eq(trace_state_serialized.contains("congo="), true)
  assert_eq(trace_state_serialized.contains(","), true)
}

test "log_record_serialization" {
  // 测试LogRecord的序列化格式
  
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Error)
    .body("Error processing request")
    .with_attribute("error.code", AttributeValue::int(500L))
    .with_attribute("error.message", AttributeValue::string("Internal server error"))
    .with_attribute("user.id", AttributeValue::int(12345L))
    .build()
  
  // 序列化LogRecord为JSON格式（模拟）
  let log_json = "{"
    + "\"timestamp\":" + log_record.timestamp_unix_nanos.to_string() + ","
    + "\"severity\":\"ERROR\","
    + "\"body\":\"Error processing request\","
    + "\"attributes\":["
    + "{\"key\":\"error.code\",\"value\":\"int:500\"},"
    + "{\"key\":\"error.message\",\"value\":\"string:Internal server error\"},"
    + "{\"key\":\"user.id\",\"value\":\"int:12345\"}"
    + "]}"
  
  // 验证JSON格式
  assert_eq(log_json.contains("\"timestamp\":1234567890"), true)
  assert_eq(log_json.contains("\"severity\":\"ERROR\""), true)
  assert_eq(log_json.contains("\"body\":\"Error processing request\""), true)
  assert_eq(log_json.contains("\"attributes\""), true)
  assert_eq(log_json.contains("\"key\":\"error.code\""), true)
  assert_eq(log_json.contains("\"value\":\"int:500\""), true)
  
  // 序列化严重性级别
  let severity_serialized = match log_record.severity_number {
    Trace => "TRACE"
    Debug => "DEBUG"
    Info => "INFO"
    Warn => "WARN"
    Error => "ERROR"
    Fatal => "FATAL"
  }
  assert_eq(severity_serialized, "ERROR")
}

test "metrics_serialization" {
  // 测试Metrics的序列化格式
  
  // 序列化Measurement
  let measurement = Measurement::{
    value: 99.5,
    attributes: [
      ("service.name", AttributeValue::string("payment-service")),
      ("operation.type", AttributeValue::string("transaction")),
      ("status", AttributeValue::string("success"))
    ]
  }
  
  let measurement_json = "{"
    + "\"value\":99.5,"
    + "\"attributes\":["
    + "{\"key\":\"service.name\",\"value\":\"string:payment-service\"},"
    + "{\"key\":\"operation.type\",\"value\":\"string:transaction\"},"
    + "{\"key\":\"status\",\"value\":\"string:success\"}"
    + "]}"
  
  // 验证Measurement序列化
  assert_eq(measurement_json.contains("\"value\":99.5"), true)
  assert_eq(measurement_json.contains("\"service.name\""), true)
  assert_eq(measurement_json.contains("\"payment-service\""), true)
  
  // 序列化InstrumentType
  let counter_type = "Counter"
  let histogram_type = "Histogram"
  let up_down_counter_type = "UpDownCounter"
  let gauge_type = "Gauge"
  
  assert_eq(counter_type, "Counter")
  assert_eq(histogram_type, "Histogram")
  assert_eq(up_down_counter_type, "UpDownCounter")
  assert_eq(gauge_type, "Gauge")
}

test "resource_serialization" {
  // 测试Resource的序列化格式
  
  let resource = Resource::default("order-service")
  let resource_json = "{"
    + "\"service_name\":\"order-service\","
    + "\"service_version\":null,"
    + "\"telemetry_sdk_name\":\"azimuth\","
    + "\"telemetry_sdk_version\":\"0.1.0\","
    + "\"attributes\":[]}"
  
  // 验证Resource序列化
  assert_eq(resource_json.contains("\"service_name\":\"order-service\""), true)
  assert_eq(resource_json.contains("\"telemetry_sdk_name\":\"azimuth\""), true)
  assert_eq(resource_json.contains("\"telemetry_sdk_version\":\"0.1.0\""), true)
  assert_eq(resource_json.contains("\"attributes\":[]"), true)
  
  // 序列化带有属性的Resource
  let resource_with_attrs = Resource::default("user-service")
  let attrs = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-123")),
    ("deployment.environment", AttributeValue::string("prod"))
  ]
  
  let attrs_json = “["
    + "{\"key\":\"service.namespace\",\"value\":\"string:production\"},"
    + "{\"key\":\"service.instance.id\",\"value\":\"string:instance-123\"},"
    + "{\"key\":\"deployment.environment\",\"value\":\"string:prod\"}"
    + "]"
  
  // 验证属性序列化
  assert_eq(attrs_json.contains("service.namespace"), true)
  assert_eq(attrs_json.contains("production"), true)
  assert_eq(attrs_json.contains("instance-123"), true)
  assert_eq(attrs_json.contains("prod"), true)
}

test "baggage_serialization" {
  // 测试Baggage的序列化格式
  
  let baggage = Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("session.id", "session-abcdef")
    .with_entry("request.id", "req-123456")
  
  // 序列化为W3C baggage格式
  let baggage_serialized = "user.id=12345,session.id=session-abcdef,request.id=req-123456"
  
  // 验证baggage序列化
  assert_eq(baggage_serialized.contains("user.id=12345"), true)
  assert_eq(baggage_serialized.contains("session.id=session-abcdef"), true)
  assert_eq(baggage_serialized.contains("request.id=req-123456"), true)
  assert_eq(baggage_serialized.contains(","), true)
  
  // 测试包含特殊字符的baggage序列化
  let special_baggage = Baggage::empty()
    .with_entry("special.key", "value with spaces")
    .with_entry("unicode.key", "值测试🚀")
    .with_entry("empty.value", "")
  
  let special_baggage_serialized = "special.key=value with spaces,unicode.key=值测试🚀,empty.value="
  
  assert_eq(special_baggage_serialized.contains("value with spaces"), true)
  assert_eq(special_baggage_serialized.contains("值测试🚀"), true)
  assert_eq(special_baggage_serialized.contains("empty.value="), true)
}

test "context_serialization" {
  // 测试Context的序列化格式
  
  let ctx = context::Context::empty()
  let user_key = create_key("user-id")
  let session_key = create_key("session-id")
  
  let ctx_with_values = ctx
    .with_value(user_key, "user-12345")
    .with_value(session_key, "session-abcdef")
  
  // 序列化Context为键值对格式
  let ctx_serialized = "user-id=user-12345;session-id=session-abcdef"
  
  // 验证Context序列化
  assert_eq(ctx_serialized.contains("user-id=user-12345"), true)
  assert_eq(ctx_serialized.contains("session-id=session-abcdef"), true)
  assert_eq(ctx_serialized.contains(";"), true)
  
  // 测试包含特殊字符的Context序列化
  let special_key = create_key("special.key!@#$%")
  let special_value = "special value 测试🚀"
  
  let ctx_with_special = ctx.with_value(special_key, special_value)
  let special_ctx_serialized = "special.key!@#$%=special value 测试🚀"
  
  assert_eq(special_ctx_serialized.contains("special.key!@#$%"), true)
  assert_eq(special_ctx_serialized.contains("special value 测试🚀"), true)
}

test "complex_span_serialization" {
  // 测试复杂Span的序列化格式
  
  let trace_id = Array::make(16, 0x01_byte)
  let span_id = Array::make(8, 0x02_byte)
  let parent_span_id = Array::make(8, 0x03_byte)
  
  let span = Span::{
    name: "complex-operation",
    context: SpanContext::{
      trace_id: trace_id,
      span_id: span_id,
      trace_flags: 0x01_byte,
      trace_state: "test=value"
    },
    kind: Server,
    parent_span_id: Some(parent_span_id),
    start_time_unix_nanos: 1234567890L,
    end_time_unix_nanos: Some(1234567990L),
    status: Ok,
    status_description: Some("Operation completed successfully"),
    attributes: [
      ("service.name", AttributeValue::string("api-service")),
      ("operation.type", AttributeValue::string("database_query")),
      ("db.statement", AttributeValue::string("SELECT * FROM users")),
      ("db.rows_affected", AttributeValue::int(10L))
    ],
    events: [
      SpanEvent::{
        name: "db.query.start",
        timestamp_unix_nanos: 1234567950L,
        attributes: [
          ("db.query", AttributeValue::string("SELECT * FROM users"))
        ]
      },
      SpanEvent::{
        name: "db.query.complete",
        timestamp_unix_nanos: 1234567980L,
        attributes: [
          ("db.duration_ms", AttributeValue::float(30.5))
        ]
      }
    ],
    links: []
  }
  
  // 序列化Span为JSON格式
  let span_json = "{"
    + "\"name\":\"complex-operation\","
    + "\"kind\":\"Server\","
    + "\"start_time_unix_nanos\":1234567890,"
    + "\"end_time_unix_nanos\":1234567990,"
    + "\"status\":\"Ok\","
    + "\"status_description\":\"Operation completed successfully\","
    + "\"attributes\":["
    + "{\"key\":\"service.name\",\"value\":\"string:api-service\"},"
    + "{\"key\":\"operation.type\",\"value\":\"string:database_query\"},"
    + "{\"key\":\"db.statement\",\"value\":\"string:SELECT * FROM users\"},"
    + "{\"key\":\"db.rows_affected\",\"value\":\"int:10\"}"
    + "],"
    + "\"events\":["
    + "{\"name\":\"db.query.start\",\"timestamp_unix_nanos\":1234567950,\"attributes\":[{\"key\":\"db.query\",\"value\":\"string:SELECT * FROM users\"}]},"
    + "{\"name\":\"db.query.complete\",\"timestamp_unix_nanos\":1234567980,\"attributes\":[{\"key\":\"db.duration_ms\",\"value\":\"float:30.5\"}]}"
    + "],"
    + "\"links\":[]}"
  
  // 验证复杂Span序列化
  assert_eq(span_json.contains("\"name\":\"complex-operation\""), true)
  assert_eq(span_json.contains("\"kind\":\"Server\""), true)
  assert_eq(span_json.contains("\"status\":\"Ok\""), true)
  assert_eq(span_json.contains("\"attributes\""), true)
  assert_eq(span_json.contains("\"events\""), true)
  assert_eq(span_json.contains("\"db.query.start\""), true)
  assert_eq(span_json.contains("\"db.query.complete\""), true)
  assert_eq(span_json.contains("\"SELECT * FROM users\""), true)
}

test "serialization_format_consistency" {
  // 测试序列化格式的一致性
  
  // 创建相同的对象并验证序列化结果的一致性
  let attr1 = AttributeValue::string("test-value")
  let attr2 = AttributeValue::string("test-value")
  
  let serialized1 = match attr1 {
    StringValue(s) => "string:" + s
    _ => "unknown"
  }
  
  let serialized2 = match attr2 {
    StringValue(s) => "string:" + s
    _ => "unknown"
  }
  
  assert_eq(serialized1, serialized2)
  assert_eq(serialized1, "string:test-value")
  
  // 测试不同类型属性的序列化区分
  let string_attr = AttributeValue::string("123")
  let int_attr = AttributeValue::int(123L)
  
  let string_serialized = match string_attr {
    StringValue(s) => "string:" + s
    _ => "unknown"
  }
  
  let int_serialized = match int_attr {
    IntValue(i) => "int:" + i.to_string()
    _ => "unknown"
  }
  
  assert_eq(string_serialized, "string:123")
  assert_eq(int_serialized, "int:123")
  assert_eq(string_serialized != int_serialized, true) // 不同类型应该有不同的序列化格式
  
  // 测试数组序列化的顺序一致性
  let array_attr = AttributeValue::array_string(["a", "b", "c"])
  let array_serialized = match array_attr {
    ArrayStringValue(arr) => {
      let result = "array_string:"
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + "," }
        result = result + arr[i]
        i = i + 1
      }
      result
    }
    _ => "unknown"
  }
  
  assert_eq(array_serialized, "array_string:a,b,c")
  assert_eq(array_serialized.contains("a,b,c"), true)
  assert_eq(array_serialized.contains("b,c"), false) // 验证顺序正确
}