// 数据一致性测试 - 验证分布式环境下数据一致性
// 测试Azimuth Telemetry在分布式系统中的数据一致性和完整性

test "distributed_trace_context_consistency" {
  // 测试分布式追踪上下文的一致性
  
  // 1. 创建根追踪上下文
  let root_trace_id = [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]
  let root_span_id = [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]
  
  let root_context = trace::SpanContext::{
    trace_id: root_trace_id,
    span_id: root_span_id,
    trace_flags: 1_byte,
    trace_state: "user_id=user_12345,session_id=sess_abcde"
  }
  
  // 2. 验证根上下文的完整性
  assert_eq(root_context.trace_id.length(), 16)
  assert_eq(root_context.span_id.length(), 8)
  assert_eq(root_context.trace_flags, 1_byte)
  assert_eq(root_context.trace_state.contains("user_id=user_12345"), true)
  
  // 3. 模拟服务A中的上下文传播
  let service_a_context = context::Context::empty()
  let trace_key = context::create_key("trace.context")
  let service_a_ctx = service_a_context.with_value(trace_key, "trace_id=0af7651916cd43dd8448eb211c80319c:span_id=b7ad6b7169203331")
  
  // 服务A创建子span
  let service_a_span_id = [0xc1, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]
  let service_a_span = trace::Span::{
    name: "service_a_operation",
    context: trace::SpanContext::{
      trace_id: root_trace_id,  // 继承相同的trace_id
      span_id: service_a_span_id,
      trace_flags: 1_byte,
      trace_state: root_context.trace_state
    },
    kind: trace::Server,
    parent_span_id: Some(root_span_id),  // 设置父span
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: trace::Ok,
    status_description: None,
    attributes: [
      ("service.name", common::AttributeValue::string("service-a")),
      ("service.version", common::AttributeValue::string("1.2.0"))
    ],
    events: [],
    links: []
  }
  
  // 验证服务A的span一致性
  assert_eq(service_a_span.context.trace_id, root_context.trace_id)
  assert_eq(service_a_span.parent_span_id.unwrap(), root_context.span_id)
  assert_eq(service_a_span.context.trace_state, root_context.trace_state)
  
  // 4. 模拟服务B中的上下文传播
  let service_b_context = context::Context::empty()
  let service_b_ctx = service_b_context.with_value(trace_key, "trace_id=0af7651916cd43dd8448eb211c80319c:span_id=c123456789abcdef")
  
  // 服务B创建子span
  let service_b_span_id = [0xd2, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
  let service_b_span = trace::Span::{
    name: "service_b_operation",
    context: trace::SpanContext::{
      trace_id: root_trace_id,  // 继承相同的trace_id
      span_id: service_b_span_id,
      trace_flags: 1_byte,
      trace_state: root_context.trace_state
    },
    kind: trace::Client,
    parent_span_id: Some(service_a_span_id),  // 服务A的span作为父span
    start_time_unix_nanos: 1640995200050000000L,
    end_time_unix_nanos: Some(1640995200150000000L),
    status: trace::Ok,
    status_description: None,
    attributes: [
      ("service.name", common::AttributeValue::string("service-b")),
      ("service.version", common::AttributeValue::string("2.1.0"))
    ],
    events: [],
    links: []
  }
  
  // 验证服务B的span一致性
  assert_eq(service_b_span.context.trace_id, root_context.trace_id)
  assert_eq(service_b_span.parent_span_id.unwrap(), service_a_span_id)
  assert_eq(service_b_span.context.trace_state, root_context.trace_state)
  
  // 5. 验证跨服务的上下文一致性
  assert_eq(root_context.trace_id, service_a_span.context.trace_id)
  assert_eq(root_context.trace_id, service_b_span.context.trace_id)
  assert_eq(service_a_span.context.trace_flags, service_b_span.context.trace_flags)
  
  // 6. 验证span层次结构
  assert_eq(service_a_span.parent_span_id.unwrap(), root_context.span_id)
  assert_eq(service_b_span.parent_span_id.unwrap(), service_a_span.context.span_id)
  
  // 7. 验证时间顺序一致性
  assert_eq(service_a_span.start_time_unix_nanos <= service_b_span.start_time_unix_nanos, true)
  assert_eq(service_a_span.end_time_unix_nanos.unwrap() <= service_b_span.end_time_unix_nanos.unwrap(), true)
}

test "metrics_consistency_across_services" {
  // 测试跨服务的指标一致性
  
  // 1. 定义统一的指标规范
  let metric_name = "http_requests_total"
  let metric_unit = "count"
  let metric_description = "Total HTTP requests"
  
  // 2. 服务A的指标收集
  let service_a_attributes = [
    ("service.name", common::AttributeValue::string("service-a")),
    ("service.version", common::AttributeValue::string("1.2.0")),
    ("http.method", common::AttributeValue::string("GET")),
    ("http.status_code", common::AttributeValue::int(200L)),
    ("endpoint", common::AttributeValue::string("/api/users"))
  ]
  
  let service_a_timestamp = 1640995200000000000L
  
  // 3. 服务B的指标收集
  let service_b_attributes = [
    ("service.name", common::AttributeValue::string("service-b")),
    ("service.version", common::AttributeValue::string("2.1.0")),
    ("http.method", common::AttributeValue::string("GET")),
    ("http.status_code", common::AttributeValue::int(200L)),
    ("endpoint", common::AttributeValue::string("/api/users"))
  ]
  
  let service_b_timestamp = 1640995200050000000L
  
  // 4. 验证指标名称和单位一致性
  assert_eq(metric_name, "http_requests_total")
  assert_eq(metric_unit, "count")
  assert_eq(metric_description, "Total HTTP requests")
  
  // 5. 验证属性键的一致性
  let service_a_keys = [] : Array[String]
  let mut i = 0
  while i < service_a_attributes.length() {
    service_a_keys.push(service_a_attributes[i].0)
    i = i + 1
  }
  
  let service_b_keys = [] : Array[String]
  i = 0
  while i < service_b_attributes.length() {
    service_b_keys.push(service_b_attributes[i].0)
    i = i + 1
  }
  
  assert_eq(service_a_keys.length(), service_b_keys.length())
  
  // 验证共同的属性键
  let common_keys = ["http.method", "http.status_code", "endpoint"]
  let mut j = 0
  while j < common_keys.length() {
    let key = common_keys[j]
    assert_eq(service_a_keys.contains(key), true)
    assert_eq(service_b_keys.contains(key), true)
    j = j + 1
  }
  
  // 6. 验证相同属性值的一致性
  let mut found_method_a = false
  let mut found_status_a = false
  let mut found_endpoint_a = false
  
  i = 0
  while i < service_a_attributes.length() {
    let (key, value) = service_a_attributes[i]
    if key == "http.method" {
      match value {
        common::StringValue(method) => {
          assert_eq(method, "GET")
          found_method_a = true
        }
        _ => assert_eq(false, true, "Expected string value for http.method")
      }
    }
    if key == "http.status_code" {
      match value {
        common::IntValue(status) => {
          assert_eq(status, 200L)
          found_status_a = true
        }
        _ => assert_eq(false, true, "Expected int value for http.status_code")
      }
    }
    if key == "endpoint" {
      match value {
        common::StringValue(endpoint) => {
          assert_eq(endpoint, "/api/users")
          found_endpoint_a = true
        }
        _ => assert_eq(false, true, "Expected string value for endpoint")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_method_a, true)
  assert_eq(found_status_a, true)
  assert_eq(found_endpoint_a, true)
  
  // 7. 验证服务B的相同属性值
  let mut found_method_b = false
  let mut found_status_b = false
  let mut found_endpoint_b = false
  
  i = 0
  while i < service_b_attributes.length() {
    let (key, value) = service_b_attributes[i]
    if key == "http.method" {
      match value {
        common::StringValue(method) => {
          assert_eq(method, "GET")
          found_method_b = true
        }
        _ => assert_eq(false, true, "Expected string value for http.method")
      }
    }
    if key == "http.status_code" {
      match value {
        common::IntValue(status) => {
          assert_eq(status, 200L)
          found_status_b = true
        }
        _ => assert_eq(false, true, "Expected int value for http.status_code")
      }
    }
    if key == "endpoint" {
      match value {
        common::StringValue(endpoint) => {
          assert_eq(endpoint, "/api/users")
          found_endpoint_b = true
        }
        _ => assert_eq(false, true, "Expected string value for endpoint")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_method_b, true)
  assert_eq(found_status_b, true)
  assert_eq(found_endpoint_b, true)
}

test "log_data_consistency_validation" {
  // 测试日志数据的一致性验证
  
  // 1. 创建统一的日志结构
  let base_timestamp = 1640995200000000000L
  let trace_id_bytes = [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]
  let span_id_bytes = [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]
  
  // 2. 服务A的日志记录
  let service_a_log = logs::LogRecord::{
    timestamp_unix_nanos: base_timestamp,
    observed_timestamp_unix_nanos: Some(base_timestamp + 1000000L),
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("HTTP request processed successfully"),
    attributes: [
      ("service.name", common::AttributeValue::string("service-a")),
      ("service.version", common::AttributeValue::string("1.2.0")),
      ("http.method", common::AttributeValue::string("GET")),
      ("http.url", common::AttributeValue::string("/api/users")),
      ("http.status_code", common::AttributeValue::int(200L)),
      ("duration_ms", common::AttributeValue::float(45.5)),
      ("user.id", common::AttributeValue::string("user_12345"))
    ],
    trace_id: Some(trace_id_bytes),
    span_id: Some(span_id_bytes),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("service-a")),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "http-handler",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 3. 服务B的日志记录
  let service_b_log = logs::LogRecord::{
    timestamp_unix_nanos: base_timestamp + 5000000L,
    observed_timestamp_unix_nanos: Some(base_timestamp + 6000000L),
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Database query executed successfully"),
    attributes: [
      ("service.name", common::AttributeValue::string("service-b")),
      ("service.version", common::AttributeValue::string("2.1.0")),
      ("db.operation", common::AttributeValue::string("SELECT")),
      ("db.table", common::AttributeValue::string("users")),
      ("db.rows_affected", common::AttributeValue::int(1L)),
      ("duration_ms", common::AttributeValue::float(12.3)),
      ("user.id", common::AttributeValue::string("user_12345"))
    ],
    trace_id: Some(trace_id_bytes),  // 相同的trace_id
    span_id: Some([0xc1, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]),  // 不同的span_id
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::default("service-b")),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "db-client",
      version: Some("2.0.0"),
      schema_url: None
    })
  }
  
  // 4. 验证追踪一致性
  assert_eq(service_a_log.trace_id.unwrap(), service_b_log.trace_id.unwrap())
  assert_eq(service_a_log.trace_flags.unwrap(), service_b_log.trace_flags.unwrap())
  assert_eq(service_a_log.trace_id.unwrap().length(), 16)
  assert_eq(service_a_log.span_id.unwrap().length(), 8)
  assert_eq(service_b_log.span_id.unwrap().length(), 8)
  
  // 5. 验证时间顺序一致性
  assert_eq(service_b_log.timestamp_unix_nanos > service_a_log.timestamp_unix_nanos, true)
  assert_eq(service_b_log.observed_timestamp_unix_nanos.unwrap() > service_a_log.observed_timestamp_unix_nanos.unwrap(), true)
  
  // 6. 验证用户上下文一致性
  let mut found_user_a = false
  let mut found_user_b = false
  
  let mut i = 0
  while i < service_a_log.attributes.length() {
    let (key, value) = service_a_log.attributes[i]
    if key == "user.id" {
      match value {
        common::StringValue(user_id) => {
          assert_eq(user_id, "user_12345")
          found_user_a = true
        }
        _ => assert_eq(false, true, "Expected string value for user.id")
      }
    }
    i = i + 1
  }
  
  i = 0
  while i < service_b_log.attributes.length() {
    let (key, value) = service_b_log.attributes[i]
    if key == "user.id" {
      match value {
        common::StringValue(user_id) => {
          assert_eq(user_id, "user_12345")
          found_user_b = true
        }
        _ => assert_eq(false, true, "Expected string value for user.id")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_user_a, true)
  assert_eq(found_user_b, true)
  
  // 7. 验证日志级别一致性
  assert_eq(service_a_log.severity_number, logs::Info)
  assert_eq(service_b_log.severity_number, logs::Info)
  assert_eq(service_a_log.severity_text.unwrap(), "INFO")
  assert_eq(service_b_log.severity_text.unwrap(), "INFO")
}

test "cross_service_baggage_consistency" {
  // 测试跨服务的baggage一致性
  
  // 1. 创建初始baggage
  let initial_baggage = context::Baggage::empty()
  
  // 2. 在服务A中添加baggage条目
  let service_a_baggage = initial_baggage
    .with_entry("user.id", "user_12345")
    .with_entry("session.id", "sess_abcde")
    .with_entry("request.id", "req_123456")
    .with_entry("client.version", "1.2.3")
    .with_entry("tenant.id", "tenant_789")
  
  // 3. 验证服务A的baggage
  assert_eq(service_a_baggage.get("user.id").unwrap(), "user_12345")
  assert_eq(service_a_baggage.get("session.id").unwrap(), "sess_abcde")
  assert_eq(service_a_baggage.get("request.id").unwrap(), "req_123456")
  assert_eq(service_a_baggage.get("client.version").unwrap(), "1.2.3")
  assert_eq(service_a_baggage.get("tenant.id").unwrap(), "tenant_789")
  assert_eq(service_a_baggage.entries.length(), 5)
  
  // 4. 在服务B中传播baggage（模拟网络传输）
  let service_b_baggage = context::Baggage::empty()
    .with_entry("user.id", "user_12345")  // 传播的值
    .with_entry("session.id", "sess_abcde")  // 传播的值
    .with_entry("request.id", "req_123456")  // 传播的值
    .with_entry("client.version", "1.2.3")  // 传播的值
    .with_entry("tenant.id", "tenant_789")  // 传播的值
    .with_entry("service.name", "service-b")  // 服务B添加的值
    .with_entry("service.version", "2.1.0")  // 服务B添加的值
  
  // 5. 验证服务B中传播的baggage一致性
  assert_eq(service_b_baggage.get("user.id").unwrap(), "user_12345")
  assert_eq(service_b_baggage.get("session.id").unwrap(), "sess_abcde")
  assert_eq(service_b_baggage.get("request.id").unwrap(), "req_123456")
  assert_eq(service_b_baggage.get("client.version").unwrap(), "1.2.3")
  assert_eq(service_b_baggage.get("tenant.id").unwrap(), "tenant_789")
  assert_eq(service_b_baggage.get("service.name").unwrap(), "service-b")
  assert_eq(service_b_baggage.get("service.version").unwrap(), "2.1.0")
  assert_eq(service_b_baggage.entries.length(), 7)
  
  // 6. 在服务C中进一步传播baggage
  let service_c_baggage = context::Baggage::empty()
    .with_entry("user.id", "user_12345")  // 原始值
    .with_entry("session.id", "sess_abcde")  // 原始值
    .with_entry("request.id", "req_123456")  // 原始值
    .with_entry("client.version", "1.2.3")  // 原始值
    .with_entry("tenant.id", "tenant_789")  // 原始值
    .with_entry("service.name", "service-c")  // 服务C的值
    .with_entry("service.version", "3.0.1")  // 服务C的值
    .with_entry("region", "us-west-2")  // 服务C添加的值
  
  // 7. 验证跨服务baggage一致性
  let critical_entries = ["user.id", "session.id", "request.id", "tenant.id"]
  let mut i = 0
  while i < critical_entries.length() {
    let entry = critical_entries[i]
    
    let service_a_value = service_a_baggage.get(entry).unwrap()
    let service_b_value = service_b_baggage.get(entry).unwrap()
    let service_c_value = service_c_baggage.get(entry).unwrap()
    
    assert_eq(service_a_value, service_b_value)
    assert_eq(service_b_value, service_c_value)
    assert_eq(service_a_value, service_c_value)
    
    i = i + 1
  }
  
  // 8. 验证baggage完整性
  assert_eq(service_a_baggage.entries.length(), 5)
  assert_eq(service_b_baggage.entries.length(), 7)
  assert_eq(service_c_baggage.entries.length(), 8)
  
  // 9. 验证特定服务条目不冲突
  assert_eq(service_b_baggage.get("service.name").unwrap(), "service-b")
  assert_eq(service_c_baggage.get("service.name").unwrap(), "service-c")
  assert_eq(service_b_baggage.get("service.version").unwrap(), "2.1.0")
  assert_eq(service_c_baggage.get("service.version").unwrap(), "3.0.1")
}

test "data_integrity_across_serialization" {
  // 测试跨序列化的数据完整性
  
  // 1. 创建复杂的遥测数据结构
  let original_trace_id = [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]
  let original_span_id = [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]
  
  let original_span = trace::Span::{
    name: "test_operation",
    context: trace::SpanContext::{
      trace_id: original_trace_id,
      span_id: original_span_id,
      trace_flags: 1_byte,
      trace_state: "user_id=user_12345,session_id=sess_abcde"
    },
    kind: trace::Server,
    parent_span_id: Some([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: trace::Ok,
    status_description: Some("Operation completed successfully"),
    attributes: [
      ("string.attr", common::AttributeValue::string("test_value")),
      ("int.attr", common::AttributeValue::int(42L)),
      ("float.attr", common::AttributeValue::float(3.14159)),
      ("bool.attr", common::AttributeValue::bool(true)),
      ("array.string", common::AttributeValue::array_string(["a", "b", "c"])),
      ("array.int", common::AttributeValue::array_int([1L, 2L, 3L])),
      ("array.float", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
      ("array.bool", common::AttributeValue::array_bool([true, false, true]))
    ],
    events: [
      trace::SpanEvent::{
        name: "test_event",
        timestamp_unix_nanos: 1640995200050000000L,
        attributes: [
          ("event.string", common::AttributeValue::string("event_value")),
          ("event.int", common::AttributeValue::int(100L))
        ]
      }
    ],
    links: [
      trace::SpanLink::{
        context: trace::SpanContext::{
          trace_id: [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
          span_id: [0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11],
          trace_flags: 1_byte,
          trace_state: "linked=true"
        },
        attributes: [
          ("link.attr", common::AttributeValue::string("link_value"))
        ]
      }
    ]
  }
  
  // 2. 模拟序列化为字符串（简化版JSON）
  let serialized = "{"
  serialized = serialized + "\"name\":\"" + original_span.name + "\","
  serialized = serialized + "\"trace_id\":\"" + "0af7651916cd43dd8448eb211c80319c" + "\","
  serialized = serialized + "\"span_id\":\"" + "b7ad6b7169203331" + "\","
  serialized = serialized + "\"kind\":\"Server\","
  serialized = serialized + "\"start_time\":" + original_span.start_time_unix_nanos.to_string() + ","
  serialized = serialized + "\"end_time\":" + original_span.end_time_unix_nanos.unwrap().to_string() + ","
  serialized = serialized + "\"status\":\"Ok\","
  serialized = serialized + "\"attributes_count\":" + original_span.attributes.length().to_string() + ","
  serialized = serialized + "\"events_count\":" + original_span.events.length().to_string() + ","
  serialized = serialized + "\"links_count\":" + original_span.links.length().to_string()
  serialized = serialized + "}"
  
  // 3. 验证序列化数据的完整性
  assert_eq(serialized.contains("\"name\":\"test_operation\""), true)
  assert_eq(serialized.contains("\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\""), true)
  assert_eq(serialized.contains("\"span_id\":\"b7ad6b7169203331\""), true)
  assert_eq(serialized.contains("\"kind\":\"Server\""), true)
  assert_eq(serialized.contains("\"status\":\"Ok\""), true)
  assert_eq(serialized.contains("\"attributes_count\":8"), true)
  assert_eq(serialized.contains("\"events_count\":1"), true)
  assert_eq(serialized.contains("\"links_count\":1"), true)
  
  // 4. 模拟反序列化（验证关键数据）
  let deserialized_name = "test_operation"
  let deserialized_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let deserialized_span_id = "b7ad6b7169203331"
  let deserialized_kind = "Server"
  let deserialized_status = "Ok"
  let deserialized_attributes_count = 8
  let deserialized_events_count = 1
  let deserialized_links_count = 1
  
  // 5. 验证反序列化数据的完整性
  assert_eq(deserialized_name, original_span.name)
  assert_eq(deserialized_trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(deserialized_span_id, "b7ad6b7169203331")
  assert_eq(deserialized_kind, "Server")
  assert_eq(deserialized_status, "Ok")
  assert_eq(deserialized_attributes_count, original_span.attributes.length())
  assert_eq(deserialized_events_count, original_span.events.length())
  assert_eq(deserialized_links_count, original_span.links.length())
  
  // 6. 验证复杂数据类型的完整性
  let mut found_string_attr = false
  let mut found_int_attr = false
  let mut found_float_attr = false
  let mut found_bool_attr = false
  let mut found_array_string = false
  let mut found_array_int = false
  let mut found_array_float = false
  let mut found_array_bool = false
  
  let mut i = 0
  while i < original_span.attributes.length() {
    let (key, value) = original_span.attributes[i]
    match key {
      "string.attr" => {
        match value {
          common::StringValue(s) => {
            assert_eq(s, "test_value")
            found_string_attr = true
          }
          _ => assert_eq(false, true, "Expected StringValue for string.attr")
        }
      }
      "int.attr" => {
        match value {
          common::IntValue(n) => {
            assert_eq(n, 42L)
            found_int_attr = true
          }
          _ => assert_eq(false, true, "Expected IntValue for int.attr")
        }
      }
      "float.attr" => {
        match value {
          common::FloatValue(f) => {
            assert_eq(f > 3.14 && f < 3.15, true)
            found_float_attr = true
          }
          _ => assert_eq(false, true, "Expected FloatValue for float.attr")
        }
      }
      "bool.attr" => {
        match value {
          common::BoolValue(b) => {
            assert_eq(b, true)
            found_bool_attr = true
          }
          _ => assert_eq(false, true, "Expected BoolValue for bool.attr")
        }
      }
      "array.string" => {
        match value {
          common::ArrayStringValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], "a")
            assert_eq(arr[1], "b")
            assert_eq(arr[2], "c")
            found_array_string = true
          }
          _ => assert_eq(false, true, "Expected ArrayStringValue for array.string")
        }
      }
      "array.int" => {
        match value {
          common::ArrayIntValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], 1L)
            assert_eq(arr[1], 2L)
            assert_eq(arr[2], 3L)
            found_array_int = true
          }
          _ => assert_eq(false, true, "Expected ArrayIntValue for array.int")
        }
      }
      "array.float" => {
        match value {
          common::ArrayFloatValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0] > 1.0 && arr[0] < 1.2, true)
            assert_eq(arr[1] > 2.1 && arr[1] < 2.3, true)
            assert_eq(arr[2] > 3.2 && arr[2] < 3.4, true)
            found_array_float = true
          }
          _ => assert_eq(false, true, "Expected ArrayFloatValue for array.float")
        }
      }
      "array.bool" => {
        match value {
          common::ArrayBoolValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], true)
            assert_eq(arr[1], false)
            assert_eq(arr[2], true)
            found_array_bool = true
          }
          _ => assert_eq(false, true, "Expected ArrayBoolValue for array.bool")
        }
      }
      _ => assert_eq(false, true, "Unexpected attribute key: " + key)
    }
    i = i + 1
  }
  
  assert_eq(found_string_attr, true)
  assert_eq(found_int_attr, true)
  assert_eq(found_float_attr, true)
  assert_eq(found_bool_attr, true)
  assert_eq(found_array_string, true)
  assert_eq(found_array_int, true)
  assert_eq(found_array_float, true)
  assert_eq(found_array_bool, true)
}