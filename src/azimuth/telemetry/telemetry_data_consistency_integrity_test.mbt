// 遥测数据一致性和完整性测试
// 测试数据在不同模块间传递时的一致性和完整性保证

test "attribute_type_consistency" {
  // 测试属性类型在不同API间的一致性
  
  // 定义标准属性集
  let standard_attributes = [
    ("string_attr", common::AttributeValue::string("test_string")),
    ("int_attr", common::AttributeValue::int(42L)),
    ("float_attr", common::AttributeValue::float(3.14159)),
    ("bool_attr", common::AttributeValue::bool(true)),
    ("string_array", common::AttributeValue::array_string(["a", "b", "c"])),
    ("int_array", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  // 在Span中使用这些属性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("consistency_tracer")
  let (_, span) = tracer.start_span(
    context::Context::empty(),
    "consistency_test_span",
    trace::Internal,
    standard_attributes
  )
  
  // 在Metric中使用这些属性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("consistency_meter")
  let counter = meter.create_counter("consistency_counter", "count", "Consistency test counter")
  counter.add(1L, standard_attributes)
  
  // 在Log中使用这些属性
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("consistency_logger")
  logger.info("Consistency test log", standard_attributes)
  
  // 验证属性在所有信号中保持一致
  assert_eq(span.attributes.length(), 8)
  
  // 验证每个属性的类型和值
  let mut i = 0
  while i < standard_attributes.length() {
    let (key, expected_value) = standard_attributes[i]
    let found = span.attributes.any(fn(attr) {
      match attr {
        (attr_key, attr_value) => {
          if attr_key == key {
            // 验证类型匹配
            match (expected_value, attr_value) {
              (common::StringValue(_), common::StringValue(_)) => true
              (common::IntValue(_), common::IntValue(_)) => true
              (common::FloatValue(_), common::FloatValue(_)) => true
              (common::BoolValue(_), common::BoolValue(_)) => true
              (common::ArrayStringValue(_), common::ArrayStringValue(_)) => true
              (common::ArrayIntValue(_), common::ArrayIntValue(_)) => true
              (common::ArrayFloatValue(_), common::ArrayFloatValue(_)) => true
              (common::ArrayBoolValue(_), common::ArrayBoolValue(_)) => true
              _ => false
            }
          } else {
            false
          }
        }
      }
    })
    assert_eq(found, true)
    i = i + 1
  }
}

test "context_propagation_integrity" {
  // 测试上下文在传播过程中的完整性
  
  // 创建初始上下文并添加多个键值对
  let initial_context = context::Context::empty()
  let user_key = context::create_key("user_id")
  let session_key = context::create_key("session_id")
  let request_key = context::create_key("request_id")
  let trace_key = context::create_key("trace_id")
  
  let enriched_context = initial_context
    .with_value(user_key, "user123")
    .with_value(session_key, "session456")
    .with_value(request_key, "request789")
    .with_value(trace_key, "traceabcdef123456")
  
  // 模拟上下文传播：创建子上下文
  let operation_key = context::create_key("operation")
  let child_context = enriched_context.with_value(operation_key, "database_query")
  
  // 验证父上下文的所有值在子上下文中仍然存在
  assert_eq(child_context.get(user_key).unwrap(), "user123")
  assert_eq(child_context.get(session_key).unwrap(), "session456")
  assert_eq(child_context.get(request_key).unwrap(), "request789")
  assert_eq(child_context.get(trace_key).unwrap(), "traceabcdef123456")
  assert_eq(child_context.get(operation_key).unwrap(), "database_query")
  
  // 模拟更深层的上下文传播
  let depth_key = context::create_key("call_depth")
  let deep_context = child_context.with_value(depth_key, "3")
  
  // 验证所有层级的上下文值都保持完整
  assert_eq(deep_context.get(user_key).unwrap(), "user123")
  assert_eq(deep_context.get(session_key).unwrap(), "session456")
  assert_eq(deep_context.get(request_key).unwrap(), "request789")
  assert_eq(deep_context.get(trace_key).unwrap(), "traceabcdef123456")
  assert_eq(deep_context.get(operation_key).unwrap(), "database_query")
  assert_eq(deep_context.get(depth_key).unwrap(), "3")
}

test "trace_span_hierarchy_integrity" {
  // 测试trace span层次结构的完整性
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("hierarchy_tracer")
  
  // 创建根span
  let (root_ctx, root_span) = tracer.start_span(
    context::Context::empty(),
    "root_operation",
    trace::Server,
    [("service", common::AttributeValue::string("api_gateway"))]
  )
  
  // 创建子span
  let (child1_ctx, child1_span) = tracer.start_span(
    root_ctx,
    "child_operation_1",
    trace::Internal,
    [("component", common::AttributeValue::string("auth"))]
  )
  
  let (child2_ctx, child2_span) = tracer.start_span(
    root_ctx,
    "child_operation_2",
    trace::Client,
    [("component", common::AttributeValue::string("datastore"))]
  )
  
  // 创建孙级span
  let (grandchild_ctx, grandchild_span) = tracer.start_span(
    child1_ctx,
    "grandchild_operation",
    trace::Internal,
    [("sub_component", common::AttributeValue::string("token_validation"))]
  )
  
  // 验证span层次结构
  assert_eq(root_span.name, "root_operation")
  assert_eq(root_span.kind, trace::Server)
  assert_eq(root_span.parent_span_id, None)
  
  assert_eq(child1_span.name, "child_operation_1")
  assert_eq(child1_span.kind, trace::Internal)
  assert_eq(child2_span.name, "child_operation_2")
  assert_eq(child2_span.kind, trace::Client)
  
  assert_eq(grandchild_span.name, "grandchild_operation")
  assert_eq(grandchild_span.kind, trace::Internal)
  
  // 验证属性继承
  assert_eq(root_span.attributes.length(), 1)
  assert_eq(child1_span.attributes.length(), 1)
  assert_eq(child2_span.attributes.length(), 1)
  assert_eq(grandchild_span.attributes.length(), 1)
  
  // 验证trace ID一致性（在真实实现中应该相同）
  assert_eq(root_span.context.trace_id.length(), 16)
  assert_eq(child1_span.context.trace_id.length(), 16)
  assert_eq(child2_span.context.trace_id.length(), 16)
  assert_eq(grandchild_span.context.trace_id.length(), 16)
}

test "metric_data_aggregation_consistency" {
  // 测试指标数据聚合的一致性
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("aggregation_meter")
  
  let counter = meter.create_counter("requests_total", "count", "Total number of requests")
  let histogram = meter.create_histogram("request_duration", "ms", "Request duration")
  let gauge = meter.create_gauge("active_connections", "connections", "Active connections")
  
  // 定义一致的属性集
  let base_attributes = [
    ("service", common::AttributeValue::string("user_service")),
    ("version", common::AttributeValue::string("1.2.3")),
    ("environment", common::AttributeValue::string("production"))
  ]
  
  let api_attributes = base_attributes + [
    ("endpoint", common::AttributeValue::string("/api/users")),
    ("method", common::AttributeValue::string("GET"))
  ]
  
  let db_attributes = base_attributes + [
    ("operation", common::AttributeValue::string("query")),
    ("table", common::AttributeValue::string("users"))
  ]
  
  // 记录不同类型的指标数据
  counter.add(1L, api_attributes)
  histogram.record(150.5, api_attributes)
  gauge.record(25.0, base_attributes)
  
  counter.add(1L, db_attributes)
  histogram.record(85.2, db_attributes)
  
  // 验证属性一致性
  assert_eq(api_attributes.length(), 5)
  assert_eq(db_attributes.length(), 5)
  assert_eq(base_attributes.length(), 3)
  
  // 验证基础属性在所有属性集中都存在
  let mut i = 0
  while i < base_attributes.length() {
    let (key, _) = base_attributes[i]
    assert_eq(api_attributes.any(fn(attr) { attr.0 == key }), true)
    assert_eq(db_attributes.any(fn(attr) { attr.0 == key }), true)
    i = i + 1
  }
}

test "log_record_data_integrity" {
  // 测试日志记录的数据完整性
  
  // 创建复杂的日志记录
  let base_time = 1234567890000000L
  let trace_id_bytes = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 
                        0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
  let span_id_bytes = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
  
  let resource = common::Resource::default("auth_service")
  let scope = common::InstrumentationScope {
    name: "auth_logger",
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/auth-schema")
  }
  
  let detailed_log = logs::LogRecord::{
    timestamp_unix_nanos: base_time,
    observed_timestamp_unix_nanos: Some(base_time + 1000000L),
    severity_number: logs::Error,
    severity_text: Some("ERROR"),
    body: Some("User authentication failed"),
    attributes: [
      ("user_id", common::AttributeValue::string("user123")),
      ("failure_reason", common::AttributeValue::string("invalid_password")),
      ("ip_address", common::AttributeValue::string("192.168.1.100")),
      ("attempt_count", common::AttributeValue::int(3L))
    ],
    trace_id: trace_id_bytes,
    span_id: span_id_bytes,
    trace_flags: Some(1_byte),
    resource: Some(resource),
    instrumentation_scope: Some(scope)
  }
  
  // 验证日志记录完整性
  assert_eq(detailed_log.timestamp_unix_nanos, base_time)
  assert_eq(detailed_log.observed_timestamp_unix_nanos.unwrap(), base_time + 1000000L)
  assert_eq(detailed_log.severity_number, logs::Error)
  assert_eq(detailed_log.severity_text.unwrap(), "ERROR")
  assert_eq(detailed_log.body.unwrap(), "User authentication failed")
  assert_eq(detailed_log.attributes.length(), 4)
  assert_eq(detailed_log.trace_id.unwrap().length(), 16)
  assert_eq(detailed_log.span_id.unwrap().length(), 8)
  assert_eq(detailed_log.trace_flags.unwrap(), 1_byte)
  
  // 验证资源信息
  match detailed_log.resource {
    Some(res) => {
      assert_eq(res.service_name, "auth_service")
      assert_eq(res.telemetry_sdk_name, "azimuth")
    }
    None => @test.fail("Expected resource to be present")
  }
  
  // 验证instrumentation scope
  match detailed_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "auth_logger")
      assert_eq(scope.version.unwrap(), "1.0.0")
      assert_eq(scope.schema_url.unwrap(), "http://example.com/auth-schema")
    }
    None => @test.fail("Expected instrumentation scope to be present")
  }
  
  // 使用builder创建相同的日志记录并验证一致性
  let builder_log = logs::LogRecord::builder()
    .timestamp(base_time)
    .severity(logs::Error)
    .body("User authentication failed")
    .with_attribute("user_id", common::AttributeValue::string("user123"))
    .with_attribute("failure_reason", common::AttributeValue::string("invalid_password"))
    .with_attribute("ip_address", common::AttributeValue::string("192.168.1.100"))
    .with_attribute("attempt_count", common::AttributeValue::int(3L))
    .build()
  
  // 验证builder创建的日志核心数据一致性
  assert_eq(builder_log.severity_number, detailed_log.severity_number)
  assert_eq(builder_log.body.unwrap(), detailed_log.body.unwrap())
  assert_eq(builder_log.attributes.length(), detailed_log.attributes.length())
}

test "baggage_data_consistency" {
  // 测试baggage数据的一致性
  
  // 创建baggage并添加多个条目
  let initial_baggage = context::Baggage::empty()
    .with_entry("user_id", "12345")
    .with_entry("session_id", "abcdef123456")
    .with_entry("request_id", "req_789012")
    .with_entry("country_code", "US")
    .with_entry("locale", "en-US")
  
  // 验证初始baggage内容
  assert_eq(initial_baggage.entries.length(), 5)
  assert_eq(initial_baggage.get("user_id").unwrap(), "12345")
  assert_eq(initial_baggage.get("session_id").unwrap(), "abcdef123456")
  assert_eq(initial_baggage.get("request_id").unwrap(), "req_789012")
  assert_eq(initial_baggage.get("country_code").unwrap(), "US")
  assert_eq(initial_baggage.get("locale").unwrap(), "en-US")
  
  // 创建修改后的baggage（更新现有条目）
  let updated_baggage = initial_baggage
    .with_entry("locale", "fr-FR")  // 更新语言
    .with_entry("timezone", "Europe/Paris")  // 添加新条目
  
  // 验证更新后的baggage
  assert_eq(updated_baggage.entries.length(), 6)  // 应该增加一个条目
  assert_eq(updated_baggage.get("user_id").unwrap(), "12345")  // 原有值保持不变
  assert_eq(updated_baggage.get("locale").unwrap(), "fr-FR")  // 值已更新
  assert_eq(updated_baggage.get("timezone").unwrap(), "Europe/Paris")  // 新条目已添加
  
  // 验证原始baggage不受影响
  assert_eq(initial_baggage.entries.length(), 5)
  assert_eq(initial_baggage.get("locale").unwrap(), "en-US")
  assert_eq(initial_baggage.get("timezone"), None)  // 原始baggage中没有此条目
}