// 跨模块数据一致性测试用例
// 测试数据在不同遥测模块间的一致性

test "trace_log_consistency" {
  // 测试Trace和Logs模块间的数据一致性
  
  // 创建一个span
  let ctx = Context::empty()
  let span_attributes = [
    ("service.name", AttributeValue::string("consistency-service")),
    ("operation.name", AttributeValue::string("trace_log_test")),
    ("user.id", AttributeValue::string("user-12345")),
    ("request.id", AttributeValue::string("req-abcdef"))
  ]
  
  let (_, span) = NoopTracer::start_span(ctx, "consistency_test_span", Server, Some(span_attributes))
  
  // 创建与span关联的日志，确保数据一致性
  let log_attributes = [
    ("service.name", AttributeValue::string("consistency-service")),
    ("operation.name", AttributeValue::string("trace_log_test")),
    ("user.id", AttributeValue::string("user-12345")),
    ("request.id", AttributeValue::string("req-abcdef")),
    ("log.phase", AttributeValue::string("processing"))
  ]
  
  let consistency_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Processing operation with consistent data"),
    attributes: log_attributes,
    trace_id: Some(span.context.trace_id),
    span_id: Some(span.context.span_id),
    trace_flags: Some(span.context.trace_flags),
    resource: Some(Resource::default("consistency-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "consistency-logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证数据一致性
  // 1. 验证trace_id和span_id的一致性
  match consistency_log.trace_id {
    Some(log_trace_id) => {
      assert_eq(log_trace_id.length(), span.context.trace_id.length())
      let mut i = 0
      while i < log_trace_id.length() {
        assert_eq(log_trace_id[i], span.context.trace_id[i])
        i = i + 1
      }
    }
    None => @test.fail("Test failed")
  }
  
  match consistency_log.span_id {
    Some(log_span_id) => {
      assert_eq(log_span_id.length(), span.context.span_id.length())
      let mut i = 0
      while i < log_span_id.length() {
        assert_eq(log_span_id[i], span.context.span_id[i])
        i = i + 1
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 2. 验证属性的一致性
  assert_eq(span.attributes.length(), 4)
  assert_eq(consistency_log.attributes.length(), 5)
  
  // 验证共同属性的一致性
  let common_attributes = ["service.name", "operation.name", "user.id", "request.id"]
  let mut i = 0
  while i < common_attributes.length() {
    let attr_name = common_attributes[i]
    
    // 在span中查找属性
    let mut span_attr_value = None
    let mut span_i = 0
    while span_i < span.attributes.length() {
      let (name, value) = span.attributes[span_i]
      if name == attr_name {
        span_attr_value = Some(value)
        break
      }
      span_i = span_i + 1
    }
    
    // 在log中查找属性
    let mut log_attr_value = None
    let mut log_i = 0
    while log_i < consistency_log.attributes.length() {
      let (name, value) = consistency_log.attributes[log_i]
      if name == attr_name {
        log_attr_value = Some(value)
        break
      }
      log_i = log_i + 1
    }
    
    // 验证属性值一致
    match (span_attr_value, log_attr_value) {
      (Some(span_val), Some(log_val)) => {
        match (span_val, log_val) {
          (StringValue(s1), StringValue(s2)) => assert_eq(s1, s2)
          (IntValue(i1), IntValue(i2)) => assert_eq(i1, i2)
          (FloatValue(f1), FloatValue(f2)) => assert_eq(f1, f2)
          (BoolValue(b1), BoolValue(b2)) => assert_eq(b1, b2)
          (ArrayStringValue(a1), ArrayStringValue(a2)) => {
            assert_eq(a1.length(), a2.length())
            let mut arr_i = 0
            while arr_i < a1.length() {
              assert_eq(a1[arr_i], a2[arr_i])
              arr_i = arr_i + 1
            }
          }
          (ArrayIntValue(a1), ArrayIntValue(a2)) => {
            assert_eq(a1.length(), a2.length())
            let mut arr_i = 0
            while arr_i < a1.length() {
              assert_eq(a1[arr_i], a2[arr_i])
              arr_i = arr_i + 1
            }
          }
          (ArrayFloatValue(a1), ArrayFloatValue(a2)) => {
            assert_eq(a1.length(), a2.length())
            let mut arr_i = 0
            while arr_i < a1.length() {
              assert_eq(a1[arr_i], a2[arr_i])
              arr_i = arr_i + 1
            }
          }
          (ArrayBoolValue(a1), ArrayBoolValue(a2)) => {
            assert_eq(a1.length(), a2.length())
            let mut arr_i = 0
            while arr_i < a1.length() {
              assert_eq(a1[arr_i], a2[arr_i])
              arr_i = arr_i + 1
            }
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => @test.fail("Test failed")
    }
    
    i = i + 1
  }
  
  // 3. 验证resource和instrumentation scope的一致性
  match consistency_log.resource {
    Some(log_resource) => {
      assert_eq(log_resource.service_name, "consistency-service")
      assert_eq(log_resource.telemetry_sdk_name, "azimuth")
      assert_eq(log_resource.telemetry_sdk_version, "0.1.0")
    }
    None => @test.fail("Test failed")
  }
  
  match consistency_log.instrumentation_scope {
    Some(log_scope) => {
      assert_eq(log_scope.name, "consistency-logger")
      match log_scope.version {
        Some(version) => assert_eq(version, "1.0.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "context_baggage_consistency" {
  // 测试Context和Baggage模块间的数据一致性
  
  // 创建上下文并设置值
  let ctx = Context::empty()
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let request_key = create_key("request.id")
  
  let ctx_with_values = ctx
    .with_value(user_key, "user-12345")
    .with_value(session_key, "sess-abcdef")
    .with_value(request_key, "req-123456")
  
  // 创建baggage并设置对应的值
  let baggage = Baggage::empty()
    .with_entry("user.id", "user-12345")
    .with_entry("session.id", "sess-abcdef")
    .with_entry("request.id", "req-123456")
    .with_entry("user.role", "admin")
    .with_entry("request.source", "web")
  
  // 验证Context和Baggage间的数据一致性
  // 1. 验证共同键值对的一致性
  let common_keys = ["user.id", "session.id", "request.id"]
  let mut i = 0
  while i < common_keys.length() {
    let key_name = common_keys[i]
    
    // 从Context获取值
    let ctx_key = create_key(key_name)
    let ctx_value = ctx_with_values.get(ctx_key)
    
    // 从Baggage获取值
    let baggage_value = baggage.get(key_name)
    
    // 验证值一致
    match (ctx_value, baggage_value) {
      (Some(ctx_val), Some(baggage_val)) => assert_eq(ctx_val, baggage_val)
      _ => @test.fail("Test failed")
    }
    
    i = i + 1
  }
  
  // 2. 验证Baggage独有的键
  match baggage.get("user.role") {
    Some(role) => assert_eq(role, "admin")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("request.source") {
    Some(source) => assert_eq(source, "web")
    None => @test.fail("Test failed")
  }
  
  // 3. 创建带有Context和Baggage一致数据的span和log
  let span_attributes = [
    ("user.id", AttributeValue::string("user-12345")),
    ("session.id", AttributeValue::string("sess-abcdef")),
    ("request.id", AttributeValue::string("req-123456")),
    ("user.role", AttributeValue::string("admin")),
    ("request.source", AttributeValue::string("web"))
  ]
  
  let (_, ctx_span) = NoopTracer::start_span(ctx_with_values, "context_baggage_span", Server, Some(span_attributes))
  
  let ctx_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Context and baggage consistency test"),
    attributes: span_attributes,
    trace_id: Some(ctx_span.context.trace_id),
    span_id: Some(ctx_span.context.span_id),
    trace_flags: Some(ctx_span.context.trace_flags),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证span、log、context和baggage的数据一致性
  assert_eq(ctx_span.attributes.length(), 5)
  assert_eq(ctx_log.attributes.length(), 5)
  
  // 验证所有数据源中的user.id一致
  match ctx_with_values.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Test failed")
  }
  
  let mut found_user_id = false
  let mut i = 0
  while i < ctx_span.attributes.length() {
    let (name, value) = ctx_span.attributes[i]
    if name == "user.id" {
      match value {
        StringValue(user_id) => {
          assert_eq(user_id, "user-12345")
          found_user_id = true
        }
        _ => @test.fail("Test failed")
      }
      break
    }
    i = i + 1
  }
  assert_eq(found_user_id, true)
}

test "propagation_context_consistency" {
  // 测试Propagation和Context模块间的数据一致性
  
  // 创建初始上下文
  let ctx = Context::empty()
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  
  let initial_ctx = ctx
    .with_value(user_key, "user-67890")
    .with_value(session_key, "sess-123456")
  
  // 创建baggage
  let baggage = Baggage::empty()
    .with_entry("user.id", "user-67890")
    .with_entry("session.id", "sess-123456")
    .with_entry("user.role", "user")
    .with_entry("request.source", "mobile")
  
  // 创建carrier
  let carrier = MapCarrier::new()
  
  // 使用propagator注入上下文
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  composite_propagator.inject(initial_ctx, carrier)
  
  // 从carrier提取上下文
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证Propagation和Context间的数据一致性
  // 1. 验证初始上下文和提取上下文的trace信息
  let (_, initial_span) = NoopTracer::start_span(initial_ctx, "initial_span", Server, None)
  let (_, extracted_span) = NoopTracer::start_span(extracted_ctx, "extracted_span", Server, None)
  
  // 2. 验证carrier中的数据
  match carrier.get("traceparent") {
    Some(trace_parent) => assert_eq(trace_parent.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get("baggage") {
    Some(baggage_header) => assert_eq(baggage_header.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  // 3. 创建与传播上下文一致的span和log
  let propagation_attributes = [
    ("user.id", AttributeValue::string("user-67890")),
    ("session.id", AttributeValue::string("sess-123456")),
    ("user.role", AttributeValue::string("user")),
    ("request.source", AttributeValue::string("mobile")),
    ("propagation.test", AttributeValue::bool(true))
  ]
  
  let (_, propagation_span) = NoopTracer::start_span(extracted_ctx, "propagation_span", Client, Some(propagation_attributes))
  
  let propagation_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Propagation and context consistency test"),
    attributes: propagation_attributes,
    trace_id: Some(propagation_span.context.trace_id),
    span_id: Some(propagation_span.context.span_id),
    trace_flags: Some(propagation_span.context.trace_flags),
    resource: Some(Resource::default("propagation-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "propagation-scope",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证传播数据的一致性
  assert_eq(propagation_span.attributes.length(), 5)
  assert_eq(propagation_log.attributes.length(), 5)
  
  // 验证所有模块中的user.id一致
  match initial_ctx.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user-67890")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-67890")
    None => @test.fail("Test failed")
  }
  
  // 验证span和log中的trace_id一致
  match propagation_log.trace_id {
    Some(log_trace_id) => {
      let mut i = 0
      while i < log_trace_id.length() {
        assert_eq(log_trace_id[i], propagation_span.context.trace_id[i])
        i = i + 1
      }
    }
    None => @test.fail("Test failed")
  }
}

test "metrics_traces_consistency" {
  // 测试Metrics和Traces模块间的数据一致性
  
  // 创建span
  let ctx = Context::empty()
  let span_attributes = [
    ("service.name", AttributeValue::string("metrics-service")),
    ("operation.name", AttributeValue::string("db_query")),
    ("db.type", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT")),
    ("user.id", AttributeValue::string("user-11111"))
  ]
  
  let (_, db_span) = NoopTracer::start_span(ctx, "db_query_span", Server, Some(span_attributes))
  
  // 创建与span属性一致的指标
  let meter = NoopMeter::{}
  let query_counter = meter.create_counter("db_queries_total", Some("queries"), Some("Total database queries"))
  let duration_histogram = meter.create_histogram("db_query_duration_ms", Some("ms"), Some("Database query duration"))
  let connection_gauge = meter.create_gauge("db_connections_active", Some("connections"), Some("Active database connections"))
  
  // 记录与span属性一致的指标
  let metrics_attributes = [
    ("service.name", AttributeValue::string("metrics-service")),
    ("operation.name", AttributeValue::string("db_query")),
    ("db.type", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT")),
    ("user.id", AttributeValue::string("user-11111")),
    ("status", AttributeValue::string("success"))
  ]
  
  query_counter.add(1L, Some(metrics_attributes))
  duration_histogram.record(125.5, Some(metrics_attributes))
  connection_gauge(25.0, Some([
    ("service.name", AttributeValue::string("metrics-service")),
    ("db.type", AttributeValue::string("postgresql"))
  ]))
  
  // 创建与span和metrics属性一致的日志
  let consistent_attributes = [
    ("service.name", AttributeValue::string("metrics-service")),
    ("operation.name", AttributeValue::string("db_query")),
    ("db.type", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT")),
    ("user.id", AttributeValue::string("user-11111")),
    ("status", AttributeValue::string("success")),
    ("duration.ms", AttributeValue::int(125L)),
    ("connections.active", AttributeValue::int(25L))
  ]
  
  let metrics_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Database query completed successfully"),
    attributes: consistent_attributes,
    trace_id: Some(db_span.context.trace_id),
    span_id: Some(db_span.context.span_id),
    trace_flags: Some(db_span.context.trace_flags),
    resource: Some(Resource::default("metrics-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "metrics-scope",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证Metrics、Traces和Logs间的数据一致性
  // 1. 验证共同属性的一致性
  let common_attributes = ["service.name", "operation.name", "db.type", "db.operation", "user.id"]
  let mut i = 0
  while i < common_attributes.length() {
    let attr_name = common_attributes[i]
    
    // 在span中查找属性
    let mut span_attr_value = None
    let mut span_i = 0
    while span_i < db_span.attributes.length() {
      let (name, value) = db_span.attributes[span_i]
      if name == attr_name {
        span_attr_value = Some(value)
        break
      }
      span_i = span_i + 1
    }
    
    // 在log中查找属性
    let mut log_attr_value = None
    let mut log_i = 0
    while log_i < metrics_log.attributes.length() {
      let (name, value) = metrics_log.attributes[log_i]
      if name == attr_name {
        log_attr_value = Some(value)
        break
      }
      log_i = log_i + 1
    }
    
    // 验证属性值一致
    match (span_attr_value, log_attr_value) {
      (Some(span_val), Some(log_val)) => {
        match (span_val, log_val) {
          (StringValue(s1), StringValue(s2)) => assert_eq(s1, s2)
          _ => @test.fail("Test failed")
        }
      }
      _ => @test.fail("Test failed")
    }
    
    i = i + 1
  }
  
  // 2. 验证trace_id的一致性
  match metrics_log.trace_id {
    Some(log_trace_id) => {
      let mut i = 0
      while i < log_trace_id.length() {
        assert_eq(log_trace_id[i], db_span.context.trace_id[i])
        i = i + 1
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 3. 验证resource和instrumentation scope的一致性
  match metrics_log.resource {
    Some(log_resource) => {
      assert_eq(log_resource.service_name, "metrics-service")
    }
    None => @test.fail("Test failed")
  }
  
  match metrics_log.instrumentation_scope {
    Some(log_scope) => {
      assert_eq(log_scope.name, "metrics-scope")
    }
    None => @test.fail("Test failed")
  }
}

test "resource_instrumentation_consistency" {
  // 测试Resource和InstrumentationScope在不同模块间的数据一致性
  
  // 创建统一的resource和instrumentation scope
  let resource = Resource::default("consistency-unified-service")
  let instrumentation_scope = InstrumentationScope::{
    name: "consistency-unified-scope",
    version: Some("2.1.0"),
    schema_url: Some("https://example.com/schema/v2")
  }
  
  // 创建带有统一resource和scope的span
  let ctx = Context::empty()
  let span_attributes = [
    ("service.name", AttributeValue::string("consistency-unified-service")),
    ("scope.name", AttributeValue::string("consistency-unified-scope")),
    ("scope.version", AttributeValue::string("2.1.0"))
  ]
  
  let (_, unified_span) = NoopTracer::start_span(ctx, "unified_span", Server, Some(span_attributes))
  
  // 创建带有统一resource和scope的log
  let unified_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Unified resource and instrumentation scope test"),
    attributes: span_attributes,
    trace_id: Some(unified_span.context.trace_id),
    span_id: Some(unified_span.context.span_id),
    trace_flags: Some(unified_span.context.trace_flags),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 创建带有统一resource的指标
  let meter = NoopMeter::{}
  let unified_counter = meter.create_counter("unified_operations", Some("operations"), Some("Unified operations"))
  
  unified_counter.add(1L, Some([
    ("service.name", AttributeValue::string("consistency-unified-service")),
    ("scope.name", AttributeValue::string("consistency-unified-scope")),
    ("scope.version", AttributeValue::string("2.1.0"))
  ]))
  
  // 验证Resource和InstrumentationScope的一致性
  // 1. 验证resource信息
  match unified_log.resource {
    Some(log_resource) => {
      assert_eq(log_resource.service_name, resource.service_name)
      assert_eq(log_resource.telemetry_sdk_name, resource.telemetry_sdk_name)
      assert_eq(log_resource.telemetry_sdk_version, resource.telemetry_sdk_version)
    }
    None => @test.fail("Test failed")
  }
  
  // 2. 验证instrumentation scope信息
  match unified_log.instrumentation_scope {
    Some(log_scope) => {
      assert_eq(log_scope.name, instrumentation_scope.name)
      match log_scope.version {
        Some(version) => {
          match instrumentation_scope.version {
            Some(orig_version) => assert_eq(version, orig_version)
            None => @test.fail("Test failed")
          }
        }
        None => @test.fail("Test failed")
      }
      match log_scope.schema_url {
        Some(schema) => {
          match instrumentation_scope.schema_url {
            Some(orig_schema) => assert_eq(schema, orig_schema)
            None => @test.fail("Test failed")
          }
        }
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 3. 验证span和log属性中的resource和scope信息
  let mut found_service_name = false
  let mut found_scope_name = false
  let mut found_scope_version = false
  
  let mut i = 0
  while i < unified_span.attributes.length() {
    let (name, value) = unified_span.attributes[i]
    match name {
      "service.name" => {
        match value {
          StringValue(service_name) => {
            assert_eq(service_name, "consistency-unified-service")
            found_service_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "scope.name" => {
        match value {
          StringValue(scope_name) => {
            assert_eq(scope_name, "consistency-unified-scope")
            found_scope_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "scope.version" => {
        match value {
          StringValue(scope_version) => {
            assert_eq(scope_version, "2.1.0")
            found_scope_version = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_scope_name, true)
  assert_eq(found_scope_version, true)
  
  // 4. 验证log属性中的resource和scope信息
  found_service_name = false
  found_scope_name = false
  found_scope_version = false
  
  i = 0
  while i < unified_log.attributes.length() {
    let (name, value) = unified_log.attributes[i]
    match name {
      "service.name" => {
        match value {
          StringValue(service_name) => {
            assert_eq(service_name, "consistency-unified-service")
            found_service_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "scope.name" => {
        match value {
          StringValue(scope_name) => {
            assert_eq(scope_name, "consistency-unified-scope")
            found_scope_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "scope.version" => {
        match value {
          StringValue(scope_version) => {
            assert_eq(scope_version, "2.1.0")
            found_scope_version = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_scope_name, true)
  assert_eq(found_scope_version, true)
}

test "end_to_end_data_consistency" {
  // 端到端数据一致性测试，验证所有模块间的数据一致性
  
  // 1. 创建统一的上下文和baggage
  let ctx = Context::empty()
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let request_key = create_key("request.id")
  
  let unified_ctx = ctx
    .with_value(user_key, "user-99999")
    .with_value(session_key, "sess-88888")
    .with_value(request_key, "req-77777")
  
  let unified_baggage = Baggage::empty()
    .with_entry("user.id", "user-99999")
    .with_entry("session.id", "sess-88888")
    .with_entry("request.id", "req-77777")
    .with_entry("user.role", "premium")
    .with_entry("request.source", "api")
    .with_entry("api.version", "v2")
  
  // 2. 创建统一的resource和instrumentation scope
  let unified_resource = Resource::default("end-to-end-service")
  let unified_scope = InstrumentationScope::{
    name: "end-to-end-scope",
    version: Some("3.0.0"),
    schema_url: Some("https://example.com/end-to-end/v3")
  }
  
  // 3. 创建统一的属性集合
  let unified_attributes = [
    ("service.name", AttributeValue::string("end-to-end-service")),
    ("service.version", AttributeValue::string("3.0.0")),
    ("scope.name", AttributeValue::string("end-to-end-scope")),
    ("scope.version", AttributeValue::string("3.0.0")),
    ("user.id", AttributeValue::string("user-99999")),
    ("session.id", AttributeValue::string("sess-88888")),
    ("request.id", AttributeValue::string("req-77777")),
    ("user.role", AttributeValue::string("premium")),
    ("request.source", AttributeValue::string("api")),
    ("api.version", AttributeValue::string("v2")),
    ("operation.type", AttributeValue::string("end_to_end_test")),
    ("operation.complexity", AttributeValue::string("high")),
    ("tags", AttributeValue::array_string(["e2e", "consistency", "test"])),
    ("status.codes", AttributeValue::array_int([200L, 201L, 400L])),
    ("latency.thresholds", AttributeValue::array_float([0.1, 0.5, 1.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 4. 创建带有统一属性的span
  let (_, e2e_span) = NoopTracer::start_span(unified_ctx, "end_to_end_span", Server, Some(unified_attributes))
  
  // 5. 创建带有统一属性的日志
  let e2e_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("End-to-end data consistency test"),
    attributes: unified_attributes,
    trace_id: Some(e2e_span.context.trace_id),
    span_id: Some(e2e_span.context.span_id),
    trace_flags: Some(e2e_span.context.trace_flags),
    resource: Some(unified_resource),
    instrumentation_scope: Some(unified_scope)
  }
  
  // 6. 创建带有统一属性的指标
  let meter = NoopMeter::{}
  let e2e_counter = meter.create_counter("e2e_operations", Some("operations"), Some("End-to-end operations"))
  let e2e_histogram = meter.create_histogram("e2e_duration_ms", Some("ms"), Some("End-to-end duration"))
  let e2e_gauge = meter.create_gauge("e2e_active", Some("operations"), Some("Active end-to-end operations"))
  
  e2e_counter.add(1L, Some(unified_attributes))
  e2e_histogram.record(500.0, Some(unified_attributes))
  e2e_gauge(10.0, Some([
    ("service.name", AttributeValue::string("end-to-end-service")),
    ("scope.name", AttributeValue::string("end-to-end-scope"))
  ]))
  
  // 7. 创建span事件
  let e2e_events = [
    SpanEvent::{
      name: "operation_started",
      timestamp_unix_nanos: 1640995200000000000L,
      attributes: [
        ("event.phase", AttributeValue::string("start")),
        ("service.name", AttributeValue::string("end-to-end-service")),
        ("user.id", AttributeValue::string("user-99999"))
      ]
    },
    SpanEvent::{
      name: "operation_completed",
      timestamp_unix_nanos: 1640995200000000500L,
      attributes: [
        ("event.phase", AttributeValue::string("complete")),
        ("service.name", AttributeValue::string("end-to-end-service")),
        ("user.id", AttributeValue::string("user-99999")),
        ("duration.ms", AttributeValue::int(500L))
      ]
    }
  ]
  
  // 8. 创建span链接
  let e2e_links = [
    SpanLink::{
      context: SpanContext::{
        trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x11_byte }),
        span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x22_byte }),
        trace_flags: 1_byte,
        trace_state: ""
      },
      attributes: [
        ("link.type", AttributeValue::string("parent")),
        ("service.name", AttributeValue::string("upstream-service"))
      ]
    }
  ]
  
  // 9. 验证端到端数据一致性
  // 验证Context和Baggage的一致性
  match unified_ctx.get(user_key) {
    Some(ctx_user_id) => {
      match unified_baggage.get("user.id") {
        Some(baggage_user_id) => assert_eq(ctx_user_id, baggage_user_id)
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 验证Span和Log的trace_id一致性
  match e2e_log.trace_id {
    Some(log_trace_id) => {
      let mut i = 0
      while i < log_trace_id.length() {
        assert_eq(log_trace_id[i], e2e_span.context.trace_id[i])
        i = i + 1
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 验证Span和Log的属性一致性
  assert_eq(e2e_span.attributes.length(), unified_attributes.length())
  assert_eq(e2e_log.attributes.length(), unified_attributes.length())
  
  // 验证Resource和InstrumentationScope的一致性
  match e2e_log.resource {
    Some(log_resource) => {
      assert_eq(log_resource.service_name, unified_resource.service_name)
      assert_eq(log_resource.telemetry_sdk_name, unified_resource.telemetry_sdk_name)
    }
    None => @test.fail("Test failed")
  }
  
  match e2e_log.instrumentation_scope {
    Some(log_scope) => {
      assert_eq(log_scope.name, unified_scope.name)
      match log_scope.version {
        Some(version) => {
          match unified_scope.version {
            Some(orig_version) => assert_eq(version, orig_version)
            None => @test.fail("Test failed")
          }
        }
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 验证事件属性的一致性
  let mut i = 0
  while i < e2e_events.length() {
    let event = e2e_events[i]
    let mut found_service_name = false
    let mut found_user_id = false
    
    let mut j = 0
    while j < event.attributes.length() {
      let (name, value) = event.attributes[j]
      match name {
        "service.name" => {
          match value {
            StringValue(service_name) => {
              assert_eq(service_name, "end-to-end-service")
              found_service_name = true
            }
            _ => @test.fail("Test failed")
          }
        }
        "user.id" => {
          match value {
            StringValue(user_id) => {
              assert_eq(user_id, "user-99999")
              found_user_id = true
            }
            _ => @test.fail("Test failed")
          }
        }
        _ => ()
      }
      j = j + 1
    }
    
    assert_eq(found_service_name, true)
    assert_eq(found_user_id, true)
    i = i + 1
  }
  
  // 验证链接属性的一致性
  let mut found_service_name = false
  let mut found_link_type = false
  
  let mut i = 0
  while i < e2e_links.length() {
    let link = e2e_links[i]
    let mut j = 0
    while j < link.attributes.length() {
      let (name, value) = link.attributes[j]
      match name {
        "link.type" => {
          match value {
            StringValue(link_type) => {
              assert_eq(link_type, "parent")
              found_link_type = true
            }
            _ => @test.fail("Test failed")
          }
        }
        "service.name" => {
          match value {
            StringValue(service_name) => {
              assert_eq(service_name, "upstream-service")
              found_service_name = true
            }
            _ => @test.fail("Test failed")
          }
        }
        _ => ()
      }
      j = j + 1
    }
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_link_type, true)
}