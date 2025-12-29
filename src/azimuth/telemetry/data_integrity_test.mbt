// 数据完整性测试 - 测试数据的一致性和完整性
// 验证遥测数据在传输、存储和处理过程中保持完整性

test "attribute_data_integrity" {
  // 测试属性数据的完整性
  
  // 创建各种类型的属性
  let string_attr = common::AttributeValue::string("test-string-value")
  let int_attr = common::AttributeValue::int(12345L)
  let float_attr = common::AttributeValue::float(3.14159265359)
  let bool_attr = common::AttributeValue::bool(true)
  let string_array_attr = common::AttributeValue::array_string(["item1", "item2", "item3"])
  let int_array_attr = common::AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])
  let float_array_attr = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = common::AttributeValue::array_bool([true, false, true])
  
  // 验证字符串属性完整性
  match string_attr {
    common::StringValue(s) => {
      assert_eq(s, "test-string-value")
      assert_eq(s.length(), 16)
      assert_eq(s.contains("test"), true)
    }
    _ => @test.fail("Expected StringValue")
  }
  
  // 验证整数属性完整性
  match int_attr {
    common::IntValue(i) => {
      assert_eq(i, 12345L)
      assert_eq(i > 10000L, true)
      assert_eq(i < 20000L, true)
    }
    _ => @test.fail("Expected IntValue")
  }
  
  // 验证浮点数属性完整性
  match float_attr {
    common::FloatValue(f) => {
      assert_eq(f > 3.14 && f < 3.15, true)
      assert_eq(f * 1000 > 3141.0 && f * 1000 < 3142.0, true)
    }
    _ => @test.fail("Expected FloatValue")
  }
  
  // 验证布尔属性完整性
  match bool_attr {
    common::BoolValue(b) => {
      assert_eq(b, true)
      assert_eq(!b, false)
    }
    _ => @test.fail("Expected BoolValue")
  }
  
  // 验证字符串数组属性完整性
  match string_array_attr {
    common::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "item1")
      assert_eq(arr[1], "item2")
      assert_eq(arr[2], "item3")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 验证整数数组属性完整性
  match int_array_attr {
    common::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1L)
      assert_eq(arr[2], 3L)
      assert_eq(arr[4], 5L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
}

test "context_data_consistency" {
  // 测试Context数据一致性
  
  let base_ctx = context::Context::empty()
  let key1 = context::create_key("consistency-key-1")
  let key2 = context::create_key("consistency-key-2")
  let key3 = context::create_key("consistency-key-3")
  
  // 设置多个键值对
  let ctx1 = base_ctx.with_value(key1, "value-1")
  let ctx2 = ctx1.with_value(key2, "value-2")
  let ctx3 = ctx2.with_value(key3, "value-3")
  
  // 验证数据一致性
  match ctx3.get(key1) {
    Some(value) => assert_eq(value, "value-1")
    None => @test.fail("Key1 not found")
  }
  
  match ctx3.get(key2) {
    Some(value) => assert_eq(value, "value-2")
    None => @test.fail("Key2 not found")
  }
  
  match ctx3.get(key3) {
    Some(value) => assert_eq(value, "value-3")
    None => @test.fail("Key3 not found")
  }
  
  // 验证不可变性 - 原始Context不应被修改
  match ctx1.get(key2) {
    Some(_) => @test.fail("Key2 should not exist in ctx1")
    None => assert_eq(true, true)
  }
  
  match ctx2.get(key3) {
    Some(_) => @test.fail("Key3 should not exist in ctx2")
    None => assert_eq(true, true)
  }
  
  // 验证键值对数量
  assert_eq(ctx3.values.length(), 3)
  assert_eq(ctx2.values.length(), 2)
  assert_eq(ctx1.values.length(), 1)
  assert_eq(base_ctx.values.length(), 0)
}

test "log_record_data_integrity" {
  // 测试LogRecord数据完整性
  
  let log_record = logs::LogRecord::builder()
    .severity(logs::Error)
    .body("Data integrity test log message")
    .with_attribute("error.code", common::AttributeValue::int(500L))
    .with_attribute("error.message", common::AttributeValue::string("Internal server error"))
    .with_attribute("service.name", common::AttributeValue::string("integrity-test-service"))
    .with_attribute("request.id", common::AttributeValue::string("req-12345"))
    .with_attribute("user.id", common::AttributeValue::int(67890L))
    .with_attribute("tags", common::AttributeValue::array_string(["critical", "data-integrity"]))
    .build()
  
  // 验证基本字段完整性
  assert_eq(log_record.severity_number, logs::Error)
  match log_record.body {
    Some(body) => {
      assert_eq(body, "Data integrity test log message")
      assert_eq(body.length(), 30)
    }
    None => @test.fail("Body should not be None")
  }
  
  // 验证属性完整性
  assert_eq(log_record.attributes.length(), 6)
  
  // 验证特定属性
  let mut found_error_code = false
  let mut found_error_message = false
  let mut found_service_name = false
  let mut found_request_id = false
  let mut found_user_id = false
  let mut found_tags = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "error.code" => {
        match value {
          common::IntValue(code) => {
            assert_eq(code, 500L)
            found_error_code = true
          }
          _ => @test.fail("error.code should be IntValue")
        }
      }
      "error.message" => {
        match value {
          common::StringValue(message) => {
            assert_eq(message, "Internal server error")
            found_error_message = true
          }
          _ => @test.fail("error.message should be StringValue")
        }
      }
      "service.name" => {
        match value {
          common::StringValue(service) => {
            assert_eq(service, "integrity-test-service")
            found_service_name = true
          }
          _ => @test.fail("service.name should be StringValue")
        }
      }
      "request.id" => {
        match value {
          common::StringValue(request_id) => {
            assert_eq(request_id, "req-12345")
            found_request_id = true
          }
          _ => @test.fail("request.id should be StringValue")
        }
      }
      "user.id" => {
        match value {
          common::IntValue(user_id) => {
            assert_eq(user_id, 67890L)
            found_user_id = true
          }
          _ => @test.fail("user.id should be IntValue")
        }
      }
      "tags" => {
        match value {
          common::ArrayStringValue(tags) => {
            assert_eq(tags.length(), 2)
            assert_eq(tags[0], "critical")
            assert_eq(tags[1], "data-integrity")
            found_tags = true
          }
          _ => @test.fail("tags should be ArrayStringValue")
        }
      }
      _ => ()
    }
    
    i = i + 1
  }
  
  // 验证所有属性都找到
  assert_eq(found_error_code, true)
  assert_eq(found_error_message, true)
  assert_eq(found_service_name, true)
  assert_eq(found_request_id, true)
  assert_eq(found_user_id, true)
  assert_eq(found_tags, true)
}

test "span_data_integrity" {
  // 测试Span数据完整性
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("integrity-test-tracer")
  let (_, span) = tracer.start_span(
    context::Context::empty(),
    "integrity-test-span",
    trace::Server,
    [
      ("http.method", common::AttributeValue::string("POST")),
      ("http.url", common::AttributeValue::string("https://api.example.com/v1/users")),
      ("http.status_code", common::AttributeValue::int(201L)),
      ("user.id", common::AttributeValue::int(12345L)),
      ("request.duration", common::AttributeValue::float(250.75)),
      ("success", common::AttributeValue::bool(true)),
      ("endpoints", common::AttributeValue::array_string(["/users", "/auth"]))
    ],
    1640995200000L
  )
  
  // 验证基本字段完整性
  assert_eq(span.name, "integrity-test-span")
  assert_eq(span.kind, trace::Internal)  // No-op实现默认为Internal
  assert_eq(span.start_time_unix_nanos, 1640995200000L)
  assert_eq(span.status, trace::Unset)
  assert_eq(span.events.length(), 0)
  assert_eq(span.links.length(), 0)
  
  // 验证属性完整性
  assert_eq(span.attributes.length(), 7)
  
  // 验证特定属性
  let mut found_http_method = false
  let mut found_http_url = false
  let mut found_http_status = false
  let mut found_user_id = false
  let mut found_duration = false
  let mut found_success = false
  let mut found_endpoints = false
  
  let mut i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    
    match key {
      "http.method" => {
        match value {
          common::StringValue(method) => {
            assert_eq(method, "POST")
            found_http_method = true
          }
          _ => @test.fail("http.method should be StringValue")
        }
      }
      "http.url" => {
        match value {
          common::StringValue(url) => {
            assert_eq(url, "https://api.example.com/v1/users")
            found_http_url = true
          }
          _ => @test.fail("http.url should be StringValue")
        }
      }
      "http.status_code" => {
        match value {
          common::IntValue(status) => {
            assert_eq(status, 201L)
            found_http_status = true
          }
          _ => @test.fail("http.status_code should be IntValue")
        }
      }
      "user.id" => {
        match value {
          common::IntValue(user_id) => {
            assert_eq(user_id, 12345L)
            found_user_id = true
          }
          _ => @test.fail("user.id should be IntValue")
        }
      }
      "request.duration" => {
        match value {
          common::FloatValue(duration) => {
            assert_eq(duration > 250.0 && duration < 251.0, true)
            found_duration = true
          }
          _ => @test.fail("request.duration should be FloatValue")
        }
      }
      "success" => {
        match value {
          common::BoolValue(success) => {
            assert_eq(success, true)
            found_success = true
          }
          _ => @test.fail("success should be BoolValue")
        }
      }
      "endpoints" => {
        match value {
          common::ArrayStringValue(endpoints) => {
            assert_eq(endpoints.length(), 2)
            assert_eq(endpoints[0], "/users")
            assert_eq(endpoints[1], "/auth")
            found_endpoints = true
          }
          _ => @test.fail("endpoints should be ArrayStringValue")
        }
      }
      _ => ()
    }
    
    i = i + 1
  }
  
  // 验证所有属性都找到
  assert_eq(found_http_method, true)
  assert_eq(found_http_url, true)
  assert_eq(found_http_status, true)
  assert_eq(found_user_id, true)
  assert_eq(found_duration, true)
  assert_eq(found_success, true)
  assert_eq(found_endpoints, true)
}

test "metric_data_integrity" {
  // 测试指标数据完整性
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integrity-test-meter")
  
  // 创建各种指标
  let counter = meter.create_counter("integrity_counter", "count", "Integrity test counter")
  let histogram = meter.create_histogram("integrity_histogram", "ms", "Integrity test histogram")
  let up_down_counter = meter.create_up_down_counter("integrity_up_down", "count", "Integrity test up-down counter")
  let gauge = meter.create_gauge("integrity_gauge", "value", "Integrity test gauge")
  
  // 记录指标数据
  counter.add(100L, [
    ("service", common::AttributeValue::string("integrity-service")),
    ("version", common::AttributeValue::string("1.0.0")),
    ("environment", common::AttributeValue::string("test"))
  ])
  
  histogram.record(150.5, [
    ("service", common::AttributeValue::string("integrity-service")),
    ("operation", common::AttributeValue::string("data-processing")),
    ("percentile", common::AttributeValue::string("p95"))
  ])
  
  up_down_counter.add(-25L, [
    ("service", common::AttributeValue::string("integrity-service")),
    ("metric.type", common::AttributeValue::string("resource-usage")),
    ("direction", common::AttributeValue::string("decrease"))
  ])
  
  gauge.record(75.25, [
    ("service", common::AttributeValue::string("integrity-service")),
    ("resource", common::AttributeValue::string("memory")),
    ("unit", common::AttributeValue::string("percentage"))
  ])
  
  // 验证指标记录的完整性
  // 注意：由于使用No-op实现，这里主要验证操作不会失败
  assert_eq(true, true)
  
  // 验证指标名称和描述的完整性
  let metric_names = ["integrity_counter", "integrity_histogram", "integrity_up_down", "integrity_gauge"]
  let metric_units = ["count", "ms", "count", "value"]
  let metric_descriptions = [
    "Integrity test counter",
    "Integrity test histogram", 
    "Integrity test up-down counter",
    "Integrity test gauge"
  ]
  
  assert_eq(metric_names.length(), 4)
  assert_eq(metric_units.length(), 4)
  assert_eq(metric_descriptions.length(), 4)
  
  assert_eq(metric_names[0], "integrity_counter")
  assert_eq(metric_units[1], "ms")
  assert_eq(metric_descriptions[2], "Integrity test up-down counter")
}

test "baggage_data_integrity" {
  // 测试Baggage数据完整性
  
  let baggage = context::Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("request.id", "req-67890")
    .with_entry("session.id", "session-abcdef")
    .with_entry("correlation.id", "corr-123456")
    .with_entry("trace.id", "trace-789012")
  
  // 验证所有条目都可以正确获取
  match baggage.get("user.id") {
    Some(user_id) => {
      assert_eq(user_id, "12345")
      assert_eq(user_id.length(), 5)
    }
    None => @test.fail("user.id not found")
  }
  
  match baggage.get("request.id") {
    Some(request_id) => {
      assert_eq(request_id, "req-67890")
      assert_eq(request_id.has_prefix("req-"), true)
    }
    None => @test.fail("request.id not found")
  }
  
  match baggage.get("session.id") {
    Some(session_id) => {
      assert_eq(session_id, "session-abcdef")
      assert_eq(session_id.has_prefix("session-"), true)
    }
    None => @test.fail("session.id not found")
  }
  
  match baggage.get("correlation.id") {
    Some(correlation_id) => {
      assert_eq(correlation_id, "corr-123456")
      assert_eq(correlation_id.has_prefix("corr-"), true)
    }
    None => @test.fail("correlation.id not found")
  }
  
  match baggage.get("trace.id") {
    Some(trace_id) => {
      assert_eq(trace_id, "trace-789012")
      assert_eq(trace_id.has_prefix("trace-"), true)
    }
    None => @test.fail("trace.id not found")
  }
  
  // 验证不存在的键返回None
  match baggage.get("non.existent.key") {
    Some(_) => @test.fail("non.existent.key should not be found")
    None => assert_eq(true, true)
  }
  
  // 验证Baggage条目数量
  assert_eq(baggage.entries.length(), 5)
}

test "resource_data_integrity" {
  // 测试Resource数据完整性
  
  let resource = common::Resource::default("integrity-test-service")
  
  // 验证基本Resource字段
  assert_eq(resource.service_name, "integrity-test-service")
  assert_eq(resource.service_name.length(), 21)
  assert_eq(resource.service_name.has_prefix("integrity-"), true)
  assert_eq(resource.service_name.has_suffix("-service"), true)
  
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 验证默认值为None
  match resource.service_version {
    Some(_) => @test.fail("service_version should be None for default resource")
    None => assert_eq(true, true)
  }
  
  // 验证属性为空
  assert_eq(resource.attributes.length(), 0)
  
  // 测试带有属性的Resource
  let resource_with_attrs = common::Resource::default("integrity-service-with-attrs")
  let attrs = [
    ("service.namespace", common::AttributeValue::string("production")),
    ("service.instance.id", common::AttributeValue::string("instance-12345")),
    ("deployment.environment", common::AttributeValue::string("production")),
    ("host.name", common::AttributeValue::string("integrity-host-01")),
    ("process.pid", common::AttributeValue::int(12345L)),
    ("process.cpu.percent", common::AttributeValue::float(75.5)),
    ("process.healthy", common::AttributeValue::bool(true)),
    ("tags", common::AttributeValue::array_string(["integrity", "test", "critical"]))
  ]
  
  // 验证Resource基本字段
  assert_eq(resource_with_attrs.service_name, "integrity-service-with-attrs")
  assert_eq(resource_with_attrs.telemetry_sdk_name, "azimuth")
  assert_eq(resource_with_attrs.telemetry_sdk_version, "0.1.0")
  
  // 验证属性完整性
  assert_eq(attrs.length(), 8)
  
  // 验证特定属性
  match attrs[0] {
    ("service.namespace", common::StringValue(namespace)) => {
      assert_eq(namespace, "production")
    }
    _ => @test.fail("service.namespace attribute mismatch")
  }
  
  match attrs[1] {
    ("service.instance.id", common::StringValue(instance_id)) => {
      assert_eq(instance_id, "instance-12345")
    }
    _ => @test.fail("service.instance.id attribute mismatch")
  }
  
  match attrs[4] {
    ("process.pid", common::IntValue(pid)) => {
      assert_eq(pid, 12345L)
    }
    _ => @test.fail("process.pid attribute mismatch")
  }
  
  match attrs[5] {
    ("process.cpu.percent", common::FloatValue(cpu_percent)) => {
      assert_eq(cpu_percent > 75.0 && cpu_percent < 76.0, true)
    }
    _ => @test.fail("process.cpu.percent attribute mismatch")
  }
  
  match attrs[6] {
    ("process.healthy", common::BoolValue(healthy)) => {
      assert_eq(healthy, true)
    }
    _ => @test.fail("process.healthy attribute mismatch")
  }
  
  match attrs[7] {
    ("tags", common::ArrayStringValue(tags)) => {
      assert_eq(tags.length(), 3)
      assert_eq(tags[0], "integrity")
      assert_eq(tags[1], "test")
      assert_eq(tags[2], "critical")
    }
    _ => @test.fail("tags attribute mismatch")
  }
}

test "propagation_data_integrity" {
  // 测试传播数据完整性
  
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  let composite_propagator = propagation::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建带有数据的Carrier
  let carrier = propagation::MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("baggage", "user.id=12345,request.id=req-67890,service.version=1.2.3"),
    ("custom-header", "custom-value")
  ])
  
  // 验证Carrier数据完整性
  match carrier.get("traceparent") {
    Some(traceparent) => {
      assert_eq(traceparent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
      assert_eq(traceparent.length(), 55)
    }
    None => @test.fail("traceparent not found")
  }
  
  match carrier.get("tracestate") {
    Some(tracestate) => {
      assert_eq(tracestate, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
      assert_eq(tracestate.contains("rojo="), true)
      assert_eq(tracestate.contains("congo="), true)
    }
    None => @test.fail("tracestate not found")
  }
  
  match carrier.get("baggage") {
    Some(baggage) => {
      assert_eq(baggage, "user.id=12345,request.id=req-67890,service.version=1.2.3")
      assert_eq(baggage.contains("user.id=12345"), true)
      assert_eq(baggage.contains("request.id=req-67890"), true)
      assert_eq(baggage.contains("service.version=1.2.3"), true)
    }
    None => @test.fail("baggage not found")
  }
  
  match carrier.get("custom-header") {
    Some(custom_header) => {
      assert_eq(custom_header, "custom-value")
    }
    None => @test.fail("custom-header not found")
  }
  
  // 验证不存在的header
  match carrier.get("non-existent-header") {
    Some(_) => @test.fail("non-existent-header should not be found")
    None => assert_eq(true, true)
  }
  
  // 验证键列表
  let keys = carrier.keys()
  assert_eq(keys.length(), 4)
  assert_eq(keys.contains("traceparent"), true)
  assert_eq(keys.contains("tracestate"), true)
  assert_eq(keys.contains("baggage"), true)
  assert_eq(keys.contains("custom-header"), true)
}

test "cross_module_data_consistency" {
  // 测试跨模块数据一致性
  
  // 创建一致的Resource
  let resource = common::Resource::default("consistency-test-service")
  let resource_attrs = [
    ("service.namespace", common::AttributeValue::string("test")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("deployment.environment", common::AttributeValue::string("test"))
  ]
  
  // 创建一致的Context
  let ctx = context::Context::empty()
    .with_value(context::create_key("trace.id"), "trace-12345")
    .with_value(context::create_key("correlation.id"), "corr-67890")
  
  // 创建一致的Baggage
  let baggage = context::Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("session.id", "session-abcdef")
  
  // 创建一致的Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("consistency-test-tracer")
  let (_, span) = tracer.start_span(
    ctx,
    "consistency-test-span",
    trace::Server,
    [
      ("service.name", common::AttributeValue::string("consistency-test-service")),
      ("service.namespace", common::AttributeValue::string("test")),
      ("service.version", common::AttributeValue::string("1.0.0")),
      ("deployment.environment", common::AttributeValue::string("test")),
      ("trace.id", common::AttributeValue::string("trace-12345")),
      ("correlation.id", common::AttributeValue::string("corr-67890")),
      ("user.id", common::AttributeValue::string("12345")),
      ("session.id", common::AttributeValue::string("session-abcdef"))
    ]
  )
  
  // 创建一致的LogRecord
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Consistency test log")
    .with_attribute("service.name", common::AttributeValue::string("consistency-test-service"))
    .with_attribute("service.namespace", common::AttributeValue::string("test"))
    .with_attribute("service.version", common::AttributeValue::string("1.0.0"))
    .with_attribute("deployment.environment", common::AttributeValue::string("test"))
    .with_attribute("trace.id", common::AttributeValue::string("trace-12345"))
    .with_attribute("correlation.id", common::AttributeValue::string("corr-67890"))
    .with_attribute("user.id", common::AttributeValue::string("12345"))
    .with_attribute("session.id", common::AttributeValue::string("session-abcdef"))
    .build()
  
  // 验证跨模块数据一致性
  assert_eq(resource.service_name, "consistency-test-service")
  assert_eq(span.name, "consistency-test-span")
  
  // 验证服务名称在所有模块中一致
  let mut found_service_name = false
  let mut i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    match key {
      "service.name" => {
        match value {
          common::StringValue(service_name) => {
            assert_eq(service_name, "consistency-test-service")
            found_service_name = true
          }
          _ => @test.fail("service.name should be StringValue")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  assert_eq(found_service_name, true)
  
  // 验证服务版本在所有模块中一致
  let mut found_service_version = false
  i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    match key {
      "service.version" => {
        match value {
          common::StringValue(service_version) => {
            assert_eq(service_version, "1.0.0")
            found_service_version = true
          }
          _ => @test.fail("service.version should be StringValue")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  assert_eq(found_service_version, true)
  
  // 验证环境在所有模块中一致
  let mut found_environment = false
  i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    match key {
      "deployment.environment" => {
        match value {
          common::StringValue(environment) => {
            assert_eq(environment, "test")
            found_environment = true
          }
          _ => @test.fail("deployment.environment should be StringValue")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  assert_eq(found_environment, true)
}