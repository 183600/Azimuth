// 跨模块集成测试用例
test "trace_and_context_integration" {
  // 测试Trace和Context模块的集成
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("integration-test-tracer", "1.0.0")
  let ctx = Context::empty()
  
  // 在Context中设置一些值
  let user_key = create_key("user.id")
  let request_key = create_key("request.id")
  let ctx_with_values = ctx
    .with_value(user_key, "user123")
    .with_value(request_key, "req456")
  
  // 使用带有Context的Tracer创建Span
  let (new_ctx, span) = tracer.start_span(
    ctx_with_values,
    "integration-test-span",
    Server,
    [("http.method", AttributeValue::string("GET"))],
    1234567890L
  )
  
  // 验证Span属性
  assert_eq(span.name, "integration-test-span")
  match span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  assert_eq(span.attributes.length(), 1)
  
  // 验证Context中的值仍然存在（在实际实现中应该被传播）
  match new_ctx.get(user_key) {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Expected user.id to be preserved in context")
  }
  
  match new_ctx.get(request_key) {
    Some(value) => assert_eq(value, "req456")
    None => @test.fail("Expected request.id to be preserved in context")
  }
}

test "logs_and_common_integration" {
  // 测试Logs和Common模块的集成
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Error)
    .body("Integration test error")
    .with_attribute("service.name", AttributeValue::string("integration-service"))
    .with_attribute("service.version", AttributeValue::string("1.0.0"))
    .with_attribute("user.id", AttributeValue::int(123L))
    .with_attribute("response.time", AttributeValue::float(250.75))
    .with_attribute("error.occurred", AttributeValue::bool(true))
    .build()
  
  // 创建带有复杂属性的Resource
  let resource = Resource::default("integration-service")
  let resource_attrs : Attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-456")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("integration-server")),
    ("process.pid", AttributeValue::int(98765L)),
    ("process.cpu.percent", AttributeValue::float(45.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["integration", "test", "api"])),
    ("ports", AttributeValue::array_int([8080L, 8443L])),
    ("cpu.thresholds", AttributeValue::array_float([70.0, 85.0, 95.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 验证LogRecord属性
  assert_eq(log_record.attributes.length(), 5)
  match log_record.attributes[0].1 {
    AttributeValue::StringValue(service_name) => assert_eq(service_name, "integration-service")
    _ => @test.fail("Expected StringValue for service.name")
  }
  
  match log_record.attributes[1].1 {
    AttributeValue::StringValue(service_version) => assert_eq(service_version, "1.0.0")
    _ => @test.fail("Expected StringValue for service.version")
  }
  
  match log_record.attributes[2].1 {
    AttributeValue::IntValue(user_id) => assert_eq(user_id, 123L)
    _ => @test.fail("Expected IntValue for user.id")
  }
  
  match log_record.attributes[3].1 {
    AttributeValue::FloatValue(response_time) => assert_eq(response_time, 250.75)
    _ => @test.fail("Expected FloatValue for response.time")
  }
  
  match log_record.attributes[4].1 {
    AttributeValue::BoolValue(error_occurred) => assert_eq(error_occurred, true)
    _ => @test.fail("Expected BoolValue for error.occurred")
  }
  
  // 验证Resource属性
  assert_eq(resource.service_name, "integration-service")
  assert_eq(resource_attrs.length(), 11)
}

test "metrics_and_context_integration" {
  // 测试Metrics和Context模块的集成
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("integration-meter", "1.0.0")
  let counter = meter.create_counter("integration_requests", "requests", "Integration test requests")
  
  // 创建带有复杂Context的属性
  let ctx = Context::empty()
  let ctx_with_values = ctx
    .with_value(create_key("user.id"), "user789")
    .with_value(create_key("session.id"), "session456")
    .with_value(create_key("request.path"), "/api/integration")
  
  // 使用Context中的值创建指标属性
  let metric_attrs = [
    ("user.id", AttributeValue::string("user789")),
    ("session.id", AttributeValue::string("session456")),
    ("request.path", AttributeValue::string("/api/integration")),
    ("http.method", AttributeValue::string("POST")),
    ("status.code", AttributeValue::int(200L)),
    ("response.time", AttributeValue::float(150.25)),
    ("cache.hit", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["api", "integration", "v1"])),
    ("retry.count", AttributeValue::int(0L))
  ]
  
  // 记录指标
  counter.add(1L, metric_attrs)
  
  // 验证调用成功
  assert_eq(true, true)
}

test "propagation_and_trace_integration" {
  // 测试Propagation和Trace模块的集成
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 创建复合传播器
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建Span
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("propagation-test-tracer", "1.0.0")
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "propagation-test-span",
    Client,
    [("service.name", AttributeValue::string("propagation-service"))],
    9876543210L
  )
  
  // 注入传播信息
  composite_propagator.inject(span_ctx, carrier)
  
  // 验证traceparent和baggage头都被设置
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(trace_parent) => {
      assert_eq(trace_parent.length(), 55)
      assert_eq(trace_parent[0..2], "00")
    }
    None => @test.fail("Expected traceparent header to be set")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(baggage_value) => {
      assert_eq(baggage_value, "key1=value1,key2=value2")
    }
    None => @test.fail("Expected baggage header to be set")
  }
  
  // 验证Span属性
  assert_eq(span.name, "propagation-test-span")
  match span.kind {
    Client => assert_eq(true, true)
    _ => @test.fail("Expected Client span kind")
  }
  assert_eq(span.attributes.length(), 1)
}

test "all_modules_integration_comprehensive" {
  // 综合集成测试，涉及所有模块
  
  // 1. 创建Context并设置值
  let ctx = Context::empty()
  let ctx_with_values = ctx
    .with_value(create_key("user.id"), "user999")
    .with_value(create_key("session.id"), "session888")
    .with_value(create_key("request.id"), "req777")
  
  // 2. 创建Trace Span
  let trace_provider = NoopTracerProvider::{}
  let tracer = trace_provider.get_tracer("comprehensive-tracer", "1.0.0")
  let (span_ctx, span) = tracer.start_span(
    ctx_with_values,
    "comprehensive-test-span",
    Server,
    [
      ("service.name", AttributeValue::string("comprehensive-service")),
      ("service.version", AttributeValue::string("2.0.0")),
      ("operation.name", AttributeValue::string("test_operation"))
    ],
    1234567890L
  )
  
  // 3. 创建LogRecord
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Comprehensive integration test log")
    .with_attribute("user.id", AttributeValue::string("user999"))
    .with_attribute("session.id", AttributeValue::string("session888"))
    .with_attribute("request.id", AttributeValue::string("req777"))
    .with_attribute("span.name", AttributeValue::string("comprehensive-test-span"))
    .with_attribute("operation.result", AttributeValue::string("success"))
    .build()
  
  // 4. 创建Metrics
  let metrics_provider = NoopMeterProvider::{}
  let meter = metrics_provider.get_meter("comprehensive-meter", "1.0.0")
  let counter = meter.create_counter("comprehensive_operations", "operations", "Comprehensive test operations")
  let histogram = meter.create_histogram("comprehensive_duration", "ms", "Comprehensive test duration")
  
  counter.add(1L, [
    ("user.id", AttributeValue::string("user999")),
    ("session.id", AttributeValue::string("session888")),
    ("operation.name", AttributeValue::string("test_operation")),
    ("operation.result", AttributeValue::string("success"))
  ])
  
  histogram.record(125.5, [
    ("user.id", AttributeValue::string("user999")),
    ("operation.name", AttributeValue::string("test_operation"))
  ])
  
  // 5. 创建Propagation
  let carrier = MapCarrier::new()
  let composite_propagator = CompositePropagator::new([
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ])
  
  composite_propagator.inject(span_ctx, carrier)
  
  // 6. 创建Resource
  let resource = Resource::default("comprehensive-service")
  let resource_attrs : Attributes = [
    ("service.namespace", AttributeValue::string("test")),
    ("service.instance.id", AttributeValue::string("instance-comprehensive")),
    ("deployment.environment", AttributeValue::string("test")),
    ("host.name", AttributeValue::string("test-host")),
    ("process.pid", AttributeValue::int(55555L)),
    ("process.cpu.percent", AttributeValue::float(55.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["comprehensive", "integration", "test"])),
    ("ports", AttributeValue::array_int([9000L, 9001L])),
    ("cpu.thresholds", AttributeValue::array_float([50.0, 75.0, 90.0])),
    ("feature.flags", AttributeValue::array_bool([true, true, false]))
  ]
  
  // 验证所有组件都正常工作
  assert_eq(span.name, "comprehensive-test-span")
  match span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  assert_eq(span.attributes.length(), 3)
  
  assert_eq(log_record.body, Some("Comprehensive integration test log"))
  match log_record.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity")
  }
  assert_eq(log_record.attributes.length(), 6)
  
  assert_eq(resource.service_name, "comprehensive-service")
  assert_eq(resource_attrs.length(), 11)
  
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header to be set")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header to be set")
  }
}

test "cross_module_error_handling_integration" {
  // 测试跨模块错误处理集成
  
  // 1. Context中的错误处理
  let ctx = Context::empty()
  let error_key = create_key("error.message")
  let ctx_with_error = ctx.with_value(error_key, "Something went wrong")
  
  // 2. Trace中的错误状态
  let trace_provider = NoopTracerProvider::{}
  let tracer = trace_provider.get_tracer("error-test-tracer", "1.0.0")
  let (error_ctx, error_span) = tracer.start_span(
    ctx_with_error,
    "error-test-span",
    Internal,
    [("error.type", AttributeValue::string("ValidationError"))],
    1234567890L
  )
  
  // 3. Logs中的错误记录
  let error_log = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Error)
    .body("Error occurred in integration test")
    .with_attribute("error.message", AttributeValue::string("Something went wrong"))
    .with_attribute("error.type", AttributeValue::string("ValidationError"))
    .with_attribute("error.stack", AttributeValue::string("at integration_test:123"))
    .with_attribute("error.recoverable", AttributeValue::bool(false))
    .build()
  
  // 4. Metrics中的错误计数
  let metrics_provider = NoopMeterProvider::{}
  let meter = metrics_provider.get_meter("error-meter", "1.0.0")
  let error_counter = meter.create_counter("errors_total", "errors", "Total number of errors")
  
  error_counter.add(1L, [
    ("error.type", AttributeValue::string("ValidationError")),
    ("error.recoverable", AttributeValue::bool(false)),
    ("service.name", AttributeValue::string("error-service"))
  ])
  
  // 验证错误处理集成
  match error_ctx.get(error_key) {
    Some(error_message) => assert_eq(error_message, "Something went wrong")
    None => @test.fail("Expected error message to be preserved in context")
  }
  
  match error_log.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error severity")
  }
  
  match error_log.attributes[0].1 {
    AttributeValue::StringValue(error_message) => assert_eq(error_message, "Something went wrong")
    _ => @test.fail("Expected StringValue for error.message")
  }
  
  assert_eq(error_span.attributes.length(), 1)
  match error_span.attributes[0].1 {
    AttributeValue::StringValue(error_type) => assert_eq(error_type, "ValidationError")
    _ => @test.fail("Expected StringValue for error.type")
  }
}