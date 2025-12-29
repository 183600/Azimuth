// 跨模块集成测试用例
test "trace_context_integration" {
  // 测试Trace和Context模块的集成
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 在Context中设置追踪相关信息
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  let correlation_key = create_key("correlation.id")
  
  let enriched_ctx = ctx
    .with_value(trace_key, "trace-12345678901234567890123456789012")
    .with_value(span_key, "span-1234567890123456")
    .with_value(correlation_key, "corr-abcdef1234567890")
  
  // 使用 enriched Context 创建 Span
  let (new_ctx, span) = tracer.start_span(
    enriched_ctx,
    "integration-test-span",
    Server,
    Some([
      ("service.name", AttributeValue::string("integration-test-service")),
      ("operation.name", AttributeValue::string("test.operation")),
      ("integration.type", AttributeValue::string("trace-context"))
    ]),
    Some(1234567890L)
  )
  
  // 验证Context中的值仍然存在
  match new_ctx.get(trace_key) {
    Some(value) => assert_eq(value, "trace-12345678901234567890123456789012")
    None => @test.fail("Expected Some(trace.id)")
  }
  
  match new_ctx.get(span_key) {
    Some(value) => assert_eq(value, "span-1234567890123456")
    None => @test.fail("Expected Some(span.id)")
  }
  
  // 验证Span的属性
  assert_eq(span.name, "integration-test-span")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 3)
}

test "context_baggage_integration" {
  // 测试Context和Baggage模块的集成
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 在Context中设置追踪信息
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  
  let enriched_ctx = ctx
    .with_value(trace_key, "trace-12345678901234567890123456789012")
    .with_value(span_key, "span-1234567890123456")
  
  // 在Baggage中设置相关元数据
  let enriched_baggage = baggage
    .with_entry("user.id", "user-123456789")
    .with_entry("session.id", "session-abcdef123456")
    .with_entry("request.id", "req-1234567890abcdef")
    .with_entry("correlation.id", "corr-abcdef1234567890")
  
  // 验证Context和Baggage中的数据可以一起使用
  match enriched_ctx.get(trace_key) {
    Some(value) => assert_eq(value, "trace-12345678901234567890123456789012")
    None => @test.fail("Expected Some(trace.id)")
  }
  
  match enriched_baggage.get("user.id") {
    Some(value) => assert_eq(value, "user-123456789")
    None => @test.fail("Expected Some(user.id)")
  }
  
  match enriched_baggage.get("correlation.id") {
    Some(value) => assert_eq(value, "corr-abcdef1234567890")
    None => @test.fail("Expected Some(correlation.id)")
  }
}

test "metrics_context_integration" {
  // 测试Metrics和Context模块的集成
  let ctx = Context::empty()
  let meter = NoopMeter::{}
  
  // 在Context中设置相关信息
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let request_key = create_key("request.id")
  
  let enriched_ctx = ctx
    .with_value(user_key, "user-123456789")
    .with_value(session_key, "session-abcdef123456")
    .with_value(request_key, "req-1234567890abcdef")
  
  // 基于Context信息创建度量
  let counter = meter.create_counter("integration.requests", "requests", "Integration test counter")
  let histogram = meter.create_histogram("integration.duration", "seconds", "Integration test histogram")
  let gauge = meter.create_gauge("integration.memory", "bytes", "Integration test gauge")
  
  // 使用Context信息添加度量数据
  match enriched_ctx.get(user_key) {
    Some(user_id) => {
      counter.add(1L, Some([
        ("user.id", AttributeValue::string(user_id)),
        ("metric.type", AttributeValue::string("counter"))
      ]))
      histogram.record(0.123, Some([
        ("user.id", AttributeValue::string(user_id)),
        ("metric.type", AttributeValue::string("histogram"))
      ]))
      gauge.record(1024.0, Some([
        ("user.id", AttributeValue::string(user_id)),
        ("metric.type", AttributeValue::string("gauge"))
      ]))
    }
    None => @test.fail("Expected Some(user.id)")
  }
  
  // 验证Context中的值仍然存在
  match enriched_ctx.get(session_key) {
    Some(value) => assert_eq(value, "session-abcdef123456")
    None => @test.fail("Expected Some(session.id)")
  }
}

test "logs_trace_integration" {
  // 测试Logs和Trace模块的集成
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integration.logger", "1.0.0", "")
  
  // 创建Span
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "integration-operation",
    Server,
    Some([
      ("service.name", AttributeValue::string("integration-service")),
      ("operation.type", AttributeValue::string("test"))
    ]),
    Some(1234567890L)
  )
  
  // 在Span上下文中记录日志
  let trace_context = SpanContext::{
    trace_id: Array::make(16, 1_byte),
    span_id: Array::make(8, 2_byte),
    trace_flags: 1_byte,
    trace_state: "test-trace-state"
  }
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567891L,
    observed_timestamp_unix_nanos: Some(1234567892L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Integration test log message"),
    attributes: [
      ("service.name", AttributeValue::string("integration-service")),
      ("operation.name", AttributeValue::string("integration-operation")),
      ("log.type", AttributeValue::string("integration"))
    ],
    trace_id: Some(trace_context.trace_id),
    span_id: Some(trace_context.span_id),
    trace_flags: Some(trace_context.trace_flags),
    resource: Some(Resource::default("integration-service")),
    instrumentation_scope: None
  }
  
  logger.emit(log_record)
  
  // 使用便捷方法记录不同级别的日志
  logger.debug("Debug message in span context", Some([
    ("span.name", AttributeValue::string(span.name)),
    ("log.level", AttributeValue::string("debug"))
  ]))
  
  logger.info("Info message in span context", Some([
    ("span.name", AttributeValue::string(span.name)),
    ("log.level", AttributeValue::string("info"))
  ]))
  
  logger.warn("Warning message in span context", Some([
    ("span.name", AttributeValue::string(span.name)),
    ("log.level", AttributeValue::string("warn"))
  ]))
  
  logger.error("Error message in span context", Some([
    ("span.name", AttributeValue::string(span.name)),
    ("log.level", AttributeValue::string("error"))
  ]))
  
  logger.fatal("Fatal message in span context", Some([
    ("span.name", AttributeValue::string(span.name)),
    ("log.level", AttributeValue::string("fatal"))
  ]))
  
  // 验证Span信息
  assert_eq(span.name, "integration-operation")
  assert_eq(span.kind, Server)
}

test "propagation_trace_context_integration" {
  // 测试Propagation、Trace和Context模块的集成
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 创建传播器
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建Span
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "propagation-test-span",
    Client,
    Some([
      ("service.name", AttributeValue::string("client-service")),
      ("operation.name", AttributeValue::string("outbound.request"))
    ]),
    Some(1234567890L)
  )
  
  // 在Context中设置额外信息
  let user_key = create_key("user.id")
  let request_key = create_key("request.id")
  
  let enriched_ctx = span_ctx
    .with_value(user_key, "user-123456789")
    .with_value(request_key, "req-abcdef1234567890")
  
  // 创建载体
  let carrier = MapCarrier::new()
  
  // 注入追踪上下文
  composite_propagator.inject(enriched_ctx, carrier)
  
  // 模拟跨进程传输后提取
  let extraction_ctx = Context::empty()
  let extracted_ctx = composite_propagator.extract(extraction_ctx, carrier)
  
  // 验证提取的上下文可以用于创建新的Span
  let (new_span_ctx, new_span) = tracer.start_span(
    extracted_ctx,
    "propagation-test-span-continued",
    Server,
    Some([
      ("service.name", AttributeValue::string("server-service")),
      ("operation.name", AttributeValue::string("inbound.request")),
      ("original.service", AttributeValue::string("client-service"))
    ]),
    Some(1234567891L)
  )
  
  // 验证Span创建成功
  assert_eq(new_span.name, "propagation-test-span-continued")
  assert_eq(new_span.kind, Server)
  assert_eq(new_span.attributes.length(), 3)
}

test "resource_common_integration" {
  // 测试Resource和Common模块的集成
  let resource1 = Resource::default("integration-service-1")
  let resource2 = Resource::default("integration-service-2")
  
  // 创建复杂的属性
  let common_attrs1 : Attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("service.tags", AttributeValue::array_string(["web", "api", "critical"])),
    ("service.ports", AttributeValue::array_int([8080L, 8443L])),
    ("service.thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
    ("service.features", AttributeValue::array_bool([true, false, true]))
  ]
  
  let common_attrs2 : Attributes = [
    ("service.namespace", AttributeValue::string("staging")),
    ("service.version", AttributeValue::string("2.0.0")),
    ("deployment.environment", AttributeValue::string("staging")),
    ("host.name", AttributeValue::string("test-server-01")),
    ("process.pid", AttributeValue::int(54321L)),
    ("process.memory.rss", AttributeValue::int(1073741824L)),
    ("process.healthy", AttributeValue::bool(false)),
    ("service.tags", AttributeValue::array_string(["test", "debug", "experimental"])),
    ("service.ports", AttributeValue::array_int([9090L, 9091L])),
    ("service.metrics", AttributeValue::array_float([0.1, 0.5, 0.9])),
    ("service.capabilities", AttributeValue::array_bool([false, true, false, true]))
  ]
  
  // 创建使用这些资源的Span和LogRecord
  let tracer = NoopTracer::{}
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integration.logger")
  
  let ctx = Context::empty()
  
  // 使用第一个资源创建Span
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "integration-span-1",
    Server,
    Some(common_attrs1),
    Some(1234567890L)
  )
  
  // 使用第二个资源创建LogRecord
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567891L,
    observed_timestamp_unix_nanos: Some(1234567892L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Integration log with resource attributes"),
    attributes: common_attrs2,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource2),
    instrumentation_scope: None
  }
  
  logger.emit(log_record)
  
  // 验证资源信息
  assert_eq(resource1.service_name, "integration-service-1")
  assert_eq(resource2.service_name, "integration-service-2")
  assert_eq(span.attributes.length(), 11)
  assert_eq(log_record.attributes.length(), 11)
}

test "full_telemetry_integration" {
  // 测试完整的遥测集成：Trace、Metrics、Logs、Context、Propagation、Resource
  let ctx = Context::empty()
  
  // 创建资源
  let resource = Resource::default("full-integration-service")
  
  // 创建传播器
  let propagators : Array[TextMapPropagator] = [
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建追踪器和度量器
  let tracer = NoopTracer::{}
  let meter = NoopMeter::{}
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("full.integration.logger")
  
  // 在Context中设置信息
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let request_key = create_key("request.id")
  
  let enriched_ctx = ctx
    .with_value(user_key, "user-123456789")
    .with_value(session_key, "session-abcdef123456")
    .with_value(request_key, "req-1234567890abcdef")
  
  // 创建Span
  let (span_ctx, span) = tracer.start_span(
    enriched_ctx,
    "full.integration.operation",
    Server,
    Some([
      ("service.name", AttributeValue::string(resource.service_name)),
      ("operation.type", AttributeValue::string("full.integration")),
      ("user.id", AttributeValue::string("user-123456789"))
    ]),
    Some(1234567890L)
  )
  
  // 创建度量
  let counter = meter.create_counter("integration.requests", "requests", "Integration counter")
  let histogram = meter.create_histogram("integration.duration", "seconds", "Integration histogram")
  let gauge = meter.create_gauge("integration.memory", "bytes", "Integration gauge")
  
  // 记录度量
  counter.add(1L, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.name", AttributeValue::string(span.name)),
    ("user.id", AttributeValue::string("user-123456789"))
  ]))
  
  histogram.record(0.123, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.name", AttributeValue::string(span.name)),
    ("user.id", AttributeValue::string("user-123456789"))
  ]))
  
  gauge.record(1024.0, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.name", AttributeValue::string(span.name)),
    ("user.id", AttributeValue::string("user-123456789"))
  ]))
  
  // 记录日志
  logger.info("Integration operation started", Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.name", AttributeValue::string(span.name)),
    ("user.id", AttributeValue::string("user-123456789")),
    ("session.id", AttributeValue::string("session-abcdef123456")),
    ("request.id", AttributeValue::string("req-1234567890abcdef"))
  ]))
  
  logger.error("Integration operation error", Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.name", AttributeValue::string(span.name)),
    ("user.id", AttributeValue::string("user-123456789")),
    ("error.code", AttributeValue::int(500L)),
    ("error.type", AttributeValue::string("Integration Error"))
  ]))
  
  // 传播上下文
  let carrier = MapCarrier::new()
  composite_propagator.inject(span_ctx, carrier)
  
  // 提取上下文
  let extracted_ctx = composite_propagator.extract(Context::empty(), carrier)
  
  // 验证所有组件都正常工作
  assert_eq(span.name, "full.integration.operation")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 3)
  
  match span_ctx.get(user_key) {
    Some(value) => assert_eq(value, "user-123456789")
    None => @test.fail("Expected Some(user.id)")
  }
  
  match span_ctx.get(session_key) {
    Some(value) => assert_eq(value, "session-abcdef123456")
    None => @test.fail("Expected Some(session.id)")
  }
  
  match span_ctx.get(request_key) {
    Some(value) => assert_eq(value, "req-1234567890abcdef")
    None => @test.fail("Expected Some(request.id)")
  }
}