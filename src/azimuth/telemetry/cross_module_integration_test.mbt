// 跨模块集成测试 - 测试Trace、Metrics、Logs和Context模块之间的协作
use azimuth.telemetry.api.common.{AttributeValue, Resource, InstrumentationScope}
use azimuth.telemetry.api.trace.{SpanContext, Span, SpanKind, StatusCode, SpanEvent, SpanLink, NoopTracer, NoopTracerProvider}
use azimuth.telemetry.api.metrics.{Measurement, Counter, Histogram, UpDownCounter, Gauge, NoopMeter, NoopMeterProvider}
use azimuth.telemetry.api.logs.{SeverityNumber, LogRecord, LogRecordBuilder, NoopLogger, NoopLoggerProvider}
use azimuth.telemetry.api.context.{Context, ContextKey, Baggage, create_key}

test "cross_module_trace_metrics_integration" {
  // 创建上下文和追踪
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("integration-test", Some("1.0.0"))
  
  // 开始一个span
  let (ctx_with_span, span) = tracer.start_span(
    ctx, 
    "operation_with_metrics", 
    Server, 
    Some([
      ("service.name", AttributeValue::string("integration-service")),
      ("operation.type", AttributeValue::string("data_processing"))
    ])
  )
  
  // 在span上下文中记录指标
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration-test-metrics", Some("1.0.0"))
  
  let counter = meter.create_counter("operations.total", Some("count"), Some("Total number of operations"))
  let histogram = meter.create_histogram("operation.duration", Some("ms"), Some("Operation duration in milliseconds"))
  let gauge = meter.create_gauge("active.connections", Some("count"), Some("Number of active connections"))
  
  // 记录指标数据
  counter.add(1L, Some([
    ("operation.name", AttributeValue::string("data_processing")),
    ("service.name", AttributeValue::string("integration-service"))
  ]))
  
  histogram.record(150.5, Some([
    ("operation.name", AttributeValue::string("data_processing")),
    ("service.version", AttributeValue::string("1.2.3"))
  ]))
  
  gauge.record(42.0, Some([
    ("service.name", AttributeValue::string("integration-service")),
    ("region", AttributeValue::string("us-west-2"))
  ]))
  
  // 验证span属性
  assert_eq(span.name, "operation_with_metrics")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 2)
  assert_eq(span.attributes[0].0, "service.name")
  assert_eq(span.attributes[1].0, "operation.type")
  
  match span.attributes[0].1 {
    StringValue(value) => assert_eq(value, "integration-service")
    _ => @test.fail("Expected StringValue for service.name")
  }
  
  match span.attributes[1].1 {
    StringValue(value) => assert_eq(value, "data_processing")
    _ => @test.fail("Expected StringValue for operation.type")
  }
}

test "cross_module_trace_logs_integration" {
  // 创建带有追踪信息的日志记录
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("logging-test", Some("1.0.0"))
  
  // 开始一个span
  let (ctx_with_span, span) = tracer.start_span(
    ctx, 
    "operation_with_logging", 
    Client, 
    Some([
      ("user.id", AttributeValue::string("user-12345")),
      ("request.id", AttributeValue::string("req-67890"))
    ])
  )
  
  // 创建logger并记录日志
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integration-test-logger", Some("1.0.0"))
  
  // 创建与span关联的日志记录
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Info)
    .body("Operation completed successfully")
    .with_attribute("service.name", AttributeValue::string("integration-service"))
    .with_attribute("operation.name", AttributeValue::string("data_processing"))
    .with_attribute("user.id", AttributeValue::string("user-12345"))
    .with_attribute("duration.ms", AttributeValue::int(250L))
    .with_attribute("success.rate", AttributeValue::float(0.95))
    .with_attribute("features", AttributeValue::array_bool([true, false, true]))
    .build()
  
  // 发出日志记录
  logger.emit(log_record)
  
  // 使用便捷方法记录不同级别的日志
  logger.debug("Debugging operation flow", Some([
    ("step", AttributeValue::string("validation")),
    ("input.size", AttributeValue::int(1024L))
  ]))
  
  logger.info("Operation started", Some([
    ("operation.id", AttributeValue::string("op-12345")),
    ("timestamp", AttributeValue::int(1640995200L))
  ]))
  
  logger.warn("Performance warning", Some([
    ("threshold.exceeded", AttributeValue::bool(true)),
    ("actual.duration", AttributeValue::float(1250.5))
  ]))
  
  logger.error("Operation failed", Some([
    ("error.code", AttributeValue::string("TIMEOUT")),
    ("error.message", AttributeValue::string("Operation timed out after 30 seconds")),
    ("retry.count", AttributeValue::int(3L))
  ]))
  
  logger.fatal("Critical system error", Some([
    ("system.component", AttributeValue::string("database")),
    ("error.severity", AttributeValue::string("critical")),
    ("impact", AttributeValue::string("service_unavailable"))
  ]))
  
  // 验证span和日志的关联性
  assert_eq(span.name, "operation_with_logging")
  assert_eq(span.kind, Client)
  assert_eq(span.attributes.length(), 2)
  
  // 验证日志记录构建器创建的记录
  assert_eq(log_record.severity_number, SeverityNumber::Info)
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  match log_record.body {
    Some(body_text) => assert_eq(body_text, "Operation completed successfully")
    None => @test.fail("Expected Some(body) for log record")
  }
  assert_eq(log_record.attributes.length(), 6)
}

test "cross_module_context_baggage_integration" {
  // 测试Context和Baggage在跨模块传播中的集成
  
  // 创建初始上下文
  let ctx = Context::empty()
  
  // 添加上下文值
  let user_key = create_key("user.id")
  let request_key = create_key("request.id")
  let trace_key = create_key("trace.id")
  
  let ctx_with_values = ctx
    .with_value(user_key, "user-12345")
    .with_value(request_key, "req-67890")
    .with_value(trace_key, "trace-abcdef")
  
  // 创建baggage
  let baggage = Baggage::empty()
    .with_entry("correlation.id", "corr-12345")
    .with_entry("session.id", "sess-67890")
    .with_entry("tenant.id", "tenant-abc")
    .with_entry("region", "us-west-2")
  
  // 在带有上下文和baggage的情况下创建span
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("context-test", Some("1.0.0"))
  
  let (final_ctx, span) = tracer.start_span(
    ctx_with_values, 
    "context_aware_operation", 
    Internal, 
    Some([
      ("correlation.id", AttributeValue::string("corr-12345")),
      ("session.id", AttributeValue::string("sess-67890")),
      ("tenant.id", AttributeValue::string("tenant-abc")),
      ("region", AttributeValue::string("us-west-2"))
    ])
  )
  
  // 验证上下文值
  match ctx_with_values.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Expected Some(user.id)")
  }
  
  match ctx_with_values.get(request_key) {
    Some(request_id) => assert_eq(request_id, "req-67890")
    None => @test.fail("Expected Some(request.id)")
  }
  
  match ctx_with_values.get(trace_key) {
    Some(trace_id) => assert_eq(trace_id, "trace-abcdef")
    None => @test.fail("Expected Some(trace.id)")
  }
  
  // 验证baggage条目
  match baggage.get("correlation.id") {
    Some(correlation_id) => assert_eq(correlation_id, "corr-12345")
    None => @test.fail("Expected Some(correlation.id)")
  }
  
  match baggage.get("session.id") {
    Some(session_id) => assert_eq(session_id, "sess-67890")
    None => @test.fail("Expected Some(session.id)")
  }
  
  match baggage.get("tenant.id") {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-abc")
    None => @test.fail("Expected Some(tenant.id)")
  }
  
  match baggage.get("region") {
    Some(region) => assert_eq(region, "us-west-2")
    None => @test.fail("Expected Some(region)")
  }
  
  // 验证span属性包含了baggage信息
  assert_eq(span.name, "context_aware_operation")
  assert_eq(span.kind, Internal)
  assert_eq(span.attributes.length(), 4)
  
  // 验证span属性与baggage的一致性
  let attribute_names = ["correlation.id", "session.id", "tenant.id", "region"]
  let mut i = 0
  while i < attribute_names.length() {
    let found = span.attributes.any(fn(attr) { attr.0 == attribute_names[i] })
    assert_eq(found, true)
    i = i + 1
  }
}

test "cross_module_resource_instrumentation_integration" {
  // 测试Resource和InstrumentationScope在跨模块使用中的集成
  
  // 创建资源
  let resource = Resource::default("integration-service")
  
  // 创建带有复杂属性的span
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("resource-test", Some("1.0.0"))
  
  let complex_attributes = [
    ("service.name", AttributeValue::string("integration-service")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("service.namespace", AttributeValue::string("production")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("host.ip", AttributeValue::string("10.0.1.100")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.executable.name", AttributeValue::string("integration-service")),
    ("process.command_line", AttributeValue::array_string(["./integration-service", "--config", "/etc/config.yaml"])),
    ("process.runtime.name", AttributeValue::string("moonbit")),
    ("process.runtime.version", AttributeValue::string("1.0.0")),
    ("process.runtime.description", AttributeValue::string("MoonBit Runtime")),
    ("os.type", AttributeValue::string("linux")),
    ("os.version", AttributeValue::string("5.15.0")),
    ("os.description", AttributeValue::string("Linux 5.15.0-generic")),
    ("cloud.provider", AttributeValue::string("aws")),
    ("cloud.region", AttributeValue::string("us-west-2")),
    ("cloud.availability_zone", AttributeValue::string("us-west-2a")),
    ("cloud.account.id", AttributeValue::string("123456789012"))
  ]
  
  let (ctx_with_span, span) = tracer.start_span(
    ctx, 
    "resource_aware_operation", 
    Server, 
    Some(complex_attributes)
  )
  
  // 创建与资源关联的指标
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("resource-metrics", Some("1.0.0"))
  
  let counter = meter.create_counter("requests.total", Some("count"), Some("Total number of requests"))
  counter.add(1L, Some([
    ("service.name", AttributeValue::string("integration-service")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("cloud.region", AttributeValue::string("us-west-2"))
  ]))
  
  // 创建与资源关联的日志
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("resource-logger", Some("1.0.0"))
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Info)
    .body("Resource-aware operation completed")
    .with_attribute("service.name", AttributeValue::string("integration-service"))
    .with_attribute("service.version", AttributeValue::string("2.1.0"))
    .with_attribute("host.name", AttributeValue::string("web-server-01"))
    .with_attribute("cloud.region", AttributeValue::string("us-west-2"))
    .build()
  
  logger.emit(log_record)
  
  // 验证资源信息
  assert_eq(resource.service_name, "integration-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 验证span属性
  assert_eq(span.name, "resource_aware_operation")
  assert_eq(span.kind, Server)
  assert_eq(span.attributes.length(), 20)
  
  // 验证日志记录
  assert_eq(log_record.severity_number, SeverityNumber::Info)
  match log_record.body {
    Some(body_text) => assert_eq(body_text, "Resource-aware operation completed")
    None => @test.fail("Expected Some(body) for log record")
  }
  assert_eq(log_record.attributes.length(), 4)
}