// 跨模块集成测试
// 测试多个遥测模块之间的协同工作

test "trace_and_context_integration" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let trace_key = @azimuth.telemetry.api.context.create_key("trace_info")
  let ctx_with_trace = ctx.with_value(trace_key, "trace-12345")
  
  let tracer = NoopTracer::{}
  let (new_ctx, span) = tracer.start_span(ctx_with_trace, "operation", kind: Some(Server))
  
  // 验证Span和Context可以一起使用
  assert_eq(span.name, "operation")
  match span.kind {
    Server => @test.succeed()
    _ => @test.fail("Expected Server span")
  }
  
  // 验证上下文信息仍然存在（在Noop实现中上下文不会被实际使用）
  match new_ctx.get(trace_key) {
    Some(value) => assert_eq(value, "trace-12345")
    None => @test.fail("Expected trace info in context")
  }
}

test "metrics_and_attributes_integration" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("integration_meter")
  let counter = meter.create_counter("api_requests")
  
  let complex_attributes = [
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string("user-service")),
    ("service.version", @azimuth.telemetry.api.common.AttributeValue::string("1.2.3")),
    ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("CREATE")),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::int(12345L)),
    ("success", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
    ("duration.ms", @azimuth.telemetry.api.common.AttributeValue::float(150.7)),
    ("tags", @azimuth.telemetry.api.common.AttributeValue::array_string(["api", "authenticated", "critical"])),
    ("retry.counts", @azimuth.telemetry.api.common.AttributeValue::array_int([1L, 2L, 3L])),
    ("percentiles", @azimuth.telemetry.api.common.AttributeValue::array_float([50.0, 90.0, 95.0, 99.0])),
    ("feature.flags", @azimuth.telemetry.api.common.AttributeValue::array_bool([true, false, true]))
  ]
  
  // 测试使用复杂属性集合的指标
  counter.add(1L, Some(complex_attributes))
  
  @test.succeed()
}

test "logs_with_trace_context_integration" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let tracer = NoopTracer::{}
  let (trace_ctx, span) = tracer.start_span(ctx, "database_operation")
  
  let provider = NoopLoggerProvider::{}
  let logger = provider.get_logger("db_logger")
  
  let log_attributes = [
    ("span.name", @azimuth.telemetry.api.common.AttributeValue::string(span.name)),
    ("span.kind", @azimuth.telemetry.api.common.AttributeValue::string("Internal")),  // 简化：将枚举转为字符串
    ("trace.active", @azimuth.telemetry.api.common.AttributeValue::bool(true))
  ]
  
  // 模拟与trace相关的日志记录
  logger.info("Database query executed", Some(log_attributes))
  
  @test.succeed()
}

test "propagation_and_context_integration" {
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let user_key = @azimuth.telemetry.api.context.create_key("user_id")
  let session_key = @azimuth.telemetry.api.context.create_key("session_id")
  
  let ctx_with_data = ctx
    .with_value(user_key, "user123")
    .with_value(session_key, "session456")
  
  let carrier = MapCarrier::new()
  let composite = CompositePropagator::new([
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ])
  
  // 注入上下文到carrier
  composite.inject(ctx_with_data, carrier)
  
  // 从carrier提取上下文
  let extracted_ctx = composite.extract(Context::empty(), carrier)
  
  // 在当前实现中，propagator不会实际传播Context中的数据
  // 但这个测试验证了接口的协同工作能力
  assert_eq(carrier.keys().length(), 2)
  
  match carrier.get("traceparent") {
    Some(_) => @test.succeed()
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get("baggage") {
    Some(_) => @test.succeed()
    None => @test.fail("Expected baggage header")
  }
}

test "resource_and_all_signals_integration" {
  let resource = @azimuth.telemetry.api.common.Resource::default("integration_test_service")
  let resource_with_attrs = @azimuth.telemetry.api.common.Resource::{
    service_name: resource.service_name,
    service_version: resource.service_version,
    telemetry_sdk_name: resource.telemetry_sdk_name,
    telemetry_sdk_version: resource.telemetry_sdk_version,
    attributes: [
      ("env", @azimuth.telemetry.api.common.AttributeValue::string("test")),
      ("region", @azimuth.telemetry.api.common.AttributeValue::string("us-west-2")),
      ("version", @azimuth.telemetry.api.common.AttributeValue::string("1.0.0"))
    ]
  }
  
  // Trace with resource
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  let (_, span) = tracer.start_span(ctx, "trace_with_resource")
  
  // Metrics with resource attributes
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("resource_meter")
  let counter = meter.create_counter("resource_operations")
  counter.add(1L, Some(resource_with_attrs.attributes))
  
  // Logs with resource
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("resource_logger")
  logger.info("Operation with resource context", Some(resource_with_attrs.attributes))
  
  @test.succeed()
}

test "instrumentation_scope_integration" {
  let instrumentation_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "database-client",
    version: Some("2.1.0"),
    schema_url: Some("http://example.com/database-schema")
  }
  
  // 创建与instrumentation scope相关的各种信号
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    severity_number: Info,
    body: Some("Database operation completed"),
    attributes: [
      ("db.operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT")),
      ("db.table", @azimuth.telemetry.api.common.AttributeValue::string("users")),
      ("scope.name", @azimuth.telemetry.api.common.AttributeValue::string(instrumentation_scope.name)),
      ("scope.version", @azimuth.telemetry.api.common.AttributeValue::string(instrumentation_scope.version.unwrap_or("unknown")))
    ],
    instrumentation_scope: Some(instrumentation_scope),
    ..LogRecord::builder().build()
  }
  
  assert_eq(log_record.instrumentation_scope.unwrap().name, "database-client")
  match log_record.instrumentation_scope.unwrap().version {
    Some(version) => assert_eq(version, "2.1.0")
    None => @test.fail("Expected version")
  }
  
  // 验证日志记录器可以使用instrumentation scope
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger(
    instrumentation_scope.name, 
    instrumentation_scope.version, 
    instrumentation_scope.schema_url
  )
  
  logger.emit(log_record)
  
  @test.succeed()
}

test "end_to_end_telemetry_flow" {
  // 模拟完整的遥测流程：创建span -> 记录指标 -> 发送日志 -> 传播上下文
  
  // 1. 创建上下文和span
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let trace_key = @azimuth.telemetry.api.context.create_key("correlation_id")
  let ctx_with_correlation = ctx.with_value(trace_key, "corr-12345")
  
  let tracer = NoopTracer::{}
  let (op_ctx, span) = tracer.start_span(ctx_with_correlation, "user_registration", kind: Some(Server))
  
  // 2. 记录指标
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("business_metrics")
  let counter = meter.create_counter("user_registrations")
  let histogram = meter.create_histogram("registration_duration")
  
  counter.add(1L, Some([
    ("service", @azimuth.telemetry.api.common.AttributeValue::string("auth-service")),
    ("correlation_id", @azimuth.telemetry.api.common.AttributeValue::string("corr-12345"))
  ]))
  
  histogram.record(250.5, Some([
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("user_registration"))
  ]))
  
  // 3. 记录日志
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("auth_logger")
  
  logger.info("User registration completed", Some([
    ("user_id", @azimuth.telemetry.api.common.AttributeValue::int(98765L)),
    ("correlation_id", @azimuth.telemetry.api.common.AttributeValue::string("corr-12345")),
    ("span_name", @azimuth.telemetry.api.common.AttributeValue::string(span.name))
  ]))
  
  // 4. 传播上下文到下一个服务
  let carrier = MapCarrier::new()
  let propagator = CompositePropagator::new([
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ])
  
  propagator.inject(op_ctx, carrier)
  
  // 验证整个流程的完整性
  assert_eq(span.name, "user_registration")
  assert_eq(carrier.keys().length(), 2)
  
  @test.succeed()
}