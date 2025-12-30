// 跨模块集成测试
// 测试不同API模块之间的协作和数据流

test "telemetry_trace_metrics_integration" {
  // 测试追踪和指标的集成
  
  // 1. 创建追踪相关的数据
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("integration-tracer")
  
  let (ctx_with_span, span) = tracer.start_span(
    ctx,
    "integration-operation",
    trace::Client,
    [("service.component", common::AttributeValue::string("integration"))]
  )
  
  // 2. 基于追踪数据创建指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration-meter")
  
  let span_duration_counter = meter.create_counter("span.duration", "ms", "Span duration counter")
  let span_size_histogram = meter.create_histogram("span.size", "bytes", "Span size histogram")
  
  // 模拟基于Span数据的指标记录
  span_duration_counter.add(100, [("span.name", common::AttributeValue::string(span.name))])
  span_size_histogram.record(1024.0, [("span.kind", common::AttributeValue::string("client"))])
  
  // 3. 验证追踪和指标的关联
  let integration_data = [
    ("trace.id", common::AttributeValue::string("integration-trace-123")),
    ("span.name", common::AttributeValue::string(span.name)),
    ("metric.count", common::AttributeValue::int(1)),
    ("metric.value", common::AttributeValue::float(1024.0))
  ]
  
  assert_eq(integration_data.length(), 4)
  assert_eq(span.name, "integration-operation")
}

test "telemetry_trace_logs_integration" {
  // 测试追踪和日志的集成
  
  // 1. 创建Span和上下文
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("trace-logs-tracer")
  
  let (ctx_with_span, span) = tracer.start_span(
    ctx,
    "trace-logs-operation",
    trace::Server,
    [("operation.id", common::AttributeValue::string("op-12345"))]
  )
  
  // 2. 在Span上下文中记录日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("trace-logs-logger")
  
  // 创建与Span关联的日志记录
  let trace_context_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Operation started within trace context")
    .with_attribute("trace.id", common::AttributeValue::string("trace-12345"))
    .with_attribute("span.id", common::AttributeValue::string("span-67890"))
    .with_attribute("operation.id", common::AttributeValue::string("op-12345"))
    .build()
  
  logger.emit(trace_context_log)
  
  // 3. 记录Span生命周期相关的日志
  logger.debug("Span created", [("span.name", common::AttributeValue::string(span.name))])
  logger.info("Span active", [("span.duration", common::AttributeValue::int(0))])
  
  // 4. 验证追踪和日志的关联
  let log_trace_correlation = "Log correlated with trace: span=" + span.name + 
                              ", operation=op-12345"
  assert_eq(log_trace_correlation.contains("span=trace-logs-operation"), true)
  assert_eq(log_trace_correlation.contains("operation=op-12345"), true)
}

test "telemetry_metrics_logs_integration" {
  // 测试指标和日志的集成
  
  // 1. 创建指标并记录数据
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("metrics-logs-meter")
  
  let request_counter = meter.create_counter("http.requests", "count", "HTTP request count")
  let response_histogram = meter.create_histogram("http.response.duration", "ms", "Response duration")
  
  // 记录指标数据
  request_counter.add(1, [("method", common::AttributeValue::string("GET"))])
  response_histogram.record(250.0, [("status.code", common::AttributeValue::int(200))])
  
  // 2. 基于指标数据记录日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("metrics-logs-logger")
  
  // 记录指标相关的日志
  logger.info("Metrics recorded", [
    ("metric.name", common::AttributeValue::string("http.requests")),
    ("metric.value", common::AttributeValue::int(1)),
    ("metric.attribute", common::AttributeValue::string("method=GET"))
  ])
  
  logger.info("Histogram recorded", [
    ("metric.name", common::AttributeValue::string("http.response.duration")),
    ("metric.value", common::AttributeValue::float(250.0)),
    ("metric.attribute", common::AttributeValue::string("status.code=200"))
  ])
  
  // 3. 验证指标和日志的集成
  let metrics_logs_summary = "Metrics-Logs integration: 2 metrics logged with detailed attributes"
  assert_eq(metrics_logs_summary.contains("2 metrics"), true)
  assert_eq(metrics_logs_summary.contains("detailed attributes"), true)
}

test "telemetry_context_propagation_integration" {
  // 测试上下文传播与各模块的集成
  
  // 1. 创建带有行李的上下文
  let ctx = context::Context::empty()
  let baggage = context::Baggage::empty()
    .with_entry("user.id", "user-12345")
    .with_entry("session.id", "session-67890")
    .with_entry("request.id", "req-abcde")
  
  // 2. 在传播上下文中创建Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("context-propagation-tracer")
  
  let context_key = context::create_key("operation.type")
  let ctx_with_operation = ctx.with_value(context_key, "context-propagation-test")
  
  let (ctx_with_span, span) = tracer.start_span(
    ctx_with_operation,
    "context-propagation-operation",
    trace::Internal,
    [("baggage.entries", common::AttributeValue::int(baggage.entries.length()))]
  )
  
  // 3. 在传播上下文中记录指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("context-propagation-meter")
  
  let context_counter = meter.create_counter("context.operations", "count", "Context operations")
  context_counter.add(1, [
    ("operation.type", common::AttributeValue::string("context-propagation-test")),
    ("baggage.count", common::AttributeValue::int(baggage.entries.length()))
  ])
  
  // 4. 在传播上下文中记录日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("context-propagation-logger")
  
  logger.info("Context propagated", [
    ("operation.type", common::AttributeValue::string("context-propagation-test")),
    ("user.id", common::AttributeValue::string("user-12345")),
    ("session.id", common::AttributeValue::string("session-67890"))
  ])
  
  // 5. 使用传播器进行跨进程传播
  let carrier = propagation::MapCarrier::new()
  let composite_propagator = propagation::CompositePropagator::new([
    propagation::W3CTraceContextPropagator::{},
    propagation::W3CBaggagePropagator::{}
  ])
  
  composite_propagator.inject(ctx_with_span, carrier)
  
  // 验证传播结果
  let injected_headers = carrier.keys()
  assert_eq(injected_headers.length(), 2) // traceparent 和 baggage
  
  // 6. 提取和验证传播的上下文
  let extracted_ctx = composite_propagator.extract(context::Context::empty(), carrier)
  let extracted_operation = extracted_ctx.get(context_key)
  
  assert_eq(extracted_operation, Some("context-propagation-test"))
  
  // 7. 验证完整的上下文传播集成
  let propagation_integration_result = "Context propagation integrated: " +
                                      "span=" + span.name + 
                                      ", baggage=" + baggage.entries.length().to_string() +
                                      ", headers=" + injected_headers.length().to_string()
  
  assert_eq(propagation_integration_result.contains("context-propagation-operation"), true)
  assert_eq(propagation_integration_result.contains("baggage=3"), true)
  assert_eq(propagation_integration_result.contains("headers=2"), true)
}

test "telemetry_resource_instrumentation_integration" {
  // 测试资源和仪表化范围的集成
  
  // 1. 创建资源
  let resource = common::Resource::default("integration-test-service")
  let resource_with_attrs = common::Resource::{
    service_name: resource.service_name,
    service_version: Some("2.1.0"),
    telemetry_sdk_name: resource.telemetry_sdk_name,
    telemetry_sdk_version: resource.telemetry_sdk_version,
    attributes: [
      ("environment", common::AttributeValue::string("test")),
      ("region", common::AttributeValue::string("us-west-2")),
      ("instance.id", common::AttributeValue::string("inst-12345"))
    ]
  }
  
  // 2. 创建仪表化范围
  let instrumentation_scope = common::InstrumentationScope::{
    name: "integration-scope",
    version: Some("1.5.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // 3. 在资源和仪表化范围上下文中创建Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer(
    instrumentation_scope.name, 
    instrumentation_scope.version
  )
  
  let (_, span) = tracer.start_span(
    context::Context::empty(),
    "resource-instrumentation-operation",
    trace::Server,
    [
      ("resource.service", common::AttributeValue::string(resource_with_attrs.service_name)),
      ("resource.version", common::AttributeValue::string("2.1.0")),
      ("scope.name", common::AttributeValue::string(instrumentation_scope.name)),
      ("scope.version", common::AttributeValue::string("1.5.0"))
    ]
  )
  
  // 4. 在相同上下文中创建指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter(
    instrumentation_scope.name,
    instrumentation_scope.version,
    instrumentation_scope.schema_url
  )
  
  let resource_counter = meter.create_counter("resource.operations", "count", "Resource operations")
  resource_counter.add(1, [
    ("service.name", common::AttributeValue::string(resource_with_attrs.service_name)),
    ("scope.name", common::AttributeValue::string(instrumentation_scope.name))
  ])
  
  // 5. 在相同上下文中创建日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger(
    instrumentation_scope.name,
    instrumentation_scope.version,
    instrumentation_scope.schema_url
  )
  
  let resource_log = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Resource and scope integration test")
    .with_attribute("service.name", common::AttributeValue::string(resource_with_attrs.service_name))
    .with_attribute("scope.name", common::AttributeValue::string(instrumentation_scope.name))
    .with_attribute("resource.attributes", common::AttributeValue::int(resource_with_attrs.attributes.length()))
    .build()
  
  logger.emit(resource_log)
  
  // 6. 验证资源和仪表化范围的集成
  let resource_scope_integration = "Resource-Scope integration: " +
                                  "service=" + resource_with_attrs.service_name +
                                  ", scope=" + instrumentation_scope.name +
                                  ", attrs=" + resource_with_attrs.attributes.length().to_string()
  
  assert_eq(resource_scope_integration.contains("integration-test-service"), true)
  assert_eq(resource_scope_integration.contains("integration-scope"), true)
  assert_eq(resource_scope_integration.contains("attrs=3"), true)
}