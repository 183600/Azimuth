// Azimuth Telemetry - Integration Tests
// 跨模块集成测试

test "context_trace_logs_integration" {
  // 测试Context、Trace和Logs模块的集成
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  let user_key = @azimuth.telemetry.api.context.create_key("user")
  let request_key = @azimuth.telemetry.api.context.create_key("request")
  
  // 在Context中设置用户信息和请求信息
  let enriched_context = base_context
    .with_value(user_key, "user123")
    .with_value(request_key, "req-456")
  
  // 创建带有上下文的Span
  let (span_context, root_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    enriched_context,
    "http-request",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("http.method", @azimuth.telemetry.api.common.AttributeValue::string("POST")),
      ("http.url", @azimuth.telemetry.api.common.AttributeValue::string("/api/process"))
    ])
  )
  
  // 验证Context中的值可以正确获取
  assert_eq(span_context.get(user_key), Some("user123"))
  assert_eq(span_context.get(request_key), Some("req-456"))
  
  // 创建与Span关联的LogRecord
  let trace_id = Array::make(16, 1_byte)
  let span_id = Array::make(8, 2_byte)
  
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Processing user request")
    .with_attribute("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user123"))
    .with_attribute("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req-456"))
    .with_attribute("operation", @azimuth.telemetry.api.common.AttributeValue::string("http-request"))
    .build()
  
  // 验证LogRecord与Span的关联信息
  assert_eq(log_record.severity_number, @azimuth.telemetry.api.logs.Info)
  assert_eq(log_record.body, Some("Processing user request"))
  assert_eq(log_record.attributes.length(), 3)
  
  // 验证Span的基本信息
  assert_eq(root_span.name, "http-request")
  assert_eq(root_span.kind, @azimuth.telemetry.api.trace.Server)
  assert_eq(root_span.attributes.length(), 2)
}

test "context_trace_logs_metrics_full_integration" {
  // 测试Context、Trace、Logs和Metrics的完整集成
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 设置Baggage信息
  let baggage = @azimuth.telemetry.api.context.Baggage::empty()
    .with_entry("user.id", "user789")
    .with_entry("session.id", "sess-123")
    .with_entry("region", "us-west-2")
  
  // 在Context中设置业务上下文
  let business_context = base_context
    .with_value(@azimuth.telemetry.api.context.create_key("operation"), "order-processing")
    .with_value(@azimuth.telemetry.api.context.create_key("order.id"), "order-456")
  
  // 创建根Span
  let (order_context, order_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    business_context,
    "order-processing",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("business")),
      ("order.value", @azimuth.telemetry.api.common.AttributeValue::float(299.99))
    ])
  )
  
  // 创建子Span用于数据库操作
  let (db_context, db_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    order_context,
    "database-operation",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("db.operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT")),
      ("db.table", @azimuth.telemetry.api.common.AttributeValue::string("orders"))
    ])
  )
  
  // 创建Metrics仪器
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  let order_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("orders.total", "orders", "Total orders processed")
  let order_value_histogram = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("order.value", "USD", "Order value distribution")
  let db_query_histogram = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("db.query.duration", "ms", "Database query duration")
  
  // 记录指标
  let order_attrs = [
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user789")),
    ("region", @azimuth.telemetry.api.common.AttributeValue::string("us-west-2"))
  ]
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(order_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(299.99, Some(order_attrs))
  
  let db_attrs = [
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("SELECT")),
    ("table", @azimuth.telemetry.api.common.AttributeValue::string("orders"))
  ]
  
  @azimuth.telemetry.api.metrics.NoopHistogram::record(25.5, Some(db_attrs))
  
  // 创建LogRecord
  let success_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Order processed successfully")
    .with_attribute("order.id", @azimuth.telemetry.api.common.AttributeValue::string("order-456"))
    .with_attribute("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user789"))
    .with_attribute("order.value", @azimuth.telemetry.api.common.AttributeValue::float(299.99))
    .with_attribute("db.query.duration", @azimuth.telemetry.api.common.AttributeValue::float(25.5))
    .with_attribute("region", @azimuth.telemetry.api.common.AttributeValue::string("us-west-2"))
    .build()
  
  // 验证所有组件都能正确协同工作
  assert_eq(order_span.name, "order-processing")
  assert_eq(db_span.name, "database-operation")
  assert_eq(success_log.body, Some("Order processed successfully"))
  assert_eq(success_log.attributes.length(), 6)
  
  // 验证Context信息保持一致
  assert_eq(order_context.get(@azimuth.telemetry.api.context.create_key("operation")), Some("order-processing"))
  assert_eq(order_context.get(@azimuth.telemetry.api.context.create_key("order.id")), Some("order-456"))
  assert_eq(db_context.get(@azimuth.telemetry.api.context.create_key("operation")), Some("order-processing"))
  assert_eq(db_context.get(@azimuth.telemetry.api.context.create_key("order.id")), Some("order-456"))
}

test "propagation_context_trace_integration" {
  // 测试Propagation、Context和Trace的集成
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 创建传播器
  let trace_propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator
  let baggage_propagator = @azimuth.telemetry.api.propagation.W3CBaggagePropagator
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建Carrier并注入上下文
  let carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(base_context, carrier)
  
  // 验证注入的header
  let trace_parent = carrier.get(@azimuth.telemetry.api.propagation.TRACE_PARENT_HEADER)
  let baggage = carrier.get(@azimuth.telemetry.api.propagation.BAGGAGE_HEADER)
  
  assert_eq(trace_parent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(baggage, Some("key1=value1,key2=value2"))
  
  // 提取上下文
  let extracted_context = @azimuth.telemetry.api.propagation.CompositePropagator::extract(base_context, carrier)
  
  // 在提取的上下文中创建Span
  let (span_context, span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    extracted_context,
    "remote-operation",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([
      ("remote.service", @azimuth.telemetry.api.common.AttributeValue::string("payment-api")),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("remote-call"))
    ])
  )
  
  // 验证Span创建成功
  assert_eq(span.name, "remote-operation")
  assert_eq(span.kind, @azimuth.telemetry.api.trace.Client)
  assert_eq(span.attributes.length(), 2)
  
  // 验证传播的上下文信息
  assert_eq(span_context.get(@azimuth.telemetry.api.context.create_key("user")), None) // 原始context应该是空的
}

test "error_handling_integration" {
  // 测试跨模块错误处理的集成
  let error_context = @azimuth.telemetry.api.context.Context::empty()
    .with_value(@azimuth.telemetry.api.context.create_key("error.handling"), "true")
  
  // 创建错误处理的Span
  let (error_span_context, error_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    error_context,
    "error-operation",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("error-prone")),
      ("retry.count", @azimuth.telemetry.api.common.AttributeValue::int(3))
    ])
  )
  
  // 创建错误日志
  let error_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Error)
    .severity_text("ERROR")
    .body("Operation failed after retries")
    .with_attribute("error.code", @azimuth.telemetry.api.common.AttributeValue::int(5001))
    .with_attribute("error.type", @azimuth.telemetry.api.common.AttributeValue::string("TimeoutError"))
    .with_attribute("retry.count", @azimuth.telemetry.api.common.AttributeValue::int(3))
    .with_attribute("error.recoverable", @azimuth.telemetry.api.common.AttributeValue::bool(false))
    .build()
  
  // 创建错误指标
  let error_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("errors.total", "errors", "Total errors")
  let retry_histogram = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram("retry.count", "attempts", "Retry attempts distribution")
  
  // 记录错误指标
  let error_attrs = [
    ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(5001)),
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("TimeoutError")),
    ("operation", @azimuth.telemetry.api.common.AttributeValue::string("error-operation"))
  ]
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(error_attrs))
  @azimuth.telemetry.api.metrics.NoopHistogram::record(3.0, Some(error_attrs))
  
  // 验证错误处理的集成
  assert_eq(error_span.name, "error-operation")
  assert_eq(error_log.severity_number, @azimuth.telemetry.api.logs.Error)
  assert_eq(error_log.body, Some("Operation failed after retries"))
  assert_eq(error_log.attributes.length(), 4)
  assert_eq(error_span_context.get(@azimuth.telemetry.api.context.create_key("error.handling")), Some("true"))
}

test "resource_instrumentation_scope_integration" {
  // 测试Resource和InstrumentationScope的跨模块集成
  let resource = @azimuth.telemetry.api.common.Resource::default("integration-test-service")
  let scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "integration-test-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  // 在所有模块中使用相同的Resource和InstrumentationScope
  
  // Trace模块
  let trace_context = @azimuth.telemetry.api.context.Context::empty()
  let (trace_span_context, trace_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    trace_context,
    "resource-aware-span",
    Some(@azimuth.telemetry.api.trace.Server),
    Some([
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string(resource.service_name)),
      ("service.version", @azimuth.telemetry.api.common.AttributeValue::string(resource.telemetry_sdk_version)),
      ("scope.name", @azimuth.telemetry.api.common.AttributeValue::string(scope.name)),
      ("scope.version", @azimuth.telemetry.api.common.AttributeValue::string(scope.version?))
    ])
  )
  
  // Logs模块
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Resource-aware log entry")
    .with_attribute("service.name", @azimuth.telemetry.api.common.AttributeValue::string(resource.service_name))
    .with_attribute("sdk.name", @azimuth.telemetry.api.common.AttributeValue::string(resource.telemetry_sdk_name))
    .with_attribute("scope.name", @azimuth.telemetry.api.common.AttributeValue::string(scope.name))
    .with_attribute("scope.schema", @azimuth.telemetry.api.common.AttributeValue::string(scope.schema_url?))
    .build()
  
  // Metrics模块
  let meter = @azimuth.telemetry.api.metrics.NoopMeter
  let resource_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("operations.total", "1", "Total operations")
  
  let resource_attrs = [
    ("service.name", @azimuth.telemetry.api.common.AttributeValue::string(resource.service_name)),
    ("sdk.version", @azimuth.telemetry.api.common.AttributeValue::string(resource.telemetry_sdk_version)),
    ("scope.name", @azimuth.telemetry.api.common.AttributeValue::string(scope.name))
  ]
  
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, Some(resource_attrs))
  
  // 验证Resource和InstrumentationScope在所有模块中的一致使用
  assert_eq(trace_span.name, "resource-aware-span")
  assert_eq(log_record.body, Some("Resource-aware log entry"))
  assert_eq(log_record.attributes.length(), 4)
  assert_eq(trace_span.attributes.length(), 4)
  
  // 验证Resource信息
  assert_eq(resource.service_name, "integration-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 验证InstrumentationScope信息
  assert_eq(scope.name, "integration-test-scope")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, Some("https://example.com/schema"))
}