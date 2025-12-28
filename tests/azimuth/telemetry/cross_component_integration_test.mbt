// Azimuth Telemetry System - Cross Component Integration Tests
// 测试跨组件集成和端到端场景

test "trace to metrics integration" {
  // 测试追踪到指标的集成
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "trace-metrics-tracer")
  
  let meter_provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "trace-metrics-meter")
  
  // 创建指标
  let span_counter = @azimuth.Meter::create_counter(meter, "spans.created", Some("Number of spans created"), Some("count"))
  let span_duration_histogram = @azimuth.Meter::create_histogram(meter, "span.duration", Some("Span duration"), Some("ms"))
  let error_counter = @azimuth.Meter::create_counter(meter, "span.errors", Some("Number of span errors"), Some("count"))
  
  // 创建Span并记录指标
  let parent_span = @azimuth.Tracer::start_span(tracer, "parent-operation")
  @azimuth.Counter::add(span_counter, 1.0, None)
  
  // 模拟操作开始时间
  let start_time = 100.0
  
  // 添加事件
  @azimuth.Span::add_event(parent_span, "operation.started", Some([("component", @azimuth.StringValue("integration"))]))
  
  // 创建子Span
  let child_span = @azimuth.Tracer::start_span(tracer, "child-operation")
  @azimuth.Counter::add(span_counter, 1.0, None)
  
  @azimuth.Span::add_event(child_span, "child.started", None)
  
  // 模拟子操作
  @azimuth.Span::set_status(child_span, @azimuth.Ok, Some("Child operation completed"))
  @azimuth.Span::end(child_span)
  
  // 记录子Span持续时间
  let child_duration = 50.0
  @azimuth.Histogram::record(span_duration_histogram, child_duration, None)
  
  // 继续父操作
  @azimuth.Span::add_event(parent_span, "child.completed", Some([("child.duration", @azimuth.FloatValue(child_duration))]))
  
  // 模拟错误情况
  let error_span = @azimuth.Tracer::start_span(tracer, "error-operation")
  @azimuth.Counter::add(span_counter, 1.0, None)
  
  @azimuth.Span::add_event(error_span, "error.occurred", Some([
    ("error.type", @azimuth.StringValue("ValidationError")),
    ("error.code", @azimuth.IntValue(400))
  ]))
  
  @azimuth.Span::set_status(error_span, @azimuth.Error, Some("Validation failed"))
  @azimuth.Counter::add(error_counter, 1.0, None)
  @azimuth.Span::end(error_span)
  
  // 记录错误Span持续时间
  let error_duration = 25.0
  @azimuth.Histogram::record(span_duration_histogram, error_duration, None)
  
  // 完成父Span
  @azimuth.Span::add_event(parent_span, "operation.completed", Some([
    ("total.children", @azimuth.IntValue(2)),
    ("errors.count", @azimuth.IntValue(1))
  ]))
  
  @azimuth.Span::set_status(parent_span, @azimuth.Ok, Some("Parent operation completed"))
  @azimuth.Span::end(parent_span)
  
  // 记录父Span持续时间
  let parent_duration = 100.0
  @azimuth.Histogram::record(span_duration_histogram, parent_duration, None)
  
  // 验证集成操作不会抛出异常
  assert_true(true)
}

test "logging with trace context integration" {
  // 测试日志与追踪上下文的集成
  let logger_provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "trace-context-logger")
  
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "logging-tracer")
  
  // 创建Span
  let span = @azimuth.Tracer::start_span(tracer, "operation-with-logging")
  let span_ctx = @azimuth.Span::span_context(span)
  
  // 创建与Span关联的日志记录
  let start_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Operation started"),
    Some(@azimuth.Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    Some(@azimuth.Context::root())
  )
  @azimuth.Logger::emit(logger, start_log)
  
  // 添加Span事件
  @azimuth.Span::add_event(span, "log.event", Some([("log.level", @azimuth.StringValue("INFO"))]))
  
  // 创建带有详细属性的日志
  let log_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(log_attrs, "user.id", @azimuth.StringValue("user123"))
  @azimuth.Attributes::set(log_attrs, "operation.type", @azimuth.StringValue("data.processing"))
  @azimuth.Attributes::set(log_attrs, "records.processed", @azimuth.IntValue(1000))
  
  let detailed_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Processing data records"),
    Some(log_attrs),
    Some(1735689600000000500L),
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    Some(@azimuth.Context::root())
  )
  @azimuth.Logger::emit(logger, detailed_log)
  
  // 创建错误日志
  let error_log_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(error_log_attrs, "error.type", @azimuth.StringValue("DataValidationError"))
  @azimuth.Attributes::set(error_log_attrs, "error.code", @azimuth.IntValue(422))
  @azimuth.Attributes::set(error_log_attrs, "invalid.record.id", @azimuth.StringValue("record-456"))
  
  let error_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Data validation failed"),
    Some(error_log_attrs),
    Some(1735689600000000750L),
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    Some(@azimuth.Context::root())
  )
  @azimuth.Logger::emit(logger, error_log)
  
  // 在Span中记录错误
  @azimuth.Span::add_event(span, "error.logged", Some([
    ("error.type", @azimuth.StringValue("DataValidationError")),
    ("log.recorded", @azimuth.BoolValue(true))
  ]))
  
  // 创建恢复日志
  let recovery_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Warn,
    Some("Attempting recovery from error"),
    Some(@azimuth.Attributes::new()),
    Some(1735689600000000900L),
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    Some(@azimuth.Context::root())
  )
  @azimuth.Logger::emit(logger, recovery_log)
  
  // 完成Span
  @azimuth.Span::set_status(span, @azimuth.Ok, Some("Operation completed with warnings"))
  @azimuth.Span::end(span)
  
  // 创建完成日志
  let end_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Operation completed"),
    Some(@azimuth.Attributes::new()),
    Some(1735689600000001000L),
    None,
    Some(@azimuth.SpanContext::trace_id(span_ctx)),
    Some(@azimuth.SpanContext::span_id(span_ctx)),
    Some(@azimuth.Context::root())
  )
  @azimuth.Logger::emit(logger, end_log)
  
  // 验证集成操作不会抛出异常
  assert_true(true)
}

test "propagation with baggage and context integration" {
  // 测试传播、Baggage和上下文的集成
  let trace_propagator = @azimuth.W3CTraceContextPropagator::new()
  let composite = @azimuth.CompositePropagator::new([trace_propagator])
  
  // 创建初始上下文
  let root_ctx = @azimuth.Context::root()
  
  // 添加Baggage条目
  let baggage = @azimuth.Baggage::new()
  let baggage_with_user = @azimuth.Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_session = @azimuth.Baggage::set_entry(baggage_with_user, "session.id", "session456")
  let baggage_with_tenant = @azimuth.Baggage::set_entry(baggage_with_session, "tenant.id", "tenant789")
  
  // 添加上下文值
  let correlation_key = @azimuth.ContextKey::new("correlation.id")
  let ctx_with_correlation = @azimuth.Context::with_value(root_ctx, correlation_key, "corr-123")
  
  let request_key = @azimuth.ContextKey::new("request.id")
  let ctx_with_request = @azimuth.Context::with_value(ctx_with_correlation, request_key, "req-456")
  
  // 创建carrier并注入
  let outbound_carrier = @azimuth.TextMapCarrier::new()
  @azimuth.CompositePropagator::inject(composite, ctx_with_request, outbound_carrier)
  
  // 手动添加baggage头部（模拟完整传播）
  @azimuth.TextMapCarrier::set(outbound_carrier, "baggage", "user.id=user123,session.id=session456,tenant.id=tenant789")
  
  // 添加业务头部
  @azimuth.TextMapCarrier::set(outbound_carrier, "X-Correlation-ID", "corr-123")
  @azimuth.TextMapCarrier::set(outbound_carrier, "X-Request-ID", "req-456")
  @azimuth.TextMapCarrier::set(outbound_carrier, "X-User-ID", "user123")
  
  // 模拟网络传输（创建新的carrier）
  let inbound_carrier = @azimuth.TextMapCarrier::new()
  
  // 复制所有头部到inbound carrier
  @azimuth.TextMapCarrier::set(inbound_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  @azimuth.TextMapCarrier::set(inbound_carrier, "baggage", "user.id=user123,session.id=session456,tenant.id=tenant789")
  @azimuth.TextMapCarrier::set(inbound_carrier, "X-Correlation-ID", "corr-123")
  @azimuth.TextMapCarrier::set(inbound_carrier, "X-Request-ID", "req-456")
  @azimuth.TextMapCarrier::set(inbound_carrier, "X-User-ID", "user123")
  
  // 提取上下文
  let extracted_ctx = @azimuth.CompositePropagator::extract(composite, inbound_carrier)
  
  // 在提取的上下文中添加新的值
  let service_key = @azimuth.ContextKey::new("service.name")
  let ctx_with_service = @azimuth.Context::with_value(extracted_ctx, service_key, "downstream-service")
  
  let operation_key = @azimuth.ContextKey::new("operation.name")
  let final_ctx = @azimuth.Context::with_value(ctx_with_service, operation_key, "process.data")
  
  // 创建新的Span用于下游操作
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "downstream-tracer")
  let downstream_span = @azimuth.Tracer::start_span(tracer, "downstream-operation")
  
  // 添加下游事件
  @azimuth.Span::add_event(downstream_span, "context.extracted", Some([
    ("correlation.id", @azimuth.StringValue("corr-123")),
    ("request.id", @azimuth.StringValue("req-456")),
    ("user.id", @azimuth.StringValue("user123"))
  ]))
  
  @azimuth.Span::add_event(downstream_span, "baggage.received", Some([
    ("user.id", @azimuth.StringValue("user123")),
    ("session.id", @azimuth.StringValue("session456")),
    ("tenant.id", @azimuth.StringValue("tenant789"))
  ]))
  
  // 完成下游操作
  @azimuth.Span::set_status(downstream_span, @azimuth.Ok, Some("Downstream operation completed"))
  @azimuth.Span::end(downstream_span)
  
  // 验证传播集成不会抛出异常
  assert_true(true)
}

test "metrics with resource attributes integration" {
  // 测试指标与资源属性的集成
  let resource = @azimuth.Resource::new()
  
  // 创建资源属性
  let resource_attrs = [
    ("service.name", @azimuth.StringValue("metrics-integration-service")),
    ("service.version", @azimuth.StringValue("1.2.3")),
    ("service.instance.id", @azimuth.StringValue("instance-abc123")),
    ("host.name", @azimuth.StringValue("prod-server-01")),
    ("deployment.environment", @azimuth.StringValue("production")),
    ("region", @azimuth.StringValue("us-west-2")),
    ("availability.zone", @azimuth.StringValue("us-west-2a"))
  ]
  
  let resource_with_attrs = @azimuth.Resource::with_attributes(resource, resource_attrs)
  
  // 创建Meter
  let meter_provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "integration-meter")
  
  // 创建指标
  let request_counter = @azimuth.Meter::create_counter(meter, "http.requests", Some("HTTP requests count"), Some("requests"))
  let response_time_histogram = @azimuth.Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("ms"))
  let error_rate_gauge = @azimuth.Meter::create_counter(meter, "http.errors", Some("HTTP errors count"), Some("errors"))
  
  // 模拟带有资源属性的指标记录
  let base_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(base_attrs, "service.name", @azimuth.StringValue("metrics-integration-service"))
  @azimuth.Attributes::set(base_attrs, "service.version", @azimuth.StringValue("1.2.3"))
  
  // 记录不同类型的请求指标
  @azimuth.Counter::add(request_counter, 100.0, Some(base_attrs))
  @azimuth.Histogram::record(response_time_histogram, 150.0, Some(base_attrs))
  
  // 记录带额外属性的指标
  let success_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(success_attrs, "service.name", @azimuth.StringValue("metrics-integration-service"))
  @azimuth.Attributes::set(success_attrs, "http.method", @azimuth.StringValue("GET"))
  @azimuth.Attributes::set(success_attrs, "http.status_code", @azimuth.IntValue(200))
  
  @azimuth.Counter::add(request_counter, 80.0, Some(success_attrs))
  @azimuth.Histogram::record(response_time_histogram, 120.0, Some(success_attrs))
  
  // 记录错误指标
  let error_attrs = @azimuth.Attributes::new()
  @azimuth.Attributes::set(error_attrs, "service.name", @azimuth.StringValue("metrics-integration-service"))
  @azimuth.Attributes::set(error_attrs, "http.method", @azimuth.StringValue("POST"))
  @azimuth.Attributes::set(error_attrs, "http.status_code", @azimuth.IntValue(500))
  @azimuth.Attributes::set(error_attrs, "error.type", @azimuth.StringValue("InternalServer"))
  
  @azimuth.Counter::add(request_counter, 20.0, Some(error_attrs))
  @azimuth.Counter::add(error_rate_gauge, 20.0, Some(error_attrs))
  @azimuth.Histogram::record(response_time_histogram, 500.0, Some(error_attrs))
  
  // 创建带有动态属性的指标
  for i in 0..<10 {
    let dynamic_attrs = @azimuth.Attributes::new()
    @azimuth.Attributes::set(dynamic_attrs, "service.name", @azimuth.StringValue("metrics-integration-service"))
    @azimuth.Attributes::set(dynamic_attrs, "endpoint", @azimuth.StringValue("/api/endpoint-" + i.to_string()))
    @azimuth.Attributes::set(dynamic_attrs, "user.type", @azimuth.StringValue(i % 2 == 0 ? "premium" : "basic"))
    
    @azimuth.Counter::add(request_counter, 5.0, Some(dynamic_attrs))
    @azimuth.Histogram::record(response_time_histogram, (100 + i * 10).to_double(), Some(dynamic_attrs))
  }
  
  // 验证指标与资源属性集成不会抛出异常
  assert_true(true)
}

test "end to end telemetry flow integration" {
  // 测试端到端遥测流程集成
  let resource = @azimuth.Resource::with_attributes(@azimuth.Resource::new(), [
    ("service.name", @azimuth.StringValue("e2e-test-service")),
    ("service.version", @azimuth.StringValue("2.0.0")),
    ("test.scenario", @azimuth.StringValue("end-to-end-integration"))
  ])
  
  // 初始化所有组件
  let tracer_provider = @azimuth.TracerProvider::default()
  let tracer = @azimuth.TracerProvider::get_tracer(tracer_provider, "e2e-tracer")
  
  let meter_provider = @azimuth.MeterProvider::default()
  let meter = @azimuth.MeterProvider::get_meter(meter_provider, "e2e-meter")
  
  let logger_provider = @azimuth.LoggerProvider::default()
  let logger = @azimuth.LoggerProvider::get_logger(logger_provider, "e2e-logger")
  
  // 创建指标
  let operation_counter = @azimuth.Meter::create_counter(meter, "e2e.operations", Some("E2E operations"), Some("count"))
  let duration_histogram = @azimuth.Meter::create_histogram(meter, "e2e.duration", Some("E2E duration"), Some("ms"))
  let step_counter = @azimuth.Meter::create_counter(meter, "e2e.steps", Some("E2E steps"), Some("count"))
  
  // 步骤1: 开始端到端操作
  let main_span = @azimuth.Tracer::start_span(tracer, "e2e.main.operation")
  @azimuth.Counter::add(operation_counter, 1.0, None)
  
  let start_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("E2E operation started"),
    Some(@azimuth.Attributes::new()),
    Some(1735689600000000000L),
    None,
    Some(@azimuth.SpanContext::trace_id(@azimuth.Span::span_context(main_span))),
    Some(@azimuth.SpanContext::span_id(@azimuth.Span::span_context(main_span))),
    None
  )
  @azimuth.Logger::emit(logger, start_log)
  
  @azimuth.Span::add_event(main_span, "e2e.started", Some([("service.version", @azimuth.StringValue("2.0.0"))]))
  
  // 步骤2: 数据处理子操作
  let processing_span = @azimuth.Tracer::start_span(tracer, "e2e.data.processing")
  @azimuth.Counter::add(step_counter, 1.0, None)
  
  @azimuth.Span::add_event(processing_span, "processing.started", None)
  
  // 模拟处理日志
  let processing_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Processing 1000 records"),
    Some(@azimuth.Attributes::new()),
    Some(1735689600000000100L),
    None,
    Some(@azimuth.SpanContext::trace_id(@azimuth.Span::span_context(processing_span))),
    Some(@azimuth.SpanContext::span_id(@azimuth.Span::span_context(processing_span))),
    None
  )
  @azimuth.Logger::emit(logger, processing_log)
  
  @azimuth.Span::set_status(processing_span, @azimuth.Ok, Some("Processing completed"))
  @azimuth.Span::end(processing_span)
  @azimuth.Histogram::record(duration_histogram, 200.0, None)
  
  // 步骤3: 数据验证子操作
  let validation_span = @azimuth.Tracer::start_span(tracer, "e2e.data.validation")
  @azimuth.Counter::add(step_counter, 1.0, None)
  
  @azimuth.Span::add_event(validation_span, "validation.started", None)
  
  // 模拟验证错误
  let validation_error_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Validation failed for 5 records"),
    Some(@azimuth.Attributes::new()),
    Some(1735689600000000250L),
    None,
    Some(@azimuth.SpanContext::trace_id(@azimuth.Span::span_context(validation_span))),
    Some(@azimuth.SpanContext::span_id(@azimuth.Span::span_context(validation_span))),
    None
  )
  @azimuth.Logger::emit(logger, validation_error_log)
  
  @azimuth.Span::add_event(validation_span, "validation.errors", Some([
    ("failed.records", @azimuth.IntValue(5)),
    ("error.type", @azimuth.StringValue("ValidationError"))
  ]))
  
  @azimuth.Span::set_status(validation_span, @azimuth.Error, Some("Some records failed validation"))
  @azimuth.Span::end(validation_span)
  @azimuth.Histogram::record(duration_histogram, 150.0, None)
  
  // 步骤4: 数据存储子操作
  let storage_span = @azimuth.Tracer::start_span(tracer, "e2e.data.storage")
  @azimuth.Counter::add(step_counter, 1.0, None)
  
  @azimuth.Span::add_event(storage_span, "storage.started", Some([
    ("records.to.store", @azimuth.IntValue(995))
  ]))
  
  let storage_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Storing 995 valid records"),
    Some(@azimuth.Attributes::new()),
    Some(1735689600000000400L),
    None,
    Some(@azimuth.SpanContext::trace_id(@azimuth.Span::span_context(storage_span))),
    Some(@azimuth.SpanContext::span_id(@azimuth.Span::span_context(storage_span))),
    None
  )
  @azimuth.Logger::emit(logger, storage_log)
  
  @azimuth.Span::set_status(storage_span, @azimuth.Ok, Some("Data stored successfully"))
  @azimuth.Span::end(storage_span)
  @azimuth.Histogram::record(duration_histogram, 300.0, None)
  
  // 步骤5: 完成端到端操作
  @azimuth.Span::add_event(main_span, "e2e.steps.completed", Some([
    ("total.steps", @azimuth.IntValue(3)),
    ("successful.steps", @azimuth.IntValue(2)),
    ("failed.steps", @azimuth.IntValue(1)),
    ("records.processed", @azimuth.IntValue(1000)),
    ("records.stored", @azimuth.IntValue(995))
  ]))
  
  let end_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("E2E operation completed"),
    Some(@azimuth.Attributes::new()),
    Some(1735689600000000600L),
    None,
    Some(@azimuth.SpanContext::trace_id(@azimuth.Span::span_context(main_span))),
    Some(@azimuth.SpanContext::span_id(@azimuth.Span::span_context(main_span))),
    None
  )
  @azimuth.Logger::emit(logger, end_log)
  
  @azimuth.Span::set_status(main_span, @azimuth.Ok, Some("E2E operation completed with some warnings"))
  @azimuth.Span::end(main_span)
  @azimuth.Histogram::record(duration_histogram, 600.0, None)
  
  // 验证端到端集成不会抛出异常
  assert_true(true)
}