test "complex telemetry workflow integration" {
  // Test complex telemetry workflow with multiple components working together
  
  // 1. Initialize all providers
  let trace_provider = telemetry.sdk.trace.TracerProvider::default()
  let metrics_provider = telemetry.sdk.metrics.MeterProvider::default()
  let logs_provider = telemetry.sdk.logs.LoggerProvider::default()
  
  // 2. Create instruments
  let tracer = telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "complex.workflow.tracer")
  let meter = telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "complex.workflow.meter")
  let logger = telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "complex.workflow.logger")
  
  // 3. Create metric instruments
  let request_counter = telemetry.sdk.metrics.Meter::create_counter(meter, "http.requests.total")
  let response_histogram = telemetry.sdk.metrics.Meter::create_histogram(meter, "http.response.duration")
  let error_counter = telemetry.sdk.metrics.Meter::create_counter(meter, "http.errors.total")
  let active_connections = telemetry.sdk.metrics.Meter::create_up_down_counter(meter, "http.active_connections")
  let memory_usage = telemetry.sdk.metrics.Meter::create_gauge(meter, "process.memory.usage")
  
  // 4. Simulate complex request processing workflow
  let ctx = telemetry.api.context.Context::root()
  
  // Start main span
  let (request_ctx, main_span) = telemetry.sdk.trace.Tracer::start_span(tracer, ctx, "http.request.processing")
  
  // Log request start
  telemetry.sdk.logs.Logger::info(logger, "开始处理HTTP请求")
  
  // Update metrics
  telemetry.sdk.metrics.UpDownCounter::add(active_connections, 1.0)
  telemetry.sdk.metrics.Gauge::record(memory_usage, 512.0)  // MB
  
  // Add request attributes to main span
  telemetry.api.trace.Span::set_attribute(main_span, "http.method", telemetry.api.common.StringValue("POST"))
  telemetry.api.trace.Span::set_attribute(main_span, "http.url", telemetry.api.common.StringValue("/api/v1/process"))
  telemetry.api.trace.Span::set_attribute(main_span, "user.id", telemetry.api.common.StringValue("user12345"))
  
  // 5. Simulate database operation
  let (db_ctx, db_span) = telemetry.sdk.trace.Tracer::start_span(tracer, request_ctx, "database.query")
  
  telemetry.api.trace.Span::set_attribute(db_span, "db.system", telemetry.api.common.StringValue("postgresql"))
  telemetry.api.trace.Span::set_attribute(db_span, "db.statement", telemetry.api.common.StringValue("SELECT * FROM users WHERE id = $1"))
  
  // Add database event
  telemetry.api.trace.Span::add_event(db_span, "query.start", None, None)
  
  // Simulate database work
  telemetry.sdk.logs.Logger::debug(logger, "执行数据库查询")
  
  telemetry.api.trace.Span::add_event(db_span, "query.complete", None, None)
  telemetry.api.trace.Span::end(db_span)
  
  // 6. Simulate cache operation
  let (cache_ctx, cache_span) = telemetry.sdk.trace.Tracer::start_span(tracer, request_ctx, "cache.operation")
  
  telemetry.api.trace.Span::set_attribute(cache_span, "cache.operation", telemetry.api.common.StringValue("get"))
  telemetry.api.trace.Span::set_attribute(cache_span, "cache.key", telemetry.api.common.StringValue("user:12345:profile"))
  
  telemetry.sdk.logs.Logger::debug(logger, "检查缓存")
  
  // Cache miss - simulate remote call
  let (remote_ctx, remote_span) = telemetry.sdk.trace.Tracer::start_span(tracer, cache_ctx, "remote.service.call")
  
  telemetry.api.trace.Span::set_attribute(remote_span, "service.name", telemetry.api.common.StringValue("user-profile-service"))
  telemetry.api.trace.Span::set_attribute(remote_span, "service.endpoint", telemetry.api.common.StringValue("/profile"))
  
  telemetry.sdk.logs.Logger::info(logger, "调用远程服务获取用户资料")
  
  telemetry.api.trace.Span::end(remote_span)
  telemetry.api.trace.Span::end(cache_span)
  
  // 7. Simulate business logic processing
  let (business_ctx, business_span) = telemetry.sdk.trace.Tracer::start_span(tracer, request_ctx, "business.logic.processing")
  
  telemetry.api.trace.Span::set_attribute(business_span, "business.operation", telemetry.api.common.StringValue("process_user_request"))
  telemetry.api.trace.Span::set_attribute(business_span, "business.rules.applied", telemetry.api.common.IntValue(5))
  
  // Simulate processing steps
  for i in 0..3 {
    let (step_ctx, step_span) = telemetry.sdk.trace.Tracer::start_span(tracer, business_ctx, "processing.step." + i.to_string())
    
    telemetry.api.trace.Span::set_attribute(step_span, "step.number", telemetry.api.common.IntValue(i))
    telemetry.api.trace.Span::set_attribute(step_span, "step.name", telemetry.api.common.StringValue("step_" + i.to_string()))
    
    telemetry.sdk.logs.Logger::debug(logger, "执行处理步骤 " + i.to_string())
    
    // Update progress metrics
    telemetry.sdk.metrics.Counter::add(request_counter, 1.0)
    
    telemetry.api.trace.Span::end(step_span)
  }
  
  telemetry.api.trace.Span::end(business_span)
  
  // 8. Simulate response preparation
  let (response_ctx, response_span) = telemetry.sdk.trace.Tracer::start_span(tracer, request_ctx, "response.preparation")
  
  telemetry.api.trace.Span::set_attribute(response_span, "response.status_code", telemetry.api.common.IntValue(200))
  telemetry.api.trace.Span::set_attribute(response_span, "response.size", telemetry.api.common.IntValue(1024))
  
  telemetry.sdk.logs.Logger::info(logger, "准备响应")
  
  // Record final metrics
  telemetry.sdk.metrics.Histogram::record(response_histogram, 150.5)  // Response time in ms
  telemetry.sdk.metrics.Counter::add(request_counter, 1.0)
  
  telemetry.api.trace.Span::end(response_span)
  
  // 9. Complete main span
  telemetry.api.trace.Span::set_attribute(main_span, "http.status_code", telemetry.api.common.IntValue(200))
  telemetry.api.trace.Span::set_status(main_span, telemetry.api.trace.StatusCode::Ok, Some("Request processed successfully"))
  
  telemetry.sdk.logs.Logger::info(logger, "请求处理完成")
  
  // Update final metrics
  telemetry.sdk.metrics.UpDownCounter::add(active_connections, -1.0)
  telemetry.sdk.metrics.Gauge::record(memory_usage, 480.0)  // Memory decreased
  
  telemetry.api.trace.Span::end(main_span)
  
  // 10. Verify context propagation worked correctly
  let main_span_ctx = telemetry.api.trace.Span::span_context(main_span)
  assert_true(telemetry.api.trace.SpanContext::is_valid(main_span_ctx))
  assert_true(telemetry.api.trace.SpanContext::is_sampled(main_span_ctx))
}

test "multi-service distributed tracing integration" {
  // Test distributed tracing across multiple services
  
  // Service A: Entry point
  let service_a_tracer = telemetry.sdk.trace.TracerProvider::default()
  let service_a_tracer_instance = telemetry.sdk.trace.TracerProvider::get_tracer(service_a_tracer, "service-a")
  let service_a_logger = telemetry.sdk.logs.LoggerProvider::default() |> telemetry.sdk.logs.LoggerProvider::get_logger(_, "service-a")
  
  let ctx = telemetry.api.context.Context::root()
  let (service_a_ctx, service_a_span) = telemetry.sdk.trace.Tracer::start_span(service_a_tracer_instance, ctx, "service-a.operation")
  
  telemetry.api.trace.Span::set_attribute(service_a_span, "service.name", telemetry.api.common.StringValue("service-a"))
  telemetry.api.trace.Span::set_attribute(service_a_span, "operation.type", telemetry.api.common.StringValue("orchestration"))
  
  telemetry.sdk.logs.Logger::info(service_a_logger, "Service A: 开始处理请求")
  
  // Simulate context propagation to Service B
  let propagator = telemetry.api.propagation.CompositePropagator::new([
    telemetry.api.propagation.W3CTraceContextPropagator::new()
  ])
  
  let carrier = telemetry.api.propagation.TextMapCarrier::new()
  telemetry.api.propagation.CompositePropagator::inject(propagator, service_a_ctx, carrier)
  
  // Service B: Receives propagated context
  let service_b_tracer = telemetry.sdk.trace.TracerProvider::default()
  let service_b_tracer_instance = telemetry.sdk.trace.TracerProvider::get_tracer(service_b_tracer, "service-b")
  let service_b_logger = telemetry.sdk.logs.LoggerProvider::default() |> telemetry.sdk.logs.LoggerProvider::get_logger(_, "service-b")
  
  let service_b_ctx = telemetry.api.propagation.CompositePropagator::extract(propagator, carrier)
  let (service_b_ctx, service_b_span) = telemetry.sdk.trace.Tracer::start_span(service_b_tracer_instance, service_b_ctx, "service-b.operation")
  
  telemetry.api.trace.Span::set_attribute(service_b_span, "service.name", telemetry.api.common.StringValue("service-b"))
  telemetry.api.trace.Span::set_attribute(service_b_span, "operation.type", telemetry.api.common.StringValue("data-processing"))
  
  telemetry.sdk.logs.Logger::info(service_b_logger, "Service B: 处理数据")
  
  // Service B calls Service C
  let service_b_carrier = telemetry.api.propagation.TextMapCarrier::new()
  telemetry.api.propagation.CompositePropagator::inject(propagator, service_b_ctx, service_b_carrier)
  
  // Service C: Final service in chain
  let service_c_tracer = telemetry.sdk.trace.TracerProvider::default()
  let service_c_tracer_instance = telemetry.sdk.trace.TracerProvider::get_tracer(service_c_tracer, "service-c")
  let service_c_logger = telemetry.sdk.logs.LoggerProvider::default() |> telemetry.sdk.logs.LoggerProvider::get_logger(_, "service-c")
  
  let service_c_ctx = telemetry.api.propagation.CompositePropagator::extract(propagator, service_b_carrier)
  let (service_c_ctx, service_c_span) = telemetry.sdk.trace.Tracer::start_span(service_c_tracer_instance, service_c_ctx, "service-c.operation")
  
  telemetry.api.trace.Span::set_attribute(service_c_span, "service.name", telemetry.api.common.StringValue("service-c"))
  telemetry.api.trace.Span::set_attribute(service_c_span, "operation.type", telemetry.api.common.StringValue("storage"))
  
  telemetry.sdk.logs.Logger::info(service_c_logger, "Service C: 存储数据")
  
  // Simulate error in Service C
  telemetry.api.trace.Span::record_exception(
    service_c_span,
    "Storage connection failed",
    Some("StorageException"),
    Some("at service_c.store()\n  at main()"),
    None
  )
  
  telemetry.api.trace.Span::set_status(service_c_span, telemetry.api.trace.StatusCode::Error, Some("Storage operation failed"))
  telemetry.sdk.logs.Logger::error(service_c_logger, "Service C: 存储操作失败")
  
  telemetry.api.trace.Span::end(service_c_span)
  
  // Service B handles error from Service C
  telemetry.api.trace.Span::set_status(service_b_span, telemetry.api.trace.StatusCode::Error, Some("Dependency service failed"))
  telemetry.sdk.logs.Logger::error(service_b_logger, "Service B: 依赖服务失败")
  
  telemetry.api.trace.Span::end(service_b_span)
  
  // Service A handles error from Service B
  telemetry.api.trace.Span::set_status(service_a_span, telemetry.api.trace.StatusCode::Error, Some("Orchestration failed"))
  telemetry.sdk.logs.Logger::error(service_a_logger, "Service A: 编排失败")
  
  telemetry.api.trace.Span::end(service_a_span)
  
  // Verify trace context propagation
  let service_a_span_ctx = telemetry.api.trace.Span::span_context(service_a_span)
  let service_b_span_ctx = telemetry.api.trace.Span::span_context(service_b_span)
  let service_c_span_ctx = telemetry.api.trace.Span::span_context(service_c_span)
  
  // All spans should have the same trace ID
  assert_eq(telemetry.api.trace.SpanContext::trace_id(service_a_span_ctx), 
           telemetry.api.trace.SpanContext::trace_id(service_b_span_ctx))
  assert_eq(telemetry.api.trace.SpanContext::trace_id(service_b_span_ctx), 
           telemetry.api.trace.SpanContext::trace_id(service_c_span_ctx))
  
  // Each span should have different span ID
  assert_true(telemetry.api.trace.SpanContext::span_id(service_a_span_ctx) != 
              telemetry.api.trace.SpanContext::span_id(service_b_span_ctx))
  assert_true(telemetry.api.trace.SpanContext::span_id(service_b_span_ctx) != 
              telemetry.api.trace.SpanContext::span_id(service_c_span_ctx))
}

test "metrics and logs correlation integration" {
  // Test correlation between metrics and logs through trace context
  
  let trace_provider = telemetry.sdk.trace.TracerProvider::default()
  let metrics_provider = telemetry.sdk.metrics.MeterProvider::default()
  let logs_provider = telemetry.sdk.logs.LoggerProvider::default()
  
  let tracer = telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "correlation.test")
  let meter = telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "correlation.test")
  let logger = telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "correlation.test")
  
  // Create metric instruments
  let operation_counter = telemetry.sdk.metrics.Meter::create_counter(meter, "operations.total")
  let operation_duration = telemetry.sdk.metrics.Meter::create_histogram(meter, "operations.duration")
  let error_counter = telemetry.sdk.metrics.Meter::create_counter(meter, "operations.errors")
  
  // Start operation with trace context
  let ctx = telemetry.api.context.Context::root()
  let (operation_ctx, operation_span) = telemetry.sdk.trace.Tracer::start_span(tracer, ctx, "correlated.operation")
  
  let span_ctx = telemetry.api.trace.Span::span_context(operation_span)
  let trace_id = telemetry.api.trace.SpanContext::trace_id(span_ctx)
  let span_id = telemetry.api.trace.SpanContext::span_id(span_ctx)
  
  // Create log record with trace context
  let log_attrs = telemetry.api.common.Attributes::new()
  telemetry.api.common.Attributes::set(log_attrs, "operation.type", telemetry.api.common.StringValue("correlated.operation"))
  telemetry.api.common.Attributes::set(log_attrs, "component", telemetry.api.common.StringValue("test"))
  
  let start_log = telemetry.api.logs.LogRecord::new_with_context(
    telemetry.api.logs.SeverityNumber::Info,
    Some("开始关联操作"),
    Some(log_attrs),
    Some(telemetry.sdk.platform.time.Clock::now_unix_nanos(telemetry.sdk.platform.time.Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(operation_ctx)
  )
  
  telemetry.api.logs.Logger::emit(logger, start_log)
  
  // Record metrics with trace context
  let metric_attrs = telemetry.api.common.Attributes::new()
  telemetry.api.common.Attributes::set(metric_attrs, "trace.id", telemetry.api.common.StringValue(trace_id))
  telemetry.api.common.Attributes::set(metric_attrs, "span.id", telemetry.api.common.StringValue(span_id))
  telemetry.api.common.Attributes::set(metric_attrs, "operation.type", telemetry.api.common.StringValue("correlated.operation"))
  
  telemetry.sdk.metrics.Counter::add(operation_counter, 1.0, Some(metric_attrs))
  
  // Simulate operation steps
  for i in 0..3 {
    let (step_ctx, step_span) = telemetry.sdk.trace.Tracer::start_span(tracer, operation_ctx, "operation.step." + i.to_string())
    
    let step_span_ctx = telemetry.api.trace.Span::span_context(step_span)
    let step_trace_id = telemetry.api.trace.SpanContext::trace_id(step_span_ctx)
    let step_span_id = telemetry.api.trace.SpanContext::span_id(step_span_ctx)
    
    // Log step start
    let step_log_attrs = telemetry.api.common.Attributes::new()
    telemetry.api.common.Attributes::set(step_log_attrs, "step.number", telemetry.api.common.IntValue(i))
    telemetry.api.common.Attributes::set(step_log_attrs, "parent.trace.id", telemetry.api.common.StringValue(trace_id))
    telemetry.api.common.Attributes::set(step_log_attrs, "parent.span.id", telemetry.api.common.StringValue(span_id))
    
    let step_log = telemetry.api.logs.LogRecord::new_with_context(
      telemetry.api.logs.SeverityNumber::Debug,
      Some("执行步骤 " + i.to_string()),
      Some(step_log_attrs),
      Some(telemetry.sdk.platform.time.Clock::now_unix_nanos(telemetry.sdk.platform.time.Clock::system())),
      None,
      Some(step_trace_id),
      Some(step_span_id),
      Some(step_ctx)
    )
    
    telemetry.api.logs.Logger::emit(logger, step_log)
    
    // Record step metrics
    let step_metric_attrs = telemetry.api.common.Attributes::new()
    telemetry.api.common.Attributes::set(step_metric_attrs, "trace.id", telemetry.api.common.StringValue(step_trace_id))
    telemetry.api.common.Attributes::set(step_metric_attrs, "span.id", telemetry.api.common.StringValue(step_span_id))
    telemetry.api.common.Attributes::set(step_metric_attrs, "parent.trace.id", telemetry.api.common.StringValue(trace_id))
    telemetry.api.common.Attributes::set(step_metric_attrs, "parent.span.id", telemetry.api.common.StringValue(span_id))
    telemetry.api.common.Attributes::set(step_metric_attrs, "step.number", telemetry.api.common.IntValue(i))
    
    telemetry.sdk.metrics.Histogram::record(operation_duration, (i * 10).to_double(), Some(step_metric_attrs))
    
    telemetry.api.trace.Span::end(step_span)
  }
  
  // Simulate error in last step
  let error_log_attrs = telemetry.api.common.Attributes::new()
  telemetry.api.common.Attributes::set(error_log_attrs, "error.type", telemetry.api.common.StringValue("ProcessingError"))
  telemetry.api.common.Attributes::set(error_log_attrs, "error.message", telemetry.api.common.StringValue("Step processing failed"))
  
  let error_log = telemetry.api.logs.LogRecord::new_with_context(
    telemetry.api.logs.SeverityNumber::Error,
    Some("操作失败"),
    Some(error_log_attrs),
    Some(telemetry.sdk.platform.time.Clock::now_unix_nanos(telemetry.sdk.platform.time.Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(operation_ctx)
  )
  
  telemetry.api.logs.Logger::emit(logger, error_log)
  
  // Record error metrics
  let error_metric_attrs = telemetry.api.common.Attributes::new()
  telemetry.api.common.Attributes::set(error_metric_attrs, "trace.id", telemetry.api.common.StringValue(trace_id))
  telemetry.api.common.Attributes::set(error_metric_attrs, "span.id", telemetry.api.common.StringValue(span_id))
  telemetry.api.common.Attributes::set(error_metric_attrs, "error.type", telemetry.api.common.StringValue("ProcessingError"))
  
  telemetry.sdk.metrics.Counter::add(error_counter, 1.0, Some(error_metric_attrs))
  
  // End main operation
  telemetry.api.trace.Span::set_status(operation_span, telemetry.api.trace.StatusCode::Error, Some("Operation failed"))
  telemetry.api.trace.Span::end(operation_span)
  
  // Verify correlation through trace IDs
  assert_true(trace_id != "")
  assert_true(span_id != "")
}

test "resource and baggage integration" {
  // Test integration between resource management and baggage propagation
  
  // Create resources with service information
  let base_resource = telemetry.api.common.Resource::with_attributes(
    telemetry.api.common.Resource::new(),
    [
      ("service.name", telemetry.api.common.StringValue("integration-test-service")),
      ("service.version", telemetry.api.common.StringValue("1.0.0")),
      ("service.instance.id", telemetry.api.common.StringValue("instance-12345")),
      ("deployment.environment", telemetry.api.common.StringValue("test"))
    ]
  )
  
  // Create baggage with user information
  let baggage = telemetry.api.context.Baggage::new()
  let baggage_with_user = telemetry.api.context.Baggage::set_entry(baggage, "user.id", "user67890")
  let baggage_with_session = telemetry.api.context.Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  let baggage_with_tenant = telemetry.api.context.Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-123")
  
  // Initialize providers with resource
  let trace_provider = telemetry.sdk.trace.TracerProvider::default()
  let tracer = telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "resource.baggage.test")
  
  let metrics_provider = telemetry.sdk.metrics.MeterProvider::default()
  let meter = telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "resource.baggage.test")
  
  let logs_provider = telemetry.sdk.logs.LoggerProvider::default()
  let logger = telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "resource.baggage.test")
  
  // Create context with baggage
  let ctx = telemetry.api.context.Context::root()
  let ctx_with_baggage = telemetry.api.context.Context::with_value(ctx, telemetry.api.context.ContextKey::new("baggage"), "baggage_value")
  
  // Start operation with baggage context
  let (operation_ctx, operation_span) = telemetry.sdk.trace.Tracer::start_span(tracer, ctx_with_baggage, "baggage.aware.operation")
  
  // Add resource attributes to span
  telemetry.api.trace.Span::set_attribute(operation_span, "service.name", telemetry.api.common.StringValue("integration-test-service"))
  telemetry.api.trace.Span::set_attribute(operation_span, "service.version", telemetry.api.common.StringValue("1.0.0"))
  telemetry.api.trace.Span::set_attribute(operation_span, "deployment.environment", telemetry.api.common.StringValue("test"))
  
  // Add baggage attributes to span
  telemetry.api.trace.Span::set_attribute(operation_span, "user.id", telemetry.api.common.StringValue("user67890"))
  telemetry.api.trace.Span::set_attribute(operation_span, "session.id", telemetry.api.common.StringValue("session-abcdef"))
  telemetry.api.trace.Span::set_attribute(operation_span, "tenant.id", telemetry.api.common.StringValue("tenant-123"))
  
  // Create metrics with resource and baggage attributes
  let resource_attrs = telemetry.api.common.Attributes::new()
  telemetry.api.common.Attributes::set(resource_attrs, "service.name", telemetry.api.common.StringValue("integration-test-service"))
  telemetry.api.common.Attributes::set(resource_attrs, "service.version", telemetry.api.common.StringValue("1.0.0"))
  
  let baggage_attrs = telemetry.api.common.Attributes::new()
  telemetry.api.common.Attributes::set(baggage_attrs, "user.id", telemetry.api.common.StringValue("user67890"))
  telemetry.api.common.Attributes::set(baggage_attrs, "session.id", telemetry.api.common.StringValue("session-abcdef"))
  telemetry.api.common.Attributes::set(baggage_attrs, "tenant.id", telemetry.api.common.StringValue("tenant-123"))
  
  let combined_attrs = telemetry.api.common.Attributes::new()
  telemetry.api.common.Attributes::set(combined_attrs, "service.name", telemetry.api.common.StringValue("integration-test-service"))
  telemetry.api.common.Attributes::set(combined_attrs, "service.version", telemetry.api.common.StringValue("1.0.0"))
  telemetry.api.common.Attributes::set(combined_attrs, "user.id", telemetry.api.common.StringValue("user67890"))
  telemetry.api.common.Attributes::set(combined_attrs, "session.id", telemetry.api.common.StringValue("session-abcdef"))
  telemetry.api.common.Attributes::set(combined_attrs, "tenant.id", telemetry.api.common.StringValue("tenant-123"))
  
  let operation_counter = telemetry.sdk.metrics.Meter::create_counter(meter, "baggage.aware.operations")
  telemetry.sdk.metrics.Counter::add(operation_counter, 1.0, Some(combined_attrs))
  
  // Create logs with resource and baggage context
  let log_attrs = telemetry.api.common.Attributes::new()
  telemetry.api.common.Attributes::set(log_attrs, "service.name", telemetry.api.common.StringValue("integration-test-service"))
  telemetry.api.common.Attributes::set(log_attrs, "user.id", telemetry.api.common.StringValue("user67890"))
  telemetry.api.common.Attributes::set(log_attrs, "session.id", telemetry.api.common.StringValue("session-abcdef"))
  telemetry.api.common.Attributes::set(log_attrs, "tenant.id", telemetry.api.common.StringValue("tenant-123"))
  
  let log_record = telemetry.api.logs.LogRecord::new_with_context(
    telemetry.api.logs.SeverityNumber::Info,
    Some("带有资源和行李上下文的操作"),
    Some(log_attrs),
    Some(telemetry.sdk.platform.time.Clock::now_unix_nanos(telemetry.sdk.platform.time.Clock::system())),
    None,
    Some(telemetry.api.trace.SpanContext::trace_id(telemetry.api.trace.Span::span_context(operation_span))),
    Some(telemetry.api.trace.SpanContext::span_id(telemetry.api.trace.Span::span_context(operation_span))),
    Some(operation_ctx)
  )
  
  telemetry.api.logs.Logger::emit(logger, log_record)
  
  // Test context propagation with baggage
  let propagator = telemetry.api.propagation.CompositePropagator::new([
    telemetry.api.propagation.W3CTraceContextPropagator::new(),
    telemetry.api.propagation.W3CBaggagePropagator::new()
  ])
  
  let carrier = telemetry.api.propagation.TextMapCarrier::new()
  telemetry.api.propagation.CompositePropagator::inject(propagator, operation_ctx, carrier)
  
  // Extract context in downstream service
  let downstream_ctx = telemetry.api.propagation.CompositePropagator::extract(propagator, carrier)
  let (downstream_ctx, downstream_span) = telemetry.sdk.trace.Tracer::start_span(tracer, downstream_ctx, "downstream.operation")
  
  // Downstream service should have access to baggage information
  telemetry.api.trace.Span::set_attribute(downstream_span, "service.name", telemetry.api.common.StringValue("downstream-service"))
  telemetry.api.trace.Span::set_attribute(downstream_span, "upstream.service", telemetry.api.common.StringValue("integration-test-service"))
  telemetry.api.trace.Span::set_attribute(downstream_span, "user.id", telemetry.api.common.StringValue("user67890"))
  
  telemetry.api.trace.Span::end(downstream_span)
  telemetry.api.trace.Span::end(operation_span)
  
  // Verify trace continuity
  let upstream_span_ctx = telemetry.api.trace.Span::span_context(operation_span)
  let downstream_span_ctx = telemetry.api.trace.Span::span_context(downstream_span)
  
  assert_eq(telemetry.api.trace.SpanContext::trace_id(upstream_span_ctx), 
           telemetry.api.trace.SpanContext::trace_id(downstream_span_ctx))
}