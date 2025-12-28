test "complex telemetry workflow integration" {
  // Test complex telemetry workflow with multiple components working together
  
  // 1. Initialize all providers
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  
  // 2. Create instruments
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "complex.workflow.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "complex.workflow.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "complex.workflow.logger")
  
  // 3. Create metric instruments
  let request_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "http.requests.total")
  let response_histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "http.response.duration")
  let error_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "http.errors.total")
  let active_connections = @azimuth.telemetry.sdk.metrics.Meter::create_up_down_counter(meter, "http.active_connections")
  let memory_usage = @azimuth.telemetry.sdk.metrics.Meter::create_gauge(meter, "process.memory.usage")
  
  // 4. Simulate complex request processing workflow
  let ctx = @azimuth.telemetry.api.context.Context::root()
  
  // Start main span
  let (request_ctx, main_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx, "http.request.processing")
  
  // Log request start
  @azimuth.telemetry.sdk.logs.Logger::info(logger, "开始处理HTTP请求")
  
  // Update metrics
  @azimuth.telemetry.sdk.metrics.UpDownCounter::add(active_connections, 1.0)
  @azimuth.telemetry.sdk.metrics.Gauge::record(memory_usage, 512.0)  // MB
  
  // Add request attributes to main span
  @azimuth.telemetry.api.trace.Span::set_attribute(main_span, "http.method", @azimuth.telemetry.api.common.StringValue("POST"))
  @azimuth.telemetry.api.trace.Span::set_attribute(main_span, "http.url", @azimuth.telemetry.api.common.StringValue("/api/v1/process"))
  @azimuth.telemetry.api.trace.Span::set_attribute(main_span, "user.id", @azimuth.telemetry.api.common.StringValue("user12345"))
  
  // 5. Simulate database operation
  let (db_ctx, db_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, request_ctx, "database.query")
  
  @azimuth.telemetry.api.trace.Span::set_attribute(db_span, "db.system", @azimuth.telemetry.api.common.StringValue("postgresql"))
  @azimuth.telemetry.api.trace.Span::set_attribute(db_span, "db.statement", @azimuth.telemetry.api.common.StringValue("SELECT * FROM users WHERE id = $1"))
  
  // Add database event
  @azimuth.telemetry.api.trace.Span::add_event(db_span, "query.start", None, None)
  
  // Simulate database work
  @azimuth.telemetry.sdk.logs.Logger::debug(logger, "执行数据库查询")
  
  @azimuth.telemetry.api.trace.Span::add_event(db_span, "query.complete", None, None)
  @azimuth.telemetry.api.trace.Span::end(db_span)
  
  // 6. Simulate cache operation
  let (cache_ctx, cache_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, request_ctx, "cache.operation")
  
  @azimuth.telemetry.api.trace.Span::set_attribute(cache_span, "cache.operation", @azimuth.telemetry.api.common.StringValue("get"))
  @azimuth.telemetry.api.trace.Span::set_attribute(cache_span, "cache.key", @azimuth.telemetry.api.common.StringValue("user:12345:profile"))
  
  @azimuth.telemetry.sdk.logs.Logger::debug(logger, "检查缓存")
  
  // Cache miss - simulate remote call
  let (remote_ctx, remote_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, cache_ctx, "remote.service.call")
  
  @azimuth.telemetry.api.trace.Span::set_attribute(remote_span, "service.name", @azimuth.telemetry.api.common.StringValue("user-profile-service"))
  @azimuth.telemetry.api.trace.Span::set_attribute(remote_span, "service.endpoint", @azimuth.telemetry.api.common.StringValue("/profile"))
  
  @azimuth.telemetry.sdk.logs.Logger::info(logger, "调用远程服务获取用户资料")
  
  @azimuth.telemetry.api.trace.Span::end(remote_span)
  @azimuth.telemetry.api.trace.Span::end(cache_span)
  
  // 7. Simulate business logic processing
  let (business_ctx, business_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, request_ctx, "business.logic.processing")
  
  @azimuth.telemetry.api.trace.Span::set_attribute(business_span, "business.operation", @azimuth.telemetry.api.common.StringValue("process_user_request"))
  @azimuth.telemetry.api.trace.Span::set_attribute(business_span, "business.rules.applied", @azimuth.telemetry.api.common.IntValue(5))
  
  // Simulate processing steps
  for i in 0..3 {
    let (step_ctx, step_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, business_ctx, "processing.step." + i.to_string())
    
    @azimuth.telemetry.api.trace.Span::set_attribute(step_span, "step.number", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span::set_attribute(step_span, "step.name", @azimuth.telemetry.api.common.StringValue("step_" + i.to_string()))
    
    @azimuth.telemetry.sdk.logs.Logger::debug(logger, "执行处理步骤 " + i.to_string())
    
    // Update progress metrics
    @azimuth.telemetry.sdk.metrics.Counter::add(request_counter, 1.0)
    
    @azimuth.telemetry.api.trace.Span::end(step_span)
  }
  
  @azimuth.telemetry.api.trace.Span::end(business_span)
  
  // 8. Simulate response preparation
  let (response_ctx, response_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, request_ctx, "response.preparation")
  
  @azimuth.telemetry.api.trace.Span::set_attribute(response_span, "response.status_code", @azimuth.telemetry.api.common.IntValue(200))
  @azimuth.telemetry.api.trace.Span::set_attribute(response_span, "response.size", @azimuth.telemetry.api.common.IntValue(1024))
  
  @azimuth.telemetry.sdk.logs.Logger::info(logger, "准备响应")
  
  // Record final metrics
  @azimuth.telemetry.sdk.metrics.Histogram::record(response_histogram, 150.5)  // Response time in ms
  @azimuth.telemetry.sdk.metrics.Counter::add(request_counter, 1.0)
  
  @azimuth.telemetry.api.trace.Span::end(response_span)
  
  // 9. Complete main span
  @azimuth.telemetry.api.trace.Span::set_attribute(main_span, "http.status_code", @azimuth.telemetry.api.common.IntValue(200))
  @azimuth.telemetry.api.trace.Span::set_status(main_span, @azimuth.telemetry.api.trace.StatusCode::Ok, Some("Request processed successfully"))
  
  @azimuth.telemetry.sdk.logs.Logger::info(logger, "请求处理完成")
  
  // Update final metrics
  @azimuth.telemetry.sdk.metrics.UpDownCounter::add(active_connections, -1.0)
  @azimuth.telemetry.sdk.metrics.Gauge::record(memory_usage, 480.0)  // Memory decreased
  
  @azimuth.telemetry.api.trace.Span::end(main_span)
  
  // 10. Verify context propagation worked correctly
  let main_span_ctx = @azimuth.telemetry.api.trace.Span::span_context(main_span)
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(main_span_ctx))
  assert_true(@azimuth.telemetry.api.trace.SpanContext::is_sampled(main_span_ctx))
}

test "multi-service distributed tracing integration" {
  // Test distributed tracing across multiple services
  
  // Service A: Entry point
  let service_a_tracer = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let service_a_tracer_instance = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(service_a_tracer, "service-a")
  let service_a_logger = @azimuth.telemetry.sdk.logs.LoggerProvider::default() |> @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(_, "service-a")
  
  let ctx = @azimuth.telemetry.api.context.Context::root()
  let (service_a_ctx, service_a_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(service_a_tracer_instance, ctx, "service-a.operation")
  
  @azimuth.telemetry.api.trace.Span::set_attribute(service_a_span, "service.name", @azimuth.telemetry.api.common.StringValue("service-a"))
  @azimuth.telemetry.api.trace.Span::set_attribute(service_a_span, "operation.type", @azimuth.telemetry.api.common.StringValue("orchestration"))
  
  @azimuth.telemetry.sdk.logs.Logger::info(service_a_logger, "Service A: 开始处理请求")
  
  // Simulate context propagation to Service B
  let propagator = @azimuth.telemetry.api.propagation.CompositePropagator::new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::new()
  ])
  
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(propagator, service_a_ctx, carrier)
  
  // Service B: Receives propagated context
  let service_b_tracer = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let service_b_tracer_instance = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(service_b_tracer, "service-b")
  let service_b_logger = @azimuth.telemetry.sdk.logs.LoggerProvider::default() |> @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(_, "service-b")
  
  let service_b_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(propagator, carrier)
  let (service_b_ctx, service_b_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(service_b_tracer_instance, service_b_ctx, "service-b.operation")
  
  @azimuth.telemetry.api.trace.Span::set_attribute(service_b_span, "service.name", @azimuth.telemetry.api.common.StringValue("service-b"))
  @azimuth.telemetry.api.trace.Span::set_attribute(service_b_span, "operation.type", @azimuth.telemetry.api.common.StringValue("data-processing"))
  
  @azimuth.telemetry.sdk.logs.Logger::info(service_b_logger, "Service B: 处理数据")
  
  // Service B calls Service C
  let service_b_carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(propagator, service_b_ctx, service_b_carrier)
  
  // Service C: Final service in chain
  let service_c_tracer = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let service_c_tracer_instance = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(service_c_tracer, "service-c")
  let service_c_logger = @azimuth.telemetry.sdk.logs.LoggerProvider::default() |> @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(_, "service-c")
  
  let service_c_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(propagator, service_b_carrier)
  let (service_c_ctx, service_c_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(service_c_tracer_instance, service_c_ctx, "service-c.operation")
  
  @azimuth.telemetry.api.trace.Span::set_attribute(service_c_span, "service.name", @azimuth.telemetry.api.common.StringValue("service-c"))
  @azimuth.telemetry.api.trace.Span::set_attribute(service_c_span, "operation.type", @azimuth.telemetry.api.common.StringValue("storage"))
  
  @azimuth.telemetry.sdk.logs.Logger::info(service_c_logger, "Service C: 存储数据")
  
  // Simulate error in Service C
  @azimuth.telemetry.api.trace.Span::record_exception(
    service_c_span,
    "Storage connection failed",
    Some("StorageException"),
    Some("at service_c.store()\n  at main()"),
    None
  )
  
  @azimuth.telemetry.api.trace.Span::set_status(service_c_span, @azimuth.telemetry.api.trace.StatusCode::Error, Some("Storage operation failed"))
  @azimuth.telemetry.sdk.logs.Logger::error(service_c_logger, "Service C: 存储操作失败")
  
  @azimuth.telemetry.api.trace.Span::end(service_c_span)
  
  // Service B handles error from Service C
  @azimuth.telemetry.api.trace.Span::set_status(service_b_span, @azimuth.telemetry.api.trace.StatusCode::Error, Some("Dependency service failed"))
  @azimuth.telemetry.sdk.logs.Logger::error(service_b_logger, "Service B: 依赖服务失败")
  
  @azimuth.telemetry.api.trace.Span::end(service_b_span)
  
  // Service A handles error from Service B
  @azimuth.telemetry.api.trace.Span::set_status(service_a_span, @azimuth.telemetry.api.trace.StatusCode::Error, Some("Orchestration failed"))
  @azimuth.telemetry.sdk.logs.Logger::error(service_a_logger, "Service A: 编排失败")
  
  @azimuth.telemetry.api.trace.Span::end(service_a_span)
  
  // Verify trace context propagation
  let service_a_span_ctx = @azimuth.telemetry.api.trace.Span::span_context(service_a_span)
  let service_b_span_ctx = @azimuth.telemetry.api.trace.Span::span_context(service_b_span)
  let service_c_span_ctx = @azimuth.telemetry.api.trace.Span::span_context(service_c_span)
  
  // All spans should have the same trace ID
  assert_eq(@azimuth.telemetry.api.trace.SpanContext::trace_id(service_a_span_ctx), 
           @azimuth.telemetry.api.trace.SpanContext::trace_id(service_b_span_ctx))
  assert_eq(@azimuth.telemetry.api.trace.SpanContext::trace_id(service_b_span_ctx), 
           @azimuth.telemetry.api.trace.SpanContext::trace_id(service_c_span_ctx))
  
  // Each span should have different span ID
  assert_true(@azimuth.telemetry.api.trace.SpanContext::span_id(service_a_span_ctx) != 
              @azimuth.telemetry.api.trace.SpanContext::span_id(service_b_span_ctx))
  assert_true(@azimuth.telemetry.api.trace.SpanContext::span_id(service_b_span_ctx) != 
              @azimuth.telemetry.api.trace.SpanContext::span_id(service_c_span_ctx))
}

test "metrics and logs correlation integration" {
  // Test correlation between metrics and logs through trace context
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "correlation.test")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "correlation.test")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "correlation.test")
  
  // Create metric instruments
  let operation_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "operations.total")
  let operation_duration = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "operations.duration")
  let error_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "operations.errors")
  
  // Start operation with trace context
  let ctx = @azimuth.telemetry.api.context.Context::root()
  let (operation_ctx, operation_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx, "correlated.operation")
  
  let span_ctx = @azimuth.telemetry.api.trace.Span::span_context(operation_span)
  let trace_id = @azimuth.telemetry.api.trace.SpanContext::trace_id(span_ctx)
  let span_id = @azimuth.telemetry.api.trace.SpanContext::span_id(span_ctx)
  
  // Create log record with trace context
  let log_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(log_attrs, "operation.type", @azimuth.telemetry.api.common.StringValue("correlated.operation"))
  @azimuth.telemetry.api.common.Attributes::set(log_attrs, "component", @azimuth.telemetry.api.common.StringValue("test"))
  
  let start_log = @azimuth.telemetry.api.logs.LogRecord::new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber::Info,
    Some("开始关联操作"),
    Some(log_attrs),
    Some(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(operation_ctx)
  )
  
  @azimuth.telemetry.api.logs.Logger::emit(logger, start_log)
  
  // Record metrics with trace context
  let metric_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(metric_attrs, "trace.id", @azimuth.telemetry.api.common.StringValue(trace_id))
  @azimuth.telemetry.api.common.Attributes::set(metric_attrs, "span.id", @azimuth.telemetry.api.common.StringValue(span_id))
  @azimuth.telemetry.api.common.Attributes::set(metric_attrs, "operation.type", @azimuth.telemetry.api.common.StringValue("correlated.operation"))
  
  @azimuth.telemetry.sdk.metrics.Counter::add(operation_counter, 1.0, Some(metric_attrs))
  
  // Simulate operation steps
  for i in 0..3 {
    let (step_ctx, step_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, operation_ctx, "operation.step." + i.to_string())
    
    let step_span_ctx = @azimuth.telemetry.api.trace.Span::span_context(step_span)
    let step_trace_id = @azimuth.telemetry.api.trace.SpanContext::trace_id(step_span_ctx)
    let step_span_id = @azimuth.telemetry.api.trace.SpanContext::span_id(step_span_ctx)
    
    // Log step start
    let step_log_attrs = @azimuth.telemetry.api.common.Attributes::new()
    @azimuth.telemetry.api.common.Attributes::set(step_log_attrs, "step.number", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.common.Attributes::set(step_log_attrs, "parent.trace.id", @azimuth.telemetry.api.common.StringValue(trace_id))
    @azimuth.telemetry.api.common.Attributes::set(step_log_attrs, "parent.span.id", @azimuth.telemetry.api.common.StringValue(span_id))
    
    let step_log = @azimuth.telemetry.api.logs.LogRecord::new_with_context(
      @azimuth.telemetry.api.logs.SeverityNumber::Debug,
      Some("执行步骤 " + i.to_string()),
      Some(step_log_attrs),
      Some(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())),
      None,
      Some(step_trace_id),
      Some(step_span_id),
      Some(step_ctx)
    )
    
    @azimuth.telemetry.api.logs.Logger::emit(logger, step_log)
    
    // Record step metrics
    let step_metric_attrs = @azimuth.telemetry.api.common.Attributes::new()
    @azimuth.telemetry.api.common.Attributes::set(step_metric_attrs, "trace.id", @azimuth.telemetry.api.common.StringValue(step_trace_id))
    @azimuth.telemetry.api.common.Attributes::set(step_metric_attrs, "span.id", @azimuth.telemetry.api.common.StringValue(step_span_id))
    @azimuth.telemetry.api.common.Attributes::set(step_metric_attrs, "parent.trace.id", @azimuth.telemetry.api.common.StringValue(trace_id))
    @azimuth.telemetry.api.common.Attributes::set(step_metric_attrs, "parent.span.id", @azimuth.telemetry.api.common.StringValue(span_id))
    @azimuth.telemetry.api.common.Attributes::set(step_metric_attrs, "step.number", @azimuth.telemetry.api.common.IntValue(i))
    
    @azimuth.telemetry.sdk.metrics.Histogram::record(operation_duration, (i * 10).to_double(), Some(step_metric_attrs))
    
    @azimuth.telemetry.api.trace.Span::end(step_span)
  }
  
  // Simulate error in last step
  let error_log_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(error_log_attrs, "error.type", @azimuth.telemetry.api.common.StringValue("ProcessingError"))
  @azimuth.telemetry.api.common.Attributes::set(error_log_attrs, "error.message", @azimuth.telemetry.api.common.StringValue("Step processing failed"))
  
  let error_log = @azimuth.telemetry.api.logs.LogRecord::new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber::Error,
    Some("操作失败"),
    Some(error_log_attrs),
    Some(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())),
    None,
    Some(trace_id),
    Some(span_id),
    Some(operation_ctx)
  )
  
  @azimuth.telemetry.api.logs.Logger::emit(logger, error_log)
  
  // Record error metrics
  let error_metric_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(error_metric_attrs, "trace.id", @azimuth.telemetry.api.common.StringValue(trace_id))
  @azimuth.telemetry.api.common.Attributes::set(error_metric_attrs, "span.id", @azimuth.telemetry.api.common.StringValue(span_id))
  @azimuth.telemetry.api.common.Attributes::set(error_metric_attrs, "error.type", @azimuth.telemetry.api.common.StringValue("ProcessingError"))
  
  @azimuth.telemetry.sdk.metrics.Counter::add(error_counter, 1.0, Some(error_metric_attrs))
  
  // End main operation
  @azimuth.telemetry.api.trace.Span::set_status(operation_span, @azimuth.telemetry.api.trace.StatusCode::Error, Some("Operation failed"))
  @azimuth.telemetry.api.trace.Span::end(operation_span)
  
  // Verify correlation through trace IDs
  assert_true(trace_id != "")
  assert_true(span_id != "")
}

test "resource and baggage integration" {
  // Test integration between resource management and baggage propagation
  
  // Create resources with service information
  let base_resource = @azimuth.telemetry.api.common.Resource::with_attributes(
    @azimuth.telemetry.api.common.Resource::new(),
    [
      ("service.name", @azimuth.telemetry.api.common.StringValue("integration-test-service")),
      ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
      ("service.instance.id", @azimuth.telemetry.api.common.StringValue("instance-12345")),
      ("deployment.environment", @azimuth.telemetry.api.common.StringValue("test"))
    ]
  )
  
  // Create baggage with user information
  let baggage = @azimuth.telemetry.api.context.Baggage::new()
  let baggage_with_user = @azimuth.telemetry.api.context.Baggage::set_entry(baggage, "user.id", "user67890")
  let baggage_with_session = @azimuth.telemetry.api.context.Baggage::set_entry(baggage_with_user, "session.id", "session-abcdef")
  let baggage_with_tenant = @azimuth.telemetry.api.context.Baggage::set_entry(baggage_with_session, "tenant.id", "tenant-123")
  
  // Initialize providers with resource
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(trace_provider, "resource.baggage.test")
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "resource.baggage.test")
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(logs_provider, "resource.baggage.test")
  
  // Create context with baggage
  let ctx = @azimuth.telemetry.api.context.Context::root()
  let ctx_with_baggage = @azimuth.telemetry.api.context.Context::with_value(ctx, @azimuth.telemetry.api.context.ContextKey::new("baggage"), "baggage_value")
  
  // Start operation with baggage context
  let (operation_ctx, operation_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx_with_baggage, "baggage.aware.operation")
  
  // Add resource attributes to span
  @azimuth.telemetry.api.trace.Span::set_attribute(operation_span, "service.name", @azimuth.telemetry.api.common.StringValue("integration-test-service"))
  @azimuth.telemetry.api.trace.Span::set_attribute(operation_span, "service.version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  @azimuth.telemetry.api.trace.Span::set_attribute(operation_span, "deployment.environment", @azimuth.telemetry.api.common.StringValue("test"))
  
  // Add baggage attributes to span
  @azimuth.telemetry.api.trace.Span::set_attribute(operation_span, "user.id", @azimuth.telemetry.api.common.StringValue("user67890"))
  @azimuth.telemetry.api.trace.Span::set_attribute(operation_span, "session.id", @azimuth.telemetry.api.common.StringValue("session-abcdef"))
  @azimuth.telemetry.api.trace.Span::set_attribute(operation_span, "tenant.id", @azimuth.telemetry.api.common.StringValue("tenant-123"))
  
  // Create metrics with resource and baggage attributes
  let resource_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(resource_attrs, "service.name", @azimuth.telemetry.api.common.StringValue("integration-test-service"))
  @azimuth.telemetry.api.common.Attributes::set(resource_attrs, "service.version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  
  let baggage_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(baggage_attrs, "user.id", @azimuth.telemetry.api.common.StringValue("user67890"))
  @azimuth.telemetry.api.common.Attributes::set(baggage_attrs, "session.id", @azimuth.telemetry.api.common.StringValue("session-abcdef"))
  @azimuth.telemetry.api.common.Attributes::set(baggage_attrs, "tenant.id", @azimuth.telemetry.api.common.StringValue("tenant-123"))
  
  let combined_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(combined_attrs, "service.name", @azimuth.telemetry.api.common.StringValue("integration-test-service"))
  @azimuth.telemetry.api.common.Attributes::set(combined_attrs, "service.version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  @azimuth.telemetry.api.common.Attributes::set(combined_attrs, "user.id", @azimuth.telemetry.api.common.StringValue("user67890"))
  @azimuth.telemetry.api.common.Attributes::set(combined_attrs, "session.id", @azimuth.telemetry.api.common.StringValue("session-abcdef"))
  @azimuth.telemetry.api.common.Attributes::set(combined_attrs, "tenant.id", @azimuth.telemetry.api.common.StringValue("tenant-123"))
  
  let operation_counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "baggage.aware.operations")
  @azimuth.telemetry.sdk.metrics.Counter::add(operation_counter, 1.0, Some(combined_attrs))
  
  // Create logs with resource and baggage context
  let log_attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(log_attrs, "service.name", @azimuth.telemetry.api.common.StringValue("integration-test-service"))
  @azimuth.telemetry.api.common.Attributes::set(log_attrs, "user.id", @azimuth.telemetry.api.common.StringValue("user67890"))
  @azimuth.telemetry.api.common.Attributes::set(log_attrs, "session.id", @azimuth.telemetry.api.common.StringValue("session-abcdef"))
  @azimuth.telemetry.api.common.Attributes::set(log_attrs, "tenant.id", @azimuth.telemetry.api.common.StringValue("tenant-123"))
  
  let log_record = @azimuth.telemetry.api.logs.LogRecord::new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber::Info,
    Some("带有资源和行李上下文的操作"),
    Some(log_attrs),
    Some(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())),
    None,
    Some(@azimuth.telemetry.api.trace.SpanContext::trace_id(@azimuth.telemetry.api.trace.Span::span_context(operation_span))),
    Some(@azimuth.telemetry.api.trace.SpanContext::span_id(@azimuth.telemetry.api.trace.Span::span_context(operation_span))),
    Some(operation_ctx)
  )
  
  @azimuth.telemetry.api.logs.Logger::emit(logger, log_record)
  
  // Test context propagation with baggage
  let propagator = @azimuth.telemetry.api.propagation.CompositePropagator::new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::new(),
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator::new()
  ])
  
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(propagator, operation_ctx, carrier)
  
  // Extract context in downstream service
  let downstream_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(propagator, carrier)
  let (downstream_ctx, downstream_span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, downstream_ctx, "downstream.operation")
  
  // Downstream service should have access to baggage information
  @azimuth.telemetry.api.trace.Span::set_attribute(downstream_span, "service.name", @azimuth.telemetry.api.common.StringValue("downstream-service"))
  @azimuth.telemetry.api.trace.Span::set_attribute(downstream_span, "upstream.service", @azimuth.telemetry.api.common.StringValue("integration-test-service"))
  @azimuth.telemetry.api.trace.Span::set_attribute(downstream_span, "user.id", @azimuth.telemetry.api.common.StringValue("user67890"))
  
  @azimuth.telemetry.api.trace.Span::end(downstream_span)
  @azimuth.telemetry.api.trace.Span::end(operation_span)
  
  // Verify trace continuity
  let upstream_span_ctx = @azimuth.telemetry.api.trace.Span::span_context(operation_span)
  let downstream_span_ctx = @azimuth.telemetry.api.trace.Span::span_context(downstream_span)
  
  assert_eq(@azimuth.telemetry.api.trace.SpanContext::trace_id(upstream_span_ctx), 
           @azimuth.telemetry.api.trace.SpanContext::trace_id(downstream_span_ctx))
}