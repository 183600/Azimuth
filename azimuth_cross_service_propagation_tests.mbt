// 跨服务遥测传播测试用例
// 测试Azimuth遥测系统的跨服务传播功能

test "基本跨服务上下文传播" {
  // 测试基本的跨服务上下文传播
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross_service.test")
  
  // 在服务A中创建根span
  let service_a_tracer = TracerProvider::get_tracer(tracer_provider, "service-a")
  let root_span = Tracer::start_span(service_a_tracer, "service-a.operation")
  Span::set_attribute(root_span, "service.name", "service-a")
  Span::set_attribute(root_span, "service.version", "1.0.0")
  
  // 获取span上下文
  let span_context = Span::span_context(root_span)
  let trace_id = SpanContext::trace_id(span_context)
  let span_id = SpanContext::span_id(span_context)
  
  // 创建传播器
  let propagator = TraceContextPropagator::new()
  
  // 注入上下文到载体
  let carrier = TextMapCarrier::new()
  propagator.inject(propagator, span_context, carrier)
  
  // 验证上下文已注入
  let traceparent = carrier.get("traceparent")
  let tracestate = carrier.get("tracestate")
  
  assert_true(traceparent != None)
  assert_true(traceparent.get().contains(trace_id))
  assert_true(traceparent.get().contains(span_id))
  
  // 模拟服务B接收请求
  let service_b_tracer = TracerProvider::get_tracer(tracer_provider, "service-b")
  
  // 从载体提取上下文
  let extracted_context = propagator.extract(propagator, carrier)
  assert_true(SpanContext::is_valid(extracted_context))
  assert_eq(SpanContext::trace_id(extracted_context), trace_id)
  
  // 在服务B中创建子span
  let child_span = Tracer::start_span_with_context(service_b_tracer, "service-b.operation", extracted_context)
  Span::set_attribute(child_span, "service.name", "service-b")
  Span::set_attribute(child_span, "service.version", "1.0.0")
  
  // 验证父子关系
  let child_context = Span::span_context(child_span)
  assert_eq(SpanContext::trace_id(child_context), trace_id)
  assert_true(SpanContext::parent_span_id(child_context) == Some(span_id))
  
  Span::end(child_span)
  Span::end(root_span)
  assert_true(true)
}

test "多服务调用链传播" {
  // 测试多服务调用链的上下文传播
  let tracer_provider = TracerProvider::default()
  let propagator = TraceContextPropagator::new()
  
  // 服务A -> 服务B -> 服务C -> 服务D 的调用链
  let services = ["service-a", "service-b", "service-c", "service-d"]
  let spans = []
  let contexts = []
  let mut current_context = None
  
  // 在服务A中创建根span
  let service_a_tracer = TracerProvider::get_tracer(tracer_provider, "service-a")
  let root_span = Tracer::start_span(service_a_tracer, "root.operation")
  Span::set_attribute(root_span, "service.name", "service-a")
  spans = spans.push(root_span)
  
  let root_context = Span::span_context(root_span)
  contexts = contexts.push(root_context)
  current_context = Some(root_context)
  
  // 模拟服务间调用链
  for i in 1..=3 {
    let service_name = services[i]
    let service_tracer = TracerProvider::get_tracer(tracer_provider, service_name)
    
    // 注入当前上下文到载体
    let carrier = TextMapCarrier::new()
    match current_context {
      Some(ctx) => propagator.inject(propagator, ctx, carrier)
      None => assert_true(false)
    }
    
    // 提取上下文
    let extracted_context = propagator.extract(propagator, carrier)
    
    // 创建子span
    let span_name = service_name + ".operation"
    let span = Tracer::start_span_with_context(service_tracer, span_name, extracted_context)
    Span::set_attribute(span, "service.name", service_name)
    Span::set_attribute(span, "call.chain.index", i)
    
    spans = spans.push(span)
    contexts = contexts.push(Span::span_context(span))
    current_context = Some(Span::span_context(span))
  }
  
  // 验证所有span在同一trace中
  let root_trace_id = SpanContext::trace_id(root_context)
  for context in contexts {
    assert_eq(SpanContext::trace_id(context), root_trace_id)
  }
  
  // 验证父子关系
  for i in 1..=3 {
    let child_context = contexts[i]
    let parent_context = contexts[i - 1]
    let parent_span_id = SpanContext::span_id(parent_context)
    
    assert_eq(SpanContext::parent_span_id(child_context), Some(parent_span_id))
  }
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  assert_true(true)
}

test "跨服务 baggage 传播" {
  // 测试跨服务的 baggage 传播
  let tracer_provider = TracerProvider::default()
  let propagator = TraceContextPropagator::new()
  
  // 在服务A中创建根span并设置baggage
  let service_a_tracer = TracerProvider::get_tracer(tracer_provider, "service-a")
  let root_span = Tracer::start_span(service_a_tracer, "service-a.operation")
  
  // 设置baggage
  let baggage = Baggage::new()
  Baggage::set_entry(baggage, "user.id", "user-12345")
  Baggage::set_entry(baggage, "request.id", "req-67890")
  Baggage::set_entry(baggage, "session.id", "session-abc")
  Baggage::set_entry(baggage, "tenant.id", "tenant-xyz")
  
  let span_context = Span::span_context(root_span)
  let context_with_baggage = Context::with_baggage(Context::root(), baggage, span_context)
  
  // 注入上下文和baggage
  let carrier = TextMapCarrier::new()
  propagator.inject(propagator, context_with_baggage, carrier)
  
  // 验证baggage已注入
  let baggage_header = carrier.get("baggage")
  assert_true(baggage_header != None)
  assert_true(baggage_header.get().contains("user.id=user-12345"))
  assert_true(baggage_header.get().contains("request.id=req-67890"))
  
  // 在服务B中提取上下文和baggage
  let service_b_tracer = TracerProvider::get_tracer(tracer_provider, "service-b")
  let extracted_context = propagator.extract(propagator, carrier)
  let extracted_baggage = Context::get_baggage(extracted_context)
  
  // 验证baggage内容
  assert_true(extracted_baggage != None)
  match extracted_baggage {
    Some(baggage) => {
      assert_eq(Baggage::get_entry(baggage, "user.id"), Some("user-12345"))
      assert_eq(Baggage::get_entry(baggage, "request.id"), Some("req-67890"))
      assert_eq(Baggage::get_entry(baggage, "session.id"), Some("session-abc"))
      assert_eq(Baggage::get_entry(baggage, "tenant.id"), Some("tenant-xyz"))
    }
    None => assert_true(false)
  }
  
  // 在服务B中添加新的baggage项
  let updated_baggage = match extracted_baggage {
    Some(b) => {
      Baggage::set_entry(b, "service.b.timestamp", "2025-01-02T10:00:00Z")
      Baggage::set_entry(b, "service.b.operation", "process_data")
      b
    }
    None => {
      let new_baggage = Baggage::new()
      Baggage::set_entry(new_baggage, "service.b.timestamp", "2025-01-02T10:00:00Z")
      new_baggage
    }
  }
  
  let context_with_updated_baggage = Context::with_baggage(extracted_context, updated_baggage, Span::span_context(root_span))
  
  // 传播到服务C
  let carrier_c = TextMapCarrier::new()
  propagator.inject(propagator, context_with_updated_baggage, carrier_c)
  
  // 在服务C中验证所有baggage项
  let service_c_tracer = TracerProvider::get_tracer(tracer_provider, "service-c")
  let final_context = propagator.extract(propagator, carrier_c)
  let final_baggage = Context::get_baggage(final_context)
  
  match final_baggage {
    Some(baggage) => {
      // 验证原始baggage项
      assert_eq(Baggage::get_entry(baggage, "user.id"), Some("user-12345"))
      assert_eq(Baggage::get_entry(baggage, "request.id"), Some("req-67890"))
      assert_eq(Baggage::get_entry(baggage, "session.id"), Some("session-abc"))
      assert_eq(Baggage::get_entry(baggage, "tenant.id"), Some("tenant-xyz"))
      
      // 验证服务B添加的baggage项
      assert_eq(Baggage::get_entry(baggage, "service.b.timestamp"), Some("2025-01-02T10:00:00Z"))
      assert_eq(Baggage::get_entry(baggage, "service.b.operation"), Some("process_data"))
    }
    None => assert_true(false)
  }
  
  Span::end(root_span)
  assert_true(true)
}

test "跨服务度量关联" {
  // 测试跨服务的度量关联
  let meter_provider = MeterProvider::default()
  let propagator = MetricsPropagator::new()
  
  // 在服务A中创建度量
  let service_a_meter = MeterProvider::get_meter(meter_provider, "service-a")
  let request_counter = Meter::create_counter(service_a_meter, "http.requests", Some("HTTP requests"), Some("count"))
  
  // 添加关联标识
  let correlation_id = "corr-" + UUID::new().to_string()
  Counter::add_with_attributes(request_counter, 1.0, [
    ("method", "POST"),
    ("endpoint", "/api/orders"),
    ("correlation.id", correlation_id),
    ("service.name", "service-a")
  ])
  
  // 创建响应时间直方图
  let response_histogram = Meter::create_histogram(service_a_meter, "http.response.time", Some("HTTP response time"), Some("ms"))
  Histogram::record_with_attributes(response_histogram, 150.5, [
    ("method", "POST"),
    ("endpoint", "/api/orders"),
    ("correlation.id", correlation_id),
    ("service.name", "service-a")
  ])
  
  // 传播关联标识到服务B
  let carrier = TextMapCarrier::new()
  propagator.inject_correlation_id(propagator, correlation_id, carrier)
  
  // 在服务B中使用相同的关联标识记录度量
  let service_b_meter = MeterProvider::get_meter(meter_provider, "service-b")
  let service_b_counter = Meter::create_counter(service_b_meter, "database.queries", Some("Database queries"), Some("count"))
  
  let extracted_correlation_id = propagator.extract_correlation_id(propagator, carrier)
  assert_eq(extracted_correlation_id, correlation_id)
  
  Counter::add_with_attributes(service_b_counter, 3.0, [
    ("query.type", "SELECT"),
    ("table", "orders"),
    ("correlation.id", extracted_correlation_id),
    ("service.name", "service-b")
  ])
  
  // 在服务C中继续使用相同的关联标识
  let service_c_meter = MeterProvider::get_meter(meter_provider, "service-c")
  let service_c_histogram = Meter::create_histogram(service_c_meter, "cache.operations", Some("Cache operations"), Some("ms"))
  
  Histogram::record_with_attributes(service_c_histogram, 5.2, [
    ("operation", "get"),
    ("cache.key", "order-123"),
    ("correlation.id", extracted_correlation_id),
    ("service.name", "service-c")
  ])
  
  // 创建跨服务度量关联器
  let cross_service_correlator = CrossServiceMetricsCorrelator::new()
  
  // 按关联ID聚合所有服务的度量
  let correlated_metrics = cross_service_correlator.aggregate_by_correlation_id(
    cross_service_correlator, 
    correlation_id,
    [request_counter, response_histogram, service_b_counter, service_c_histogram]
  )
  
  // 验证关联结果
  assert_true(correlated_metrics.contains_service("service-a"))
  assert_true(correlated_metrics.contains_service("service-b"))
  assert_true(correlated_metrics.contains_service("service-c"))
  
  // 验证每个服务的度量数据
  let service_a_metrics = correlated_metrics.get_service_metrics("service-a")
  assert_true(service_a_metrics.contains_metric("http.requests"))
  assert_true(service_a_metrics.contains_metric("http.response.time"))
  
  let service_b_metrics = correlated_metrics.get_service_metrics("service-b")
  assert_true(service_b_metrics.contains_metric("database.queries"))
  
  let service_c_metrics = correlated_metrics.get_service_metrics("service-c")
  assert_true(service_c_metrics.contains_metric("cache.operations"))
  
  assert_true(true)
}

test "跨服务日志关联" {
  // 测试跨服务的日志关联
  let logger_provider = LoggerProvider::default()
  let propagator = LogCorrelationPropagator::new()
  
  // 在服务A中创建日志记录
  let service_a_logger = LoggerProvider::get_logger(logger_provider, "service-a")
  
  let trace_id = "trace-" + UUID::new().to_string()
  let span_id = "span-" + UUID::new().to_string()
  
  let start_log = LogRecord::new(Info, "Processing order request")
  LogRecord::add_attribute(start_log, "trace.id", trace_id)
  LogRecord::add_attribute(start_log, "span.id", span_id)
  LogRecord::add_attribute(start_log, "service.name", "service-a")
  LogRecord::add_attribute(start_log, "order.id", "order-12345")
  
  Logger::emit(service_a_logger, start_log)
  
  // 注入日志关联上下文
  let carrier = TextMapCarrier::new()
  propagator.inject_log_context(propagator, trace_id, span_id, carrier)
  
  // 在服务B中提取日志关联上下文
  let service_b_logger = LoggerProvider::get_logger(logger_provider, "service-b")
  
  let log_context = propagator.extract_log_context(propagator, carrier)
  assert_eq(log_context.trace_id, trace_id)
  assert_eq(log_context.span_id, span_id)
  
  // 在服务B中创建关联的日志记录
  let db_query_log = LogRecord::new(Info, "Executing database query")
  LogRecord::add_attribute(db_query_log, "trace.id", log_context.trace_id)
  LogRecord::add_attribute(db_query_log, "span.id", log_context.span_id)
  LogRecord::add_attribute(db_query_log, "service.name", "service-b")
  LogRecord::add_attribute(db_query_log, "query.type", "SELECT")
  LogRecord::add_attribute(db_query_log, "order.id", "order-12345")
  
  Logger::emit(service_b_logger, db_query_log)
  
  // 在服务C中继续日志关联
  let service_c_logger = LoggerProvider::get_logger(logger_provider, "service-c")
  
  let cache_log = LogRecord::new(Info, "Checking cache for order data")
  LogRecord::add_attribute(cache_log, "trace.id", log_context.trace_id)
  LogRecord::add_attribute(cache_log, "span.id", "span-" + UUID::new().to_string()) // 新的span ID
  LogRecord::add_attribute(cache_log, "parent.span.id", log_context.span_id) // 父span ID
  LogRecord::add_attribute(cache_log, "service.name", "service-c")
  LogRecord::add_attribute(cache_log, "cache.key", "order-12345")
  
  Logger::emit(service_c_logger, cache_log)
  
  // 创建跨服务日志关联器
  let log_correlator = CrossServiceLogCorrelator::new()
  
  // 按trace ID关联所有日志
  let correlated_logs = log_correlator.correlate_by_trace_id(log_correlator, trace_id)
  
  // 验证关联结果
  assert_true(correlated_logs.length() >= 3)
  
  // 验证每个日志记录都有相同的trace ID
  for log in correlated_logs {
    let log_trace_id = LogRecord::get_attribute(log, "trace.id")
    match log_trace_id {
      Some(id) => assert_eq(id, trace_id)
      None => assert_true(false)
    }
  }
  
  // 验证服务A的日志
  let service_a_logs = correlated_logs.filter(fn(log) {
    match LogRecord::get_attribute(log, "service.name") {
      Some(name) => name == "service-a"
      None => false
    }
  })
  assert_true(service_a_logs.length() >= 1)
  
  // 验证服务B的日志
  let service_b_logs = correlated_logs.filter(fn(log) {
    match LogRecord::get_attribute(log, "service.name") {
      Some(name) => name == "service-b"
      None => false
    }
  })
  assert_true(service_b_logs.length() >= 1)
  
  // 验证服务C的日志
  let service_c_logs = correlated_logs.filter(fn(log) {
    match LogRecord::get_attribute(log, "service.name") {
      Some(name) => name == "service-c"
      None => false
    }
  })
  assert_true(service_c_logs.length() >= 1)
  
  assert_true(true)
}

test "跨服务传播的错误处理" {
  // 测试跨服务传播的错误处理
  let tracer_provider = TracerProvider::default()
  let propagator = TraceContextPropagator::new()
  
  // 在服务A中创建span并模拟错误
  let service_a_tracer = TracerProvider::get_tracer(tracer_provider, "service-a")
  let root_span = Tracer::start_span(service_a_tracer, "service.a.operation")
  
  Span::set_attribute(root_span, "service.name", "service-a")
  Span::set_status(root_span, Error, Some("Invalid request parameters"))
  
  // 添加错误事件
  Span::add_event(root_span, "error.occurred", [
    ("error.type", "ValidationError"),
    ("error.message", "Missing required field: user_id"),
    ("error.code", "ERR_001")
  ])
  
  let span_context = Span::span_context(root_span)
  
  // 注入错误上下文
  let carrier = TextMapCarrier::new()
  propagator.inject(propagator, span_context, carrier)
  
  // 在服务B中提取错误上下文
  let service_b_tracer = TracerProvider::get_tracer(tracer_provider, "service-b")
  let extracted_context = propagator.extract(propagator, carrier)
  
  // 在服务B中创建子span并传播错误
  let child_span = Tracer::start_span_with_context(service_b_tracer, "service.b.operation", extracted_context)
  Span::set_attribute(child_span, "service.name", "service-b")
  
  // 检查父span的错误状态
  let parent_status = SpanContext::get_parent_status(extracted_context)
  match parent_status {
    Some(status) => {
      assert_eq(status.status_code, Error)
      assert_eq(status.description, Some("Invalid request parameters"))
    }
    None => assert_true(false)
  }
  
  // 在子span中记录错误影响
  Span::set_status(child_span, Error, Some("Operation failed due to upstream error"))
  Span::add_event(child_span, "error.propagated", [
    ("error.type", "UpstreamError"),
    ("error.source", "service-a"),
    ("error.code", "ERR_001")
  ])
  
  // 测试错误传播恢复
  let service_c_tracer = TracerProvider::get_tracer(tracer_provider, "service-c")
  let recovery_span = Tracer::start_span_with_context(service_c_tracer, "service.c.recovery", extracted_context)
  Span::set_attribute(recovery_span, "service.name", "service-c")
  Span::set_status(recovery_span, Ok, Some("Error recovery successful"))
  
  Span::add_event(recovery_span, "recovery.attempted", [
    ("recovery.strategy", "fallback"),
    ("recovery.result", "success")
  ])
  
  // 验证错误传播链
  let error_propagator = ErrorPropagationAnalyzer::new()
  let error_chain = error_propagator.analyze_error_propagation(error_propagator, [
    root_span, child_span, recovery_span
  ])
  
  // 验证错误链结构
  assert_eq(error_chain.length(), 3)
  assert_eq(error_chain[0].service_name, "service-a")
  assert_eq(error_chain[0].status, Error)
  
  assert_eq(error_chain[1].service_name, "service-b")
  assert_eq(error_chain[1].status, Error)
  assert_eq(error_chain[1].parent_service, Some("service-a"))
  
  assert_eq(error_chain[2].service_name, "service-c")
  assert_eq(error_chain[2].status, Ok)
  assert_eq(error_chain[2].parent_service, Some("service-a")) // 恢复span直接关联到根span
  
  Span::end(recovery_span)
  Span::end(child_span)
  Span::end(root_span)
  
  assert_true(true)
}