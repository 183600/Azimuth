// 日志关联测试用例
// 测试 Azimuth 遥测系统中的日志关联功能

test "log_trace_correlation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "log-correlation-test")
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "correlation-logger")
  
  // 创建span
  let span = Tracer::start_span(tracer, "user-operation")
  let span_ctx = SpanContext::from_span(span)
  
  // 创建与trace关联的日志记录
  let log_record = LogRecord::new()
  LogRecord::set_body(log_record, "User operation started")
  LogRecord::set_trace_id(log_record, SpanContext::trace_id(span_ctx))
  LogRecord::set_span_id(log_record, SpanContext::span_id(span_ctx))
  LogRecord::set_attribute(log_record, "user.id", StringValue("12345"))
  LogRecord::set_attribute(log_record, "operation", StringValue("login"))
  
  // 发出日志
  Logger::emit(logger, log_record)
  
  // 验证日志与trace的关联
  assert_eq(LogRecord::trace_id(log_record), SpanContext::trace_id(span_ctx))
  assert_eq(LogRecord::span_id(log_record), SpanContext::span_id(span_ctx))
  
  // 在span中添加事件
  let event_attrs = Attributes::new()
  Attributes::set(event_attrs, "event.name", StringValue("user.authenticated"))
  Attributes::set(event_attrs, "user.role", StringValue("admin"))
  
  Span::add_event(span, "authentication_success", Some(event_attrs))
  
  Span::end(span)
}

test "log_baggage_correlation" {
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "baggage-logger")
  
  // 创建baggage
  let baggage = Baggage::new()
  Baggage::set(baggage, "user.id", "67890")
  Baggage::set(baggage, "session.id", "session-abc")
  Baggage::set(baggage, "request.id", "req-xyz")
  
  // 创建与baggage关联的日志记录
  let log_record = LogRecord::new()
  LogRecord::set_body(log_record, "Processing user request")
  
  // 将baggage项作为日志属性
  let user_id = Baggage::get(baggage, "user.id")
  let session_id = Baggage::get(baggage, "session.id")
  let request_id = Baggage::get(baggage, "request.id")
  
  match user_id {
    Some(id) => LogRecord::set_attribute(log_record, "user.id", StringValue(id))
    _ => {}
  }
  
  match session_id {
    Some(id) => LogRecord::set_attribute(log_record, "session.id", StringValue(id))
    _ => {}
  }
  
  match request_id {
    Some(id) => LogRecord::set_attribute(log_record, "request.id", StringValue(id))
    _ => {}
  }
  
  // 发出日志
  Logger::emit(logger, log_record)
  
  // 验证baggage与日志的关联
  assert_true(user_id != None)
  assert_true(session_id != None)
  assert_true(request_id != None)
}

test "log_resource_correlation" {
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "resource-logger")
  
  // 创建资源
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("prod-server-01"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // 创建与资源关联的日志记录
  let log_record = LogRecord::new()
  LogRecord::set_body(log_record, "Payment processed successfully")
  
  // 将资源属性作为日志属性
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let environment = Resource::get_attribute(resource_with_attrs, "deployment.environment")
  let host_name = Resource::get_attribute(resource_with_attrs, "host.name")
  
  match service_name {
    Some(StringValue(name)) => LogRecord::set_attribute(log_record, "service.name", StringValue(name))
    _ => {}
  }
  
  match service_version {
    Some(StringValue(version)) => LogRecord::set_attribute(log_record, "service.version", StringValue(version))
    _ => {}
  }
  
  match environment {
    Some(StringValue(env)) => LogRecord::set_attribute(log_record, "deployment.environment", StringValue(env))
    _ => {}
  }
  
  match host_name {
    Some(StringValue(host)) => LogRecord::set_attribute(log_record, "host.name", StringValue(host))
    _ => {}
  }
  
  // 发出日志
  Logger::emit(logger, log_record)
  
  // 验证资源与日志的关联
  assert_true(service_name != None)
  assert_true(service_version != None)
  assert_true(environment != None)
  assert_true(host_name != None)
}

test "log_severity_correlation" {
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "severity-logger")
  
  // 测试不同严重级别的日志
  let severity_levels = [
    (Severity::TRACE, "Detailed trace information"),
    (Severity::DEBUG, "Debug information"),
    (Severity::INFO, "Informational message"),
    (Severity::WARN, "Warning condition"),
    (Severity::ERROR, "Error condition"),
    (Severity::FATAL, "Fatal error")
  ]
  
  for (severity, message) in severity_levels {
    let log_record = LogRecord::new()
    LogRecord::set_body(log_record, message)
    LogRecord::set_severity(log_record, severity)
    LogRecord::set_attribute(log_record, "severity.level", StringValue(severity.to_string()))
    
    // 发出日志
    Logger::emit(logger, log_record)
    
    // 验证严重级别设置
    assert_eq(LogRecord::severity(log_record), severity)
  }
}

test "log_context_chain_correlation" {
  let ctx = Context::root()
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "context-chain-logger")
  
  // 创建上下文链
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  let request_key = ContextKey::new("request.id")
  
  let ctx_with_user = Context::with_value(ctx, user_key, "user-123")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session-456")
  let ctx_with_request = Context::with_value(ctx_with_session, request_key, "request-789")
  
  // 创建与上下文链关联的日志记录
  let log_record = LogRecord::new()
  LogRecord::set_body(log_record, "Processing request in context chain")
  
  // 从上下文中提取值并设置为日志属性
  let user_id = Context::get(ctx_with_request, user_key)
  let session_id = Context::get(ctx_with_request, session_key)
  let request_id = Context::get(ctx_with_request, request_key)
  
  match user_id {
    Some(id) => LogRecord::set_attribute(log_record, "user.id", StringValue(id))
    _ => {}
  }
  
  match session_id {
    Some(id) => LogRecord::set_attribute(log_record, "session.id", StringValue(id))
    _ => {}
  }
  
  match request_id {
    Some(id) => LogRecord::set_attribute(log_record, "request.id", StringValue(id))
    _ => {}
  }
  
  // 发出日志
  Logger::emit(logger, log_record)
  
  // 验证上下文链与日志的关联
  assert_eq(user_id, Some("user-123"))
  assert_eq(session_id, Some("session-456"))
  assert_eq(request_id, Some("request-789"))
}

test "log_cross_service_correlation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "cross-service-test")
  let logger_a = LoggerProvider::get_logger(LoggerProvider::default(), "service-a-logger")
  let logger_b = LoggerProvider::get_logger(LoggerProvider::default(), "service-b-logger")
  
  // 在服务A中创建span
  let span_a = Tracer::start_span(tracer, "service-a-operation")
  let ctx_a = SpanContext::from_span(span_a)
  
  // 在服务A中创建日志
  let log_record_a = LogRecord::new()
  LogRecord::set_body(log_record_a, "Operation started in service A")
  LogRecord::set_trace_id(log_record_a, SpanContext::trace_id(ctx_a))
  LogRecord::set_span_id(log_record_a, SpanContext::span_id(ctx_a))
  LogRecord::set_attribute(log_record_a, "service", StringValue("service-a"))
  
  Logger::emit(logger_a, log_record_a)
  
  // 传播上下文到服务B
  let carrier = TextMapCarrier::new()
  let propagator = TraceContextPropagator::new()
  Propagator::inject(propagator, ctx_a, carrier)
  
  // 在服务B中提取上下文并创建日志
  let ctx_b = Propagator::extract(propagator, carrier)
  let log_record_b = LogRecord::new()
  LogRecord::set_body(log_record_b, "Operation continued in service B")
  LogRecord::set_trace_id(log_record_b, SpanContext::trace_id(ctx_b))
  LogRecord::set_span_id(log_record_b, SpanContext::span_id(ctx_b))
  LogRecord::set_attribute(log_record_b, "service", StringValue("service-b"))
  
  Logger::emit(logger_b, log_record_b)
  
  // 验证跨服务日志关联
  assert_eq(LogRecord::trace_id(log_record_a), LogRecord::trace_id(log_record_b))
  assert_eq(LogRecord::trace_id(log_record_a), SpanContext::trace_id(ctx_a))
  assert_eq(LogRecord::trace_id(log_record_b), SpanContext::trace_id(ctx_b))
  
  Span::end(span_a)
}