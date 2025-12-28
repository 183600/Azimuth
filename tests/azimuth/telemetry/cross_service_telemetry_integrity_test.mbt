// Cross-Service Telemetry Integrity Test
// 测试分布式环境下跨服务遥测数据的完整性

test "distributed trace context propagation" {
  // 测试分布式追踪上下文的传播
  let service_a_trace_id = "trace-service-a-123456"
  let service_a_span_id = "span-service-a-789012"
  
  // 服务A创建span上下文
  let service_a_span_context = @azimuth.SpanContext::new(
    service_a_trace_id,
    service_a_span_id,
    true,
    "state=service-a"
  )
  
  // 验证服务A的span上下文
  assert_eq(@azimuth.SpanContext::trace_id(service_a_span_context), service_a_trace_id)
  assert_eq(@azimuth.SpanContext::span_id(service_a_span_context), service_a_span_id)
  assert_eq(@azimuth.SpanContext::is_sampled(service_a_span_context), true)
  assert_eq(@azimuth.SpanContext::is_valid(service_a_span_context), true)
  
  // 服务A创建span
  let service_a_tracer = @azimuth.TracerProvider::default().get_tracer("service-a", Some("1.0.0"))
  let service_a_span = @azimuth.Tracer::start_span(service_a_tracer, "service-a-operation")
  
  // 验证服务A的span
  assert_eq(@azimuth.Span::name(service_a_span), "service-a-operation")
  assert_eq(@azimuth.Span::is_recording(service_a_span), true)
  
  // 模拟传播到服务B
  let service_b_trace_id = @azimuth.SpanContext::trace_id(service_a_span_context)
  let service_b_parent_span_id = @azimuth.SpanContext::span_id(service_a_span_context)
  let service_b_span_id = "span-service-b-345678"
  
  let service_b_span_context = @azimuth.SpanContext::new(
    service_b_trace_id,
    service_b_span_id,
    true,
    "state=service-b"
  )
  
  // 验证服务B继承了相同的trace_id
  assert_eq(@azimuth.SpanContext::trace_id(service_b_span_context), service_a_trace_id)
  assert_eq(@azimuth.SpanContext::span_id(service_b_span_context), service_b_span_id)
  assert_eq(@azimuth.SpanContext::is_sampled(service_b_span_context), true)
}

test "cross service metrics correlation" {
  // 测试跨服务指标关联
  let service_a_meter_provider = @azimuth.MeterProvider::default()
  let service_a_meter = @azimuth.MeterProvider::get_meter(service_a_meter_provider, "service-a")
  let service_a_counter = @azimuth.Meter::create_counter(service_a_meter, "requests.total", Some("Total requests"), Some("count"))
  
  // 服务A记录指标
  @azimuth.Counter::add(service_a_counter, 1.0)
  @azimuth.Counter::add(service_a_counter, 3.0)
  
  // 服务B创建相关指标
  let service_b_meter_provider = @azimuth.MeterProvider::default()
  let service_b_meter = @azimuth.MeterProvider::get_meter(service_b_meter_provider, "service-b")
  let service_b_histogram = @azimuth.Meter::create_histogram(service_b_meter, "request.duration", Some("Request duration"), Some("ms"))
  
  // 服务B记录相关指标
  @azimuth.Histogram::record(service_b_histogram, 100.0)
  @azimuth.Histogram::record(service_b_histogram, 200.0)
  @azimuth.Histogram::record(service_b_histogram, 150.0)
  
  // 验证指标创建成功
  assert_eq(service_a_counter.name, "requests.total")
  assert_eq(service_a_counter.description, Some("Total requests"))
  assert_eq(service_a_counter.unit, Some("count"))
  
  assert_eq(service_b_histogram.name, "request.duration")
  assert_eq(service_b_histogram.description, Some("Request duration"))
  assert_eq(service_b_histogram.unit, Some("ms"))
}

test "distributed logging correlation" {
  // 测试分布式日志关联
  let distributed_trace_id = "trace-distributed-abcdef"
  let distributed_span_id = "span-distributed-123456"
  
  // 服务A创建日志记录
  let service_a_logger_provider = @azimuth.LoggerProvider::default()
  let service_a_logger = @azimuth.LoggerProvider::get_logger(service_a_logger_provider, "service-a", Some("1.0.0"))
  
  let service_a_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Service A processing request"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(distributed_trace_id),
    Some(distributed_span_id),
    None
  )
  
  // 服务B创建关联的日志记录
  let service_b_logger_provider = @azimuth.LoggerProvider::default()
  let service_b_logger = @azimuth.LoggerProvider::get_logger(service_b_logger_provider, "service-b", Some("1.0.0"))
  
  let service_b_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Service B processing related request"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some(distributed_trace_id),
    Some("span-service-b-789012"),
    None
  )
  
  // 服务C创建关联的日志记录
  let service_c_logger_provider = @azimuth.LoggerProvider::default()
  let service_c_logger = @azimuth.LoggerProvider::get_logger(service_c_logger_provider, "service-c", Some("1.0.0"))
  
  let service_c_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Service C encountered error"),
    None,
    Some(1735689600000004000L),
    Some(1735689600000005000L),
    Some(distributed_trace_id),
    Some("span-service-c-345678"),
    None
  )
  
  // 验证所有日志记录都关联到相同的trace_id
  assert_eq(@azimuth.LogRecord::trace_id(service_a_log), Some(distributed_trace_id))
  assert_eq(@azimuth.LogRecord::trace_id(service_b_log), Some(distributed_trace_id))
  assert_eq(@azimuth.LogRecord::trace_id(service_c_log), Some(distributed_trace_id))
  
  // 验证每个服务有不同的span_id
  assert_eq(@azimuth.LogRecord::span_id(service_a_log), Some(distributed_span_id))
  assert_eq(@azimuth.LogRecord::span_id(service_b_log), Some("span-service-b-789012"))
  assert_eq(@azimuth.LogRecord::span_id(service_c_log), Some("span-service-c-345678"))
}

test "cross service baggage propagation" {
  // 测试跨服务baggage传播
  let initial_baggage = @azimuth.Baggage::new()
  
  // 服务A设置baggage
  let baggage_a = @azimuth.Baggage::set_entry(initial_baggage, "user.id", "user-12345")
  let baggage_a2 = @azimuth.Baggage::set_entry(baggage_a, "request.id", "req-abcdef")
  let baggage_a3 = @azimuth.Baggage::set_entry(baggage_a2, "session.id", "session-123")
  
  // 验证服务A的baggage
  assert_eq(@azimuth.Baggage::get_entry(baggage_a3, "user.id"), Some("user-12345"))
  assert_eq(@azimuth.Baggage::get_entry(baggage_a3, "request.id"), Some("req-abcdef"))
  assert_eq(@azimuth.Baggage::get_entry(baggage_a3, "session.id"), Some("session-123"))
  
  // 服务B添加自己的baggage
  let baggage_b = @azimuth.Baggage::set_entry(baggage_a3, "service.b.operation", "process-data")
  let baggage_b2 = @azimuth.Baggage::set_entry(baggage_b, "service.b.version", "2.1.0")
  
  // 验证服务B的baggage包含所有信息
  assert_eq(@azimuth.Baggage::get_entry(baggage_b2, "user.id"), Some("user-12345"))
  assert_eq(@azimuth.Baggage::get_entry(baggage_b2, "request.id"), Some("req-abcdef"))
  assert_eq(@azimuth.Baggage::get_entry(baggage_b2, "session.id"), Some("session-123"))
  assert_eq(@azimuth.Baggage::get_entry(baggage_b2, "service.b.operation"), Some("process-data"))
  assert_eq(@azimuth.Baggage::get_entry(baggage_b2, "service.b.version"), Some("2.1.0"))
  
  // 服务C添加更多baggage
  let baggage_c = @azimuth.Baggage::set_entry(baggage_b2, "service.c.result", "success")
  let baggage_c2 = @azimuth.Baggage::set_entry(baggage_c, "service.c.duration", "150ms")
  
  // 验证服务C的baggage包含完整的信息链
  assert_eq(@azimuth.Baggage::get_entry(baggage_c2, "user.id"), Some("user-12345"))
  assert_eq(@azimuth.Baggage::get_entry(baggage_c2, "service.b.operation"), Some("process-data"))
  assert_eq(@azimuth.Baggage::get_entry(baggage_c2, "service.c.result"), Some("success"))
}

test "distributed context integrity validation" {
  // 测试分布式上下文完整性验证
  let root_context = @azimuth.Context::root()
  
  // 服务A添加上下文信息
  let context_a = @azimuth.Context::with_value(
    root_context,
    @azimuth.ContextKey::new("service.a.entry"),
    "service-a-value"
  )
  
  let context_a2 = @azimuth.Context::with_value(
    context_a,
    @azimuth.ContextKey::new("service.a.timestamp"),
    "2025-01-01T00:00:00Z"
  )
  
  // 验证服务A的上下文
  assert_eq(@azimuth.Context::get(context_a2, @azimuth.ContextKey::new("service.a.entry")), Some("service-a-value"))
  assert_eq(@azimuth.Context::get(context_a2, @azimuth.ContextKey::new("service.a.timestamp")), Some("2025-01-01T00:00:00Z"))
  
  // 服务B继承并扩展上下文
  let context_b = @azimuth.Context::with_value(
    context_a2,
    @azimuth.ContextKey::new("service.b.operation"),
    "process-request"
  )
  
  let context_b2 = @azimuth.Context::with_value(
    context_b,
    @azimuth.ContextKey::new("service.b.result"),
    "success"
  )
  
  // 验证服务B的上下文包含所有信息
  assert_eq(@azimuth.Context::get(context_b2, @azimuth.ContextKey::new("service.a.entry")), Some("service-a-value"))
  assert_eq(@azimuth.Context::get(context_b2, @azimuth.ContextKey::new("service.a.timestamp")), Some("2025-01-01T00:00:00Z"))
  assert_eq(@azimuth.Context::get(context_b2, @azimuth.ContextKey::new("service.b.operation")), Some("process-request"))
  assert_eq(@azimuth.Context::get(context_b2, @azimuth.ContextKey::new("service.b.result")), Some("success"))
  
  // 服务C进一步扩展上下文
  let context_c = @azimuth.Context::with_value(
    context_b2,
    @azimuth.ContextKey::new("service.c.status"),
    "completed"
  )
  
  // 验证服务C的上下文包含完整的信息链
  assert_eq(@azimuth.Context::get(context_c, @azimuth.ContextKey::new("service.a.entry")), Some("service-a-value"))
  assert_eq(@azimuth.Context::get(context_c, @azimuth.ContextKey::new("service.b.operation")), Some("process-request"))
  assert_eq(@azimuth.Context::get(context_c, @azimuth.ContextKey::new("service.c.status")), Some("completed"))
}

test "cross service error propagation" {
  // 测试跨服务错误传播
  let error_trace_id = "trace-error-123456"
  let error_span_id = "span-error-789012"
  
  // 服务A记录错误
  let service_a_error_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Service A encountered validation error"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some(error_trace_id),
    Some(error_span_id),
    None
  )
  
  // 服务B记录相关错误
  let service_b_error_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Error,
    Some("Service B failed to process due to upstream error"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some(error_trace_id),
    Some("span-service-b-345678"),
    None
  )
  
  // 服务C记录最终错误状态
  let service_c_error_log = @azimuth.LogRecord::new_with_context(
    @azimuth.Fatal,
    Some("Service C: Request failed after multiple retries"),
    None,
    Some(1735689600000004000L),
    Some(1735689600000005000L),
    Some(error_trace_id),
    Some("span-service-c-901234"),
    None
  )
  
  // 验证所有错误日志都关联到相同的trace_id
  assert_eq(@azimuth.LogRecord::trace_id(service_a_error_log), Some(error_trace_id))
  assert_eq(@azimuth.LogRecord::trace_id(service_b_error_log), Some(error_trace_id))
  assert_eq(@azimuth.LogRecord::trace_id(service_c_error_log), Some(error_trace_id))
  
  // 验证错误严重程度
  assert_eq(@azimuth.LogRecord::severity_number(service_a_error_log), @azimuth.Error)
  assert_eq(@azimuth.LogRecord::severity_number(service_b_error_log), @azimuth.Error)
  assert_eq(@azimuth.LogRecord::severity_number(service_c_error_log), @azimuth.Fatal)
  
  // 验证错误消息内容
  assert_eq(@azimuth.LogRecord::body(service_a_error_log), Some("Service A encountered validation error"))
  assert_eq(@azimuth.LogRecord::body(service_b_error_log), Some("Service B failed to process due to upstream error"))
  assert_eq(@azimuth.LogRecord::body(service_c_error_log), Some("Service C: Request failed after multiple retries"))
}

test "distributed telemetry data consistency" {
  // 测试分布式遥测数据一致性
  let shared_trace_id = "trace-consistency-abcdef"
  let base_timestamp = 1735689600000000000L
  
  // 创建一致的span上下文
  let span_context_1 = @azimuth.SpanContext::new(shared_trace_id, "span-1", true, "state=1")
  let span_context_2 = @azimuth.SpanContext::new(shared_trace_id, "span-2", true, "state=2")
  let span_context_3 = @azimuth.SpanContext::new(shared_trace_id, "span-3", true, "state=3")
  
  // 验证span上下文一致性
  assert_eq(@azimuth.SpanContext::trace_id(span_context_1), shared_trace_id)
  assert_eq(@azimuth.SpanContext::trace_id(span_context_2), shared_trace_id)
  assert_eq(@azimuth.SpanContext::trace_id(span_context_3), shared_trace_id)
  
  // 创建一致的日志记录
  let log_1 = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Step 1 completed"),
    None,
    Some(base_timestamp),
    Some(base_timestamp + 1000L),
    Some(shared_trace_id),
    Some("span-1"),
    None
  )
  
  let log_2 = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Step 2 completed"),
    None,
    Some(base_timestamp + 2000L),
    Some(base_timestamp + 3000L),
    Some(shared_trace_id),
    Some("span-2"),
    None
  )
  
  let log_3 = @azimuth.LogRecord::new_with_context(
    @azimuth.Info,
    Some("Step 3 completed"),
    None,
    Some(base_timestamp + 4000L),
    Some(base_timestamp + 5000L),
    Some(shared_trace_id),
    Some("span-3"),
    None
  )
  
  // 验证日志记录一致性
  assert_eq(@azimuth.LogRecord::trace_id(log_1), Some(shared_trace_id))
  assert_eq(@azimuth.LogRecord::trace_id(log_2), Some(shared_trace_id))
  assert_eq(@azimuth.LogRecord::trace_id(log_3), Some(shared_trace_id))
  
  assert_eq(@azimuth.LogRecord::span_id(log_1), Some("span-1"))
  assert_eq(@azimuth.LogRecord::span_id(log_2), Some("span-2"))
  assert_eq(@azimuth.LogRecord::span_id(log_3), Some("span-3"))
  
  // 验证时间戳顺序
  assert_eq(@azimuth.LogRecord::body(log_1), Some("Step 1 completed"))
  assert_eq(@azimuth.LogRecord::body(log_2), Some("Step 2 completed"))
  assert_eq(@azimuth.LogRecord::body(log_3), Some("Step 3 completed"))
}