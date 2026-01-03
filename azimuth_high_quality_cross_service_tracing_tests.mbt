// Azimuth Telemetry System - High Quality Cross-Service Tracing Tests
// 跨服务分布式追踪测试

test "分布式追踪链路完整性" {
  // 创建分布式追踪链路
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  
  // 服务A：入口点
  let service_a_span_id = "00f067aa0ba902b7"
  let service_a_context = azimuth::SpanContext::new(trace_id, service_a_span_id, true, "rojo=00f067aa0ba902b7")
  let service_a_span = azimuth::Span::new("service_a_operation", azimuth::Server, service_a_context)
  
  // 服务A添加属性和事件
  let service_a_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_a_attrs, "service.name", azimuth::StringValue("service-a"))
  azimuth::Attributes::set(service_a_attrs, "service.version", azimuth::StringValue("1.2.3"))
  azimuth::Attributes::set(service_a_attrs, "http.method", azimuth::StringValue("GET"))
  azimuth::Attributes::set(service_a_attrs, "http.url", azimuth::StringValue("/api/process"))
  
  azimuth::Span::add_event(service_a_span, "request_received", Some(service_a_attrs))
  
  // 服务B：调用链中的第二个服务
  let service_b_span_id = "b7ad6b7169203331"
  let service_b_context = azimuth::SpanContext::new(trace_id, service_b_span_id, true, "rojo=00f067aa0ba902b7")
  let service_b_span = azimuth::Span::new("service_b_operation", azimuth::Client, service_b_context)
  
  // 服务B添加属性
  let service_b_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_b_attrs, "service.name", azimuth::StringValue("service-b"))
  azimuth::Attributes::set(service_b_attrs, "service.version", azimuth::StringValue("2.1.0"))
  azimuth::Attributes::set(service_b_attrs, "rpc.service", azimuth::StringValue("service-b.process"))
  
  azimuth::Span::add_event(service_b_span, "rpc_call_initiated", Some(service_b_attrs))
  
  // 服务C：调用链中的第三个服务
  let service_c_span_id = "00f067aa0ba902b7"
  let service_c_context = azimuth::SpanContext::new(trace_id, service_c_span_id, true, "rojo=00f067aa0ba902b7")
  let service_c_span = azimuth::Span::new("service_c_operation", azimuth::Internal, service_c_context)
  
  // 服务C添加属性
  let service_c_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_c_attrs, "service.name", azimuth::StringValue("service-c"))
  azimuth::Attributes::set(service_c_attrs, "service.version", azimuth::StringValue("3.0.1"))
  azimuth::Attributes::set(service_c_attrs, "db.system", azimuth::StringValue("postgresql"))
  azimuth::Attributes::set(service_c_attrs, "db.statement", azimuth::StringValue("SELECT * FROM users"))
  
  azimuth::Span::add_event(service_c_span, "db_query_executed", Some(service_c_attrs))
  
  // 验证追踪链路完整性
  assert_eq(
    azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_a_span)),
    azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_b_span))
  )
  
  assert_eq(
    azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_b_span)),
    azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_c_span))
  )
  
  // 验证跨度ID唯一性
  assert_ne(
    azimuth::SpanContext::span_id(azimuth::Span::span_context(service_a_span)),
    azimuth::SpanContext::span_id(azimuth::Span::span_context(service_b_span))
  )
  
  assert_ne(
    azimuth::SpanContext::span_id(azimuth::Span::span_context(service_b_span)),
    azimuth::SpanContext::span_id(azimuth::Span::span_context(service_c_span))
  )
  
  assert_ne(
    azimuth::SpanContext::span_id(azimuth::Span::span_context(service_a_span)),
    azimuth::SpanContext::span_id(azimuth::Span::span_context(service_c_span))
  )
  
  // 结束所有跨度
  azimuth::Span::end(service_c_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
  
  // 验证所有跨度都已结束
  assert_false(azimuth::Span::is_recording(service_a_span))
  assert_false(azimuth::Span::is_recording(service_b_span))
  assert_false(azimuth::Span::is_recording(service_c_span))
}

test "跨服务上下文传播" {
  // 创建初始上下文
  let trace_id = "5c7c8c8c8c8c8c8c8c8c8c8c8c8c8c8c"
  let initial_span_id = "1111111111111111"
  let initial_context = azimuth::SpanContext::new(trace_id, initial_span_id, true, "")
  
  // 创建行李用于跨服务传播
  let baggage = azimuth::Baggage::new()
  let updated_baggage = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let final_baggage = azimuth::Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  // 创建HTTP请求载体
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(carrier, "traceparent", "00-" + trace_id + "-" + initial_span_id + "-01")
  azimuth::TextMapCarrier::set(carrier, "tracestate", "key1=value1,key2=value2")
  azimuth::TextMapCarrier::set(carrier, "x-user-id", "user-12345")
  azimuth::TextMapCarrier::set(carrier, "x-request-id", "req-67890")
  
  // 验证载体中的追踪信息
  let traceparent = azimuth::TextMapCarrier::get(carrier, "traceparent")
  match traceparent {
    Some(value) => {
      assert_true(value.contains(trace_id))
      assert_true(value.contains(initial_span_id))
    }
    None => assert_true(false)
  }
  
  let user_id = azimuth::TextMapCarrier::get(carrier, "x-user-id")
  match user_id {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_true(false)
  }
  
  let request_id = azimuth::TextMapCarrier::get(carrier, "x-request-id")
  match request_id {
    Some(value) => assert_eq(value, "req-67890")
    None => assert_true(false)
  }
  
  // 模拟服务间传播
  let service_b_span_id = "2222222222222222"
  let service_b_context = azimuth::SpanContext::new(trace_id, service_b_span_id, true, "key1=value1,key2=value2")
  let service_b_span = azimuth::Span::new("service_b_operation", azimuth::Server, service_b_context)
  
  // 验证上下文传播
  assert_eq(
    azimuth::SpanContext::trace_id(azimuth::Span::span_context(service_b_span)),
    trace_id
  )
  
  assert_eq(
    azimuth::SpanContext::span_id(azimuth::Span::span_context(service_b_span)),
    service_b_span_id
  )
  
  // 验证行李传播
  let propagated_user_id = azimuth::Baggage::get_entry(final_baggage, "user.id")
  match propagated_user_id {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_true(false) // 简化实现可能返回None
  }
  
  let propagated_request_id = azimuth::Baggage::get_entry(final_baggage, "request.id")
  match propagated_request_id {
    Some(value) => assert_eq(value, "req-67890")
    None => assert_true(false) // 简化实现可能返回None
  }
  
  // 结束跨度
  azimuth::Span::end(service_b_span)
}

test "跨服务错误传播" {
  // 创建错误场景测试
  let trace_id = "error_trace_id_1234567890"
  let service_a_span_id = "service_a_error_span"
  let service_a_context = azimuth::SpanContext::new(trace_id, service_a_span_id, true, "")
  let service_a_span = azimuth::Span::new("service_a_error_operation", azimuth::Server, service_a_context)
  
  // 服务A遇到错误
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("TimeoutError"))
  azimuth::Attributes::set(error_attrs, "error.message", azimuth::StringValue("Service B timeout after 30 seconds"))
  azimuth::Attributes::set(error_attrs, "service.name", azimuth::StringValue("service-a"))
  
  azimuth::Span::add_event(service_a_span, "error_occurred", Some(error_attrs))
  azimuth::Span::set_status(service_a_span, azimuth::Error, Some("Operation failed due to timeout"))
  
  // 服务B也记录错误
  let service_b_span_id = "service_b_error_span"
  let service_b_context = azimuth::SpanContext::new(trace_id, service_b_span_id, true, "")
  let service_b_span = azimuth::Span::new("service_b_error_operation", azimuth::Client, service_b_context)
  
  let service_b_error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_b_error_attrs, "error.type", azimuth::StringValue("DatabaseError"))
  azimuth::Attributes::set(service_b_error_attrs, "error.message", azimuth::StringValue("Connection to database failed"))
  azimuth::Attributes::set(service_b_error_attrs, "service.name", azimuth::StringValue("service-b"))
  azimuth::Attributes::set(service_b_error_attrs, "db.connection_pool", azimuth::StringValue("exhausted"))
  
  azimuth::Span::add_event(service_b_span, "database_error", Some(service_b_error_attrs))
  azimuth::Span::set_status(service_b_span, azimuth::Error, Some("Database connection failed"))
  
  // 验证错误状态
  assert_eq(azimuth::Span::status(service_a_span), azimuth::Error)
  assert_eq(azimuth::Span::status(service_b_span), azimuth::Error)
  
  // 验证错误属性
  let service_a_error_type = azimuth::Attributes::get(error_attrs, "error.type")
  match service_a_error_type {
    Some(azimuth::StringValue(error_type)) => assert_eq(error_type, "TimeoutError")
    _ => assert_true(false)
  }
  
  let service_b_error_type = azimuth::Attributes::get(service_b_error_attrs, "error.type")
  match service_b_error_type {
    Some(azimuth::StringValue(error_type)) => assert_eq(error_type, "DatabaseError")
    _ => assert_true(false)
  }
  
  // 结束跨度
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
  
  // 验证错误状态保持
  assert_eq(azimuth::Span::status(service_a_span), azimuth::Error)
  assert_eq(azimuth::Span::status(service_b_span), azimuth::Error)
}

test "跨服务性能追踪" {
  // 创建性能追踪测试
  let trace_id = "performance_trace_id"
  let start_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  // 服务A：记录开始时间
  let service_a_span_id = "perf_service_a_span"
  let service_a_context = azimuth::SpanContext::new(trace_id, service_a_span_id, true, "")
  let service_a_span = azimuth::Span::new("service_a_performance", azimuth::Server, service_a_context)
  
  let service_a_perf_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_a_perf_attrs, "service.name", azimuth::StringValue("service-a"))
  azimuth::Attributes::set(service_a_perf_attrs, "operation.type", azimuth::StringValue("cpu_intensive"))
  azimuth::Attributes::set(service_a_perf_attrs, "cpu.usage", azimuth::FloatValue(85.5))
  azimuth::Attributes::set(service_a_perf_attrs, "memory.usage", azimuth::FloatValue(512.0))
  
  azimuth::Span::add_event(service_a_span, "performance_metrics", Some(service_a_perf_attrs))
  
  // 服务B：记录中间处理时间
  let service_b_span_id = "perf_service_b_span"
  let service_b_context = azimuth::SpanContext::new(trace_id, service_b_span_id, true, "")
  let service_b_span = azimuth::Span::new("service_b_performance", azimuth::Client, service_b_context)
  
  let service_b_perf_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_b_perf_attrs, "service.name", azimuth::StringValue("service-b"))
  azimuth::Attributes::set(service_b_perf_attrs, "operation.type", azimuth::StringValue("io_intensive"))
  azimuth::Attributes::set(service_b_perf_attrs, "io.wait.time", azimuth::FloatValue(120.5))
  azimuth::Attributes::set(service_b_perf_attrs, "network.latency", azimuth::FloatValue(45.2))
  
  azimuth::Span::add_event(service_b_span, "io_metrics", Some(service_b_perf_attrs))
  
  // 服务C：记录结束时间
  let service_c_span_id = "perf_service_c_span"
  let service_c_context = azimuth::SpanContext::new(trace_id, service_c_span_id, true, "")
  let service_c_span = azimuth::Span::new("service_c_performance", azimuth::Internal, service_c_context)
  
  let service_c_perf_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(service_c_perf_attrs, "service.name", azimuth::StringValue("service-c"))
  azimuth::Attributes::set(service_c_perf_attrs, "operation.type", azimuth::StringValue("memory_intensive"))
  azimuth::Attributes::set(service_c_perf_attrs, "memory.allocated", azimuth::FloatValue(1024.0))
  azimuth::Attributes::set(service_c_perf_attrs, "gc.collections", azimuth::IntValue(3))
  
  azimuth::Span::add_event(service_c_span, "memory_metrics", Some(service_c_perf_attrs))
  
  // 创建性能度量
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "performance_meter")
  
  // 创建性能计数器
  let operation_counter = azimuth::Meter::create_counter(meter, "operations_total", Some("Total operations"), Some("count"))
  let latency_histogram = azimuth::Meter::create_histogram(meter, "operation_latency", Some("Operation latency"), Some("ms"))
  let resource_gauge = azimuth::Meter::create_gauge(meter, "resource_usage", Some("Resource usage"), Some("percent"))
  
  // 记录性能数据
  azimuth::Counter::add(operation_counter, 1.0)
  azimuth::Histogram::record(latency_histogram, 250.5)
  azimuth::Gauge::record(resource_gauge, 75.8)
  
  // 验证性能属性
  let service_a_cpu = azimuth::Attributes::get(service_a_perf_attrs, "cpu.usage")
  match service_a_cpu {
    Some(azimuth::FloatValue(cpu)) => assert_eq(cpu, 85.5)
    _ => assert_true(false)
  }
  
  let service_b_io = azimuth::Attributes::get(service_b_perf_attrs, "io.wait.time")
  match service_b_io {
    Some(azimuth::FloatValue(io)) => assert_eq(io, 120.5)
    _ => assert_true(false)
  }
  
  let service_c_memory = azimuth::Attributes::get(service_c_perf_attrs, "memory.allocated")
  match service_c_memory {
    Some(azimuth::FloatValue(memory)) => assert_eq(memory, 1024.0)
    _ => assert_true(false)
  }
  
  // 结束跨度
  azimuth::Span::end(service_c_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
}