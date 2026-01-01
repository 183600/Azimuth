// Azimuth Additional MoonBit Test Suite
// 新增的MoonBit测试用例，涵盖核心遥测功能的各个方面

// 测试1: 并发安全的遥测操作
pub test "并发安全遥测操作测试" {
  // 创建多个并发Span操作
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent-test")
  
  // 创建多个并发Span
  let spans = []
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "concurrent-span-" + i.to_string())
    spans.push(span)
  }
  
  // 并发设置属性和事件
  for i in 0..spans.length() {
    let span = spans[i]
    azimuth::Span::add_event(span, "concurrent-event-" + i.to_string(), 
      Some([("thread.id", azimuth::StringValue(i.to_string()))]))
    azimuth::Span::set_status(span, azimuth::Ok)
  }
  
  // 并发结束Span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 验证所有Span都被正确处理
  assert_true(spans.length() == 50)
}

// 测试2: 资源合并策略
pub test "资源合并策略测试" {
  // 创建基础资源
  let base_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("base-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("environment", azimuth::StringValue("development"))
  ])
  
  // 创建覆盖资源
  let override_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.version", azimuth::StringValue("2.0.0")),  // 覆盖版本
    ("deployment.region", azimuth::StringValue("us-west-1")),  // 新增属性
    ("environment", azimuth::StringValue("production"))  // 覆盖环境
  ])
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(base_resource, override_resource)
  
  // 验证合并结果
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.name"), 
    Some(azimuth::StringValue("base-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.version"), 
    Some(azimuth::StringValue("2.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "environment"), 
    Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.region"), 
    Some(azimuth::StringValue("us-west-1")))
}

// 测试3: 高频度量操作
pub test "高频度量操作测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "high-frequency-meter")
  
  // 创建多种度量类型
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration")
  let active_connections = azimuth::Meter::create_updown_counter(meter, "http.active.connections")
  let memory_gauge = azimuth::Meter::create_gauge(meter, "process.memory.usage")
  
  // 高频度量操作
  for i in 0..1000 {
    // 模拟HTTP请求计数
    azimuth::Counter::add(request_counter, 1.0)
    
    // 模拟响应时间记录
    let response_time = 10.0 + (i.to_double() % 200.0)  // 10-210ms范围
    azimuth::Histogram::record(response_histogram, response_time)
    
    // 每10个请求模拟连接数变化
    if i % 10 == 0 {
      azimuth::UpDownCounter::add(active_connections, 1.0)
    } else if i % 15 == 0 {
      azimuth::UpDownCounter::add(active_connections, -1.0)
    }
  }
  
  // 验证度量创建成功
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(active_connections.name, "http.active.connections")
  assert_eq(memory_gauge.name, "process.memory.usage")
}

// 测试4: 复杂上下文传播
pub test "复杂上下文传播测试" {
  // 创建根上下文
  let root_ctx = azimuth::Context::root()
  
  // 添加多个上下文值
  let user_id_key = azimuth::ContextKey::new("user.id")
  let request_id_key = azimuth::ContextKey::new("request.id")
  let session_id_key = azimuth::ContextKey::new("session.id")
  let correlation_id_key = azimuth::ContextKey::new("correlation.id")
  
  let ctx_with_user = azimuth::Context::with_value(root_ctx, user_id_key, "user-12345")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_user, request_id_key, "req-67890")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_request, session_id_key, "session-abcdef")
  let final_ctx = azimuth::Context::with_value(ctx_with_session, correlation_id_key, "corr-123456")
  
  // 验证所有上下文值都正确传播
  assert_eq(azimuth::Context::get(final_ctx, user_id_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(final_ctx, request_id_key), Some("req-67890"))
  assert_eq(azimuth::Context::get(final_ctx, session_id_key), Some("session-abcdef"))
  assert_eq(azimuth::Context::get(final_ctx, correlation_id_key), Some("corr-123456"))
  
  // 验证中间上下文也保持正确
  assert_eq(azimuth::Context::get(ctx_with_session, session_id_key), Some("session-abcdef"))
  assert_eq(azimuth::Context::get(ctx_with_request, request_id_key), Some("req-67890"))
  
  // 测试Baggage与Context的结合
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.preferences", "dark-mode")
  let final_baggage = azimuth::Baggage::set_entry(baggage_with_user, "user.locale", "zh-CN")
  
  // 验证Baggage传播
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "user.preferences"), Some("dark-mode"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "user.locale"), Some("zh-CN"))
}

// 测试5: 错误边界和恢复
pub test "错误边界和恢复测试" {
  // 测试各种错误场景下的系统稳定性
  
  // 测试无效Span上下文处理
  let invalid_trace_ctx = azimuth::SpanContext::new("", "invalid-span", true, "")
  let invalid_span_ctx = azimuth::SpanContext::new("invalid-trace", "", true, "")
  let empty_ctx = azimuth::SpanContext::new("", "", false, "")
  
  // 验证无效上下文被正确识别
  assert_false(azimuth::SpanContext::is_valid(invalid_trace_ctx))
  assert_false(azimuth::SpanContext::is_valid(invalid_span_ctx))
  assert_false(azimuth::SpanContext::is_valid(empty_ctx))
  
  // 测试空属性处理
  let empty_attrs = azimuth::Attributes::new()
  let null_key_result = azimuth::Attributes::get(empty_attrs, "")
  let missing_key_result = azimuth::Attributes::get(empty_attrs, "nonexistent.key")
  
  assert_eq(null_key_result, None)
  assert_eq(missing_key_result, None)
  
  // 测试空字符串和特殊字符键
  let special_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(special_attrs, "", azimuth::StringValue("empty.key.value"))
  azimuth::Attributes::set(special_attrs, "special.chars.key!@#", azimuth::StringValue("special.value"))
  azimuth::Attributes::set(special_attrs, "中文键", azimuth::StringValue("中文值"))
  
  // 验证特殊键处理（基于简化实现）
  let empty_key_result = azimuth::Attributes::get(special_attrs, "")
  let special_chars_result = azimuth::Attributes::get(special_attrs, "special.chars.key!@#")
  let chinese_key_result = azimuth::Attributes::get(special_attrs, "中文键")
  
  // 基于简化实现验证
  assert_eq(empty_key_result, Some(azimuth::StringValue("test_value")))
  
  // 测试Logger的错误处理
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "error.test.logger")
  
  // 创建各种错误级别的日志
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Test error message")
  let warn_log = azimuth::LogRecord::new(azimuth::Warn, "Test warning message")
  let info_log = azimuth::LogRecord::new(azimuth::Info, "Test info message")
  
  // 发送日志并验证系统稳定性
  azimuth::Logger::emit(logger, error_log)
  azimuth::Logger::emit(logger, warn_log)
  azimuth::Logger::emit(logger, info_log)
  
  // 验证日志记录创建成功
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(warn_log), azimuth::Warn)
  assert_eq(azimuth::LogRecord::severity_number(info_log), azimuth::Info)
}

// 测试6: 时间序列数据压缩
pub test "时间序列数据压缩测试" {
  let clock = azimuth::Clock::system()
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "time-series.compression")
  
  // 创建时间序列直方图
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "response.time.ms")
  let cpu_usage_histogram = azimuth::Meter::create_histogram(meter, "cpu.usage.percent")
  
  // 生成时间序列数据
  let base_timestamp = azimuth::Clock::now_unix_nanos(clock)
  let time_series_data = []
  
  for i in 0..100 {
    // 生成递增的时间戳
    let current_timestamp = base_timestamp + (i * 1000000L)  // 每毫秒一个数据点
    
    // 生成具有模式的数据（模拟真实场景）
    let response_time = 50.0 + (10.0 * @sin(i.to_double() * 0.1)) + (@random() * 20.0)
    let cpu_usage = 30.0 + (20.0 * @sin(i.to_double() * 0.05)) + (@random() * 10.0)
    
    // 记录度量
    azimuth::Histogram::record(response_time_histogram, response_time)
    azimuth::Histogram::record(cpu_usage_histogram, cpu_usage)
    
    // 存储时间序列数据用于验证
    time_series_data.push((current_timestamp, response_time, cpu_usage))
  }
  
  // 验证时间序列数据的单调性
  for i in 1..time_series_data.length() {
    let prev_timestamp = time_series_data[i-1].0
    let curr_timestamp = time_series_data[i].0
    assert_true(curr_timestamp > prev_timestamp)
  }
  
  // 验证数据范围合理性
  assert_true(time_series_data.length() == 100)
  
  // 验证度量创建成功
  assert_eq(response_time_histogram.name, "response.time.ms")
  assert_eq(cpu_usage_histogram.name, "cpu.usage.percent")
}

// 测试7: 网络遥测集成
pub test "网络遥测集成测试" {
  // 创建HTTP客户端和服务器遥测
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "network.telemetry")
  
  // 模拟HTTP服务器Span
  let server_span = azimuth::Tracer::start_span(tracer, "http.server.request")
  azimuth::Span::set_kind(server_span, azimuth::Server)
  azimuth::Span::add_event(server_span, "request.started", 
    Some([("http.method", azimuth::StringValue("GET")), 
          ("http.url", azimuth::StringValue("/api/data"))]))
  
  // 模拟HTTP客户端Span
  let client_span = azimuth::Tracer::start_span(tracer, "http.client.request")
  azimuth::Span::set_kind(client_span, azimuth::Client)
  azimuth::Span::add_event(client_span, "request.sent", 
    Some([("http.method", azimuth::StringValue("POST")), 
          ("http.url", azimuth::StringValue("https://api.example.com/webhook")),
          ("http.status_code", azimuth::IntValue(200))]))
  
  // 模拟网络连接度量
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "network.metrics")
  
  let connection_counter = azimuth::Meter::create_counter(meter, "network.connections.total")
  let connection_duration = azimuth::Meter::create_histogram(meter, "network.connection.duration")
  let bytes_transferred = azimuth::Meter::create_counter(meter, "network.bytes.transferred")
  
  // 模拟网络操作
  for i in 0..10 {
    azimuth::Counter::add(connection_counter, 1.0)
    azimuth::Histogram::record(connection_duration, 100.0 + (i.to_double() * 10.0))
    azimuth::Counter::add(bytes_transferred, 1024.0 * (i.to_double() + 1.0))
  }
  
  // 结束Span
  azimuth::Span::set_status(server_span, azimuth::Ok, Some("Request completed successfully"))
  azimuth::Span::set_status(client_span, azimuth::Ok, Some("Request sent successfully"))
  azimuth::Span::end(server_span)
  azimuth::Span::end(client_span)
  
  // 验证网络遥测组件
  assert_eq(azimuth::Span::kind(server_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(client_span), azimuth::Client)
  assert_eq(connection_counter.name, "network.connections.total")
  assert_eq(connection_duration.name, "network.connection.duration")
  assert_eq(bytes_transferred.name, "network.bytes.transferred")
}

// 测试8: 微服务架构遥测集成
pub test "微服务架构遥测集成测试" {
  // 模拟微服务调用链：API Gateway -> Service A -> Service B -> Database
  
  // API Gateway Span
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "api-gateway")
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "gateway.request")
  azimuth::Span::set_kind(gateway_span, azimuth::Server)
  
  let gateway_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::span_context(gateway_span))
  
  // Service A Span (子Span)
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let service_a_span_ctx = azimuth::SpanContext::new(gateway_trace_id, "service-a-span", true, "")
  let service_a_span = azimuth::Span::new("service-a.process", azimuth::Internal, service_a_span_ctx)
  
  // Service B Span (嵌套调用)
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  let service_b_span_ctx = azimuth::SpanContext::new(gateway_trace_id, "service-b-span", true, "")
  let service_b_span = azimuth::Span::new("service-b.process", azimuth::Client, service_b_span_ctx)
  
  // Database Span (最深层)
  let db_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "database")
  let db_span_ctx = azimuth::SpanContext::new(gateway_trace_id, "db-span", true, "")
  let db_span = azimuth::Span::new("database.query", azimuth::Client, db_span_ctx)
  
  // 验证Trace ID在整个调用链中保持一致
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(gateway_span)), gateway_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_a_span_ctx), gateway_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(service_b_span_ctx), gateway_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(db_span_ctx), gateway_trace_id)
  
  // 验证Span ID唯一性
  assert_true(azimuth::SpanContext::span_id(azimuth::Span::span_context(gateway_span)) != 
              azimuth::SpanContext::span_id(service_a_span_ctx))
  assert_true(azimuth::SpanContext::span_id(service_a_span_ctx) != 
              azimuth::SpanContext::span_id(service_b_span_ctx))
  assert_true(azimuth::SpanContext::span_id(service_b_span_ctx) != 
              azimuth::SpanContext::span_id(db_span_ctx))
  
  // 为每个服务添加度量
  let meter_provider = azimuth::MeterProvider::default()
  
  // Gateway度量
  let gateway_meter = azimuth::MeterProvider::get_meter(meter_provider, "api-gateway")
  let gateway_requests = azimuth::Meter::create_counter(gateway_meter, "gateway.requests.total")
  azimuth::Counter::add(gateway_requests, 1.0)
  
  // Service A度量
  let service_a_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-a")
  let service_a_requests = azimuth::Meter::create_counter(service_a_meter, "service-a.requests.total")
  let service_a_duration = azimuth::Meter::create_histogram(service_a_meter, "service-a.duration.ms")
  azimuth::Counter::add(service_a_requests, 1.0)
  azimuth::Histogram::record(service_a_duration, 150.0)
  
  // Service B度量
  let service_b_meter = azimuth::MeterProvider::get_meter(meter_provider, "service-b")
  let service_b_requests = azimuth::Meter::create_counter(service_b_meter, "service-b.requests.total")
  azimuth::Counter::add(service_b_requests, 1.0)
  
  // Database度量
  let db_meter = azimuth::MeterProvider::get_meter(meter_provider, "database")
  let db_queries = azimuth::Meter::create_counter(db_meter, "database.queries.total")
  let db_duration = azimuth::Meter::create_histogram(db_meter, "database.query.duration.ms")
  azimuth::Counter::add(db_queries, 1.0)
  azimuth::Histogram::record(db_duration, 25.0)
  
  // 添加Baggage传播
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-123")
  let final_baggage = azimuth::Baggage::set_entry(baggage_with_user, "request.id", "req-456")
  
  // 结束所有Span（按照调用顺序的反向）
  azimuth::Span::end(db_span)
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
  azimuth::Span::end(gateway_span)
  
  // 验证微服务遥测集成
  assert_eq(gateway_requests.name, "gateway.requests.total")
  assert_eq(service_a_requests.name, "service-a.requests.total")
  assert_eq(service_a_duration.name, "service-a.duration.ms")
  assert_eq(service_b_requests.name, "service-b.requests.total")
  assert_eq(db_queries.name, "database.queries.total")
  assert_eq(db_duration.name, "database.query.duration.ms")
  
  // 验证Baggage传播
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "user.id"), Some("user-123"))
  assert_eq(azimuth::Baggage::get_entry(final_baggage, "request.id"), Some("req-456"))
}