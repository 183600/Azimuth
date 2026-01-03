// Azimuth 高级遥测系统测试用例
// 专为MoonBit设计的高质量测试套件

// 测试1: Span状态管理和事件处理
test "span lifecycle state management with events" {
  // 创建Span上下文
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_context = SpanContext::new(trace_id, span_id, true, "rojo=00f067aa0ba902b7")
  
  // 创建Span
  let span = Span::new("http_request", Client, span_context)
  
  // 验证Span初始状态
  assert_eq(Span::name(span), "http_request")
  assert_eq(Span::kind(span), Client)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_context)
  
  // 测试Span状态变更
  assert_eq(Span::status(span), Unset)
  
  // 添加事件并验证
  Span::add_event(span, "request_started", Some([("http.method", StringValue("GET")), ("http.url", StringValue("https://api.example.com"))]))
  Span::add_event(span, "request_completed", Some([("http.status_code", IntValue(200))]))
  
  // 设置状态并验证
  Span::set_status(span, Ok, Some("Request completed successfully"))
  
  // 结束Span并验证状态
  Span::end(span)
  
  // 验证Span上下文有效性
  assert_true(SpanContext::is_valid(span_context))
  assert_true(SpanContext::is_sampled(span_context))
  assert_eq(SpanContext::trace_id(span_context), trace_id)
  assert_eq(SpanContext::span_id(span_context), span_id)
}

// 测试2: Metrics仪表盘和聚合功能
test "metrics dashboard aggregation functionality" {
  // 创建Meter和Provider
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "azimuth_dashboard")
  
  // 创建各种指标
  let request_counter = Meter::create_counter(meter, "http_requests_total")
  let response_histogram = Meter::create_histogram(meter, "http_response_duration_ms", Some("HTTP response duration in milliseconds"), Some("ms"))
  let active_connections = Meter::create_gauge(meter, "active_connections", Some("Currently active connections"), Some("connections"))
  let error_rate = Meter::create_updown_counter(meter, "error_rate", Some("Error rate percentage"), Some("percent"))
  
  // 模拟指标数据
  Counter::add(request_counter, 1000.0, Some(Attributes::new()))
  Counter::add(request_counter, 500.0, Some(Attributes::new()))
  
  Histogram::record(response_histogram, 150.5, Some(Attributes::new()))
  Histogram::record(response_histogram, 200.3, Some(Attributes::new()))
  Histogram::record(response_histogram, 125.7, Some(Attributes::new()))
  
  // 模拟仪表盘聚合计算
  let counter_values = [1000.0, 500.0]
  let total_requests = counter_values.reduce(|acc, val| acc + val, 0.0)
  let avg_histogram_value = (150.5 + 200.3 + 125.7) / 3.0
  
  // 验证聚合结果
  assert_eq(total_requests, 1500.0)
  assert_eq(avg_histogram_value, 158.83333333333334)
  
  // 验证指标属性
  assert_eq(request_counter.name, "http_requests_total")
  assert_eq(response_histogram.name, "http_response_duration_ms")
  assert_eq(response_histogram.description, Some("HTTP response duration in milliseconds"))
  assert_eq(response_histogram.unit, Some("ms"))
  
  // 验证仪表盘数据点
  let dashboard_data_points = [
    ("metric_name", "http_requests_total"),
    ("metric_value", total_requests.to_string()),
    ("metric_type", "counter"),
    ("timestamp", "1640995200000")
  ]
  
  assert_eq(dashboard_data_points.length(), 4)
  assert_true(dashboard_data_points.map(|(k, v)| k + "=" + v).reduce(|acc, pair| acc + "," + pair, "").contains("metric_value=1500.0"))
}

// 测试3: 日志关联性和上下文传播
test "log correlation with context propagation" {
  // 创建日志记录器
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "azimuth.correlation.test")
  
  // 创建关联的Span上下文
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "1111111111111111"
  let span_context = SpanContext::new(trace_id, span_id, true, "")
  
  // 创建带有上下文的日志记录
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Request processed successfully"),
    Some(Attributes::new()),
    Some(1640995200000L),
    Some(1640995200100L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // 验证日志记录属性
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Request processed successfully"))
  assert_eq(LogRecord::trace_id(log_record), Some(trace_id))
  assert_eq(LogRecord::span_id(log_record), Some(span_id))
  
  // 测试跨服务日志关联
  let service_a_logs = [
    ("trace.id", trace_id),
    ("span.id", "1111111111111111"),
    ("service.name", "service-a"),
    ("log.message", "Processing request")
  ]
  
  let service_b_logs = [
    ("trace.id", trace_id),
    ("span.id", "2222222222222222"),
    ("parent.span.id", "1111111111111111"),
    ("service.name", "service-b"),
    ("log.message", "Handling sub-request")
  ]
  
  // 验证跨服务关联
  assert_eq(service_a_logs[0].1, service_b_logs[0].1) // 相同的trace ID
  assert_eq(service_b_logs[2].1, "1111111111111111") // 正确的父span ID
  
  // 测试日志严重性级别处理
  let log_levels = [
    (Trace, "Entering debug mode"),
    (Debug, "Cache hit for key: user_123"),
    (Info, "User authentication successful"),
    (Warn, "Rate limit approaching threshold"),
    (Error, "Database connection failed"),
    (Fatal, "System shutdown initiated")
  ]
  
  // 验证严重性级别处理
  for (severity, message) in log_levels {
    let record = LogRecord::new(severity, message)
    assert_eq(LogRecord::severity_number(record), severity)
    assert_eq(LogRecord::body(record), Some(message))
  }
}

// 测试4: 资源合并和继承策略
test "resource merge and inheritance strategies" {
  // 创建基础资源
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("0.1.0"))
  ]
  
  // 创建覆盖资源
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("azimuth-service")), // 相同键，应被覆盖
    ("service.namespace", StringValue("production")), // 新键
    ("host.name", StringValue("web-server-01")),
    ("deployment.environment", StringValue("staging"))
  ]
  
  // 测试资源合并策略
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some(StringValue("azimuth-service")))
  assert_eq(Resource::get_attribute(merged_resource, "service.namespace"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(merged_resource, "host.name"), Some(StringValue("web-server-01")))
  
  // 测试资源继承层次结构
  let global_resource = Resource::with_attributes([
    ("global.region", StringValue("us-west-2")),
    ("global.zone", StringValue("us-west-2a"))
  ])
  
  let service_resource = Resource::with_attributes([
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("2.1.0"))
  ])
  
  let instance_resource = Resource::with_attributes([
    ("instance.id", StringValue("i-1234567890abcdef0")),
    ("instance.ip", StringValue("10.0.1.100"))
  ])
  
  // 模拟继承链合并
  let inherited_resource = Resource::merge(
    Resource::merge(global_resource, service_resource),
    instance_resource
  )
  
  // 验证继承链
  assert_eq(Resource::get_attribute(inherited_resource, "global.region"), Some(StringValue("us-west-2")))
  assert_eq(Resource::get_attribute(inherited_resource, "service.name"), Some(StringValue("payment-service")))
  assert_eq(Resource::get_attribute(inherited_resource, "instance.id"), Some(StringValue("i-1234567890abcdef0")))
}

// 测试5: 复合传播器的注入和提取
test "composite propagator injection and extraction" {
  // 创建传播器组件
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建复合传播器
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 创建源上下文
  let source_context = Context::root()
  let enriched_context = Context::with_value(
    Context::with_value(source_context, ContextKey::new("user.id"), "user123"),
    ContextKey::new("request.id"), "req-456"
  )
  
  // 创建载体
  let carrier = TextMapCarrier::new()
  
  // 注入上下文
  CompositePropagator::inject(composite_propagator, enriched_context, carrier)
  
  // 验证注入结果
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // 测试跨服务传播
  let service_a_carrier = TextMapCarrier::new()
  TextMapCarrier::set(service_a_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(service_a_carrier, "baggage", "user.id=user123,session.id=sess-789")
  
  // 提取上下文
  let extracted_context = CompositePropagator::extract(composite_propagator, service_a_carrier)
  let extracted_value = Context::get(extracted_context, ContextKey::new("extracted"))
  
  // 验证提取结果
  assert_eq(extracted_value, Some("true"))
  
  // 测试传播链
  let propagation_chain = [
    ("service-a", "traceparent:00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("service-b", "traceparent:00-0af7651916cd43dd8448eb211c80319c-cdef89ab01234567-01"),
    ("service-c", "traceparent:00-0af7651916cd43dd8448eb211c80319c-fedcba9876543210-01")
  ]
  
  // 验证传播链一致性
  for (service, header) in propagation_chain {
    let parts = header.split(":")
    assert_eq(parts.length(), 2)
    assert_eq(parts[0], "traceparent")
    
    let trace_context_parts = parts[1].split("-")
    assert_eq(trace_context_parts.length(), 4)
    assert_eq(trace_context_parts[0], "00") // 版本
    assert_eq(trace_context_parts[1], "0af7651916cd43dd8448eb211c80319c") // 相同的trace ID
  }
}

// 测试6: 多维度属性查询
test "multidimensional attribute queries" {
  // 创建复杂属性结构
  let complex_attributes = [
    ("user.id", StringValue("user123")),
    ("user.type", StringValue("premium")),
    ("user.age", IntValue(35)),
    ("user.location.country", StringValue("US")),
    ("user.location.state", StringValue("CA")),
    ("user.preferences.theme", StringValue("dark")),
    ("user.preferences.language", StringValue("en")),
    ("request.method", StringValue("POST")),
    ("request.endpoint", StringValue("/api/v1/orders")),
    ("request.status_code", IntValue(201)),
    ("service.name", StringValue("order-service")),
    ("service.version", StringValue("2.3.1")),
    ("service.environment", StringValue("production"))
  ]
  
  // 测试按前缀过滤
  let user_attrs = complex_attributes.filter(|(key, _)| key.starts_with("user."))
  let request_attrs = complex_attributes.filter(|(key, _)| key.starts_with("request."))
  let service_attrs = complex_attributes.filter(|(key, _)| key.starts_with("service."))
  
  // 验证过滤结果
  assert_eq(user_attrs.length(), 7)
  assert_eq(request_attrs.length(), 3)
  assert_eq(service_attrs.length(), 3)
  
  // 测试嵌套属性查询
  let location_attrs = complex_attributes.filter(|(key, _)| key.starts_with("user.location."))
  let preferences_attrs = complex_attributes.filter(|(key, _)| key.starts_with("user.preferences."))
  
  assert_eq(location_attrs.length(), 2)
  assert_eq(preferences_attrs.length(), 2)
  
  // 测试多条件查询
  let premium_users_in_ca = complex_attributes.filter(|(key, value)| {
    (key == "user.type" && value == StringValue("premium")) ||
    (key == "user.location.state" && value == StringValue("CA"))
  })
  
  assert_eq(premium_users_in_ca.length(), 2)
  
  // 测试属性值类型转换
  let string_values = complex_attributes.filter_map(|(_, value)| {
    match value {
      StringValue(s) => Some(s)
      IntValue(i) => Some(i.to_string())
      _ => None
    }
  })
  
  assert_eq(string_values.length(), 13)
  assert_true(string_values.contains("user123"))
  assert_true(string_values.contains("35"))
  
  // 测试属性聚合
  let grouped_attrs = complex_attributes.group_by(|(key, _)| {
    if key.starts_with("user.") { "user" }
    else if key.starts_with("request.") { "request" }
    else if key.starts_with("service.") { "service" }
    else { "other" }
  })
  
  assert_eq(grouped_attrs.get("user").unwrap().length(), 7)
  assert_eq(grouped_attrs.get("request").unwrap().length(), 3)
  assert_eq(grouped_attrs.get("service").unwrap().length(), 3)
}

// 测试7: 时间序列数据分析
test "time series data analysis and processing" {
  // 创建时间序列数据点
  let time_series_data = [
    (1640995200000L, 100.5),  // 2022-01-01 00:00:00
    (1640995260000L, 105.3),  // 2022-01-01 00:01:00
    (1640995320000L, 98.7),   // 2022-01-01 00:02:00
    (1640995380000L, 110.2),  // 2022-01-01 00:03:00
    (1640995440000L, 115.8),  // 2022-01-01 00:04:00
    (1640995500000L, 108.4),  // 2022-01-01 00:05:00
    (1640995560000L, 112.1),  // 2022-01-01 00:06:00
    (1640995620000L, 120.5),  // 2022-01-01 00:07:00
    (1640995680000L, 118.9),  // 2022-01-01 00:08:00
    (1640995740000L, 125.3)   // 2022-01-01 00:09:00
  ]
  
  // 计算基本统计指标
  let values = time_series_data.map(|(_, value)| value)
  let sum = values.reduce(|acc, val| acc + val, 0.0)
  let count = values.length().to_float()
  let mean = sum / count
  let min = values.reduce(|acc, val| if val < acc { val } else { acc }, values[0])
  let max = values.reduce(|acc, val)| if val > acc { val } else { acc }, values[0])
  
  // 验证统计指标
  assert_eq(sum, 1115.7)
  assert_eq(count, 10.0)
  assert_eq(mean, 111.57)
  assert_eq(min, 98.7)
  assert_eq(max, 125.3)
  
  // 计算移动平均
  let window_size = 3
  let moving_averages = [
    (time_series_data[2].0, (time_series_data[0].1 + time_series_data[1].1 + time_series_data[2].1) / 3.0),
    (time_series_data[3].0, (time_series_data[1].1 + time_series_data[2].1 + time_series_data[3].1) / 3.0),
    (time_series_data[4].0, (time_series_data[2].1 + time_series_data[3].1 + time_series_data[4].1) / 3.0)
  ]
  
  // 验证移动平均
  assert_eq(moving_averages[0].1, (100.5 + 105.3 + 98.7) / 3.0)
  assert_eq(moving_averages[1].1, (105.3 + 98.7 + 110.2) / 3.0)
  assert_eq(moving_averages[2].1, (98.7 + 110.2 + 115.8) / 3.0)
  
  // 计算趋势分析
  let first_half = values.slice(0, 5)
  let second_half = values.slice(5, 5)
  let first_half_avg = first_half.reduce(|acc, val| acc + val, 0.0) / 5.0
  let second_half_avg = second_half.reduce(|acc, val| acc + val, 0.0) / 5.0
  
  // 验证趋势
  assert_eq(first_half_avg, (100.5 + 105.3 + 98.7 + 110.2 + 115.8) / 5.0)
  assert_eq(second_half_avg, (108.4 + 112.1 + 120.5 + 118.9 + 125.3) / 5.0)
  assert_true(second_half_avg > first_half_avg) // 上升趋势
  
  // 测试时间窗口聚合
  let time_windows = time_series_data.group_by(|(timestamp, _)| {
    let hour = timestamp / 3600000L // 按小时分组
    hour
  })
  
  // 验证时间窗口
  assert_eq(time_windows.keys().length(), 1) // 所有点在同一小时内
  assert_eq(time_windows.values()[0].length(), 10) // 10个数据点
}

// 测试8: 错误边界和恢复机制
test "error boundary handling and recovery mechanisms" {
  // 测试Span错误处理
  let trace_id = "error_trace_1234567890abcdef"
  let span_id = "error_span_1111111111111111"
  let error_span_context = SpanContext::new(trace_id, span_id, true, "")
  let error_span = Span::new("error_prone_operation", Internal, error_span_context)
  
  // 模拟错误场景
  let error_scenarios = [
    ("network_timeout", "Network operation timed out after 30 seconds"),
    ("database_connection", "Failed to connect to database: connection refused"),
    ("invalid_input", "Invalid input parameter: value cannot be negative"),
    ("resource_exhausted", "Memory limit exceeded: cannot allocate more resources"),
    ("permission_denied", "Access denied: insufficient privileges for operation")
  ]
  
  // 测试错误处理流程
  for (error_type, error_message) in error_scenarios {
    // 设置错误状态
    Span::set_status(error_span, Error, Some(error_message))
    
    // 添加错误事件
    Span::add_event(error_span, "error_occurred", Some([
      ("error.type", StringValue(error_type)),
      ("error.message", StringValue(error_message)),
      ("error.timestamp", StringValue("1640995200000"))
    ]))
    
    // 验证错误状态
    assert_eq(Span::status(error_span), Error)
  }
  
  // 测试恢复机制
  let recovery_strategies = [
    ("retry", 3),           // 重试3次
    ("circuit_breaker", 5), // 熔断器阈值
    ("fallback", 1),        // 降级策略
    ("timeout", 30)         // 超时时间(秒)
  ]
  
  // 验证恢复策略
  for (strategy, value) in recovery_strategies {
    match strategy {
      "retry" => assert_eq(value, 3),
      "circuit_breaker" => assert_eq(value, 5),
      "fallback" => assert_eq(value, 1),
      "timeout" => assert_eq(value, 30),
      _ => assert_false(true) // 不应该到达这里
    }
  }
  
  // 测试日志错误记录
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error.handler")
  
  let error_log_record = LogRecord::new_with_context(
    Error,
    Some("Operation failed with critical error"),
    Some(Attributes::new()),
    Some(1640995200000L),
    Some(1640995200100L),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // 验证错误日志
  assert_eq(LogRecord::severity_number(error_log_record), Error)
  assert_eq(LogRecord::body(error_log_record), Some("Operation failed with critical error"))
  assert_eq(LogRecord::trace_id(error_log_record), Some(trace_id))
  assert_eq(LogRecord::span_id(error_log_record), Some(span_id))
  
  // 测试错误边界指标
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "error.metrics")
  let error_counter = Meter::create_counter(meter, "errors_total")
  let recovery_counter = Meter::create_counter(meter, "recoveries_total")
  
  // 模拟错误和恢复计数
  Counter::add(error_counter, 5.0, Some(Attributes::new()))
  Counter::add(recovery_counter, 3.0, Some(Attributes::new()))
  
  // 验证错误恢复率
  let error_recovery_rate = 3.0 / 5.0 * 100.0
  assert_eq(error_recovery_rate, 60.0)
  
  // 结束错误Span
  Span::end(error_span)
}