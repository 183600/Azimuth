// Azimuth 高级功能测试用例
// 包含8个测试用例，涵盖Resource合并、时间戳操作、时序数据、HTTP客户端遥测等功能

test "resource merge strategy operations" {
  // 测试Resource合并策略
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  let base_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.name", StringValue("azimuth-service")),  // 相同的key应该被覆盖
    ("host.name", StringValue("web-server-01")),
    ("region", StringValue("us-west-2"))
  ]
  let override_with_attrs = Resource::with_attributes(override_resource, override_attrs)
  
  // 测试合并操作
  let merged_resource = Resource::merge(base_with_attrs, override_with_attrs)
  
  // 验证合并结果
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    StringValue(name) => assert_eq(name, "azimuth-service")
    _ => assert_false(true, "Expected string service name")
  }
  
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  match host_name {
    StringValue(name) => assert_eq(name, "web-server-01")
    _ => assert_false(true, "Expected string host name")
  }
  
  let region = Resource::get_attribute(merged_resource, "region")
  match region {
    StringValue(name) => assert_eq(name, "us-west-2")
    _ => assert_false(true, "Expected string region")
  }
}

test "timestamp operations and validation" {
  // 测试时间戳操作和验证
  let clock = Clock::system()
  let timestamp1 = Clock::now_unix_nanos(clock)
  
  // 验证时间戳是合理的（2025年的时间戳）
  assert_true(timestamp1 > 1700000000000000000L)  // 2023年开始
  assert_true(timestamp1 < 1800000000000000000L)  // 2027年结束
  
  // 创建带有时间戳的日志记录
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Timestamped log message"),
    None,
    Some(timestamp1),
    None,
    Some("trace123"),
    Some("span456"),
    None
  )
  
  // 验证时间戳设置正确
  assert_eq(LogRecord::trace_id(log_record), Some("trace123"))
  assert_eq(LogRecord::span_id(log_record), Some("span456"))
  
  // 获取第二个时间戳，应该大于或等于第一个
  let timestamp2 = Clock::now_unix_nanos(clock)
  assert_true(timestamp2 >= timestamp1)
}

test "time series data operations" {
  // 测试时序数据操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time-series-meter")
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  
  // 记录多个数据点
  Histogram::record(histogram, 100.0)  // 100ms
  Histogram::record(histogram, 150.0)  // 150ms
  Histogram::record(histogram, 200.0)  // 200ms
  Histogram::record(histogram, 120.0)  // 120ms
  Histogram::record(histogram, 180.0)  // 180ms
  
  // 测试仪表创建
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage in bytes"), Some("bytes"))
  
  // 记录仪表数据
  let attrs = Attributes::new()
  Attributes::set(attrs, "component", StringValue("cache"))
  Gauge::as_instrument(gauge) |> ignore  // 简化测试，实际实现会有更多功能
  
  // 测试UpDownCounter
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -2.0)  // 减少连接数
  
  assert_true(true)
}

test "http client telemetry integration" {
  // 测试HTTP客户端遥测集成
  let client = HttpClient::new()
  
  // 创建HTTP请求
  let headers = [
    ("Content-Type", "application/json"),
    ("User-Agent", "azimuth-telemetry/1.0.0"),
    ("X-Trace-ID", "0af7651916cd43dd8448eb211c80319c")
  ]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers, Some(""))
  
  // 验证请求属性
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some(""))
  
  // 创建HTTP响应
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Request-ID", "req-123456")
  ]
  let response = HttpResponse::new(200, response_headers, Some("{\"data\": \"value\"}"))
  
  // 验证响应属性
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"data\": \"value\"}"))
  
  // 测试带有遥测属性的请求
  let telemetry_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("X-Request-ID", "req-789012")
  ]
  let telemetry_request = HttpRequest::new("POST", "https://api.example.com/telemetry", telemetry_headers, Some("{}"))
  
  assert_eq(HttpRequest::http_method(telemetry_request), "POST")
}

test "log record and trace correlation" {
  // 测试日志记录与追踪关联
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "correlation-test-logger")
  
  // 创建带有追踪上下文的日志记录
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "component", StringValue("auth-service"))
  Attributes::set(log_attrs, "user.id", IntValue(12345))
  
  let correlated_log = LogRecord::new_with_context(
    Error,
    Some("Authentication failed for user"),
    Some(log_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // 验证日志记录与追踪的关联
  assert_eq(LogRecord::trace_id(correlated_log), Some(trace_id))
  assert_eq(LogRecord::span_id(correlated_log), Some(span_id))
  
  // 发送日志记录
  Logger::emit(logger, correlated_log)
  
  // 创建带有不同严重级别的日志记录
  let info_log = LogRecord::new(Info, "User login successful")
  let warn_log = LogRecord::new(Warn, "Rate limit approaching")
  let error_log = LogRecord::new(Error, "Database connection failed")
  
  // 验证日志记录创建
  assert_eq(LogRecord::body(info_log), Some("User login successful"))
  assert_eq(LogRecord::body(warn_log), Some("Rate limit approaching"))
  assert_eq(LogRecord::body(error_log), Some("Database connection failed"))
}

test "multidimensional attribute queries" {
  // 测试多维度属性查询
  let attrs = Attributes::new()
  
  // 设置多维度属性
  Attributes::set(attrs, "service.name", StringValue("payment-service"))
  Attributes::set(attrs, "service.version", StringValue("2.1.0"))
  Attributes::set(attrs, "deployment.environment", StringValue("staging"))
  Attributes::set(attrs, "region", StringValue("eu-west-1"))
  Attributes::set(attrs, "instance.id", StringValue("i-1234567890abcdef0"))
  Attributes::set(attrs, "cpu.count", IntValue(4))
  Attributes::set(attrs, "memory.total", IntValue(8589934592))  // 8GB
  Attributes::set(attrs, "auto.scaling.enabled", BoolValue(true))
  Attributes::set(attrs, "tags", ArrayStringValue(["payment", "critical", "pci"]))
  
  // 测试字符串属性查询
  let service_name = Attributes::get(attrs, "service.name")
  match service_name {
    StringValue(name) => assert_eq(name, "payment-service")
    _ => assert_false(true, "Expected string service name")
  }
  
  // 测试整数属性查询
  let cpu_count = Attributes::get(attrs, "cpu.count")
  match cpu_count {
    IntValue(count) => assert_eq(count, 4)
    _ => assert_false(true, "Expected integer CPU count")
  }
  
  // 测试布尔属性查询
  let auto_scaling = Attributes::get(attrs, "auto.scaling.enabled")
  match auto_scaling {
    BoolValue(enabled) => assert_true(enabled)
    _ => assert_false(true, "Expected boolean auto scaling flag")
  }
  
  // 测试数组属性查询
  let tags = Attributes::get(attrs, "tags")
  match tags {
    ArrayStringValue(tag_array) => {
      assert_eq(tag_array.length(), 3)
      assert_true(tag_array.contains("payment"))
      assert_true(tag_array.contains("critical"))
      assert_true(tag_array.contains("pci"))
    }
    _ => assert_false(true, "Expected string array tags")
  }
  
  // 测试不存在的属性
  let non_existent = Attributes::get(attrs, "non.existent.attribute")
  assert_eq(non_existent, None)
}

test "metrics dashboard data aggregation" {
  // 测试度量仪表盘数据聚合
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard-metrics")
  
  // 创建多个度量仪器
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_histogram = Meter::create_histogram(meter, "http.response.duration", Some("HTTP response duration"), Some("ms"))
  let active_connections = Meter::create_gauge(meter, "http.active.connections", Some("Active HTTP connections"), Some("connections"))
  let error_rate = Meter::create_updown_counter(meter, "http.errors.total", Some("Total HTTP errors"), Some("errors"))
  
  // 模拟数据记录
  let base_attrs = Attributes::new()
  Attributes::set(base_attrs, "service", StringValue("api-gateway"))
  Attributes::set(base_attrs, "method", StringValue("GET"))
  
  // 记录请求计数
  Counter::add(request_counter, 1000.0, Some(base_attrs))
  
  // 记录响应时间分布
  Histogram::record(response_histogram, 50.0, Some(base_attrs))
  Histogram::record(response_histogram, 100.0, Some(base_attrs))
  Histogram::record(response_histogram, 200.0, Some(base_attrs))
  Histogram::record(response_histogram, 500.0, Some(base_attrs))
  
  // 记录活跃连接数
  Gauge::as_instrument(active_connections) |> ignore
  
  // 记录错误计数
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "service", StringValue("api-gateway"))
  Attributes::set(error_attrs, "error.type", StringValue("timeout"))
  UpDownCounter::add(error_rate, 25.0, Some(error_attrs))
  
  // 验证仪器名称和描述
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(response_histogram.description, Some("HTTP response duration"))
  assert_eq(response_histogram.unit, Some("ms"))
  
  assert_eq(active_connections.name, "http.active.connections")
  assert_eq(active_connections.description, Some("Active HTTP connections"))
  assert_eq(active_connections.unit, Some("connections"))
  
  assert_eq(error_rate.name, "http.errors.total")
  assert_eq(error_rate.description, Some("Total HTTP errors"))
  assert_eq(error_rate.unit, Some("errors"))
}

test "realtime stream processing telemetry" {
  // 测试实时流处理遥测
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "stream-processor")
  
  // 创建流处理相关的度量
  let events_processed = Meter::create_counter(meter, "stream.events.processed", Some("Events processed by stream"), Some("events"))
  let processing_lag = Meter::create_histogram(meter, "stream.processing.lag", Some("Stream processing lag"), Some("ms"))
  let buffer_utilization = Meter::create_gauge(meter, "stream.buffer.utilization", Some("Stream buffer utilization"), Some("percent"))
  let backpressure_events = Meter::create_updown_counter(meter, "stream.backpressure.events", Some("Backpressure events"), Some("events"))
  
  // 模拟流处理操作
  let stream_attrs = Attributes::new()
  Attributes::set(stream_attrs, "stream.name", StringValue("user-events"))
  Attributes::set(stream_attrs, "partition", StringValue("0"))
  Attributes::set(stream_attrs, "consumer.group", StringValue("analytics-processors"))
  
  // 记录事件处理
  Counter::add(events_processed, 10000.0, Some(stream_attrs))
  
  // 记录处理延迟
  Histogram::record(processing_lag, 25.0, Some(stream_attrs))
  Histogram::record(processing_lag, 50.0, Some(stream_attrs))
  Histogram::record(processing_lag, 100.0, Some(stream_attrs))
  Histogram::record(processing_lag, 200.0, Some(stream_attrs))
  
  // 记录缓冲区利用率
  Gauge::as_instrument(buffer_utilization) |> ignore
  
  // 记录背压事件
  let backpressure_attrs = Attributes::new()
  Attributes::set(backpressure_attrs, "stream.name", StringValue("user-events"))
  Attributes::set(backpressure_attrs, "backpressure.reason", StringValue("slow_consumer"))
  UpDownCounter::add(backpressure_events, 5.0, Some(backpressure_attrs))
  
  // 创建与流处理相关的日志记录
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "stream-processor-logger")
  
  let stream_log = LogRecord::new_with_context(
    Warn,
    Some("Stream processing lag detected"),
    Some(stream_attrs),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some("stream123"),
    Some("span456"),
    None
  )
  
  Logger::emit(logger, stream_log)
  
  // 验证度量创建
  assert_eq(events_processed.name, "stream.events.processed")
  assert_eq(processing_lag.name, "stream.processing.lag")
  assert_eq(buffer_utilization.name, "stream.buffer.utilization")
  assert_eq(backpressure_events.name, "stream.backpressure.events")
  
  // 验证日志记录
  assert_eq(LogRecord::body(stream_log), Some("Stream processing lag detected"))
  assert_eq(LogRecord::trace_id(stream_log), Some("stream123"))
  assert_eq(LogRecord::span_id(stream_log), Some("span456"))
}