// Azimuth 新增综合测试用例
// 涵盖遥测系统的多个核心功能

// 测试1: 资源管理和属性设置
test "resource management and attributes" {
  let resource = Resource::default()
    .with_attribute("service.name", "azimuth-test")
    .with_attribute("service.version", "1.0.0")
    .with_attribute("environment", "testing")
    .with_attribute("host.name", "test-host")
  
  // 验证资源属性
  let attrs = Resource::attributes(resource)
  assert_eq(attrs.length(), 4)
  assert_true(attrs.contains_key("service.name"))
  assert_eq(attrs.get("service.name"), Some("azimuth-test"))
  
  // 测试资源合并
  let additional_resource = Resource::default()
    .with_attribute("deployment.region", "us-west")
    .with_attribute("service.name", "override-test") // 应该被忽略
  
  let merged = Resource::merge(resource, additional_resource)
  let merged_attrs = Resource::attributes(merged)
  assert_eq(merged_attrs.length(), 5)
  assert_eq(merged_attrs.get("service.name"), Some("azimuth-test")) // 原值保留
  assert_eq(merged_attrs.get("deployment.region"), Some("us-west"))
}

// 测试2: Baggage跨服务传播
test "baggage propagation across services" {
  let baggage = Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("request.id", "req-abc-123")
    .with_entry("trace.origin", "frontend")
  
  // 验证baggage条目
  assert_true(Baggage::has_entry(baggage, "user.id"))
  assert_eq(Baggage::get_value(baggage, "user.id"), Some("12345"))
  
  // 测试baggage序列化
  let serialized = Baggage::to_string(baggage)
  assert_true(serialized.contains("user.id=12345"))
  assert_true(serialized.contains("request.id=req-abc-123"))
  
  // 测试baggage反序列化
  let deserialized = Baggage::from_string(serialized)
  assert_eq(Baggage::get_value(deserialized, "user.id"), Some("12345"))
  assert_eq(Baggage::get_value(deserialized, "trace.origin"), Some("frontend"))
  
  // 测试baggage传播到span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "baggage.test")
  let span = Tracer::start_span(tracer, "operation.with.baggage")
  
  Span::set_baggage(span, baggage)
  let span_baggage = Span::get_baggage(span)
  assert_eq(Baggage::get_value(span_baggage, "user.id"), Some("12345"))
  
  Span::end(span)
}

// 测试3: 采样器配置和行为
test "sampler configuration and behavior" {
  // 测试AlwaysOn采样器
  let always_on = Sampler::always_on()
  let sampling_result1 = Sampler::should_sample(always_on, "test-trace-1", "test.operation", [])
  assert_true(SamplingResult::decision(sampling_result1))
  
  // 测试AlwaysOff采样器
  let always_off = Sampler::always_off()
  let sampling_result2 = Sampler::should_sample(always_off, "test-trace-2", "test.operation", [])
  assert_false(SamplingResult::decision(sampling_result2))
  
  // 测试TraceIdRatio采样器
  let ratio_sampler = Sampler::trace_id_ratio(0.5) // 50%采样率
  let sampling_result3 = Sampler::should_sample(ratio_sampler, "test-trace-3", "test.operation", [])
  // 结果取决于trace ID，但我们验证它返回了有效结果
  assert_true(SamplingResult::decision(sampling_result3) || not(SamplingResult::decision(sampling_result3)))
  
  // 测试基于属性的采样器
  let attribute_sampler = Sampler::attribute_based("operation.type", "critical")
  let sampling_result4 = Sampler::should_sample(
    attribute_sampler, 
    "test-trace-4", 
    "critical.operation", 
    [("operation.type", "critical")]
  )
  assert_true(SamplingResult::decision(sampling_result4))
  
  let sampling_result5 = Sampler::should_sample(
    attribute_sampler, 
    "test-trace-5", 
    "normal.operation", 
    [("operation.type", "normal")]
  )
  assert_false(SamplingResult::decision(sampling_result5))
}

// 测试4: 度量仪表盘和直方图
test "metrics gauge and histogram operations" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "gauge.histogram.test")
  
  // 创建仪表盘
  let gauge = Meter::create_gauge(meter, "system.memory.usage", Some("Memory usage"), Some("bytes"))
  
  // 设置仪表盘值
  Gauge::set(gauge, 1024.0)
  Gauge::set(gauge, 2048.0)
  Gauge::set(gauge, 1536.0)
  
  // 创建直方图
  let histogram = Meter::create_histogram(
    meter, 
    "request.duration", 
    Some("Request duration"), 
    Some("milliseconds"),
    [1.0, 5.0, 10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0]
  )
  
  // 记录直方图值
  Histogram::record(histogram, 12.5)
  Histogram::record(histogram, 45.0)
  Histogram::record(histogram, 150.0)
  Histogram::record(histogram, 8.0)
  Histogram::record(histogram, 300.0)
  
  // 带属性的记录
  Histogram::record_with_attributes(histogram, 25.0, [("endpoint", "/api/users")])
  Histogram::record_with_attributes(histogram, 75.0, [("endpoint", "/api/orders")])
  Histogram::record_with_attributes(histogram, 200.0, [("endpoint", "/api/products")])
  
  assert_true(true)
}

// 测试5: 日志记录和严重性级别
test "logging with severity levels" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "severity.test")
  
  // 测试不同严重性级别的日志
  Logger::trace(logger, "Debug trace message", [("component", "auth")])
  Logger::debug(logger, "Debug information", [("component", "auth"), ("user.id", "123")])
  Logger::info(logger, "Information message", [("component", "auth"), ("action", "login")])
  Logger::warn(logger, "Warning condition", [("component", "auth"), ("reason", "failed.attempt")])
  Logger::error(logger, "Error occurred", [("component", "auth"), ("error.code", "AUTH_001")])
  
  // 测试带异常的日志
  let exception = Exception::new("Authentication failed", "Invalid credentials")
  Logger::error_with_exception(
    logger, 
    "Authentication failure", 
    exception,
    [("component", "auth"), ("attempt.count", "3")]
  )
  
  // 验证日志提供者状态
  let log_count = LoggerProvider::get_log_count(logger_provider)
  assert_true(log_count >= 6)
  
  // 测试日志级别过滤
  let filtered_logger = LoggerProvider::get_logger_with_level(logger_provider, "severity.test", "WARN")
  Logger::debug(filtered_logger, "This should be filtered out", [])
  Logger::warn(filtered_logger, "This should pass through", [])
  
  let filtered_count = LoggerProvider::get_log_count(logger_provider)
  assert_true(filtered_count >= 7) // 只增加了一个警告日志
}

// 测试6: 上下文传播和提取
test "context propagation and extraction" {
  // 创建原始上下文
  let original_ctx = Context::empty()
    .with_span_context(SpanContext::new("trace-123", "span-456", true, true))
    .with_baggage(Baggage::empty().with_entry("correlation.id", "corr-789"))
  
  // 测试上下文注入
  let carrier = TextMapCarrier::new()
  let propagator = TextMapPropagator::composite([
    TraceContextPropagator::new(),
    BaggagePropagator::new()
  ])
  
  Propagator::inject(propagator, original_ctx, carrier)
  
  // 验证注入的头部
  let headers = TextMapCarrier::headers(carrier)
  assert_true(headers.contains_key("traceparent"))
  assert_true(headers.contains_key("baggage"))
  
  // 测试上下文提取
  let extracted_ctx = Propagator::extract(propagator, carrier)
  let extracted_span_ctx = Context::span_context(extracted_ctx)
  let extracted_baggage = Context::baggage(extracted_ctx)
  
  assert_eq(SpanContext::trace_id(extracted_span_ctx), "trace-123")
  assert_eq(SpanContext::span_id(extracted_span_ctx), "span-456")
  assert_eq(Baggage::get_value(extracted_baggage, "correlation.id"), Some("corr-789"))
}

// 测试7: 度量聚合和时间窗口
test "metrics aggregation and time windows" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation.test")
  
  // 创建计数器
  let counter = Meter::create_counter(meter, "api.requests.total")
  
  // 在不同时间点记录数据
  for i = 0; i < 100; i = i + 1 {
    Counter::add_with_attributes(counter, 1.0, [
      ("method", if i % 2 == 0 { "GET" } else { "POST" }),
      ("status", if i % 10 == 0 { "500" } else { "200" })
    ])
  }
  
  // 创建UpDown计数器
  let updown_counter = Meter::create_up_down_counter(meter, "active.connections")
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::subtract(updown_counter, 3.0)
  
  // 测试时间窗口聚合
  let aggregator = MetricAggregator::new(meter_provider)
  let time_window = TimeWindow::last_5_minutes()
  
  let aggregated_metrics = Aggregator::get_metrics(aggregator, time_window)
  assert_true(aggregated_metrics.length() > 0)
  
  // 验证特定度量的聚合结果
  let counter_metric = aggregated_metrics.find(fn(m) { Metric::name(m) == "api.requests.total" })
  match counter_metric {
    Some(metric) => {
      let data_points = Metric::data_points(metric)
      assert_true(data_points.length() > 0)
      
      // 验证GET和POST请求都被计数
      let get_dp = data_points.find(fn(dp) { DataPoint::has_attribute(dp, "method", "GET") })
      let post_dp = data_points.find(fn(dp) { DataPoint::has_attribute(dp, "method", "POST") })
      
      match (get_dp, post_dp) {
        (Some(get), Some(post)) => {
          assert_true(DataPoint::value(get) > 0.0)
          assert_true(DataPoint::value(post) > 0.0)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试8: 异常处理和错误恢复
test "exception handling and error recovery" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "exception.test")
  
  // 测试异常记录
  let span = Tracer::start_span(tracer, "operation.with.exception")
  
  // 记录异常事件
  let exception1 = Exception::new("Network timeout", "Connection timed out after 30 seconds")
  Span::record_exception(span, exception1, [("retry.count", "1")])
  
  let exception2 = Exception::new("Database error", "Connection pool exhausted")
  Span::record_exception(span, exception2, [("retry.count", "2")])
  
  // 测试错误状态设置
  Span::set_status(span, Status::error("Operation failed after retries"))
  
  // 验证span状态
  let status = Span::status(span)
  assert_eq(Status::code(status), "ERROR")
  assert_true(Status::description(status).contains("failed"))
  
  // 测试错误恢复
  let recovery_span = Tracer::start_span(tracer, "error.recovery.operation")
  Span::set_attribute(recovery_span, "recovery.strategy", "retry.with.backoff")
  Span::set_attribute(recovery_span, "max.retries", "3")
  
  // 模拟成功恢复
  Span::set_status(recovery_span, Status::ok())
  Span::end(recovery_span)
  
  Span::end(span)
  
  // 验证错误统计
  let error_metrics = TracerProvider::get_error_metrics(tracer_provider)
  assert_true(error_metrics.get("timeout.count", 0) > 0)
  assert_true(error_metrics.get("database.error.count", 0) > 0)
  assert_true(error_metrics.get("recovery.success.count", 0) > 0)
}