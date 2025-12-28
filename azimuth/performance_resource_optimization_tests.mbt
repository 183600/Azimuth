// 性能和资源管理测试用例
// 测试遥测系统在高负载下的性能表现和资源使用情况

test "high frequency span creation and termination performance" {
  // 测试大量span的创建和终止性能
  let tracer = azimuth::TracerProvider::get_tracer(
    azimuth::TracerProvider::default(), 
    "performance-test"
  )
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建大量span
  let span_count = 1000
  let mut spans = []
  
  for i in range(0, span_count) {
    let span_name = "performance-span-" + i.to_string()
    let span = azimuth::Tracer::start_span(tracer, span_name)
    azimuth::Span::add_event(span, "Span created")
    spans = spans.push(span)
  }
  
  // 快速终止所有span
  for span in spans {
    azimuth::Span::add_event(span, "Span ending")
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration_ms = (end_time - start_time) / 1000000L
  
  // 验证性能在合理范围内（应该在几秒内完成）
  assert_true(duration_ms < 5000L, "Span creation and termination should complete within 5 seconds")
  
  // 验证所有span都已正确创建和终止
  assert_eq(spans.length(), span_count)
}

test "metrics aggregation performance under load" {
  // 测试在高负载下指标聚合的性能
  let meter = azimuth::MeterProvider::get_meter(
    azimuth::MeterProvider::default(), 
    "performance-metrics"
  )
  
  let counter = azimuth::Meter::create_counter(
    meter, 
    "performance.counter",
    Some("Performance test counter"),
    Some("operations")
  )
  
  let histogram = azimuth::Meter::create_histogram(
    meter, 
    "performance.histogram",
    Some("Performance test histogram"),
    Some("ms")
  )
  
  let updown_counter = azimuth::Meter::create_updown_counter(
    meter, 
    "performance.updown",
    Some("Performance test up-down counter"),
    Some("units")
  )
  
  let gauge = azimuth::Meter::create_gauge(
    meter, 
    "performance.gauge",
    Some("Performance test gauge"),
    Some("units")
  )
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 大量指标操作
  let operation_count = 10000
  
  for i in range(0, operation_count) {
    // Counter操作
    azimuth::Counter::add(counter, 1.0)
    
    // Histogram操作
    azimuth::Histogram::record(histogram, (i % 100).to_double())
    
    // UpDownCounter操作
    if i % 2 == 0 {
      azimuth::UpDownCounter::add(updown_counter, 1.0)
    } else {
      azimuth::UpDownCounter::add(updown_counter, -1.0)
    }
    
    // Gauge操作
    azimuth::Gauge::record(gauge, (i % 50).to_double())
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration_ms = (end_time - start_time) / 1000000L
  
  // 验证性能在合理范围内
  assert_true(duration_ms < 3000L, "Metrics operations should complete within 3 seconds")
  
  // 验证指标名称和属性
  assert_eq(counter.name, "performance.counter")
  assert_eq(histogram.name, "performance.histogram")
  assert_eq(updown_counter.name, "performance.updown")
  assert_eq(gauge.name, "performance.gauge")
}

test "logging throughput performance test" {
  // 测试日志记录的吞吐量性能
  let logger = azimuth::LoggerProvider::get_logger(
    azimuth::LoggerProvider::default(), 
    "performance-logger"
  )
  
  let log_count = 5000
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 大量日志记录
  for i in range(0, log_count) {
    let severity = match i % 5 {
      0 => azimuth::Trace
      1 => azimuth::Debug
      2 => azimuth::Info
      3 => azimuth::Warn
      _ => azimuth::Error
    }
    
    let log_message = "Performance test log message " + i.to_string()
    let record = azimuth::LogRecord::new(severity, log_message)
    
    // 为部分日志添加属性
    if i % 10 == 0 {
      let enhanced_record = azimuth::LogRecord::new_with_context(
        severity,
        Some(log_message),
        Some(azimuth::Attributes::new()),
        Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
        None,
        Some("trace-" + i.to_string()),
        Some("span-" + i.to_string()),
        Some(azimuth::Context::root())
      )
      azimuth::Logger::emit(logger, enhanced_record)
    } else {
      azimuth::Logger::emit(logger, record)
    }
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration_ms = (end_time - start_time) / 1000000L
  let logs_per_second = (log_count.to_int64() * 1000L) / duration_ms
  
  // 验证日志吞吐量性能
  assert_true(logs_per_second > 1000L, "Should achieve at least 1000 logs per second")
  assert_true(duration_ms < 10000L, "Logging should complete within 10 seconds")
}

test "context propagation efficiency test" {
  // 测试上下文传播的效率
  let propagator = azimuth::W3CTraceContextPropagator::new()
  let composite_propagator = azimuth::CompositePropagator::new([propagator])
  
  let propagation_count = 2000
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 大量上下文传播操作
  for i in range(0, propagation_count) {
    let ctx = azimuth::Context::root()
    let key = azimuth::ContextKey::new("test.key." + i.to_string())
    let ctx_with_value = azimuth::Context::with_value(ctx, key, "test.value." + i.to_string())
    
    let carrier = azimuth::TextMapCarrier::new()
    azimuth::CompositePropagator::inject(composite_propagator, ctx_with_value, carrier)
    
    let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
    let retrieved_value = azimuth::Context::get(extracted_ctx, key)
    
    // 验证传播成功
    assert_eq(retrieved_value, Some("test.value." + i.to_string()))
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration_ms = (end_time - start_time) / 1000000L
  let propagations_per_second = (propagation_count.to_int64() * 1000L) / duration_ms
  
  // 验证传播效率
  assert_true(propagations_per_second > 500L, "Should achieve at least 500 propagations per second")
  assert_true(duration_ms < 5000L, "Context propagation should complete within 5 seconds")
}

test "resource memory usage optimization" {
  // 测试资源使用的内存优化
  let initial_resources = []
  
  // 创建大量资源对象
  for i in range(0, 100) {
    let resource = azimuth::Resource::new()
    let attrs = [
      ("service.name", azimuth::StringValue("service-" + i.to_string())),
      ("service.instance.id", azimuth::StringValue("instance-" + i.to_string())),
      ("service.version", azimuth::StringValue("1.0." + i.to_string())),
      ("environment", azimuth::StringValue("test"))
    ]
    let resource_with_attrs = azimuth::Resource::with_attributes(resource, attrs)
    initial_resources = initial_resources.push(resource_with_attrs)
  }
  
  // 测试资源合并操作
  let base_resource = azimuth::Resource::new()
  let base_attrs = [
    ("telemetry.sdk.name", azimuth::StringValue("azimuth")),
    ("telemetry.sdk.version", azimuth::StringValue("0.1.0")),
    ("telemetry.sdk.language", azimuth::StringValue("moonbit"))
  ]
  let base_resource_with_attrs = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 合并所有资源
  let mut final_resource = base_resource_with_attrs
  for resource in initial_resources {
    final_resource = azimuth::Resource::merge(final_resource, resource)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration_ms = (end_time - start_time) / 1000000L
  
  // 验证合并操作的性能
  assert_true(duration_ms < 1000L, "Resource merging should complete within 1 second")
  
  // 验证最终资源包含关键属性
  assert_eq(azimuth::Resource::get_attribute(final_resource, "telemetry.sdk.name"), 
            Some(azimuth::StringValue("azimuth")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "telemetry.sdk.version"), 
            Some(azimuth::StringValue("0.1.0")))
}