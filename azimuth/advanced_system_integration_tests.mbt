// 高级系统综合测试用例
// 测试整个遥测系统的综合功能和端到端集成

pub test "end-to-end telemetry workflow" {
  // 完整的端到端遥测工作流测试
  
  // 1. 初始化所有组件
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "e2e-test-service")
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "e2e-test-metrics")
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "e2e-test-logger")
  
  // 2. 创建资源
  let resource = azimuth::Resource::new()
  let resource_attrs = [
    ("service.name", azimuth::StringValue("e2e-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("e2e-instance-123")),
    ("environment", azimuth::StringValue("test")),
    ("test.suite", azimuth::StringValue("end-to-end"))
  ]
  let service_resource = azimuth::Resource::with_attributes(resource, resource_attrs)
  
  // 验证资源属性
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.name"), 
            Some(azimuth::StringValue("e2e-test-service")))
  assert_eq(azimuth::Resource::get_attribute(service_resource, "service.version"), 
            Some(azimuth::StringValue("1.0.0")))
  
  // 3. 创建根span
  let root_span = azimuth::Tracer::start_span(tracer, "e2e-workflow")
  let root_span_ctx = azimuth::Span::span_context(root_span)
  
  // 验证根span
  assert_eq(azimuth::Span::name(root_span), "e2e-workflow")
  assert_true(azimuth::SpanContext::is_valid(root_span_ctx))
  
  // 4. 添加span事件
  azimuth::Span::add_event(root_span, "Workflow started")
  azimuth::Span::add_event(root_span, "Initializing components")
  
  // 5. 创建子span
  let child_span = azimuth::Tracer::start_span(tracer, "data-processing")
  azimuth::Span::add_event(child_span, "Data processing started")
  azimuth::Span::add_event(child_span, "Data validation completed")
  azimuth::Span::add_event(child_span, "Data transformation completed")
  azimuth::Span::set_status(child_span, azimuth::Ok)
  azimuth::Span::end(child_span)
  
  // 6. 创建指标
  let request_counter = azimuth::Meter::create_counter(
    meter, 
    "e2e.requests.total",
    Some("Total E2E requests"),
    Some("requests")
  )
  
  let duration_histogram = azimuth::Meter::create_histogram(
    meter, 
    "e2e.duration",
    Some("E2E operation duration"),
    Some("ms")
  )
  
  let active_operations_gauge = azimuth::Meter::create_gauge(
    meter, 
    "e2e.active_operations",
    Some("Currently active E2E operations"),
    Some("operations")
  )
  
  // 记录指标
  azimuth::Counter::add(request_counter, 1.0)
  azimuth::Histogram::record(duration_histogram, 250.0)
  
  // 验证指标
  assert_eq(request_counter.name, "e2e.requests.total")
  assert_eq(duration_histogram.name, "e2e.duration")
  assert_eq(active_operations_gauge.name, "e2e.active_operations")
  
  // 7. 创建和发射日志
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "workflow.id", azimuth::StringValue("workflow-123"))
  azimuth::Attributes::set(log_attrs, "workflow.status", azimuth::StringValue("in_progress"))
  azimuth::Attributes::set(log_attrs, "processing.steps", azimuth::IntValue(3))
  azimuth::Attributes::set(log_attrs, "processing.duration_ms", azimuth::IntValue(250))
  
  let workflow_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("E2E workflow progressing normally"),
    Some(log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(root_span_ctx)),
    Some(azimuth::SpanContext::span_id(root_span_ctx)),
    Some(azimuth::Context::root())
  )
  
  // 验证日志记录
  assert_eq(azimuth::LogRecord::severity_number(workflow_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(workflow_log), Some("E2E workflow progressing normally"))
  assert_eq(azimuth::LogRecord::trace_id(workflow_log), 
            Some(azimuth::SpanContext::trace_id(root_span_ctx)))
  
  // 发射日志
  azimuth::Logger::emit(logger, workflow_log)
  
  // 8. 完成根span
  azimuth::Span::add_event(root_span, "Workflow completed successfully")
  azimuth::Span::set_status(root_span, azimuth::Ok)
  azimuth::Span::end(root_span)
  
  // 9. 创建完成日志
  let completion_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(completion_attrs, "workflow.id", azimuth::StringValue("workflow-123"))
  azimuth::Attributes::set(completion_attrs, "workflow.status", azimuth::StringValue("completed"))
  azimuth::Attributes::set(completion_attrs, "total.duration_ms", azimuth::IntValue(300))
  azimuth::Attributes::set(completion_attrs, "total.spans", azimuth::IntValue(2))
  
  let completion_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("E2E workflow completed successfully"),
    Some(completion_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(root_span_ctx)),
    Some(azimuth::SpanContext::span_id(root_span_ctx)),
    Some(azimuth::Context::root())
  )
  
  // 验证完成日志
  assert_eq(azimuth::LogRecord::body(completion_log), Some("E2E workflow completed successfully"))
  
  // 发射完成日志
  azimuth::Logger::emit(logger, completion_log)
}

pub test "telemetry system resilience and recovery" {
  // 测试遥测系统的弹性和恢复能力
  
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "resilience-test-service")
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "resilience-test-metrics")
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "resilience-test-logger")
  
  // 模拟正常操作
  let normal_span = azimuth::Tracer::start_span(tracer, "normal-operation")
  azimuth::Span::add_event(normal_span, "Normal operation started")
  azimuth::Span::add_event(normal_span, "Normal operation completed")
  azimuth::Span::set_status(normal_span, azimuth::Ok)
  azimuth::Span::end(normal_span)
  
  // 模拟错误条件
  let error_span = azimuth::Tracer::start_span(tracer, "error-operation")
  azimuth::Span::add_event(error_span, "Error operation started")
  azimuth::Span::add_event(error_span, "Error condition detected")
  azimuth::Span::set_status(error_span, azimuth::Error, Some("Simulated error for testing"))
  azimuth::Span::end(error_span)
  
  // 创建错误日志
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("SimulatedError"))
  azimuth::Attributes::set(error_attrs, "error.severity", azimuth::StringValue("medium"))
  azimuth::Attributes::set(error_attrs, "error.recoverable", azimuth::BoolValue(true))
  azimuth::Attributes::set(error_attrs, "error.retry_count", azimuth::IntValue(3))
  
  let error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Simulated error for resilience testing"),
    Some(error_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("resilience-error-trace"),
    Some("resilience-error-span"),
    Some(azimuth::Context::root())
  )
  
  // 验证错误日志
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(error_log), Some("Simulated error for resilience testing"))
  
  // 发射错误日志
  azimuth::Logger::emit(logger, error_log)
  
  // 模拟恢复操作
  let recovery_span = azimuth::Tracer::start_span(tracer, "recovery-operation")
  azimuth::Span::add_event(recovery_span, "Recovery operation started")
  azimuth::Span::add_event(recovery_span, "System recovered successfully")
  azimuth::Span::set_status(recovery_span, azimuth::Ok)
  azimuth::Span::end(recovery_span)
  
  // 创建恢复日志
  let recovery_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(recovery_attrs, "recovery.action", azimuth::StringValue("automatic_recovery"))
  azimuth::Attributes::set(recovery_attrs, "recovery.duration_ms", azimuth::IntValue(150))
  azimuth::Attributes::set(recovery_attrs, "recovery.success", azimuth::BoolValue(true))
  
  let recovery_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("System recovered from simulated error"),
    Some(recovery_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("resilience-recovery-trace"),
    Some("resilience-recovery-span"),
    Some(azimuth::Context::root())
  )
  
  // 验证恢复日志
  assert_eq(azimuth::LogRecord::severity_number(recovery_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(recovery_log), Some("System recovered from simulated error"))
  
  // 发射恢复日志
  azimuth::Logger::emit(logger, recovery_log)
  
  // 创建弹性指标
  let error_counter = azimuth::Meter::create_counter(
    meter, 
    "resilience.errors.total",
    Some("Total errors encountered"),
    Some("errors")
  )
  
  let recovery_counter = azimuth::Meter::create_counter(
    meter, 
    "resilience.recoveries.total",
    Some("Total successful recoveries"),
    Some("recoveries")
  )
  
  let uptime_gauge = azimuth::Meter::create_gauge(
    meter, 
    "resilience.uptime",
    Some("System uptime percentage"),
    Some("percent")
  )
  
  // 记录弹性指标
  azimuth::Counter::add(error_counter, 1.0)
  azimuth::Counter::add(recovery_counter, 1.0)
  
  // 验证弹性指标
  assert_eq(error_counter.name, "resilience.errors.total")
  assert_eq(recovery_counter.name, "resilience.recoveries.total")
  assert_eq(uptime_gauge.name, "resilience.uptime")
  
  // 验证span状态
  assert_eq(azimuth::Span::status(normal_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(error_span), azimuth::Error)
  assert_eq(azimuth::Span::status(recovery_span), azimuth::Ok)
}

pub test "telemetry system scalability" {
  // 测试遥测系统的可扩展性
  
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "scalability-test-service")
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "scalability-test-metrics")
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "scalability-test-logger")
  
  // 测试大量span创建
  let start_span_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let mut spans = []
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "scalability-span-" + i.to_string())
    azimuth::Span::add_event(span, "Event " + i.to_string())
    azimuth::Span::set_status(span, azimuth::Ok)
    spans.push(span)
  }
  
  let end_span_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let span_duration = end_span_time - start_span_time
  
  // 验证span创建在合理时间内完成
  assert_true(span_duration < 10000000000L, "100个span创建应该在10秒内完成")
  
  // 结束所有span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 测试大量指标创建和操作
  let start_metrics_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let mut counters = []
  let mut histograms = []
  
  for i in 0..50 {
    let counter = azimuth::Meter::create_counter(
      meter, 
      "scalability.counter." + i.to_string(),
      Some("Scalability test counter " + i.to_string()),
      Some("operations")
    )
    counters.push(counter)
    
    let histogram = azimuth::Meter::create_histogram(
      meter, 
      "scalability.histogram." + i.to_string(),
      Some("Scalability test histogram " + i.to_string()),
      Some("ms")
    )
    histograms.push(histogram)
  }
  
  // 对所有指标进行操作
  for counter in counters {
    azimuth::Counter::add(counter, 100.0)
  }
  
  for histogram in histograms {
    for j in 0..10 {
      azimuth::Histogram::record(histogram, (j * 10).to_double())
    }
  }
  
  let end_metrics_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let metrics_duration = end_metrics_time - start_metrics_time
  
  // 验证指标操作在合理时间内完成
  assert_true(metrics_duration < 15000000000L, "大量指标操作应该在15秒内完成")
  
  // 测试大量日志记录
  let start_log_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..200 {
    let log_attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(log_attrs, "log.id", azimuth::IntValue(i))
    azimuth::Attributes::set(log_attrs, "log.batch", azimuth::StringValue("scalability-test"))
    
    let log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Scalability test log message " + i.to_string()),
      Some(log_attrs),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("scalability-trace-" + (i % 10).to_string()),
      Some("scalability-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log)
  }
  
  let end_log_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let log_duration = end_log_time - start_log_time
  
  // 验证日志记录在合理时间内完成
  assert_true(log_duration < 20000000000L, "200条日志记录应该在20秒内完成")
  
  // 验证所有组件创建成功
  assert_eq(spans.length(), 100)
  assert_eq(counters.length(), 50)
  assert_eq(histograms.length(), 50)
}

pub test "telemetry system integration consistency" {
  // 测试遥测系统各组件间的一致性
  
  // 创建共享的trace ID
  let shared_trace_id = "integration-trace-12345678901234567890123456789012"
  
  // 初始化组件
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "integration-test-service")
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "integration-test-metrics")
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "integration-test-logger")
  
  // 创建一致的资源
  let resource = azimuth::Resource::new()
  let resource_attrs = [
    ("service.name", azimuth::StringValue("integration-test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("test.type", azimuth::StringValue("integration-consistency"))
  ]
  let consistent_resource = azimuth::Resource::with_attributes(resource, resource_attrs)
  
  // 创建一致的span
  let span = azimuth::Tracer::start_span(tracer, "integration-operation")
  let span_ctx = azimuth::Span::span_context(span)
  
  // 添加span事件
  azimuth::Span::add_event(span, "Integration test started")
  azimuth::Span::add_event(span, "Consistency validation in progress")
  
  // 创建一致的指标
  let counter = azimuth::Meter::create_counter(
    meter, 
    "integration.operations.total",
    Some("Total integration operations"),
    Some("operations")
  )
  
  let histogram = azimuth::Meter::create_histogram(
    meter, 
    "integration.operation.duration",
    Some("Integration operation duration"),
    Some("ms")
  )
  
  // 记录指标
  azimuth::Counter::add(counter, 1.0)
  azimuth::Histogram::record(histogram, 100.0)
  
  // 创建一致的日志
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "service.name", azimuth::StringValue("integration-test-service"))
  azimuth::Attributes::set(log_attrs, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(log_attrs, "test.type", azimuth::StringValue("integration-consistency"))
  azimuth::Attributes::set(log_attrs, "operation.name", azimuth::StringValue("integration-operation"))
  azimuth::Attributes::set(log_attrs, "operation.status", azimuth::StringValue("in_progress"))
  
  let log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Integration test: Consistency validation in progress"),
    Some(log_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(shared_trace_id),
    Some("integration-span-123"),
    Some(azimuth::Context::root())
  )
  
  // 验证一致性
  // 1. 验证资源属性一致性
  assert_eq(azimuth::Resource::get_attribute(consistent_resource, "service.name"), 
            Some(azimuth::StringValue("integration-test-service")))
  assert_eq(azimuth::Resource::get_attribute(consistent_resource, "service.version"), 
            Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(consistent_resource, "test.type"), 
            Some(azimuth::StringValue("integration-consistency")))
  
  // 2. 验证日志属性与资源属性一致性
  assert_eq(azimuth::Attributes::get(log_attrs, "service.name"), 
            Some(azimuth::StringValue("integration-test-service")))
  assert_eq(azimuth::Attributes::get(log_attrs, "service.version"), 
            Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Attributes::get(log_attrs, "test.type"), 
            Some(azimuth::StringValue("integration-consistency")))
  
  // 3. 验证span名称与日志操作名称一致性
  assert_eq(azimuth::Span::name(span), "integration-operation")
  assert_eq(azimuth::Attributes::get(log_attrs, "operation.name"), 
            Some(azimuth::StringValue("integration-operation")))
  
  // 4. 验证指标命名一致性
  assert_eq(counter.name, "integration.operations.total")
  assert_eq(histogram.name, "integration.operation.duration")
  
  // 完成span
  azimuth::Span::add_event(span, "Integration test completed successfully")
  azimuth::Span::set_status(span, azimuth::Ok)
  azimuth::Span::end(span)
  
  // 创建完成日志
  let completion_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(completion_attrs, "service.name", azimuth::StringValue("integration-test-service"))
  azimuth::Attributes::set(completion_attrs, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Attributes::set(completion_attrs, "test.type", azimuth::StringValue("integration-consistency"))
  azimuth::Attributes::set(completion_attrs, "operation.name", azimuth::StringValue("integration-operation"))
  azimuth::Attributes::set(completion_attrs, "operation.status", azimuth::StringValue("completed"))
  
  let completion_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Integration test: Consistency validation completed"),
    Some(completion_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(shared_trace_id),
    Some("integration-span-123"),
    Some(azimuth::Context::root())
  )
  
  // 验证完成日志
  assert_eq(azimuth::LogRecord::body(completion_log), Some("Integration test: Consistency validation completed"))
  
  // 发射日志
  azimuth::Logger::emit(logger, log)
  azimuth::Logger::emit(logger, completion_log)
}

pub test "telemetry system performance under load" {
  // 测试遥测系统在负载下的性能
  
  let tracer_provider = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "load-test-service")
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "load-test-metrics")
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "load-test-logger")
  
  // 负载测试开始时间
  let load_test_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 并发创建多个span
  let mut spans = []
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "load-test-span-" + i.to_string())
    spans.push(span)
  }
  
  // 并发记录指标
  let request_counter = azimuth::Meter::create_counter(
    meter, 
    "load.test.requests",
    Some("Load test requests"),
    Some("requests")
  )
  
  let latency_histogram = azimuth::Meter::create_histogram(
    meter, 
    "load.test.latency",
    Some("Load test latency"),
    Some("ms")
  )
  
  for i in 0..100 {
    azimuth::Counter::add(request_counter, 1.0)
    azimuth::Histogram::record(latency_histogram, (i % 100).to_double())
  }
  
  // 并发记录日志
  for i in 0..100 {
    let log_attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(log_attrs, "load.test.id", azimuth::IntValue(i))
    azimuth::Attributes::set(log_attrs, "load.test.batch", azimuth::StringValue("high-load"))
    
    let log = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Load test log message " + i.to_string()),
      Some(log_attrs),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("load-test-trace-" + (i % 10).to_string()),
      Some("load-test-span-" + (i % 50).to_string()),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log)
  }
  
  // 完成所有span
  for span in spans {
    azimuth::Span::add_event(span, "Load test span completed")
    azimuth::Span::set_status(span, azimuth::Ok)
    azimuth::Span::end(span)
  }
  
  // 负载测试结束时间
  let load_test_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let load_test_duration = load_test_end - load_test_start
  
  // 验证负载测试在合理时间内完成
  assert_true(load_test_duration < 30000000000L, "负载测试应该在30秒内完成")
  
  // 创建性能指标
  let performance_counter = azimuth::Meter::create_counter(
    meter, 
    "performance.operations.total",
    Some("Total performance operations"),
    Some("operations")
  )
  
  let performance_histogram = azimuth::Meter::create_histogram(
    meter, 
    "performance.duration",
    Some("Performance operation duration"),
    Some("ns")
  )
  
  // 记录性能指标
  azimuth::Counter::add(performance_counter, 250.0)  // 50 spans + 100 metrics + 100 logs
  azimuth::Histogram::record(performance_histogram, load_test_duration.to_double())
  
  // 验证性能指标
  assert_eq(performance_counter.name, "performance.operations.total")
  assert_eq(performance_histogram.name, "performance.duration")
  
  // 创建性能报告日志
  let performance_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(performance_attrs, "test.type", azimuth::StringValue("load_test"))
  azimuth::Attributes::set(performance_attrs, "test.duration_ns", azimuth::IntValue(load_test_end - load_test_start))
  azimuth::Attributes::set(performance_attrs, "spans.created", azimuth::IntValue(50))
  azimuth::Attributes::set(performance_attrs, "metrics.recorded", azimuth::IntValue(100))
  azimuth::Attributes::set(performance_attrs, "logs.emitted", azimuth::IntValue(100))
  azimuth::Attributes::set(performance_attrs, "total.operations", azimuth::IntValue(250))
  azimuth::Attributes::set(performance_attrs, "operations.per_second", azimuth::FloatValue(250.0 * 1000000000.0 / load_test_duration.to_double()))
  
  let performance_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Load test performance report"),
    Some(performance_attrs),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("performance-trace-123"),
    Some("performance-span-456"),
    Some(azimuth::Context::root())
  )
  
  // 验证性能报告日志
  assert_eq(azimuth::LogRecord::body(performance_log), Some("Load test performance report"))
  
  // 发射性能报告日志
  azimuth::Logger::emit(logger, performance_log)
}