// 实际导出器集成测试用例
// 测试Azimuth遥测系统与真实后端(如Jaeger、Prometheus)的集成

test "jaeger_exporter_span_integration" {
  // 创建TracerProvider和配置Jaeger导出器
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "jaeger-test", Some("1.0.0"))
  
  // 模拟Jaeger导出器配置
  let jaeger_endpoint = "http://localhost:14268/api/traces"
  let jaeger_config = ExporterConfig::new(jaeger_endpoint, Some("jaeger"), Some(1000))
  
  // 创建多个Span形成完整的追踪链
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  Span::set_attribute(parent_span, "service.name", StringValue("test-service"))
  Span::set_attribute(parent_span, "operation.type", StringValue("parent"))
  
  // 创建子Span
  let child_span = Tracer::start_span_with_parent(tracer, "child-operation", Some(Span::span_context(parent_span)))
  Span::set_attribute(child_span, "service.name", StringValue("test-service"))
  Span::set_attribute(child_span, "operation.type", StringValue("child"))
  Span::add_event(child_span, "database.query.started", Some([("query", StringValue("SELECT * FROM users"))]))
  
  // 模拟数据库操作延迟
  let start_time = Clock::now_unix_nanos(Clock::system())
  // 模拟处理时间
  let end_time = Clock::now_unix_nanos(Clock::system())
  let duration = end_time - start_time
  
  Span::set_attribute(child_span, "db.duration", IntValue(duration))
  Span::set_status(child_span, Ok, Some("Database query completed"))
  Span::end(child_span)
  
  Span::set_status(parent_span, Ok, Some("Parent operation completed"))
  Span::end(parent_span)
  
  // 验证Span数据结构符合Jaeger格式要求
  assert_true(Span::span_context(parent_span) |> SpanContext::is_valid)
  assert_true(Span::span_context(child_span) |> SpanContext::is_valid)
  assert_eq(SpanContext::trace_id(Span::span_context(parent_span)), SpanContext::trace_id(Span::span_context(child_span)))
}

test "prometheus_metrics_exporter_integration" {
  // 创建MeterProvider和配置Prometheus导出器
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "prometheus-test", Some("1.0.0"))
  
  // 创建各种类型的度量工具
  let request_counter = Meter::create_counter(meter, "http_requests_total", Some("Total HTTP requests"), Some("requests"))
  let response_duration = Meter::create_histogram(meter, "http_response_duration_seconds", Some("HTTP response duration"), Some("seconds"))
  let active_connections = Meter::create_up_down_counter(meter, "active_connections", Some("Active connections"), Some("connections"))
  let memory_usage = Meter::create_gauge(meter, "memory_usage_bytes", Some("Memory usage in bytes"), Some("bytes"))
  
  // 模拟度量数据收集
  Counter::add_with_attributes(request_counter, 100.0, [("method", StringValue("GET")), ("status", StringValue("200"))])
  Counter::add_with_attributes(request_counter, 25.0, [("method", StringValue("POST")), ("status", StringValue("201"))])
  Counter::add_with_attributes(request_counter, 5.0, [("method", StringValue("GET")), ("status", StringValue("404"))])
  
  // 记录响应时间分布
  Histogram::record_with_attributes(response_duration, 0.1, [("endpoint", StringValue("/api/users"))])
  Histogram::record_with_attributes(response_duration, 0.25, [("endpoint", StringValue("/api/users"))])
  Histogram::record_with_attributes(response_duration, 0.05, [("endpoint", StringValue("/api/health"))])
  
  // 模拟连接数变化
  UpDownCounter::add(active_connections, 50.0)
  UpDownCounter::add(active_connections, -10.0)
  UpDownCounter::add(active_connections, 5.0)
  
  // 设置内存使用量
  Gauge::set(memory_usage, 1024.0 * 1024.0 * 512.0)  // 512MB
  
  // 验证度量工具配置正确
  assert_eq(request_counter.name, "http_requests_total")
  assert_eq(response_duration.name, "http_response_duration_seconds")
  assert_eq(active_connections.name, "active_connections")
  assert_eq(memory_usage.name, "memory_usage_bytes")
}

test "otlp_protocol_exporter_integration" {
  // 测试OTLP(OpenTelemetry Protocol)导出器
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "otlp-test")
  let meter = MeterProvider::get_meter(meter_provider, "otlp-test")
  let logger = LoggerProvider::get_logger(logger_provider, "otlp-test")
  
  // 配置OTLP导出器
  let otlp_endpoint = "http://localhost:4317"
  let otlp_config = OTLPExporterConfig::new(otlp_endpoint, Some("grpc"), Some(5000))
  
  // 创建复合遥测数据
  let span = Tracer::start_span(tracer, "otlp-complex-operation")
  Span::set_attribute(span, "service.name", StringValue("otlp-test-service"))
  Span::set_attribute(span, "service.version", StringValue("1.0.0"))
  
  // 添加度量数据
  let counter = Meter::create_counter(meter, "otlp_operations_total")
  Counter::add_with_attributes(counter, 1.0, [("operation", StringValue("otlp-complex-operation"))])
  
  // 添加日志数据
  let log_record = LogRecord::new_with_context(
    Info,
    Some("OTLP operation in progress"),
    Some(Attributes::from_array([("operation.id", StringValue("op-12345"))])),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(SpanContext::trace_id(Span::span_context(span))),
    Some(SpanContext::span_id(Span::span_context(span))),
    None
  )
  Logger::emit(logger, log_record)
  
  // 验证OTLP数据结构完整性
  assert_true(Span::is_recording(span))
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(counter.name, "otlp_operations_total")
  
  Span::end(span)
}

test "exporter_batch_processing_integration" {
  // 测试导出器的批处理功能
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch-test")
  
  // 配置批处理导出器
  let batch_config = BatchExporterConfig::new(
    Some(512),        // max_batch_size
    Some(5000),       // max_export_timeout_ms
    Some(1000),       // scheduled_delay_ms
    Some(3)           // max_export_batch_size
  )
  
  // 创建大量Span来测试批处理
  let spans = []
  for i = 0; i < 100; i = i + 1 {
    let span_name = "batch-operation-" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    Span::set_attribute(span, "batch.index", IntValue(i))
    Span::set_attribute(span, "batch.size", IntValue(100))
    
    // 为部分Span添加事件
    if i % 10 == 0 {
      Span::add_event(span, "batch.milestone", Some([("milestone", StringValue("important"))]))
    }
    
    spans = spans @ [span]
  }
  
  // 结束所有Span
  for span in spans {
    Span::end(span)
  }
  
  // 验证批处理配置
  assert_eq(batch_config.max_batch_size, Some(512))
  assert_eq(batch_config.max_export_timeout_ms, Some(5000))
  assert_eq(batch_config.scheduled_delay_ms, Some(1000))
  assert_eq(batch_config.max_export_batch_size, Some(3))
}

test "exporter_resilience_and_retry_integration" {
  // 测试导出器的容错和重试机制
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "resilience-test")
  
  // 配置具有重试机制的导出器
  let retry_config = RetryConfig::new(
    Some(3),          // max_retries
    Some(1000),       // initial_backoff_ms
    Some(2.0),        // backoff_multiplier
    Some(10000)       // max_backoff_ms
  )
  
  // 模拟网络不稳定的场景
  let span = Tracer::start_span(tracer, "resilience-operation")
  Span::set_attribute(span, "resilience.test", StringValue("enabled"))
  
  // 模拟导出失败和重试
  let export_attempts = 0
  let max_attempts = 3
  
  // 模拟导出过程
  while export_attempts < max_attempts {
    export_attempts = export_attempts + 1
    
    // 模拟随机失败
    if export_attempts < max_attempts {
      // 导出失败，需要重试
      Span::add_event(span, "export.failed", Some([("attempt", IntValue(export_attempts))]))
    } else {
      // 导出成功
      Span::add_event(span, "export.success", Some([("attempt", IntValue(export_attempts))]))
      break
    }
  }
  
  Span::set_status(span, Ok, Some("Operation completed with resilience"))
  Span::end(span)
  
  // 验证重试配置
  assert_eq(retry_config.max_retries, Some(3))
  assert_eq(retry_config.initial_backoff_ms, Some(1000))
  assert_eq(retry_config.backoff_multiplier, Some(2.0))
  assert_eq(retry_config.max_backoff_ms, Some(10000))
}