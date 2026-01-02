// 遥测数据序列化测试用例
// 测试Azimuth遥测系统的数据序列化和反序列化功能

test "span序列化为JSON格式" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.test")
  
  // 创建带有完整信息的span
  let span = Tracer::start_span(tracer, "serialization.operation")
  Span::set_attribute(span, "service.name", "azimuth-test")
  Span::set_attribute(span, "operation.type", "serialization")
  Span::set_attribute(span, "operation.duration_ms", 250)
  Span::set_attribute(span, "operation.success", true)
  
  // 添加事件
  Span::add_event(span, "operation.started", [
    ("timestamp", "2025-01-02T10:00:00Z"),
    ("component", "serializer")
  ])
  Span::add_event(span, "data.serialized", [
    ("timestamp", "2025-01-02T10:00:01Z"),
    ("data_size", "1024")
  ])
  
  // 序列化span
  let serialized = Span::serialize_to_json(span)
  assert_true(serialized.length() > 0)
  assert_true(serialized.contains("serialization.operation"))
  assert_true(serialized.contains("azimuth-test"))
  
  // 结束span
  Span::end(span)
  assert_true(true)
}

test "度量数据序列化和反序列化" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metrics.serialization.test")
  
  // 创建各种类型的度量
  let counter = Meter::create_counter(meter, "http.requests.total", 
    Some("Total HTTP requests"), Some("requests"))
  let gauge = Meter::create_gauge(meter, "system.memory.usage", 
    Some("Memory usage percentage"), Some("percent"))
  let histogram = Meter::create_histogram(meter, "request.duration", 
    Some("Request duration"), Some("ms"))
  
  // 添加度量数据
  Counter::add_with_attributes(counter, 10.0, [
    ("method", "GET"), 
    ("status", "200"),
    ("endpoint", "/api/data")
  ])
  
  Gauge::set_with_attributes(gauge, 75.5, [
    ("instance", "server-1"),
    ("region", "us-west")
  ])
  
  Histogram::record_with_attributes(histogram, 150.0, [
    ("method", "POST"),
    ("endpoint", "/api/submit")
  ])
  
  // 序列化度量数据
  let metrics_json = Meter::serialize_metrics_to_json(meter)
  assert_true(metrics_json.length() > 0)
  assert_true(metrics_json.contains("http.requests.total"))
  assert_true(metrics_json.contains("system.memory.usage"))
  assert_true(metrics_json.contains("request.duration"))
  
  // 测试反序列化
  let deserialized_metrics = MetricsSerializer::deserialize_from_json(metrics_json)
  assert_true(deserialized_metrics.length() > 0)
  assert_true(true)
}

test "日志记录序列化测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "serialization.logger")
  
  // 创建不同级别的日志记录
  Logger::emit_log(logger, LogLevel::INFO, "System initialization completed", [
    ("component", "bootstrap"),
    ("duration_ms", "1500"),
    ("config_file", "/etc/azimuth/config.yaml")
  ])
  
  Logger::emit_log(logger, LogLevel::WARN, "High memory usage detected", [
    ("component", "monitor"),
    ("memory_usage", "85.2"),
    ("threshold", "80.0")
  ])
  
  Logger::emit_log(logger, LogLevel::ERROR, "Database connection failed", [
    ("component", "database"),
    ("error_code", "CONN_TIMEOUT"),
    ("retry_count", "3"),
    ("max_retries", "5")
  ])
  
  // 序列化日志数据
  let logs_json = Logger::serialize_logs_to_json(logger)
  assert_true(logs_json.length() > 0)
  assert_true(logs_json.contains("System initialization completed"))
  assert_true(logs_json.contains("High memory usage detected"))
  assert_true(logs_json.contains("Database connection failed"))
  assert_true(logs_json.contains("INFO"))
  assert_true(logs_json.contains("WARN"))
  assert_true(logs_json.contains("ERROR"))
  
  assert_true(true)
}

test "批量遥测数据序列化" {
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch.test")
  let meter = MeterProvider::get_meter(meter_provider, "batch.test")
  let logger = LoggerProvider::get_logger(logger_provider, "batch.test")
  
  // 创建多个span
  for i = 0; i < 5; i = i + 1 {
    let span = Tracer::start_span(tracer, "batch.operation." + i.to_string())
    Span::set_attribute(span, "batch.id", i.to_string())
    Span::set_attribute(span, "operation.type", "batch_processing")
    Span::end(span)
  }
  
  // 创建多个度量
  let counter = Meter::create_counter(meter, "batch.operations.total")
  for i = 0; i < 10; i = i + 1 {
    Counter::add_with_attributes(counter, 1.0, [
      ("batch_id", i.to_string()),
      ("status", if i % 2 == 0 { "success" } else { "partial" })
    ])
  }
  
  // 创建多个日志
  for i = 0; i < 3; i = i + 1 {
    Logger::emit_log(logger, LogLevel::INFO, "Batch item processed", [
      ("item_id", i.to_string()),
      ("status", "completed")
    ])
  }
  
  // 批量序列化
  let batch_data = TelemetryBatchSerializer::serialize_all(
    tracer_provider, meter_provider, logger_provider
  )
  
  assert_true(batch_data.length() > 0)
  assert_true(batch_data.contains("batch.operation"))
  assert_true(batch_data.contains("batch.operations.total"))
  assert_true(batch_data.contains("Batch item processed"))
  
  assert_true(true)
}