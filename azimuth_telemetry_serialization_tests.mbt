// 遥测数据序列化测试
// 测试Azimuth遥测数据的序列化和反序列化功能

test "span序列化到JSON格式" {
  // 创建测试span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.test")
  
  let span = Tracer::start_span(tracer, "serialization.test.operation")
  
  // 设置span属性
  Span::set_attribute(span, "http.method", "GET")
  Span::set_attribute(span, "http.url", "https://api.example.com/users")
  Span::set_attribute(span, "http.status_code", 200)
  Span::set_attribute(span, "user.id", "user-12345")
  
  // 添加事件
  Span::add_event(span, "http.request.started", [
    ("timestamp", "2025-01-02T10:00:00Z"),
    ("request.size", "1024")
  ])
  
  Span::add_event(span, "http.response.completed", [
    ("timestamp", "2025-01-02T10:00:05Z"),
    ("response.size", "2048"),
    ("duration", "5000")
  ])
  
  // 序列化span到JSON
  let serialized = SpanSerializer::to_json(span)
  
  // 验证JSON包含必要字段
  assert_true(serialized.contains("\"name\":\"serialization.test.operation\""))
  assert_true(serialized.contains("\"http.method\":\"GET\""))
  assert_true(serialized.contains("\"http.url\":\"https://api.example.com/users\""))
  assert_true(serialized.contains("\"http.status_code\":200"))
  assert_true(serialized.contains("\"user.id\":\"user-12345\""))
  assert_true(serialized.contains("\"http.request.started\""))
  assert_true(serialized.contains("\"http.response.completed\""))
  
  Span::end(span)
}

test "度量数据序列化到二进制格式" {
  // 创建测试度量
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metrics.serialization.test")
  
  // 创建计数器
  let counter = Meter::create_counter(meter, "requests.total", Some("Total requests"), Some("count"))
  
  // 添加度量数据
  Counter::add_with_attributes(counter, 1.0, [
    ("method", "GET"),
    ("endpoint", "/api/users"),
    ("status", "200")
  ])
  
  Counter::add_with_attributes(counter, 1.0, [
    ("method", "POST"),
    ("endpoint", "/api/orders"),
    ("status", "201")
  ])
  
  // 创建直方图
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  
  Histogram::record_with_attributes(histogram, 123.5, [
    ("endpoint", "/api/users"),
    ("method", "GET")
  ])
  
  Histogram::record_with_attributes(histogram, 456.7, [
    ("endpoint", "/api/orders"),
    ("method", "POST")
  ])
  
  // 序列化到二进制格式
  let binary_data = MetricsSerializer::to_binary(counter, histogram)
  
  // 验证二进制数据不为空
  assert_true(binary_data.length() > 0)
  
  // 反序列化并验证
  let deserialized_metrics = MetricsSerializer::from_binary(binary_data)
  assert_true(deserialized_metrics.is_valid())
  
  // 验证反序列化的数据
  let counter_data = deserialized_metrics.get_counter("requests.total")
  assert_true(counter_data.is_some())
  
  let histogram_data = deserialized_metrics.get_histogram("response.time")
  assert_true(histogram_data.is_some())
}

test "日志记录序列化到结构化格式" {
  // 创建测试日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "log.serialization.test")
  
  // 创建不同级别的日志记录
  let info_log = LogRecord::new(Info, "User login successful")
  LogRecord::add_attribute(info_log, "user.id", "user-12345")
  LogRecord::add_attribute(info_log, "ip.address", "192.168.1.100")
  LogRecord::add_attribute(info_log, "user.agent", "Mozilla/5.0")
  LogRecord::add_attribute(info_log, "session.id", "sess-abcdef")
  
  let warn_log = LogRecord::new(Warn, "Rate limit approaching")
  LogRecord::add_attribute(warn_log, "current.rate", "95")
  LogRecord::add_attribute(warn_log, "limit", "100")
  LogRecord::add_attribute(warn_log, "window", "1m")
  LogRecord::add_attribute(warn_log, "client.id", "client-67890")
  
  let error_log = LogRecord::new(Error, "Database connection failed")
  LogRecord::add_attribute(error_log, "error.code", "DB_CONN_ERROR")
  LogRecord::add_attribute(error_log, "retry.attempt", "3")
  LogRecord::add_attribute(error_log, "max.retries", "5")
  LogRecord::add_attribute(error_log, "connection.pool", "primary")
  
  // 序列化日志到结构化格式
  let structured_logs = LogSerializer::to_structured([info_log, warn_log, error_log])
  
  // 验证结构化日志
  assert_eq(structured_logs.length(), 3)
  
  let info_serialized = structured_logs.get(0)
  assert_true(info_serialized.contains("\"severity\":\"INFO\""))
  assert_true(info_serialized.contains("\"message\":\"User login successful\""))
  assert_true(info_serialized.contains("\"user.id\":\"user-12345\""))
  
  let warn_serialized = structured_logs.get(1)
  assert_true(warn_serialized.contains("\"severity\":\"WARN\""))
  assert_true(warn_serialized.contains("\"message\":\"Rate limit approaching\""))
  assert_true(warn_serialized.contains("\"current.rate\":\"95\""))
  
  let error_serialized = structured_logs.get(2)
  assert_true(error_serialized.contains("\"severity\":\"ERROR\""))
  assert_true(error_serialized.contains("\"message\":\"Database connection failed\""))
  assert_true(error_serialized.contains("\"error.code\":\"DB_CONN_ERROR\""))
}

test "批量遥测数据序列化" {
  // 创建多个span用于批量测试
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch.serialization.test")
  
  let spans = []
  for i in 1..=5 {
    let span = Tracer::start_span(tracer, "batch.operation." + i.to_string())
    Span::set_attribute(span, "batch.id", "batch-123")
    Span::set_attribute(span, "operation.index", i)
    Span::set_attribute(span, "operation.type", "batch.process")
    
    spans = spans.push(span)
  }
  
  // 创建多个度量
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "batch.metrics.test")
  
  let batch_counter = Meter::create_counter(meter, "batch.operations", Some("Batch operations"), Some("count"))
  for i in 1..=5 {
    Counter::add_with_attributes(batch_counter, 1.0, [
      ("batch.id", "batch-123"),
      "operation.index", i,
      ("operation.status", "completed")
    ])
  }
  
  // 批量序列化
  let batch_serializer = BatchSerializer::new()
  let serialized_data = batch_serializer.serialize_spans(spans)
  let serialized_metrics = batch_serializer.serialize_metrics(batch_counter)
  
  // 验证批量序列化结果
  assert_true(serialized_data.contains("\"batch.id\":\"batch-123\""))
  assert_true(serialized_data.contains("\"operation.type\":\"batch.process\""))
  
  // 验证压缩功能
  let compressed_data = batch_serializer.compress(serialized_data)
  assert_true(compressed_data.length() < serialized_data.length())
  
  // 验证解压缩功能
  let decompressed_data = batch_serializer.decompress(compressed_data)
  assert_eq(decompressed_data, serialized_data)
  
  // 结束所有spans
  for span in spans {
    Span::end(span)
  }
}

test "序列化错误处理和恢复" {
  // 测试序列化过程中的错误处理
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.serialization.test")
  
  let span = Tracer::start_span(tracer, "error.test.operation")
  
  // 添加可能导致序列化问题的特殊字符
  Span::set_attribute(span, "special.chars", "特殊字符: \"quotes\", 'apostrophes', \n\t\r")
  Span::set_attribute(span, "unicode.chars", "🚀 emoji and üñíçødé")
  Span::set_attribute(span, "null.value", "")
  Span::set_attribute(span, "large.data", "x" * 10000)  // 大数据
  
  // 测试序列化错误处理
  match SpanSerializer::to_json_safe(span) {
    Ok(serialized) => {
      // 验证特殊字符被正确处理
      assert_true(serialized.contains("特殊字符"))
      assert_true(serialized.contains("🚀"))
      assert_true(serialized.contains("üñíçødé"))
    }
    Err(error) => {
      // 如果序列化失败，验证错误信息
      assert_true(error.contains("serialization") || error.contains("encoding"))
    }
  }
  
  // 测试损坏数据的反序列化
  let corrupted_data = "{\"invalid\": json data"
  match SpanSerializer::from_json(corrupted_data) {
    Ok(_) => assert_true(false)  // 不应该成功
    Err(error) => assert_true(error.contains("parse") || error.contains("invalid"))
  }
  
  // 测试空数据处理
  let empty_data = ""
  match SpanSerializer::from_json(empty_data) {
    Ok(_) => assert_true(false)  // 不应该成功
    Err(error) => assert_true(error.contains("empty") || error.contains("parse"))
  }
  
  Span::end(span)
}

test "跨格式序列化兼容性" {
  // 测试不同序列化格式之间的兼容性
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "compatibility.test")
  
  let span = Tracer::start_span(tracer, "compatibility.test.operation")
  
  // 设置测试属性
  Span::set_attribute(span, "service.name", "test-service")
  Span::set_attribute(span, "service.version", "1.0.0")
  Span::set_attribute(span, "environment", "test")
  Span::set_attribute(span, "trace.id", "trace-123456")
  Span::set_attribute(span, "span.id", "span-789012")
  
  // 序列化到JSON
  let json_data = SpanSerializer::to_json(span)
  
  // 从JSON转换为其他格式
  let xml_data = FormatConverter::json_to_xml(json_data)
  let yaml_data = FormatConverter::json_to_yaml(json_data)
  let protobuf_data = FormatConverter::json_to_protobuf(json_data)
  
  // 验证格式转换
  assert_true(xml_data.contains("<service.name>test-service</service.name>"))
  assert_true(yaml_data.contains("service.name: test-service"))
  assert_true(protobuf_data.length() > 0)
  
  // 验证往返转换的一致性
  let json_from_xml = FormatConverter::xml_to_json(xml_data)
  let json_from_yaml = FormatConverter::yaml_to_json(yaml_data)
  let json_from_protobuf = FormatConverter::protobuf_to_json(protobuf_data)
  
  // 验证关键数据在所有格式中都保持一致
  assert_true(json_from_xml.contains("test-service"))
  assert_true(json_from_yaml.contains("test-service"))
  assert_true(json_from_protobuf.contains("test-service"))
  
  Span::end(span)
}