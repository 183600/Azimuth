// Azimuth 数据序列化和反序列化综合测试用例
// 专注于验证遥测数据的序列化、反序列化和完整性

// 测试1: Span数据的JSON序列化
test "Span数据的JSON序列化" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.test")
  
  let span = Tracer::start_span(tracer, "serialization.test.span")
  
  // 设置span属性
  Span::set_attribute(span, "service.name", "test-service")
  Span::set_attribute(span, "service.version", "1.0.0")
  Span::set_attribute(span, "operation.type", "test")
  Span::set_attribute(span, "operation.duration", 150)
  Span::set_attribute(span, "operation.success", true)
  Span::set_attribute(span, "user.id", "user-12345")
  
  // 添加事件
  Span::add_event(span, "operation.started", [
    ("timestamp", "2025-01-02T10:00:00Z"),
    ("event.type", "lifecycle")
  ])
  Span::add_event(span, "operation.completed", [
    ("timestamp", "2025-01-02T10:00:01Z"),
    ("result", "success"),
    ("duration.ms", "150")
  ])
  
  // 序列化span为JSON
  let serialized = Span::to_json(span)
  
  // 验证序列化结果包含必要字段
  assert_true(serialized.contains("\"name\":\"serialization.test.span\""))
  assert_true(serialized.contains("\"service.name\":\"test-service\""))
  assert_true(serialized.contains("\"operation.duration\":150"))
  assert_true(serialized.contains("\"operation.success\":true"))
  assert_true(serialized.contains("\"operation.started\""))
  assert_true(serialized.contains("\"operation.completed\""))
  
  Span::end(span)
}

// 测试2: Span数据的JSON反序列化
test "Span数据的JSON反序列化" {
  // 创建JSON格式的span数据
  let span_json = {
    "trace_id": "trace-001",
    "span_id": "span-001",
    "parent_span_id": "",
    "name": "deserialization.test.span",
    "start_time": 1640995200000000000L,
    "end_time": 1640995200150000000L,
    "status": "success",
    "attributes": {
      "service.name": "test-service",
      "service.version": "1.0.0",
      "operation.type": "test",
      "user.id": "user-67890"
    },
    "events": [
      {
        "name": "operation.started",
        "timestamp": 1640995200000000000L,
        "attributes": {
          "event.type": "lifecycle",
          "initiator": "system"
        }
      },
      {
        "name": "operation.completed",
        "timestamp": 1640995200150000000L,
        "attributes": {
          "result": "success",
          "duration.ms": "150"
        }
      }
    ]
  }
  
  // 反序列化JSON为span对象
  let deserialized_span = Span::from_json(span_json)
  
  // 验证反序列化结果
  assert_eq(Span::name(deserialized_span), "deserialization.test.span")
  assert_eq(Span::get_attribute(deserialized_span, "service.name"), Some("test-service"))
  assert_eq(Span::get_attribute(deserialized_span, "user.id"), Some("user-67890"))
  
  // 验证事件
  let events = Span::get_events(deserialized_span)
  assert_eq(events.length(), 2)
  assert_eq(events[0].name, "operation.started")
  assert_eq(events[1].name, "operation.completed")
}

// 测试3: 度量数据的二进制序列化
test "度量数据的二进制序列化" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "binary.serialization.test")
  
  let counter = Meter::create_counter(meter, "binary.test.counter", Some("Binary test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "binary.test.histogram", Some("Binary test histogram"), Some("ms"))
  
  // 添加度量数据
  Counter::add_with_attributes(counter, 10.0, [
    ("service", "api-gateway"),
    ("method", "GET"),
    ("status", "200")
  ])
  Counter::add_with_attributes(counter, 5.0, [
    ("service", "api-gateway"),
    ("method", "POST"),
    ("status", "201")
  ])
  
  Histogram::record_with_attributes(histogram, 45.5, [
    ("endpoint", "/api/users"),
    ("method", "GET")
  ])
  Histogram::record_with_attributes(histogram, 123.7, [
    ("endpoint", "/api/orders"),
    ("method", "POST")
  ])
  
  // 序列化为二进制格式
  let binary_data = Metric::to_binary(counter)
  let histogram_binary = Metric::to_binary(histogram)
  
  // 验证二进制数据不为空
  assert_true(binary_data.length() > 0)
  assert_true(histogram_binary.length() > 0)
  
  // 反序列化二进制数据
  let deserialized_counter = Metric::from_binary(binary_data)
  let deserialized_histogram = Metric::from_binary(histogram_binary)
  
  // 验证反序列化结果
  assert_eq(Metric::name(deserialized_counter), "binary.test.counter")
  assert_eq(Metric::name(deserialized_histogram), "binary.test.histogram")
}

// 测试4: 日志数据的Protocol Buffers序列化
test "日志数据的Protocol Buffers序列化" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "protobuf.test")
  
  // 创建日志记录
  let info_log = LogRecord::new(Info, "Application started successfully")
  LogRecord::add_attribute(info_log, "service.name", "auth-service")
  LogRecord::add_attribute(info_log, "service.version", "2.1.0")
  LogRecord::add_attribute(info_log, "startup.time", "2025-01-02T09:00:00Z")
  
  let error_log = LogRecord::new(Error, "Database connection failed")
  LogRecord::add_attribute(error_log, "error.type", "ConnectionError")
  LogRecord::add_attribute(error_log, "error.code", "DB_CONN_001")
  LogRecord::add_attribute(error_log, "retry.count", 3)
  LogRecord::add_attribute(error_log, "component", "database")
  
  // 序列化为Protocol Buffers格式
  let info_protobuf = LogRecord::to_protobuf(info_log)
  let error_protobuf = LogRecord::to_protobuf(error_log)
  
  // 验证Protocol Buffers数据
  assert_true(info_protobuf.length() > 0)
  assert_true(error_protobuf.length() > 0)
  
  // 反序列化Protocol Buffers数据
  let deserialized_info = LogRecord::from_protobuf(info_protobuf)
  let deserialized_error = LogRecord::from_protobuf(error_protobuf)
  
  // 验证反序列化结果
  assert_eq(LogRecord::severity_number(deserialized_info), Info)
  assert_eq(LogRecord::body(deserialized_info), Some("Application started successfully"))
  assert_eq(LogRecord::get_attribute(deserialized_info, "service.name"), Some("auth-service"))
  
  assert_eq(LogRecord::severity_number(deserialized_error), Error)
  assert_eq(LogRecord::body(deserialized_error), Some("Database connection failed"))
  assert_eq(LogRecord::get_attribute(deserialized_error, "error.code"), Some("DB_CONN_001"))
}

// 测试5: 上下文数据的压缩序列化
test "上下文数据的压缩序列化" {
  let ctx = Context::root()
  
  // 添加大量上下文值
  let ctx_with_values = ctx
  for i = 0; i < 100; i = i + 1 {
    let key = ContextKey::new("key." + i.to_string())
    let value = "value-".repeat(50) + i.to_string() // 创建较长的值
    ctx_with_values = Context::with_value(ctx_with_values, key, value)
  }
  
  // 序列化上下文
  let serialized_ctx = Context::serialize(ctx_with_values)
  
  // 压缩序列化数据
  let compressed_ctx = Compression::compress(serialized_ctx)
  
  // 验证压缩效果
  assert_true(compressed_ctx.length() < serialized_ctx.length())
  let compression_ratio = compressed_ctx.length() / serialized_ctx.length()
  assert_true(compression_ratio < 0.8) // 至少20%的压缩率
  
  // 解压缩数据
  let decompressed_ctx = Compression::decompress(compressed_ctx)
  
  // 反序列化上下文
  let deserialized_ctx = Context::deserialize(decompressed_ctx)
  
  // 验证反序列化结果的完整性
  for i = 0; i < 100; i = i + 1 {
    let key = ContextKey::new("key." + i.to_string())
    let original_value = "value-".repeat(50) + i.to_string()
    let retrieved_value = Context::get(deserialized_ctx, key)
    assert_eq(retrieved_value, Some(original_value))
  }
}

// 测试6: 批量数据的序列化性能测试
test "批量数据的序列化性能测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch.serialization")
  
  // 创建大量span
  let spans = []
  let span_count = 1000
  
  let start_time = Time::now()
  
  for i = 0; i < span_count; i = i + 1 {
    let span = Tracer::start_span(tracer, "batch.span." + i.to_string())
    Span::set_attribute(span, "span.index", i)
    Span::set_attribute(span, "span.category", "batch.test")
    Span::add_event(span, "span.created", [("index", i.to_string())])
    spans.push(span)
  }
  
  // 批量序列化
  let serialized_spans = []
  for i = 0; i < spans.length(); i = i + 1 {
    let serialized = Span::to_json(spans[i])
    serialized_spans.push(serialized)
  }
  
  let serialization_end_time = Time::now()
  let serialization_duration = serialization_end_time - start_time
  
  // 批量反序列化
  let deserialized_spans = []
  for i = 0; i < serialized_spans.length(); i = i + 1 {
    let deserialized = Span::from_json(serialized_spans[i])
    deserialized_spans.push(deserialized)
  }
  
  let end_time = Time::now()
  let total_duration = end_time - start_time
  let deserialization_duration = end_time - serialization_end_time
  
  // 验证性能指标
  let avg_serialization_time = serialization_duration / span_count
  let avg_deserialization_time = deserialization_duration / span_count
  
  assert_true(avg_serialization_time < 100000) // 0.1ms per span
  assert_true(avg_deserialization_time < 150000) // 0.15ms per span
  
  // 验证数据完整性
  assert_eq(deserialized_spans.length(), span_count)
  for i = 0; i < deserialized_spans.length(); i = i + 1 {
    assert_eq(Span::get_attribute(deserialized_spans[i], "span.index"), Some(i.to_string()))
  }
  
  // 结束所有span
  for i = 0; i < spans.length(); i = i + 1 {
    Span::end(spans[i])
  }
}

// 测试7: 跨格式数据转换测试
test "跨格式数据转换测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "format.conversion")
  
  let span = Tracer::start_span(tracer, "format.conversion.test")
  
  // 设置span属性
  Span::set_attribute(span, "service.name", "conversion-service")
  Span::set_attribute(span, "operation.type", "format-test")
  Span::set_attribute(span, "conversion.chain", "json->binary->protobuf->json")
  
  // 添加事件
  Span::add_event(span, "conversion.started", [
    ("source.format", "json"),
    ("target.format", "binary")
  ])
  
  // JSON -> Binary
  let json_data = Span::to_json(span)
  let binary_data = Serialization::json_to_binary(json_data)
  
  // Binary -> Protobuf
  let protobuf_data = Serialization::binary_to_protobuf(binary_data)
  
  // Protobuf -> JSON
  let final_json_data = Serialization::protobuf_to_json(protobuf_data)
  
  // 验证转换链的完整性
  assert_true(binary_data.length() > 0)
  assert_true(protobuf_data.length() > 0)
  assert_true(final_json_data.contains("\"service.name\":\"conversion-service\""))
  assert_true(final_json_data.contains("\"operation.type\":\"format-test\""))
  
  // 从最终JSON反序列化
  let final_span = Span::from_json(final_json_data)
  
  // 验证最终结果
  assert_eq(Span::name(final_span), "format.conversion.test")
  assert_eq(Span::get_attribute(final_span, "service.name"), Some("conversion-service"))
  
  Span::end(span)
}

// 测试8: 序列化数据完整性验证
test "序列化数据完整性验证" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "integrity.test")
  
  let counter = Meter::create_counter(meter, "integrity.counter", Some("Integrity test counter"), Some("count"))
  
  // 添加度量数据
  Counter::add_with_attributes(counter, 42.0, [
    ("test.type", "integrity"),
    ("test.version", "1.0"),
    ("test.timestamp", "2025-01-02T10:00:00Z"),
    ("test.checksum", "abc123def456")
  ])
  
  // 序列化数据
  let serialized_data = Metric::to_json(counter)
  
  // 计算原始数据的校验和
  let original_checksum = Hash::calculate(serialized_data)
  
  // 模拟数据传输
  let transmitted_data = serialized_data
  
  // 验证传输后的数据完整性
  let transmitted_checksum = Hash::calculate(transmitted_data)
  assert_eq(original_checksum, transmitted_checksum)
  
  // 反序列化数据
  let deserialized_counter = Metric::from_json(transmitted_data)
  
  // 验证反序列化后的数据完整性
  assert_eq(Metric::name(deserialized_counter), "integrity.counter")
  assert_eq(Metric::get_attribute(deserialized_counter, "test.type"), Some("integrity"))
  assert_eq(Metric::get_attribute(deserialized_counter, "test.checksum"), Some("abc123def456"))
  
  // 验证度量值
  let metric_value = Metric::get_value(deserialized_counter)
  assert_eq(metric_value, 42.0)
}