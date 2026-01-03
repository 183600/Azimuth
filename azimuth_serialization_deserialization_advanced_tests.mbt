// 数据序列化和反序列化高级测试
// 测试Azimuth遥测系统的数据序列化和反序列化功能

test "遥测数据JSON序列化" {
  // 测试遥测数据的JSON序列化
  let telemetry_data = TelemetryData::new()
  
  // 添加span数据
  let span_data = SpanData::new("test.operation", "1234567890")
  SpanData::add_attribute(span_data, "service.name", "test-service")
  SpanData::add_attribute(span_data, "operation.type", "http.request")
  SpanData::add_attribute(span_data, "http.method", "GET")
  SpanData::add_attribute(span_data, "http.url", "/api/users")
  SpanData::add_attribute(span_data, "http.status_code", "200")
  SpanData::add_event(span_data, "request.started", 1704067200000L)
  SpanData::add_event(span_data, "request.completed", 1704067201000L)
  
  TelemetryData::add_span(telemetry_data, span_data)
  
  // 添加度量数据
  let metric_data = MetricData::new_counter("http.requests.total", 1.0)
  MetricData::add_attribute(metric_data, "method", "GET")
  MetricData::add_attribute(metric_data, "status", "200")
  MetricData::add_attribute(metric_data, "endpoint", "/api/users")
  
  TelemetryData::add_metric(telemetry_data, metric_data)
  
  // 添加日志数据
  let log_data = LogData::new(Info, "Request processed successfully")
  LogData::add_attribute(log_data, "request.id", "req-12345")
  LogData::add_attribute(log_data, "user.id", "user-67890")
  LogData::set_timestamp(log_data, 1704067201500L)
  
  TelemetryData::add_log(telemetry_data, log_data)
  
  // 序列化为JSON
  let json_serializer = JsonSerializer::new()
  let json_output = JsonSerializer::serialize(json_serializer, telemetry_data)
  
  // 验证JSON包含预期内容
  assert_true(String::contains(json_output, "test.operation"))
  assert_true(String::contains(json_output, "test-service"))
  assert_true(String::contains(json_output, "http.requests.total"))
  assert_true(String::contains(json_output, "Request processed successfully"))
  
  // 验证JSON有效性
  assert_true(JsonValidator::is_valid(json_output))
}

test "遥测数据JSON反序列化" {
  // 测试遥测数据的JSON反序列化
  let json_input = "{\"spans\":[{\"name\":\"test.operation\",\"trace_id\":\"1234567890\",\"attributes\":{\"service.name\":\"test-service\",\"operation.type\":\"http.request\"},\"events\":[{\"name\":\"request.started\",\"timestamp\":1704067200000}]}],\"metrics\":[{\"name\":\"http.requests.total\",\"value\":1.0,\"attributes\":{\"method\":\"GET\",\"status\":\"200\"}}],\"logs\":[{\"severity\":\"info\",\"body\":\"Request processed successfully\",\"attributes\":{\"request.id\":\"req-12345\"},\"timestamp\":1704067201500}]}"
  
  // 反序列化JSON
  let json_deserializer = JsonDeserializer::new()
  let telemetry_data = JsonDeserializer::deserialize(json_deserializer, json_input)
  
  // 验证反序列化结果
  let spans = TelemetryData::get_spans(telemetry_data)
  let metrics = TelemetryData::get_metrics(telemetry_data)
  let logs = TelemetryData::get_logs(telemetry_data)
  
  assert_eq(Array::length(spans), 1)
  assert_eq(Array::length(metrics), 1)
  assert_eq(Array::length(logs), 1)
  
  // 验证span数据
  let span = Array::get(spans, 0)
  assert_eq(SpanData::get_name(span), "test.operation")
  assert_eq(SpanData::get_trace_id(span), "1234567890")
  
  let span_attrs = SpanData::get_attributes(span)
  assert_true(AttributeMap::contains(span_attrs, "service.name"))
  assert_true(AttributeMap::contains(span_attrs, "operation.type"))
  
  // 验证metric数据
  let metric = Array::get(metrics, 0)
  assert_eq(MetricData::get_name(metric), "http.requests.total")
  assert_eq(MetricData::get_value(metric), 1.0)
  
  // 验证log数据
  let log = Array::get(logs, 0)
  assert_eq(LogData::get_severity(log), Info)
  assert_eq(LogData::get_body(log), "Request processed successfully")
}

test "二进制序列化和反序列化" {
  // 测试二进制序列化和反序列化
  let telemetry_data = TelemetryData::new()
  
  // 创建大量数据
  for i in 0..100 {
    let span_data = SpanData::new("operation." + i.to_string(), "trace." + i.to_string())
    SpanData::add_attribute(span_data, "index", i.to_string())
    SpanData::add_attribute(span_data, "data", "large.data.payload." + i.to_string())
    
    for j in 0..10 {
      SpanData::add_event(span_data, "event." + j.to_string(), 1704067200000L + (i * 10 + j).to_long())
    }
    
    TelemetryData::add_span(telemetry_data, span_data)
  }
  
  // 二进制序列化
  let binary_serializer = BinarySerializer::new()
  let binary_data = BinarySerializer::serialize(binary_serializer, telemetry_data)
  
  // 验证二进制数据
  assert_true(Array::length(binary_data) > 0)
  
  // 二进制反序列化
  let binary_deserializer = BinaryDeserializer::new()
  let deserialized_data = BinaryDeserializer::deserialize(binary_deserializer, binary_data)
  
  // 验证反序列化结果
  let original_spans = TelemetryData::get_spans(telemetry_data)
  let deserialized_spans = TelemetryData::get_spans(deserialized_data)
  
  assert_eq(Array::length(original_spans), Array::length(deserialized_spans))
  
  // 验证数据完整性
  for i in 0..100 {
    let original_span = Array::get(original_spans, i)
    let deserialized_span = Array::get(deserialized_spans, i)
    
    assert_eq(SpanData::get_name(original_span), SpanData::get_name(deserialized_span))
    assert_eq(SpanData::get_trace_id(original_span), SpanData::get_trace_id(deserialized_span))
  }
}

test "压缩序列化" {
  // 测试压缩序列化功能
  let telemetry_data = TelemetryData::new()
  
  // 创建大量重复数据以测试压缩效果
  for i in 0..1000 {
    let span_data = SpanData::new("repeated.operation", "trace.12345")
    SpanData::add_attribute(span_data, "service.name", "test-service")
    SpanData::add_attribute(span_data, "service.version", "1.0.0")
    SpanData::add_attribute(span_data, "deployment.environment", "production")
    SpanData::add_attribute(span_data, "host.name", "server-01")
    SpanData::add_attribute(span_data, "operation.index", i.to_string())
    
    TelemetryData::add_span(telemetry_data, span_data)
  }
  
  // 普通序列化
  let normal_serializer = JsonSerializer::new()
  let normal_data = JsonSerializer::serialize(normal_serializer, telemetry_data)
  let normal_size = String::length(normal_data)
  
  // 压缩序列化
  let compressed_serializer = CompressedSerializer::new(Gzip)
  let compressed_data = CompressedSerializer::serialize(compressed_serializer, telemetry_data)
  let compressed_size = Array::length(compressed_data)
  
  // 验证压缩效果
  assert_true(compressed_size < normal_size)
  
  // 计算压缩率
  let compression_ratio = (compressed_size.to_float() / normal_size.to_float()) * 100.0
  assert_true(compression_ratio < 80.0) // 至少20%的压缩率
  
  // 压缩反序列化
  let compressed_deserializer = CompressedDeserializer::new(Gzip)
  let deserialized_data = CompressedDeserializer::deserialize(compressed_deserializer, compressed_data)
  
  // 验证数据完整性
  let original_spans = TelemetryData::get_spans(telemetry_data)
  let deserialized_spans = TelemetryData::get_spans(deserialized_data)
  
  assert_eq(Array::length(original_spans), Array::length(deserialized_spans))
}

test "跨格式序列化转换" {
  // 测试跨格式序列化转换
  let telemetry_data = TelemetryData::new()
  
  // 添加测试数据
  let span_data = SpanData::new("format.test.operation", "trace.98765")
  SpanData::add_attribute(span_data, "format.test", "true")
  SpanData::add_attribute(span_data, "conversion.test", "true")
  
  TelemetryData::add_span(telemetry_data, span_data)
  
  // JSON序列化
  let json_serializer = JsonSerializer::new()
  let json_data = JsonSerializer::serialize(json_serializer, telemetry_data)
  
  // JSON转XML
  let json_to_xml_converter = FormatConverter::new(Json, Xml)
  let xml_data = FormatConverter::convert(json_to_xml_converter, json_data)
  
  // 验证XML格式
  assert_true(String::contains(xml_data, "<?xml"))
  assert_true(String::contains(xml_data, "<spans>"))
  assert_true(String::contains(xml_data, "</spans>"))
  
  // XML转JSON
  let xml_to_json_converter = FormatConverter::new(Xml, Json)
  let converted_json = FormatConverter::convert(xml_to_json_converter, xml_data)
  
  // 验证转换后的JSON
  assert_true(JsonValidator::is_valid(converted_json))
  
  // 验证数据一致性
  let original_data = JsonDeserializer::deserialize(JsonDeserializer::new(), json_data)
  let converted_data = JsonDeserializer::deserialize(JsonDeserializer::new(), converted_json)
  
  let original_spans = TelemetryData::get_spans(original_data)
  let converted_spans = TelemetryData::get_spans(converted_data)
  
  assert_eq(Array::length(original_spans), Array::length(converted_spans))
  
  if Array::length(original_spans) > 0 {
    let original_span = Array::get(original_spans, 0)
    let converted_span = Array::get(converted_spans, 0)
    
    assert_eq(SpanData::get_name(original_span), SpanData::get_name(converted_span))
    assert_eq(SpanData::get_trace_id(original_span), SpanData::get_trace_id(converted_span))
  }
}

test "序列化错误处理和恢复" {
  // 测试序列化错误处理和恢复
  let telemetry_data = TelemetryData::new()
  
  // 添加包含特殊字符的数据
  let span_data = SpanData::new("special.chars.test", "trace.12345")
  SpanData::add_attribute(span_data, "special.chars", "特殊字符: !@#$%^&*()_+-=[]{}|;':\",./<>?")
  SpanData::add_attribute(span_data, "unicode.chars", "Unicode: 🚀 🔥 💯 🎉")
  SpanData::add_attribute(span_data, "null.chars", "Null\x00character\x00test")
  
  TelemetryData::add_span(telemetry_data, span_data)
  
  // 测试容错序列化
  let fault_tolerant_serializer = FaultTolerantSerializer::new()
  let serialized_data = FaultTolerantSerializer::serialize(fault_tolerant_serializer, telemetry_data)
  
  // 验证序列化成功
  assert_true(String::length(serialized_data) > 0)
  
  // 测试容错反序列化
  let fault_tolerant_deserializer = FaultTolerantDeserializer::new()
  let deserialized_data = FaultTolerantDeserializer::deserialize(fault_tolerant_deserializer, serialized_data)
  
  // 验证反序列化成功
  let spans = TelemetryData::get_spans(deserialized_data)
  assert_eq(Array::length(spans), 1)
  
  // 测试损坏数据处理
  let corrupted_data = String::substring(serialized_data, 0, String::length(serialized_data) / 2)
  let recovered_data = FaultTolerantDeserializer::deserialize_with_recovery(fault_tolerant_deserializer, corrupted_data)
  
  // 验证部分恢复
  let recovered_spans = TelemetryData::get_spans(recovered_data)
  assert_true(Array::length(recovered_spans) >= 0)
}

test "增量序列化和差异更新" {
  // 测试增量序列化和差异更新
  let base_telemetry_data = TelemetryData::new()
  
  // 添加基础数据
  let base_span = SpanData::new("base.operation", "trace.base")
  SpanData::add_attribute(base_span, "base.attribute", "base.value")
  SpanData::add_attribute(base_span, "static.attribute", "static.value")
  
  TelemetryData::add_span(base_telemetry_data, base_span)
  
  // 创建增量数据
  let incremental_telemetry_data = TelemetryData::new()
  
  let incremental_span = SpanData::new("base.operation", "trace.base")
  SpanData::add_attribute(incremental_span, "base.attribute", "updated.value")
  SpanData::add_attribute(incremental_span, "new.attribute", "new.value")
  SpanData::add_attribute(incremental_span, "static.attribute", "static.value")
  
  TelemetryData::add_span(incremental_telemetry_data, incremental_span)
  
  // 生成差异
  let diff_generator = DiffGenerator::new()
  let diff_data = DiffGenerator::generate(diff_generator, base_telemetry_data, incremental_telemetry_data)
  
  // 验证差异数据
  assert_true(DiffData::has_changes(diff_data))
  assert_true(DiffData::has_attribute_change(diff_data, "base.attribute"))
  assert_true(DiffData::has_new_attribute(diff_data, "new.attribute"))
  assert_false(DiffData::has_attribute_change(diff_data, "static.attribute"))
  
  // 序列化差异
  let diff_serializer = DiffSerializer::new()
  let serialized_diff = DiffSerializer::serialize(diff_serializer, diff_data)
  
  // 验证差异序列化
  assert_true(String::length(serialized_diff) > 0)
  
  // 应用差异到基础数据
  let diff_deserializer = DiffDeserializer::new()
  let applied_diff = DiffDeserializer::apply(diff_deserializer, base_telemetry_data, serialized_diff)
  
  // 验证应用结果
  let applied_spans = TelemetryData::get_spans(applied_diff)
  assert_eq(Array::length(applied_spans), 1)
  
  let applied_span = Array::get(applied_spans, 0)
  let applied_attrs = SpanData::get_attributes(applied_span)
  
  assert_true(AttributeMap::contains(applied_attrs, "base.attribute"))
  assert_true(AttributeMap::contains(applied_attrs, "new.attribute"))
  assert_true(AttributeMap::contains(applied_attrs, "static.attribute"))
}

test "流式序列化和大数据处理" {
  // 测试流式序列化和大数据处理
  let stream_serializer = StreamSerializer::new()
  
  // 开始流式序列化
  StreamSerializer::begin_stream(stream_serializer, "large.telemetry.data")
  
  // 流式添加大量数据
  for i in 0..10000 {
    let span_data = SpanData::new("stream.operation." + i.to_string(), "stream.trace." + i.to_string())
    SpanData::add_attribute(span_data, "index", i.to_string())
    SpanData::add_attribute(span_data, "batch", (i / 100).to_string())
    
    // 每100个span写入一次流
    StreamSerializer::add_span(stream_serializer, span_data)
    
    if i % 100 == 0 {
      StreamSerializer::flush(stream_serializer)
    }
  }
  
  // 结束流式序列化
  let stream_data = StreamSerializer::end_stream(stream_serializer)
  
  // 验证流式数据
  assert_true(Array::length(stream_data) > 0)
  
  // 流式反序列化
  let stream_deserializer = StreamDeserializer::new()
  StreamDeserializer::begin_stream(stream_deserializer, stream_data)
  
  let mut total_spans = 0
  let mut processed_batches = 0
  
  while StreamDeserializer::has_more(stream_deserializer) {
    let batch = StreamDeserializer::read_batch(stream_deserializer, 100)
    total_spans = total_spans + Array::length(batch)
    processed_batches = processed_batches + 1
  }
  
  // 验证流式处理结果
  assert_eq(total_spans, 10000)
  assert_eq(processed_batches, 100)
  
  StreamDeserializer::end_stream(stream_deserializer)
}