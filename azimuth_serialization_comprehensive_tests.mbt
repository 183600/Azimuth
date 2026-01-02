// Azimuth 序列化综合测试用例
// 测试序列化和反序列化功能，包括多种格式、压缩和加密

// 测试1: JSON序列化基础功能
test "JSON序列化基础功能" {
  // 创建JSON序列化器
  let json_serializer = JsonSerializer::new()
  
  // 创建测试数据
  let span_data = SpanData::new("test.operation", 1000, 1100, [
    ("service.name", "test.service"),
    ("operation.type", "http.request"),
    ("user.id", "12345")
  ])
  
  // 序列化
  let json_string = JsonSerializer::serialize_span(json_serializer, span_data)
  assert_true(json_string.length() > 0)
  assert_true(json_string.contains("test.operation"))
  assert_true(json_string.contains("test.service"))
  assert_true(json_string.contains("12345"))
  
  // 反序列化
  let deserialized_span = JsonSerializer::deserialize_span(json_serializer, json_string)
  assert_eq(SpanData::name(deserialized_span), SpanData::name(span_data))
  assert_eq(SpanData::start_time(deserialized_span), SpanData::start_time(span_data))
  assert_eq(SpanData::end_time(deserialized_span), SpanData::end_time(span_data))
  
  let deserialized_attributes = SpanData::attributes(deserialized_span)
  assert_eq(deserialized_attributes.get("service.name"), "test.service")
  assert_eq(deserialized_attributes.get("operation.type"), "http.request")
  assert_eq(deserialized_attributes.get("user.id"), "12345")
  
  // 测试度量数据序列化
  let metric_data = MetricData::counter("http.requests.total", 150.0, [
    ("method", "GET"),
    ("status", "200")
  ])
  
  let metric_json = JsonSerializer::serialize_metric(json_serializer, metric_data)
  assert_true(metric_json.contains("http.requests.total"))
  assert_true(metric_json.contains("150.0"))
  
  let deserialized_metric = JsonSerializer::deserialize_metric(json_serializer, metric_json)
  assert_eq(MetricData::name(deserialized_metric), MetricData::name(metric_data))
  assert_eq(MetricData::value(deserialized_metric), MetricData::value(metric_data))
  
  // 测试日志数据序列化
  let log_data = LogData::new(LogLevel::Info, "Test log message", 1005, [
    ("logger.name", "test.logger"),
    ("thread.id", "main")
  ])
  
  let log_json = JsonSerializer::serialize_log(json_serializer, log_data)
  assert_true(log_json.contains("Test log message"))
  assert_true(log_json.contains("INFO"))
  
  let deserialized_log = JsonSerializer::deserialize_log(json_serializer, log_json)
  assert_eq(LogData::message(deserialized_log), LogData::message(log_data))
  assert_eq(LogData::level(deserialized_log), LogData::level(log_data))
}

// 测试2: 二进制序列化功能
test "二进制序列化功能" {
  // 创建二进制序列化器
  let binary_serializer = BinarySerializer::new()
  
  // 创建测试数据
  let span_data = SpanData::new("binary.test.operation", 1000, 1100, [
    ("service.name", "binary.test.service"),
    ("operation.type", "binary.request"),
    ("binary.data", "x".repeat(1000)) // 大量二进制数据
  ])
  
  // 序列化
  let binary_data = BinarySerializer::serialize_span(binary_serializer, span_data)
  assert_true(binary_data.length() > 0)
  
  // 反序列化
  let deserialized_span = BinarySerializer::deserialize_span(binary_serializer, binary_data)
  assert_eq(SpanData::name(deserialized_span), SpanData::name(span_data))
  assert_eq(SpanData::start_time(deserialized_span), SpanData::start_time(span_data))
  assert_eq(SpanData::end_time(deserialized_span), SpanData::end_time(span_data))
  
  let deserialized_attributes = SpanData::attributes(deserialized_span)
  assert_eq(deserialized_attributes.get("service.name"), "binary.test.service")
  assert_eq(deserialized_attributes.get("operation.type"), "binary.request")
  assert_eq(deserialized_attributes.get("binary.data"), "x".repeat(1000))
  
  // 比较序列化大小
  let json_serializer = JsonSerializer::new()
  let json_data = JsonSerializer::serialize_span(json_serializer, span_data)
  
  // 二进制序列化应该更紧凑
  assert_true(binary_data.length() < json_data.length())
  
  // 测试批量序列化
  let spans = []
  for i = 0; i < 10; i = i + 1 {
    let span = SpanData::new("batch.operation." + i.to_string(), 1000 + i, 1100 + i, [
      ("batch.id", i.to_string())
    ])
    spans.push(span)
  }
  
  let batch_binary_data = BinarySerializer::serialize_span_batch(binary_serializer, spans)
  let deserialized_spans = BinarySerializer::deserialize_span_batch(binary_serializer, batch_binary_data)
  
  assert_eq(deserialized_spans.length(), spans.length())
  for i in 0..10 {
    assert_eq(SpanData::name(deserialized_spans[i]), SpanData::name(spans[i]))
  }
}

// 测试3: 压缩序列化功能
test "压缩序列化功能" {
  // 创建压缩序列化器
  let compressed_serializer = CompressedSerializer::new(CompressionType::Gzip)
  
  // 创建大型测试数据
  let large_attributes = []
  for i = 0; i < 100; i = i + 1 {
    large_attributes.push(("large.attr." + i.to_string(), "x".repeat(100)))
  }
  
  let span_data = SpanData::new("compression.test.operation", 1000, 1100, large_attributes)
  
  // 普通序列化
  let json_serializer = JsonSerializer::new()
  let uncompressed_data = JsonSerializer::serialize_span(json_serializer, span_data)
  
  // 压缩序列化
  let compressed_data = CompressedSerializer::serialize_span(compressed_serializer, span_data)
  
  // 验证压缩效果
  assert_true(compressed_data.length() < uncompressed_data.length())
  let compression_ratio = compressed_data.length().to_float() / uncompressed_data.length().to_float()
  assert_true(compression_ratio < 0.8) // 至少20%的压缩率
  
  // 解压缩和反序列化
  let deserialized_span = CompressedSerializer::deserialize_span(compressed_serializer, compressed_data)
  assert_eq(SpanData::name(deserialized_span), SpanData::name(span_data))
  
  let deserialized_attributes = SpanData::attributes(deserialized_span)
  for i in 0..100 {
    let key = "large.attr." + i.to_string()
    assert_eq(deserialized_attributes.get(key), "x".repeat(100))
  }
  
  // 测试不同压缩算法
  let lz4_serializer = CompressedSerializer::new(CompressionType::LZ4)
  let deflate_serializer = CompressedSerializer::new(CompressionType::Deflate)
  
  let lz4_data = CompressedSerializer::serialize_span(lz4_serializer, span_data)
  let deflate_data = CompressedSerializer::serialize_span(deflate_serializer, span_data)
  
  // 比较压缩效果
  assert_true(lz4_data.length() < uncompressed_data.length())
  assert_true(deflate_data.length() < uncompressed_data.length())
  
  // 测试压缩级别
  let fast_serializer = CompressedSerializer::with_level(CompressionType::Gzip, 1) // 最快压缩
  let best_serializer = CompressedSerializer::with_level(CompressionType::Gzip, 9) // 最佳压缩
  
  let fast_data = CompressedSerializer::serialize_span(fast_serializer, span_data)
  let best_data = CompressedSerializer::serialize_span(best_serializer, span_data)
  
  // 最佳压缩应该产生更小的数据
  assert_true(best_data.length() <= fast_data.length())
}

// 测试4: 加密序列化功能
test "加密序列化功能" {
  // 创建加密密钥
  let encryption_key = EncryptionKey::generate("AES-256-GCM")
  
  // 创建加密序列化器
  let encrypted_serializer = EncryptedSerializer::new(encryption_key)
  
  // 创建敏感测试数据
  let sensitive_span = SpanData::new("sensitive.operation", 1000, 1100, [
    ("user.id", "user12345"),
    ("credit.card", "4111-1111-1111-1111"),
    ("ssn", "123-45-6789"),
    ("api.key", "sk-1234567890abcdef")
  ])
  
  // 加密序列化
  let encrypted_data = EncryptedSerializer::serialize_span(encrypted_serializer, sensitive_span)
  assert_true(encrypted_data.length() > 0)
  
  // 验证数据已加密（不应包含明文敏感信息）
  let encrypted_string = String::from_utf8(encrypted_data)
  assert_false(encrypted_string.contains("user12345"))
  assert_false(encrypted_string.contains("4111-1111-1111-1111"))
  assert_false(encrypted_string.contains("123-45-6789"))
  assert_false(encrypted_string.contains("sk-1234567890abcdef"))
  
  // 解密和反序列化
  let deserialized_span = EncryptedSerializer::deserialize_span(encrypted_serializer, encrypted_data)
  assert_eq(SpanData::name(deserialized_span), SpanData::name(sensitive_span))
  
  let deserialized_attributes = SpanData::attributes(deserialized_span)
  assert_eq(deserialized_attributes.get("user.id"), "user12345")
  assert_eq(deserialized_attributes.get("credit.card"), "4111-1111-1111-1111")
  assert_eq(deserialized_attributes.get("ssn"), "123-45-6789")
  assert_eq(deserialized_attributes.get("api.key"), "sk-1234567890abcdef")
  
  // 测试不同密钥无法解密
  let wrong_key = EncryptionKey::generate("AES-256-GCM")
  let wrong_serializer = EncryptedSerializer::new(wrong_key)
  
  let decryption_result = EncryptedSerializer::try_deserialize_span(wrong_serializer, encrypted_data)
  assert_true(decryption_result.is_error())
  assert_true(decryption_result.error_message().contains("decryption failed"))
  
  // 测试选择性字段加密
  let selective_serializer = SelectiveEncryptionSerializer::new(encryption_key, [
    "credit.card",
    "ssn",
    "api.key"
  ])
  
  let selectively_encrypted_data = SelectiveEncryptionSerializer::serialize_span(
    selective_serializer, sensitive_span)
  
  // 解密并验证
  let selectively_decrypted_span = SelectiveEncryptionSerializer::deserialize_span(
    selective_serializer, selectively_encrypted_data)
  
  let decrypted_attributes = SpanData::attributes(selectively_decrypted_span)
  assert_eq(decrypted_attributes.get("user.id"), "user12345") // 未加密
  assert_eq(decrypted_attributes.get("credit.card"), "4111-1111-1111-1111") // 加密后解密
  assert_eq(decrypted_attributes.get("ssn"), "123-45-6789") // 加密后解密
  assert_eq(decrypted_attributes.get("api.key"), "sk-1234567890abcdef") // 加密后解密
}

// 测试5: 流式序列化功能
test "流式序列化功能" {
  // 创建流式序列化器
  let stream_serializer = StreamSerializer::new()
  
  // 创建大量数据
  let large_span_count = 1000
  let spans = []
  for i = 0; i < large_span_count; i = i + 1 {
    let span = SpanData::new("stream.operation." + i.to_string(), 1000 + i, 1100 + i, [
      ("stream.id", i.to_string()),
      ("batch.id", (i / 100).to_string())
    ])
    spans.push(span)
  }
  
  // 流式序列化
  let stream = StreamSerializer::create_span_stream(stream_serializer)
  for span in spans {
    StreamSerializer::write_span(stream, span)
  }
  StreamSerializer::finalize_stream(stream)
  
  // 获取流式数据
  let stream_data = StreamSerializer::get_stream_data(stream)
  assert_true(stream_data.length() > 0)
  
  // 流式反序列化
  let read_stream = StreamSerializer::create_span_read_stream(stream_serializer, stream_data)
  let deserialized_spans = []
  
  while StreamSerializer::has_more_spans(read_stream) {
    let span = StreamSerializer::read_span(read_stream)
    deserialized_spans.push(span)
  }
  
  // 验证数据完整性
  assert_eq(deserialized_spans.length(), spans.length())
  for i in 0..large_span_count {
    assert_eq(SpanData::name(deserialized_spans[i]), SpanData::name(spans[i]))
    assert_eq(SpanData::start_time(deserialized_spans[i]), SpanData::start_time(spans[i]))
    assert_eq(SpanData::end_time(deserialized_spans[i]), SpanData::end_time(spans[i]))
  }
  
  // 测试增量流式处理
  let incremental_stream = StreamSerializer::create_incremental_stream(stream_serializer)
  
  // 分批写入数据
  for batch in 0..10 {
    for i in 0..100 {
      let span_index = batch * 100 + i
      let span = SpanData::new("incremental.operation." + span_index.to_string(), 
        1000 + span_index, 1100 + span_index, [
        ("batch.id", batch.to_string()),
        ("item.id", i.to_string())
      ])
      StreamSerializer::write_span(incremental_stream, span)
    }
    
    // 每批处理后获取部分数据
    let partial_data = StreamSerializer::get_partial_stream_data(incremental_stream)
    assert_true(partial_data.length() > 0)
  }
  
  StreamSerializer::finalize_stream(incremental_stream)
  
  // 测试流式压缩
  let compressed_stream = StreamSerializer::create_compressed_stream(stream_serializer, CompressionType::Gzip)
  
  for span in spans.take(100) { // 只处理前100个span以节省时间
    StreamSerializer::write_span(compressed_stream, span)
  }
  StreamSerializer::finalize_stream(compressed_stream)
  
  let compressed_stream_data = StreamSerializer::get_stream_data(compressed_stream)
  let uncompressed_stream_data = StreamSerializer::get_stream_data(stream)
  
  // 压缩流应该更小
  assert_true(compressed_stream_data.length() < uncompressed_stream_data.length())
}

// 测试6: 跨格式序列化转换
test "跨格式序列化转换功能" {
  // 创建不同格式的序列化器
  let json_serializer = JsonSerializer::new()
  let binary_serializer = BinarySerializer::new()
  let compressed_serializer = CompressedSerializer::new(CompressionType::Gzip)
  let encrypted_serializer = EncryptedSerializer::new(EncryptionKey::generate("AES-256-GCM"))
  
  // 创建测试数据
  let span_data = SpanData::new("conversion.test.operation", 1000, 1100, [
    ("service.name", "conversion.test.service"),
    ("operation.type", "conversion.request"),
    ("conversion.data", "x".repeat(500))
  ])
  
  // JSON -> 二进制
  let json_data = JsonSerializer::serialize_span(json_serializer, span_data)
  let binary_data = SerializationConverter::json_to_binary(json_data)
  let converted_span = BinarySerializer::deserialize_span(binary_serializer, binary_data)
  
  assert_eq(SpanData::name(converted_span), SpanData::name(span_data))
  assert_eq(SpanData::attributes(converted_span).get("service.name"), "conversion.test.service")
  
  // 二进制 -> JSON
  let original_binary = BinarySerializer::serialize_span(binary_serializer, span_data)
  let converted_json = SerializationConverter::binary_to_json(original_binary)
  let reconverted_span = JsonSerializer::deserialize_span(json_serializer, converted_json)
  
  assert_eq(SpanData::name(reconverted_span), SpanData::name(span_data))
  
  // JSON -> 压缩
  let compressed_json = SerializationConverter::compress_json(json_data, CompressionType::Gzip)
  let decompressed_json = SerializationConverter::decompress_to_json(compressed_json, CompressionType::Gzip)
  let decompressed_span = JsonSerializer::deserialize_span(json_serializer, decompressed_json)
  
  assert_eq(SpanData::name(decompressed_span), SpanData::name(span_data))
  
  // 二进制 -> 压缩
  let compressed_binary = SerializationConverter::compress_binary(original_binary, CompressionType::LZ4)
  let decompressed_binary = SerializationConverter::decompress_to_binary(compressed_binary, CompressionType::LZ4)
  let decompressed_binary_span = BinarySerializer::deserialize_span(binary_serializer, decompressed_binary)
  
  assert_eq(SpanData::name(decompressed_binary_span), SpanData::name(span_data))
  
  // 测试批量转换
  let spans = []
  for i = 0; i < 10; i = i + 1 {
    let span = SpanData::new("batch.conversion." + i.to_string(), 1000 + i, 1100 + i, [
      ("batch.id", i.to_string())
    ])
    spans.push(span)
  }
  
  let json_batch = JsonSerializer::serialize_span_batch(json_serializer, spans)
  let binary_batch = SerializationConverter::json_batch_to_binary(json_batch)
  let converted_spans = BinarySerializer::deserialize_span_batch(binary_serializer, binary_batch)
  
  assert_eq(converted_spans.length(), spans.length())
  for i in 0..10 {
    assert_eq(SpanData::name(converted_spans[i]), SpanData::name(spans[i]))
  }
  
  // 测试格式检测
  let json_sample = JsonSerializer::serialize_span(json_serializer, span_data)
  let binary_sample = BinarySerializer::serialize_span(binary_serializer, span_data)
  let compressed_sample = CompressedSerializer::serialize_span(compressed_serializer, span_data)
  
  assert_eq(SerializationFormatDetector::detect_format(json_sample), SerializationFormat::Json)
  assert_eq(SerializationFormatDetector::detect_format(binary_sample), SerializationFormat::Binary)
  assert_eq(SerializationFormatDetector::detect_format(compressed_sample), SerializationFormat::Compressed)
  
  // 测试自动反序列化
  let auto_span1 = SerializationConverter::auto_deserialize_span(json_sample)
  let auto_span2 = SerializationConverter::auto_deserialize_span(binary_sample)
  let auto_span3 = SerializationConverter::auto_deserialize_span(compressed_sample)
  
  assert_eq(SpanData::name(auto_span1), SpanData::name(span_data))
  assert_eq(SpanData::name(auto_span2), SpanData::name(span_data))
  assert_eq(SpanData::name(auto_span3), SpanData::name(span_data))
}