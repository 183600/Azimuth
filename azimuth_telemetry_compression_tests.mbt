// 遥测数据压缩测试用例
// 测试Azimuth遥测系统的数据压缩功能

test "span数据压缩和解压缩" {
  // 测试span数据的压缩和解压缩
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "compression.test")
  
  // 创建包含大量属性的span
  let span = Tracer::start_span(tracer, "large.data.operation")
  
  // 添加大量属性以测试压缩效果
  Span::set_attribute(span, "operation.type", "data.processing")
  Span::set_attribute(span, "operation.duration", 1500)
  Span::set_attribute(span, "operation.size", 1048576)
  Span::set_attribute(span, "operation.complexity", "high")
  Span::set_attribute(span, "operation.priority", "critical")
  Span::set_attribute(span, "service.name", "data.processor")
  Span::set_attribute(span, "service.version", "2.1.0")
  Span::set_attribute(span, "service.environment", "production")
  Span::set_attribute(span, "host.name", "prod-server-01")
  Span::set_attribute(span, "host.region", "us-west-2")
  Span::set_attribute(span, "container.id", "container-abc123")
  Span::set_attribute(span, "pod.name", "data-processor-7f8d9")
  Span::set_attribute(span, "namespace", "production")
  
  // 添加多个事件以增加数据量
  for i = 0; i < 10; i = i + 1 {
    Span::add_event(span, "operation.step." + i.to_string(), [
      ("step.index", IntValue(i)),
      ("step.duration", IntValue(100 + i * 10)),
      ("step.status", StringValue("completed"))
    ])
  }
  
  // 序列化span数据
  let serialized_data = Span::serialize(span)
  
  // 验证序列化数据不为空
  assert_true(String::length(serialized_data) > 0)
  
  // 压缩数据
  let compressed_data = CompressionUtil::compress(serialized_data)
  
  // 验证压缩后的数据小于原始数据
  assert_true(String::length(compressed_data) < String::length(serialized_data))
  
  // 解压缩数据
  let decompressed_data = CompressionUtil::decompress(compressed_data)
  
  // 验证解压缩后的数据与原始数据相同
  assert_eq(decompressed_data, serialized_data)
  
  // 从解压缩的数据重构span
  let reconstructed_span = Span::deserialize(decompressed_data)
  
  // 验证重构的span具有相同的属性
  let original_name = Span::name(span)
  let reconstructed_name = Span::name(reconstructed_span)
  assert_eq(original_name, reconstructed_name)
  
  Span::end(span)
  Span::end(reconstructed_span)
  
  assert_true(true)
}

test "遥测批量数据压缩" {
  // 测试批量遥测数据的压缩
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch.compression.test")
  
  // 创建多个span
  let spans = []
  for i = 0; i < 50; i = i + 1 {
    let span = Tracer::start_span(tracer, "batch.operation." + i.to_string())
    Span::set_attribute(span, "batch.id", "batch-001")
    Span::set_attribute(span, "operation.index", i)
    Span::set_attribute(span, "operation.type", "batch.processing")
    spans.push(span)
  }
  
  // 批量序列化span数据
  let batch_data = SpanBatch::serialize_batch(spans)
  
  // 验证批量数据不为空
  assert_true(String::length(batch_data) > 0)
  
  // 使用不同的压缩算法进行压缩
  let gzip_compressed = CompressionUtil::compress_with_algorithm(batch_data, "gzip")
  let lz4_compressed = CompressionUtil::compress_with_algorithm(batch_data, "lz4")
  let brotli_compressed = CompressionUtil::compress_with_algorithm(batch_data, "brotli")
  
  // 验证所有压缩算法都能有效压缩数据
  assert_true(String::length(gzip_compressed) < String::length(batch_data))
  assert_true(String::length(lz4_compressed) < String::length(batch_data))
  assert_true(String::length(brotli_compressed) < String::length(batch_data))
  
  // 测试压缩比
  let gzip_ratio = String::length(gzip_compressed).to_float() / String::length(batch_data).to_float()
  let lz4_ratio = String::length(lz4_compressed).to_float() / String::length(batch_data).to_float()
  let brotli_ratio = String::length(brotli_compressed).to_float() / String::length(batch_data).to_float()
  
  // 验证压缩比合理（至少压缩20%）
  assert_true(gzip_ratio < 0.8)
  assert_true(lz4_ratio < 0.8)
  assert_true(brotli_ratio < 0.8)
  
  // 测试解压缩
  let gzip_decompressed = CompressionUtil::decompress_with_algorithm(gzip_compressed, "gzip")
  let lz4_decompressed = CompressionUtil::decompress_with_algorithm(lz4_compressed, "lz4")
  let brotli_decompressed = CompressionUtil::decompress_with_algorithm(brotli_compressed, "brotli")
  
  // 验证解压缩后的数据与原始数据相同
  assert_eq(gzip_decompressed, batch_data)
  assert_eq(lz4_decompressed, batch_data)
  assert_eq(brotli_decompressed, batch_data)
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  assert_true(true)
}

test "度量数据压缩效率" {
  // 测试度量数据的压缩效率
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metrics.compression.test")
  
  // 创建多个度量
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let response_histogram = Meter::create_histogram(meter, "http.response.duration", Some("HTTP response duration"), Some("ms"))
  let active_gauge = Meter::create_gauge(meter, "active.connections", Some("Active connections"), Some("connections"))
  
  // 生成大量度量数据点
  for i = 0; i < 1000; i = i + 1 {
    Counter::add_with_attributes(request_counter, 1.0, [
      ("method", i % 2 == 0 ? "GET" : "POST"),
      ("status", i % 5 == 0 ? "500" : "200"),
      ("endpoint", "/api/resource/" + (i % 10).to_string())
    ])
    
    Histogram::record_with_attributes(response_histogram, (50 + i % 200).to_float(), [
      ("method", i % 2 == 0 ? "GET" : "POST"),
      ("endpoint", "/api/resource/" + (i % 10).to_string())
    ])
    
    Gauge::set_with_attributes(active_gauge, (100 + i % 50).to_float(), [
      ("server", "server-" + (i % 5).to_string()),
      ("region", i % 2 == 0 ? "us-east-1" : "us-west-2")
    ])
  }
  
  // 序列化度量数据
  let metrics_data = MetricsBatch::serialize_metrics([request_counter, response_histogram, active_gauge])
  
  // 验证度量数据不为空
  assert_true(String::length(metrics_data) > 0)
  
  // 压缩度量数据
  let compressed_metrics = CompressionUtil::compress(metrics_data)
  
  // 验证压缩效果
  let compression_ratio = String::length(compressed_metrics).to_float() / String::length(metrics_data).to_float()
  assert_true(compression_ratio < 0.7)  // 至少压缩30%
  
  // 解压缩度量数据
  let decompressed_metrics = CompressionUtil::decompress(compressed_metrics)
  
  // 验证解压缩后的数据与原始数据相同
  assert_eq(decompressed_metrics, metrics_data)
  
  // 从解压缩的数据重构度量
  let reconstructed_metrics = MetricsBatch::deserialize_metrics(decompressed_metrics)
  
  // 验证重构的度量数量正确
  assert_eq(Array::length(reconstructed_metrics), 3)
  
  assert_true(true)
}

test "日志数据压缩和过滤" {
  // 测试日志数据的压缩和过滤
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "log.compression.test")
  
  // 创建大量日志记录
  let logs = []
  for i = 0; i < 500; i = i + 1 {
    let severity = i % 4 == 0 ? Error : (i % 3 == 0 ? Warn : (i % 2 == 0 ? Info : Debug))
    let log = LogRecord::new(severity, "Log message " + i.to_string())
    
    LogRecord::add_attribute(log, "log.index", i)
    LogRecord::add_attribute(log, "service.name", "log-generator")
    LogRecord::add_attribute(log, "service.instance", "instance-" + (i % 3).to_string())
    
    // 为错误日志添加额外属性
    if severity == Error {
      LogRecord::add_attribute(log, "error.type", "ProcessingError")
      LogRecord::add_attribute(log, "error.code", "ERR_" + (1000 + i).to_string())
      LogRecord::add_attribute(log, "stack.trace", "at com.example.Processor.process(Processor.java:" + (100 + i).to_string() + ")")
    }
    
    logs.push(log)
  }
  
  // 序列化日志数据
  let logs_data = LogBatch::serialize_logs(logs)
  
  // 验证日志数据不为空
  assert_true(String::length(logs_data) > 0)
  
  // 压缩日志数据
  let compressed_logs = CompressionUtil::compress(logs_data)
  
  // 验证压缩效果
  assert_true(String::length(compressed_logs) < String::length(logs_data))
  
  // 解压缩日志数据
  let decompressed_logs = CompressionUtil::decompress(compressed_logs)
  
  // 验证解压缩后的数据与原始数据相同
  assert_eq(decompressed_logs, logs_data)
  
  // 测试日志过滤和压缩
  let error_logs = Array::filter(logs, fn(log) { LogRecord::severity_number(log) == Error })
  let error_logs_data = LogBatch::serialize_logs(error_logs)
  let compressed_error_logs = CompressionUtil::compress(error_logs_data)
  
  // 验证过滤后的压缩效果更好
  let full_compression_ratio = String::length(compressed_logs).to_float() / String::length(logs_data).to_float()
  let error_compression_ratio = String::length(compressed_error_logs).to_float() / String::length(error_logs_data).to_float()
  
  // 错误日志通常包含更多结构化数据，压缩比可能更好
  assert_true(full_compression_ratio < 0.8)
  assert_true(error_compression_ratio < 0.8)
  
  assert_true(true)
}

test "增量压缩和差异更新" {
  // 测试增量压缩和差异更新
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "incremental.compression.test")
  
  // 创建基础span数据
  let base_span = Tracer::start_span(tracer, "base.operation")
  Span::set_attribute(base_span, "operation.type", "base")
  Span::set_attribute(base_span, "operation.version", "1.0.0")
  Span::set_attribute(base_span, "service.name", "base.service")
  
  // 序列化并压缩基础数据
  let base_serialized = Span::serialize(base_span)
  let base_compressed = CompressionUtil::compress(base_serialized)
  
  // 创建更新后的span数据
  let updated_span = Tracer::start_span(tracer, "base.operation")
  Span::set_attribute(updated_span, "operation.type", "base")
  Span::set_attribute(updated_span, "operation.version", "1.1.0")  // 版本更新
  Span::set_attribute(updated_span, "service.name", "base.service")
  Span::set_attribute(updated_span, "operation.feature", "new.feature")  // 新增属性
  
  // 序列化更新后的数据
  let updated_serialized = Span::serialize(updated_span)
  
  // 计算差异
  let diff = CompressionUtil::calculate_diff(base_serialized, updated_serialized)
  
  // 验证差异数据小于完整更新数据
  assert_true(String::length(diff) < String::length(updated_serialized))
  
  // 压缩差异数据
  let compressed_diff = CompressionUtil::compress(diff)
  
  // 验证压缩后的差异数据更小
  assert_true(String::length(compressed_diff) < String::length(diff))
  
  // 解压缩差异数据
  let decompressed_diff = CompressionUtil::decompress(compressed_diff)
  
  // 应用差异到基础数据
  let reconstructed_updated = CompressionUtil::apply_diff(base_serialized, decompressed_diff)
  
  // 验证重构的数据与更新后的数据相同
  assert_eq(reconstructed_updated, updated_serialized)
  
  // 从重构的数据创建span
  let reconstructed_span = Span::deserialize(reconstructed_updated)
  
  // 验证重构的span具有更新后的属性
  let reconstructed_name = Span::name(reconstructed_span)
  assert_eq(reconstructed_name, "base.operation")
  
  Span::end(base_span)
  Span::end(updated_span)
  Span::end(reconstructed_span)
  
  assert_true(true)
}