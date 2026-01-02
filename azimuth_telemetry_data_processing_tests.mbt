// Azimuth 遥测数据处理测试用例
// 专注于遥测数据的收集、转换和聚合处理

// 测试1: 遥测数据批处理
test "遥测数据批处理测试" {
  let processor = TelemetryProcessor::new()
  let batch_size = 100
  
  // 创建测试数据
  for i = 0; i < batch_size; i = i + 1 {
    let metric = TelemetryMetric::new_counter(
      "batch.processing.items",
      1.0,
      Attributes::from([("batch_id", IntValue(i / 10))])
    )
    TelemetryProcessor::add_metric(processor, metric)
  }
  
  // 处理批次
  let result = TelemetryProcessor::process_batch(processor)
  assert_true(TelemetryResult::is_success(result))
  
  // 验证聚合结果
  let aggregated = TelemetryResult::get_aggregated_metrics(result)
  assert_eq(aggregated.length(), 10) // 10个批次
  assert_eq(TelemetryAggregated::total_count(aggregated[0]), 10)
}

// 测试2: 遥测数据实时流处理
test "遥测数据实时流处理测试" {
  let stream_processor = StreamProcessor::new()
  let stream_buffer_size = 1000
  
  // 配置流处理器
  StreamProcessor::configure(stream_processor, {
    "buffer_size" => IntValue(stream_buffer_size),
    "flush_interval_ms" => IntValue(100),
    "compression_enabled" => BoolValue(true)
  })
  
  // 模拟实时数据流
  for i = 0; i < 500; i = i + 1 {
    let event = TelemetryEvent::new(
      "realtime.metric",
      Time::now(),
      Attributes::from([("event_id", IntValue(i))])
    )
    StreamProcessor::push_event(stream_processor, event)
  }
  
  // 触发流处理
  let stream_result = StreamProcessor::flush(stream_processor)
  assert_true(StreamResult::is_success(stream_result))
  
  // 验证流处理结果
  let processed_events = StreamResult::get_processed_count(stream_result)
  assert_eq(processed_events, 500)
}

// 测试3: 遥测数据压缩和存储优化
test "遥测数据压缩和存储优化测试" {
  let compressor = TelemetryCompressor::new()
  let raw_data_size = 10000
  
  // 生成大量遥测数据
  let raw_metrics = Array::empty()
  for i = 0; i < raw_data_size; i = i + 1 {
    let metric = TelemetryMetric::new_histogram(
      "performance.latency",
      FloatValue(i * 0.001),
      Attributes::from([
        ("service", StringValue("api-gateway")),
        ("endpoint", StringValue("/api/v1/resource"))
      ])
    )
    Array::push(raw_metrics, metric)
  }
  
  // 压缩数据
  let compressed_data = TelemetryCompressor::compress(compressor, raw_metrics)
  assert_true(CompressedData::is_valid(compressed_data))
  
  // 验证压缩率
  let compression_ratio = CompressedData::compression_ratio(compressed_data)
  assert_true(compression_ratio > 0.5) // 至少50%压缩率
  
  // 解压缩并验证数据完整性
  let decompressed_metrics = TelemetryCompressor::decompress(compressor, compressed_data)
  assert_eq(decompressed_metrics.length(), raw_data_size)
}

// 测试4: 遥测数据质量验证
test "遥测数据质量验证测试" {
  let validator = DataQualityValidator::new()
  
  // 配置验证规则
  DataQualityValidator::add_rule(validator, "metric_name", ValidationRule::required())
  DataQualityValidator::add_rule(validator, "timestamp", ValidationRule::timestamp_range())
  DataQualityValidator::add_rule(validator, "value", ValidationRule::numeric_range(0.0, 1000000.0))
  
  // 创建测试数据集
  let test_metrics = [
    TelemetryMetric::new_counter("valid.metric", 100.0, Attributes::empty()),
    TelemetryMetric::new_counter("", 50.0, Attributes::empty()), // 无效：缺少名称
    TelemetryMetric::new_gauge("invalid.metric", -10.0, Attributes::empty()), // 无效：负值
    TelemetryMetric::new_histogram("valid.metric", 0.5, Attributes::empty())
  ]
  
  // 执行数据质量验证
  let validation_result = DataQualityValidator::validate(validator, test_metrics)
  
  // 验证结果
  assert_eq(ValidationResult::total_count(validation_result), 4)
  assert_eq(ValidationResult::valid_count(validation_result), 2)
  assert_eq(ValidationResult::invalid_count(validation_result), 2)
  
  // 获取详细的验证错误
  let errors = ValidationResult::get_errors(validation_result)
  assert_eq(errors.length(), 2)
}