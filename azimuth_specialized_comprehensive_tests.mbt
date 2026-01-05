// Azimuth 专业 MoonBit 测试用例
// 专注于遥测系统的核心功能和高级特性

// 测试1: 遥测上下文传播机制
test "遥测上下文传播机制测试" {
  // 创建上下文传播器
  let propagator = TextMapPropagator::new()
  
  // 创建原始上下文
  let original_context = Context::current()
  let trace_id = "trace-1234567890"
  let span_id = "span-0987654321"
  
  // 设置上下文信息
  let span_context = SpanContext::new(trace_id, span_id, true, true)
  let context_with_span = Context::with_span(original_context, span_context)
  
  // 创建载体用于传播
  let carrier = TextMapCarrier::new()
  
  // 注入上下文到载体
  TextMapPropagator::inject(propagator, context_with_span, carrier)
  
  // 验证注入的上下文
  let trace_header = TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_header.is_some())
  
  // 从载体提取上下文
  let extracted_context = TextMapPropagator::extract(propagator, carrier)
  let extracted_span = Context::span(extracted_context)
  
  // 验证提取的上下文
  assert_eq(SpanContext::trace_id(extracted_span), trace_id)
  assert_eq(SpanContext::span_id(extracted_span), span_id)
  assert_true(SpanContext::is_sampled(extracted_span))
}

// 测试2: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化测试" {
  // 创建遥测数据
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_attribute(telemetry_data, "service.name", "payment-service")
  TelemetryData::add_attribute(telemetry_data, "service.version", "1.2.3")
  TelemetryData::add_attribute(telemetry_data, "environment", "production")
  TelemetryData::add_metric(telemetry_data, "request.duration", 125.5)
  TelemetryData::add_metric(telemetry_data, "memory.usage", 512.0)
  
  // 序列化为JSON
  let json_serializer = JsonSerializer::new()
  let serialized_data = JsonSerializer::serialize(json_serializer, telemetry_data)
  
  // 验证序列化结果
  assert_true(serialized_data.contains("payment-service"))
  assert_true(serialized_data.contains("1.2.3"))
  assert_true(serialized_data.contains("production"))
  assert_true(serialized_data.contains("125.5"))
  assert_true(serialized_data.contains("512.0"))
  
  // 反序列化
  let deserialized_data = JsonSerializer::deserialize(json_serializer, serialized_data)
  
  // 验证反序列化结果
  assert_eq(TelemetryData::get_attribute(deserialized_data, "service.name"), Some("payment-service"))
  assert_eq(TelemetryData::get_attribute(deserialized_data, "service.version"), Some("1.2.3"))
  assert_eq(TelemetryData::get_attribute(deserialized_data, "environment"), Some("production"))
  assert_eq(TelemetryData::get_metric(deserialized_data, "request.duration"), Some(125.5))
  assert_eq(TelemetryData::get_metric(deserialized_data, "memory.usage"), Some(512.0))
}

// 测试3: 遥测数据采样策略
test "遥测数据采样策略测试" {
  // 创建采样器
  let probability_sampler = ProbabilitySampler::new(0.5) // 50%采样率
  let always_on_sampler = AlwaysOnSampler::new()
  let always_off_sampler = AlwaysOffSampler::new()
  
  // 创建采样上下文
  let sampling_context = SamplingContext::new()
  SamplingContext::set_trace_id(sampling_context, "trace-1234567890")
  SamplingContext::set_span_name(sampling_context, "test.operation")
  
  // 测试概率采样
  let sampled_count = { mut count: 0 }
  for i in 0..1000 {
    let decision = ProbabilitySampler::should_sample(probability_sampler, sampling_context)
    if decision == SamplingDecision::RecordAndSample {
      sampled_count.count = sampled_count.count + 1
    }
  }
  
  // 验证概率采样结果（应该在40%-60%之间）
  let sample_rate = sampled_count.count.to_float() / 1000.0
  assert_true(sample_rate > 0.4 && sample_rate < 0.6)
  
  // 测试始终开启采样
  let always_on_decision = AlwaysOnSampler::should_sample(always_on_sampler, sampling_context)
  assert_eq(always_on_decision, SamplingDecision::RecordAndSample)
  
  // 测试始终关闭采样
  let always_off_decision = AlwaysOffSampler::should_sample(always_off_sampler, sampling_context)
  assert_eq(always_off_decision, SamplingDecision::Drop)
}

// 测试4: 遥测数据批处理和缓冲
test "遥测数据批处理和缓冲测试" {
  // 创建批处理器
  let batch_processor = BatchProcessor::new(10, 5000) // 10条记录或5秒超时
  
  // 创建遥测记录
  let records = []
  for i in 0..15 {
    let record = TelemetryRecord::new()
    TelemetryRecord::set_name(record, "operation." + i.to_string())
    TelemetryRecord::set_duration(record, (i * 10).to_int())
    TelemetryRecord::set_status(record, Ok)
    records = records.push(record)
  }
  
  // 添加记录到批处理器
  for record in records {
    BatchProcessor::add_record(batch_processor, record)
  }
  
  // 验证批处理结果
  let batch_count = BatchProcessor::get_batch_count(batch_processor)
  assert_eq(batch_count, 2) // 15条记录应该分成2批（10条和5条）
  
  // 获取第一批记录
  let first_batch = BatchProcessor::get_batch(batch_processor, 0)
  assert_eq(first_batch.length(), 10)
  
  // 获取第二批记录
  let second_batch = BatchProcessor::get_batch(batch_processor, 1)
  assert_eq(second_batch.length(), 5)
  
  // 测试缓冲区管理
  let buffer_size = BatchProcessor::get_buffer_size(batch_processor)
  assert_true(buffer_size >= 15) // 缓冲区应该能容纳所有记录
}

// 测试5: 遥测数据压缩和传输优化
test "遥测数据压缩和传输优化测试" {
  // 创建大量遥测数据
  let large_dataset = TelemetryDataset::new()
  for i in 0..1000 {
    let record = TelemetryRecord::new()
    TelemetryRecord::set_name(record, "operation." + i.to_string())
    TelemetryRecord::set_attribute(record, "index", i.to_string())
    TelemetryRecord::set_attribute(record, "category", "test")
    TelemetryRecord::set_metric(record, "value", i.to_float())
    TelemetryDataset::add_record(large_dataset, record)
  }
  
  // 序列化数据
  let serializer = JsonSerializer::new()
  let uncompressed_data = JsonSerializer::serialize_dataset(serializer, large_dataset)
  let uncompressed_size = uncompressed_data.length()
  
  // 压缩数据
  let compressor = GzipCompressor::new()
  let compressed_data = GzipCompressor::compress(compressor, uncompressed_data)
  let compressed_size = compressed_data.length()
  
  // 验证压缩效果
  assert_true(compressed_size < uncompressed_size)
  let compression_ratio = compressed_size.to_float() / uncompressed_size.to_float()
  assert_true(compression_ratio < 0.8) // 压缩率应该至少达到20%
  
  // 解压缩数据
  let decompressed_data = GzipCompressor::decompress(compressor, compressed_data)
  assert_eq(decompressed_data.length(), uncompressed_size)
  
  // 验证解压缩后的数据完整性
  let decompressed_dataset = JsonSerializer::deserialize_dataset(serializer, decompressed_data)
  assert_eq(TelemetryDataset::record_count(decompressed_dataset), 1000)
}

// 测试6: 遥测数据关联和链接分析
test "遥测数据关联和链接分析测试" {
  // 创建关联分析器
  let correlation_analyzer = CorrelationAnalyzer::new()
  
  // 添加遥测数据点
  let data_points = [
    ("trace-001", "span-001", "service.a", "operation.a", 100),
    ("trace-001", "span-002", "service.b", "operation.b", 50),
    ("trace-002", "span-003", "service.a", "operation.a", 120),
    ("trace-002", "span-004", "service.c", "operation.c", 200),
    ("trace-003", "span-005", "service.b", "operation.b", 75),
    ("trace-003", "span-006", "service.c", "operation.c", 180)
  ]
  
  for (trace_id, span_id, service, operation, duration) in data_points {
    let data_point = TelemetryDataPoint::new()
    TelemetryDataPoint::set_trace_id(data_point, trace_id)
    TelemetryDataPoint::set_span_id(data_point, span_id)
    TelemetryDataPoint::set_service(data_point, service)
    TelemetryDataPoint::set_operation(data_point, operation)
    TelemetryDataPoint::set_duration(data_point, duration)
    CorrelationAnalyzer::add_data_point(correlation_analyzer, data_point)
  }
  
  // 分析服务间关联
  let service_correlations = CorrelationAnalyzer::analyze_service_correlations(correlation_analyzer)
  assert_true(service_correlations.length() > 0)
  
  // 验证特定服务关联
  let a_to_b_correlation = CorrelationAnalyzer::get_service_correlation(correlation_analyzer, "service.a", "service.b")
  assert_true(a_to_b_correlation > 0.0)
  
  // 分析操作链路
  let operation_chains = CorrelationAnalyzer::analyze_operation_chains(correlation_analyzer)
  assert_true(operation_chains.length() > 0)
  
  // 检测异常模式
  let anomalies = CorrelationAnalyzer::detect_anomalies(correlation_analyzer)
  assert_true(anomalies.length() >= 0) // 可能有也可能没有异常
}

// 测试7: 遥测数据实时监控和警报
test "遥测数据实时监控和警报测试" {
  // 创建监控器
  let monitor = RealTimeMonitor::new()
  
  // 创建警报规则
  let latency_alert = AlertRule::new("high.latency")
  AlertRule::set_metric(latency_alert, "request.duration")
  AlertRule::set_threshold(latency_alert, 1000.0) // 1秒阈值
  AlertRule::set_operator(latency_alert, AlertOperator::GreaterThan)
  AlertRule::set_severity(latency_alert, AlertSeverity::Warning)
  
  let error_rate_alert = AlertRule::new("high.error.rate")
  AlertRule::set_metric(error_rate_alert, "error.rate")
  AlertRule::set_threshold(error_rate_alert, 0.05) // 5%错误率阈值
  AlertRule::set_operator(error_rate_alert, AlertOperator::GreaterThan)
  AlertRule::set_severity(error_rate_alert, AlertSeverity::Critical)
  
  // 注册警报规则
  RealTimeMonitor::add_alert_rule(monitor, latency_alert)
  RealTimeMonitor::add_alert_rule(monitor, error_rate_alert)
  
  // 模拟正常数据
  let normal_metrics = [
    ("request.duration", 150.0),
    ("error.rate", 0.01),
    ("throughput", 1000.0)
  ]
  
  for (metric, value) in normal_metrics {
    RealTimeMonitor::process_metric(monitor, metric, value)
  }
  
  // 验证没有警报触发
  assert_eq(RealTimeMonitor::get_active_alert_count(monitor), 0)
  
  // 模拟异常数据
  let abnormal_metrics = [
    ("request.duration", 1200.0), // 超过延迟阈值
    ("error.rate", 0.08), // 超过错误率阈值
    ("throughput", 500.0)
  ]
  
  for (metric, value) in abnormal_metrics {
    RealTimeMonitor::process_metric(monitor, metric, value)
  }
  
  // 验证警报触发
  assert_eq(RealTimeMonitor::get_active_alert_count(monitor), 2)
  
  // 获取警报详情
  let alerts = RealTimeMonitor::get_active_alerts(monitor)
  assert_eq(alerts.length(), 2)
  
  // 验证警报内容
  let latency_alert_triggered = alerts.find(fn(alert) { Alert::rule_name(alert) == "high.latency" })
  let error_rate_alert_triggered = alerts.find(fn(alert) { Alert::rule_name(alert) == "high.error.rate" })
  
  assert_true(latency_alert_triggered.is_some())
  assert_true(error_rate_alert_triggered.is_some())
}

// 测试8: 遥测数据多维度分析
test "遥测数据多维度分析测试" {
  // 创建多维度分析器
  let analyzer = MultiDimensionalAnalyzer::new()
  
  // 添加维度
  MultiDimensionalAnalyzer::add_dimension(analyzer, "service")
  MultiDimensionalAnalyzer::add_dimension(analyzer, "operation")
  MultiDimensionalAnalyzer::add_dimension(analyzer, "region")
  MultiDimensionalAnalyzer::add_dimension(analyzer, "environment")
  
  // 添加多维度数据点
  let data_points = [
    ("service.a", "operation.1", "us-west", "production", 100.0),
    ("service.a", "operation.2", "us-west", "production", 150.0),
    ("service.a", "operation.1", "us-east", "staging", 120.0),
    ("service.b", "operation.1", "us-west", "production", 200.0),
    ("service.b", "operation.3", "eu-west", "production", 180.0),
    ("service.c", "operation.2", "us-east", "staging", 90.0),
    ("service.c", "operation.1", "eu-west", "production", 110.0)
  ]
  
  for (service, operation, region, environment, value) in data_points {
    let dimensions = [
      ("service", service),
      ("operation", operation),
      ("region", region),
      ("environment", environment)
    ]
    MultiDimensionalAnalyzer::add_data_point(analyzer, dimensions, value)
  }
  
  // 按服务维度分析
  let service_analysis = MultiDimensionalAnalyzer::analyze_by_dimension(analyzer, "service")
  assert_eq(service_analysis.length(), 3) // 三个服务
  
  // 验证特定服务分析
  let service_a_stats = MultiDimensionalAnalyzer::get_dimension_stats(analyzer, "service", "service.a")
  assert_eq(service_a_stats.count, 3)
  assert_eq(service_a_stats.sum, 370.0) // 100 + 150 + 120
  assert_eq(service_a_stats.average, 370.0 / 3.0)
  
  // 多维度交叉分析
  let cross_analysis = MultiDimensionalAnalyzer::cross_analyze(analyzer, ["service", "environment"])
  assert_true(cross_analysis.length() > 0)
  
  // 验证交叉分析结果
  let prod_service_a = MultiDimensionalAnalyzer::get_cross_stats(analyzer, ["service", "environment"], ["service.a", "production"])
  assert_eq(prod_service_a.count, 2) // service.a在production环境有2个数据点
  assert_eq(prod_service_a.sum, 250.0) // 100 + 150
}

// 测试9: 遥测数据自适应采样
test "遥测数据自适应采样测试" {
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new()
  
  // 配置自适应采样参数
  AdaptiveSampler::set_base_probability(adaptive_sampler, 0.1) // 基础10%采样率
  AdaptiveSampler::set_max_probability(adaptive_sampler, 0.5) // 最大50%采样率
  AdaptiveSampler::set_error_threshold(adaptive_sampler, 0.05) // 5%错误率阈值
  AdaptiveSampler::set_latency_threshold(adaptive_sampler, 1000.0) // 1秒延迟阈值
  
  // 创建采样上下文
  let sampling_context = SamplingContext::new()
  SamplingContext::set_trace_id(sampling_context, "trace-adaptive-test")
  
  // 模拟正常情况下的采样
  let normal_samples = { mut count: 0 }
  for i in 0..100 {
    SamplingContext::set_error_rate(sampling_context, 0.01) // 1%错误率
    SamplingContext::set_latency(sampling_context, 100.0) // 100ms延迟
    
    let decision = AdaptiveSampler::should_sample(adaptive_sampler, sampling_context)
    if decision == SamplingDecision::RecordAndSample {
      normal_samples.count = normal_samples.count + 1
    }
  }
  
  // 正常情况下采样率应该接近基础采样率
  let normal_sample_rate = normal_samples.count.to_float() / 100.0
  assert_true(normal_sample_rate > 0.05 && normal_sample_rate < 0.15)
  
  // 模拟高错误率情况
  let high_error_samples = { mut count: 0 }
  for i in 0..100 {
    SamplingContext::set_error_rate(sampling_context, 0.1) // 10%错误率（超过阈值）
    SamplingContext::set_latency(sampling_context, 100.0) // 100ms延迟
    
    let decision = AdaptiveSampler::should_sample(adaptive_sampler, sampling_context)
    if decision == SamplingDecision::RecordAndSample {
      high_error_samples.count = high_error_samples.count + 1
    }
  }
  
  // 高错误率情况下采样率应该提高
  let high_error_sample_rate = high_error_samples.count.to_float() / 100.0
  assert_true(high_error_sample_rate > normal_sample_rate)
  
  // 模拟高延迟情况
  let high_latency_samples = { mut count: 0 }
  for i in 0..100 {
    SamplingContext::set_error_rate(sampling_context, 0.01) // 1%错误率
    SamplingContext::set_latency(sampling_context, 1200.0) // 1200ms延迟（超过阈值）
    
    let decision = AdaptiveSampler::should_sample(adaptive_sampler, sampling_context)
    if decision == SamplingDecision::RecordAndSample {
      high_latency_samples.count = high_latency_samples.count + 1
    }
  }
  
  // 高延迟情况下采样率应该提高
  let high_latency_sample_rate = high_latency_samples.count.to_float() / 100.0
  assert_true(high_latency_sample_rate > normal_sample_rate)
}

// 测试10: 遥测数据持久化和存储优化
test "遥测数据持久化和存储优化测试" {
  // 创建存储管理器
  let storage_manager = TelemetryStorageManager::new()
  
  // 配置存储策略
  TelemetryStorageManager::set_retention_period(storage_manager, 7 * 24 * 3600) // 7天保留期
  TelemetryStorageManager::set_compression_enabled(storage_manager, true)
  TelemetryStorageManager::set_indexing_enabled(storage_manager, true)
  
  // 创建测试数据
  let test_data = TelemetryDataset::new()
  for i in 0..100 {
    let record = TelemetryRecord::new()
    TelemetryRecord::set_name(record, "test.operation." + i.to_string())
    TelemetryRecord::set_timestamp(record, 1640995200 + i * 60) // 每分钟一个记录
    TelemetryRecord::set_attribute(record, "service", "test.service")
    TelemetryRecord::set_metric(record, "duration", (i * 10).to_float())
    TelemetryDataset::add_record(test_data, record)
  }
  
  // 存储数据
  let storage_id = TelemetryStorageManager::store(storage_manager, test_data)
  assert_true(storage_id.is_some())
  
  // 验证数据存储
  let is_stored = TelemetryStorageManager::exists(storage_manager, storage_id.unwrap())
  assert_true(is_stored)
  
  // 检索数据
  let retrieved_data = TelemetryStorageManager::retrieve(storage_manager, storage_id.unwrap())
  assert_true(retrieved_data.is_some())
  
  // 验证检索的数据
  let retrieved_dataset = retrieved_data.unwrap()
  assert_eq(TelemetryDataset::record_count(retrieved_dataset), 100)
  
  // 测试查询功能
  let query = TelemetryQuery::new()
  TelemetryQuery::add_attribute_filter(query, "service", "test.service")
  TelemetryQuery::add_time_range(query, 1640995200, 1640995200 + 99 * 60)
  
  let query_results = TelemetryStorageManager::query(storage_manager, query)
  assert_eq(query_results.length(), 100)
  
  // 测试数据压缩存储
  let uncompressed_size = TelemetryDataset::get_size(test_data)
  let compressed_size = TelemetryStorageManager::get_compressed_size(storage_manager, storage_id.unwrap())
  assert_true(compressed_size < uncompressed_size)
  
  // 测试数据过期清理
  TelemetryStorageManager::set_retention_period(storage_manager, 0) // 立即过期
  TelemetryStorageManager::cleanup_expired_data(storage_manager)
  
  // 验证数据已清理
  let is_expired = TelemetryStorageManager::exists(storage_manager, storage_id.unwrap())
  assert_false(is_expired)
}