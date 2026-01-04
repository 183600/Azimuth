// Azimuth 高级遥测功能测试用例
// 专注于遥测系统的高级功能和特性测试

// 测试1: 遥测数据采样策略
test "遥测数据采样策略测试" {
  // 创建采样策略配置
  let sampling_config = SamplingConfig::new()
  
  // 设置基于概率的采样
  SamplingConfig::set_probability_based(sampling_config, 0.1)  // 10%采样率
  
  // 创建采样器
  let sampler = ProbabilitySampler::new(0.1)
  
  // 测试采样决策
  let trace_id1 = TraceId::from_hex("00112233445566778899aabbccddeeff")
  let trace_id2 = TraceId::from_hex("ffeeddccbbaa99887766554433221100")
  
  let decision1 = Sampler::should_sample(sampler, trace_id1, "test.span")
  let decision2 = Sampler::should_sample(sampler, trace_id2, "test.span")
  
  // 验证采样决策是基于概率的
  match decision1 {
    SamplingDecision::RecordAndSample => assert_true(true)
    SamplingDecision::Drop => assert_true(true)
    SamplingDecision::RecordOnly => assert_true(false)  // 不应该出现此决策
  }
  
  // 测试基于属性的采样
  let attribute_sampler = AttributeSampler::new()
  AttributeSampler::add_condition(attribute_sampler, "http.status_code", "500")
  AttributeSampler::add_condition(attribute_sampler, "error.type", "timeout")
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "http.status_code", IntValue(500))
  Attributes::set(attrs, "error.type", StringValue("timeout"))
  
  let attribute_decision = Sampler::should_sample_with_attributes(attribute_sampler, trace_id1, "error.span", attrs)
  assert_eq(attribute_decision, SamplingDecision::RecordAndSample)
  
  // 测试自适应采样
  let adaptive_sampler = AdaptiveSampler::new(100)  // 每秒最多100个样本
  AdaptiveSampler::adjust_sampling_rate(adaptive_sampler, 0.05)  // 降低采样率
  
  let adaptive_decision = Sampler::should_sample(adaptive_sampler, trace_id2, "adaptive.span")
  match adaptive_decision {
    SamplingDecision::RecordAndSample => assert_true(true)
    SamplingDecision::Drop => assert_true(true)
    SamplingDecision::RecordOnly => assert_true(false)
  }
}

// 测试2: 遥测数据批处理
test "遥测数据批处理测试" {
  // 创建批处理器
  let batch_processor = BatchSpanProcessor::new(
    SimpleSpanExporter::new(),
    BatchConfig::new()
      .with_max_queue_size(512)
      .with_max_export_batch_size(100)
      .with_scheduled_delay(5000)
  )
  
  // 创建多个span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch-test")
  
  let span1 = Tracer::start_span(tracer, "batch.span.1")
  let span2 = Tracer::start_span(tracer, "batch.span.2")
  let span3 = Tracer::start_span(tracer, "batch.span.3")
  
  // 设置span属性
  Span::set_attribute(span1, "batch.id", IntValue(1))
  Span::set_attribute(span2, "batch.id", IntValue(1))
  Span::set_attribute(span3, "batch.id", IntValue(1))
  
  Span::set_attribute(span1, "operation.type", StringValue("read"))
  Span::set_attribute(span2, "operation.type", StringValue("write"))
  Span::set_attribute(span3, "operation.type", StringValue("delete"))
  
  // 结束span（触发批处理）
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  
  // 验证批处理配置
  let batch_config = BatchSpanProcessor::config(batch_processor)
  assert_eq(BatchConfig::max_queue_size(batch_config), 512)
  assert_eq(BatchConfig::max_export_batch_size(batch_config), 100)
  assert_eq(BatchConfig::scheduled_delay(batch_config), 5000)
  
  // 测试强制刷新
  BatchSpanProcessor::force_flush(batch_processor)
  
  // 测试关闭批处理器
  BatchSpanProcessor::shutdown(batch_processor)
}

// 测试3: 遥测数据导出和传输
test "遥测数据导出和传输测试" {
  // 创建内存导出器用于测试
  let memory_exporter = InMemorySpanExporter::new()
  
  // 创建HTTP导出器
  let http_exporter = HttpSpanExporter::new()
  HttpSpanExporter::set_endpoint(http_exporter, "https://otel-collector.example.com:4318/v1/traces")
  HttpSpanExporter::set_headers(http_exporter, [("Authorization", "Bearer test-token")])
  HttpSpanExporter::set_timeout(http_exporter, 30000)
  
  // 创建gRPC导出器
  let grpc_exporter = OtlpGrpcSpanExporter::new()
  OtlpGrpcSpanExporter::set_endpoint(grpc_exporter, "otel-collector.example.com:4317")
  OtlpGrpcSpanExporter::set_headers(grpc_exporter, [("x-api-key", "test-api-key")])
  OtlpGrpcSpanExporter::set_compression(grpc_exporter, Gzip)
  
  // 创建复合导出器
  let composite_exporter = MultiSpanExporter::new([memory_exporter, http_exporter, grpc_exporter])
  
  // 创建测试span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "export-test")
  
  let test_span = Tracer::start_span(tracer, "export.test.span")
  Span::set_attribute(test_span, "export.type", StringValue("test"))
  Span::set_attribute(test_span, "export.timestamp", IntValue(1640995200))
  Span::end(test_span)
  
  // 导出span数据
  let export_result = SpanExporter::export(composite_exporter, [test_span])
  
  // 验证导出结果
  match export_result {
    ExportResult::Success => assert_true(true)
    ExportResult::Failure(code, message) => {
      assert_true(false)  // 导出不应该失败
    }
  }
  
  // 验证内存导出器中的数据
  let exported_spans = InMemorySpanExporter::get_finished_spans(memory_exporter)
  assert_eq(exported_spans.length(), 1)
  assert_eq(Span::name(exported_spans[0]), "export.test.span")
  
  // 测试导出器关闭
  SpanExporter::shutdown(composite_exporter)
}

// 测试4: 遥测数据聚合和统计
test "遥测数据聚合和统计测试" {
  // 创建度量聚合器
  let aggregation = Aggregation::new()
  
  // 测试计数器聚合
  let counter_aggregation = Aggregation::counter()
  let counter_instrument = Instrument::new("http.requests.total", Counter, "Total HTTP requests")
  
  let counter_data_point1 = DataPoint::new(1640995200, 100.0, [("method", "GET"), ("status", "200")])
  let counter_data_point2 = DataPoint::new(1640995260, 50.0, [("method", "POST"), ("status", "201")])
  let counter_data_point3 = DataPoint::new(1640995320, 10.0, [("method", "GET"), ("status", "404")])
  
  let counter_metric = Metric::new(counter_instrument, [counter_data_point1, counter_data_point2, counter_data_point3])
  let aggregated_counter = Aggregator::aggregate(counter_aggregation, counter_metric)
  
  // 验证计数器聚合结果
  assert_eq(Metric::name(aggregated_counter), "http.requests.total")
  assert_eq(Metric::data_points(aggregated_counter).length(), 3)
  
  // 测试直方图聚合
  let histogram_aggregation = Aggregation::explicit_bucket_histogram([0.0, 0.1, 0.5, 1.0, 5.0, 10.0])
  let histogram_instrument = Instrument::new("http.request.duration", Histogram, "HTTP request duration")
  
  let histogram_data_point1 = DataPoint::new_histogram(1640995200, 
    [10.0, 5.0, 2.0, 1.0, 0.5, 0.1], 
    100.0, 18.6,
    [("endpoint", "/api/users")]
  )
  
  let histogram_metric = Metric::new(histogram_instrument, [histogram_data_point1])
  let aggregated_histogram = Aggregator::aggregate(histogram_aggregation, histogram_metric)
  
  // 验证直方图聚合结果
  assert_eq(Metric::name(aggregated_histogram), "http.request.duration")
  let histogram_points = Metric::data_points(aggregated_histogram)
  assert_eq(histogram_points.length(), 1)
  
  // 测试仪表聚合
  let gauge_aggregation = Aggregation::last_value()
  let gauge_instrument = Instrument::new("system.memory.usage", Gauge, "System memory usage")
  
  let gauge_data_point1 = DataPoint::new(1640995200, 75.5, [("instance", "server-1")])
  let gauge_data_point2 = DataPoint::new(1640995260, 80.2, [("instance", "server-1")])
  let gauge_data_point3 = DataPoint::new(1640995320, 65.8, [("instance", "server-2")])
  
  let gauge_metric = Metric::new(gauge_instrument, [gauge_data_point1, gauge_data_point2, gauge_data_point3])
  let aggregated_gauge = Aggregator::aggregate(gauge_aggregation, gauge_metric)
  
  // 验证仪表聚合结果
  assert_eq(Metric::name(aggregated_gauge), "system.memory.usage")
  let gauge_points = Metric::data_points(aggregated_gauge)
  assert_eq(gauge_points.length(), 2)  // 每个实例一个最新值
  
  // 测试时间序列统计
  let time_series_stats = TimeSeriesStats::new()
  TimeSeriesStats::add_point(time_series_stats, 1640995200, 100.0)
  TimeSeriesStats::add_point(time_series_stats, 1640995260, 120.0)
  TimeSeriesStats::add_point(time_series_stats, 1640995320, 90.0)
  TimeSeriesStats::add_point(time_series_stats, 1640995380, 110.0)
  
  // 验证统计结果
  assert_eq(TimeSeriesStats::count(time_series_stats), 4)
  assert_eq(TimeSeriesStats::sum(time_series_stats), 420.0)
  assert_eq(TimeSeriesStats::min(time_series_stats), 90.0)
  assert_eq(TimeSeriesStats::max(time_series_stats), 120.0)
  assert_eq(TimeSeriesStats::average(time_series_stats), 105.0)
}

// 测试5: 遥测系统配置验证
test "遥测系统配置验证测试" {
  // 创建配置验证器
  let validator = ConfigValidator::new()
  
  // 测试有效配置
  let valid_config = TelemetryConfig::new()
  TelemetryConfig::set_service_name(valid_config, "payment-service")
  TelemetryConfig::set_service_version(valid_config, "1.2.3")
  TelemetryConfig::set_sampling_rate(valid_config, 0.1)
  TelemetryConfig::set_exporter_endpoint(valid_config, "https://otel-collector.example.com:4317")
  TelemetryConfig::set_batch_size(valid_config, 512)
  TelemetryConfig::set_export_timeout(valid_config, 30000)
  
  let valid_result = ConfigValidator::validate(validator, valid_config)
  assert_true(ConfigValidationResult::is_valid(valid_result))
  assert_eq(ConfigValidationResult::errors(valid_result).length(), 0)
  
  // 测试无效配置 - 缺少必需字段
  let invalid_config1 = TelemetryConfig::new()
  // 未设置服务名称
  
  let invalid_result1 = ConfigValidator::validate(validator, invalid_config1)
  assert_false(ConfigValidationResult::is_valid(invalid_result1))
  assert_true(ConfigValidationResult::errors(invalid_result1).length() > 0)
  
  // 测试无效配置 - 无效的采样率
  let invalid_config2 = TelemetryConfig::new()
  TelemetryConfig::set_service_name(invalid_config2, "test-service")
  TelemetryConfig::set_sampling_rate(invalid_config2, -0.1)  // 负采样率无效
  
  let invalid_result2 = ConfigValidator::validate(validator, invalid_config2)
  assert_false(ConfigValidationResult::is_valid(invalid_result2))
  assert_true(ConfigValidationResult::errors(invalid_result2).length() > 0)
  
  // 测试无效配置 - 无效的URL
  let invalid_config3 = TelemetryConfig::new()
  TelemetryConfig::set_service_name(invalid_config3, "test-service")
  TelemetryConfig::set_sampling_rate(invalid_config3, 0.1)
  TelemetryConfig::set_exporter_endpoint(invalid_config3, "not-a-valid-url")
  
  let invalid_result3 = ConfigValidator::validate(validator, invalid_config3)
  assert_false(ConfigValidationResult::is_valid(invalid_result3))
  assert_true(ConfigValidationResult::errors(invalid_result3).length() > 0)
  
  // 测试配置规则
  ConfigValidator::add_rule(validator, "service.name", ConfigRule::required())
  ConfigValidator::add_rule(validator, "service.name", ConfigRule::max_length(100))
  ConfigValidator::add_rule(validator, "sampling.rate", ConfigRule::range(0.0, 1.0))
  ConfigValidator::add_rule(validator, "batch.size", ConfigRule::min(1))
  ConfigValidator::add_rule(validator, "export.timeout", ConfigRule::min(1000))
  
  // 测试带有规则的验证
  let config_with_rules = TelemetryConfig::new()
  TelemetryConfig::set_service_name(config_with_rules, "a".repeat(150))  // 超过最大长度
  TelemetryConfig::set_sampling_rate(config_with_rules, 1.5)  // 超过范围
  TelemetryConfig::set_batch_size(config_with_rules, 0)  // 小于最小值
  TelemetryConfig::set_export_timeout(config_with_rules, 500)  // 小于最小值
  
  let rules_result = ConfigValidator::validate(validator, config_with_rules)
  assert_false(ConfigValidationResult::is_valid(rules_result))
  assert_true(ConfigValidationResult::errors(rules_result).length() >= 4)
  
  // 测试配置修复
  let fixable_config = TelemetryConfig::new()
  TelemetryConfig::set_service_name(fixable_config, "a".repeat(150))
  TelemetryConfig::set_sampling_rate(fixable_config, 1.5)
  
  let fixed_config = ConfigValidator::auto_fix(validator, fixable_config)
  assert_true(ConfigValidationResult::is_valid(ConfigValidator::validate(validator, fixed_config)))
}

// 测试6: 遥测数据压缩和优化
test "遥测数据压缩和优化测试" {
  // 创建压缩器
  let gzip_compressor = GzipCompressor::new()
  let lz4_compressor = Lz4Compressor::new()
  let zstd_compressor = ZstdCompressor::new()
  
  // 创建测试数据
  let test_spans = []
  for i in 0..=100 {
    let span_name = "test.span." + i.to_string()
    let test_span = Span::new(span_name, Internal, SpanContext::new("trace-123", "span-" + i.to_string(), true, ""))
    Span::set_attribute(test_span, "iteration", IntValue(i))
    Span::set_attribute(test_span, "data", StringValue("sample data for compression test " + i.to_string()))
    test_spans = test_spans.push(test_span)
  }
  
  // 序列化span数据
  let serialized_data = Span::serialize_batch(test_spans)
  let original_size = serialized_data.length()
  
  // 测试Gzip压缩
  let gzip_compressed = Compressor::compress(gzip_compressor, serialized_data)
  let gzip_decompressed = Compressor::decompress(gzip_compressor, gzip_compressed)
  let gzip_ratio = gzip_compressed.length() as Float / original_size as Float
  
  assert_eq(gzip_decompressed, serialized_data)
  assert_true(gzip_ratio < 1.0)  // 压缩后应该更小
  
  // 测试LZ4压缩
  let lz4_compressed = Compressor::compress(lz4_compressor, serialized_data)
  let lz4_decompressed = Compressor::decompress(lz4_compressor, lz4_compressed)
  let lz4_ratio = lz4_compressed.length() as Float / original_size as Float
  
  assert_eq(lz4_decompressed, serialized_data)
  assert_true(lz4_ratio < 1.0)
  
  // 测试Zstd压缩
  let zstd_compressed = Compressor::compress(zstd_compressor, serialized_data)
  let zstd_decompressed = Compressor::decompress(zstd_compressor, zstd_compressed)
  let zstd_ratio = zstd_compressed.length() as Float / original_size as Float
  
  assert_eq(zstd_decompressed, serialized_data)
  assert_true(zstd_ratio < 1.0)
  
  // 比较压缩效率
  assert_true(zstd_ratio <= gzip_ratio)  // Zstd通常比Gzip更高效
  assert_true(gzip_ratio < lz4_ratio)  // Gzip通常比LZ4压缩率更高
  
  // 测试数据优化
  let optimizer = DataOptimizer::new()
  
  // 测试属性去重
  let optimized_data = DataOptimizer::deduplicate_attributes(optimizer, test_spans)
  assert_eq(optimized_data.length(), test_spans.length())
  
  // 测试字符串去重
  let string_deduped = DataOptimizer::deduplicate_strings(optimizer, serialized_data)
  assert_true(string_deduped.length() <= serialized_data.length())
  
  // 测试数据采样
  let sampled_spans = DataOptimizer::sample_spans(optimizer, test_spans, 0.2)  // 保留20%
  assert_eq(sampled_spans.length(), 20)  // 100 * 0.2 = 20
  
  // 测试数据聚合
  let aggregated_metrics = DataOptimizer::aggregate_metrics(optimizer, test_spans)
  assert_true(aggregated_metrics.length() > 0)
  
  // 测试数据过滤
  let filtered_spans = DataOptimizer::filter_spans(optimizer, test_spans, fn(span) {
    let iteration = Span::get_attribute(span, "iteration")
    match iteration {
      Some(IntValue(i)) => i % 10 == 0  // 只保留第10个span
      _ => false
    }
  })
  assert_eq(filtered_spans.length(), 11)  // 0, 10, 20, ..., 100
}

// 测试7: 遥测系统错误恢复
test "遥测系统错误恢复测试" {
  // 创建错误恢复管理器
  let recovery_manager = ErrorRecoveryManager::new()
  
  // 配置错误恢复策略
  ErrorRecoveryManager::set_retry_strategy(recovery_manager, ExponentialBackoff(1000, 30000, 2.0))
  ErrorRecoveryManager::set_max_retries(recovery_manager, 5)
  ErrorRecoveryManager::set_circuit_breaker_threshold(recovery_manager, 10)
  ErrorRecoveryManager::set_recovery_timeout(recovery_manager, 60000)
  
  // 创建模拟失败的导出器
  let failing_exporter = MockFailingSpanExporter::new()
  MockFailingSpanExporter::set_failure_rate(failing_exporter, 0.8)  // 80%失败率
  MockFailingSpanExporter::set_failure_types(failing_exporter, [NetworkError, TimeoutError])
  
  // 创建带有错误恢复的导出器
  let resilient_exporter = ResilientSpanExporter::new(failing_exporter, recovery_manager)
  
  // 创建测试span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "recovery-test")
  
  let test_spans = []
  for i in 0..=20 {
    let span_name = "recovery.test.span." + i.to_string()
    let test_span = Tracer::start_span(tracer, span_name)
    Span::set_attribute(test_span, "test.id", IntValue(i))
    Span::end(test_span)
    test_spans = test_spans.push(test_span)
  }
  
  // 尝试导出span（会触发错误恢复）
  let export_result = SpanExporter::export(resilient_exporter, test_spans)
  
  // 验证错误恢复机制
  match export_result {
    ExportResult::Success => assert_true(true)  // 最终应该成功
    ExportResult::Failure(code, message) => {
      // 即使失败，也应该进行了多次重试
      let retry_count = MockFailingSpanExporter::retry_count(failing_exporter)
      assert_true(retry_count > 0)
    }
  }
  
  // 验证错误统计
  let error_stats = ErrorRecoveryManager::error_statistics(recovery_manager)
  assert_true(ErrorStatistics::total_attempts(error_stats) > 0)
  assert_true(ErrorStatistics::total_failures(error_stats) > 0)
  assert_true(ErrorStatistics::total_retries(error_stats) > 0)
  
  // 测试熔断器
  let circuit_breaker = ErrorRecoveryManager::circuit_breaker(recovery_manager)
  assert_eq(CircuitBreaker::state(circuit_breaker), CircuitBreakerState::Closed)
  
  // 触发熔断器
  for i in 0..=15 {
    MockFailingSpanExporter::force_failure(failing_exporter)
    let result = SpanExporter::export(resilient_exporter, [test_spans[0]])
    match result {
      ExportResult::Failure(_, _) => assert_true(true)
      ExportResult::Success => assert_true(false)  // 应该失败
    }
  }
  
  // 验证熔断器状态
  assert_eq(CircuitBreaker::state(circuit_breaker), CircuitBreakerState::Open)
  
  // 测试熔断器恢复
  CircuitBreaker::attempt_reset(circuit_breaker)
  MockFailingSpanExporter::set_failure_rate(failing_exporter, 0.0)  // 不再失败
  
  let recovery_result = SpanExporter::export(resilient_exporter, [test_spans[0]])
  match recovery_result {
    ExportResult::Success => {
      assert_eq(CircuitBreaker::state(circuit_breaker), CircuitBreakerState::Closed)
    }
    ExportResult::Failure(_, _) => assert_true(false)
  }
  
  // 测试备用导出器
  let primary_exporter = MockFailingSpanExporter::new()
  MockFailingSpanExporter::set_failure_rate(primary_exporter, 1.0)  // 总是失败
  
  let fallback_exporter = InMemorySpanExporter::new()
  
  let failover_exporter = FailoverSpanExporter::new([primary_exporter, fallback_exporter])
  
  let failover_result = SpanExporter::export(failover_exporter, [test_spans[0]])
  assert_eq(failover_result, ExportResult::Success)
  
  // 验证备用导出器接收到数据
  let fallback_spans = InMemorySpanExporter::get_finished_spans(fallback_exporter)
  assert_eq(fallback_spans.length(), 1)
}

// 测试8: 遥测数据缓存机制
test "遥测数据缓存机制测试" {
  // 创建缓存管理器
  let cache_manager = CacheManager::new()
  
  // 配置缓存策略
  CacheManager::set_max_size(cache_manager, 1000)
  CacheManager::set_ttl(cache_manager, 300000)  // 5分钟TTL
  CacheManager::set_eviction_policy(cache_manager, LRU)  // 最近最少使用淘汰
  
  // 创建属性缓存
  let attribute_cache = AttributeCache::new(cache_manager)
  
  // 测试属性缓存
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "service.name", StringValue("payment-service"))
  Attributes::set(attrs1, "service.version", StringValue("1.2.3"))
  Attributes::set(attrs1, "environment", StringValue("production"))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "service.name", StringValue("payment-service"))
  Attributes::set(attrs2, "service.version", StringValue("1.2.3"))
  Attributes::set(attrs2, "environment", StringValue("production"))
  
  let attrs3 = Attributes::new()
  Attributes::set(attrs3, "service.name", StringValue("payment-service"))
  Attributes::set(attrs3, "service.version", StringValue("1.2.4"))  // 不同版本
  Attributes::set(attrs3, "environment", StringValue("production"))
  
  // 缓存属性
  let cache_key1 = AttributeCache::generate_key(attrs1)
  let cache_key2 = AttributeCache::generate_key(attrs2)
  let cache_key3 = AttributeCache::generate_key(attrs3)
  
  AttributeCache::put(attribute_cache, cache_key1, attrs1)
  AttributeCache::put(attribute_cache, cache_key2, attrs2)
  AttributeCache::put(attribute_cache, cache_key3, attrs3)
  
  // 验证缓存命中
  let cached_attrs1 = AttributeCache::get(attribute_cache, cache_key1)
  let cached_attrs2 = AttributeCache::get(attribute_cache, cache_key2)
  let cached_attrs3 = AttributeCache::get(attribute_cache, cache_key3)
  
  assert_eq(cached_attrs1, Some(attrs1))
  assert_eq(cached_attrs2, Some(attrs2))
  assert_eq(cached_attrs3, Some(attrs3))
  
  // 验证相同属性生成相同缓存键
  assert_eq(cache_key1, cache_key2)
  assert_ne(cache_key1, cache_key3)
  
  // 测试度量数据缓存
  let metric_cache = MetricDataCache::new(cache_manager)
  
  let metric_data1 = MetricData::new("http.requests.total", 100.0, 1640995200)
  let metric_data2 = MetricData::new("http.requests.total", 150.0, 1640995260)
  let metric_data3 = MetricData::new("http.response.time", 0.125, 1640995200)
  
  // 缓存度量数据
  MetricDataCache::put(metric_cache, "metric1", metric_data1)
  MetricDataCache::put(metric_cache, "metric2", metric_data2)
  MetricDataCache::put(metric_cache, "metric3", metric_data3)
  
  // 验证度量数据缓存
  let cached_metric1 = MetricDataCache::get(metric_cache, "metric1")
  let cached_metric2 = MetricDataCache::get(metric_cache, "metric2")
  let cached_metric3 = MetricDataCache::get(metric_cache, "metric3")
  
  assert_eq(cached_metric1, Some(metric_data1))
  assert_eq(cached_metric2, Some(metric_data2))
  assert_eq(cached_metric3, Some(metric_data3))
  
  // 测试缓存统计
  let cache_stats = CacheManager::statistics(cache_manager)
  assert_eq(CacheStatistics::hits(cache_stats), 6)  // 6次缓存命中
  assert_eq(CacheStatistics::misses(cache_stats), 0)  // 0次缓存未命中
  assert_eq(CacheStatistics::size(cache_stats), 4)  // 4个缓存项
  
  // 测试缓存淘汰
  for i in 0..=1000 {
    let test_attrs = Attributes::new()
    Attributes::set(test_attrs, "test.key", StringValue("test.value." + i.to_string()))
    let test_key = AttributeCache::generate_key(test_attrs)
    AttributeCache::put(attribute_cache, test_key, test_attrs)
  }
  
  // 验证缓存大小不超过最大值
  let updated_stats = CacheManager::statistics(cache_manager)
  assert_true(CacheStatistics::size(updated_stats) <= 1000)
  assert_true(CacheStatistics::evictions(updated_stats) > 0)
  
  // 测试缓存过期
  let short_ttl_cache = CacheManager::new()
  CacheManager::set_ttl(short_ttl_cache, 100)  // 100ms TTL
  
  let short_ttl_attribute_cache = AttributeCache::new(short_ttl_cache)
  AttributeCache::put(short_ttl_attribute_cache, "short_lived", attrs1)
  
  // 立即获取应该成功
  let immediate_get = AttributeCache::get(short_ttl_attribute_cache, "short_lived")
  assert_eq(immediate_get, Some(attrs1))
  
  // 等待过期（在实际测试中需要时间等待）
  // 这里我们模拟过期
  CacheManager::expire_all(short_ttl_cache)
  
  // 过期后获取应该失败
  let expired_get = AttributeCache::get(short_ttl_attribute_cache, "short_lived")
  assert_eq(expired_get, None)
}

// 测试9: 遥测系统资源管理
test "遥测系统资源管理测试" {
  // 创建资源管理器
  let resource_manager = ResourceManager::new()
  
  // 配置资源限制
  ResourceManager::set_max_memory_usage(resource_manager, 100 * 1024 * 1024)  // 100MB
  ResourceManager::set_max_cpu_usage(resource_manager, 80.0)  // 80%
  ResourceManager::set_max_file_descriptors(resource_manager, 1000)
  ResourceManager::set_max_network_connections(resource_manager, 100)
  
  // 创建资源监控器
  let monitor = ResourceMonitor::new(resource_manager)
  
  // 测试内存使用监控
  let memory_usage = ResourceMonitor::memory_usage(monitor)
  assert_true(memory_usage > 0)
  assert_true(memory_usage < 100 * 1024 * 1024)
  
  // 测试CPU使用监控
  let cpu_usage = ResourceMonitor::cpu_usage(monitor)
  assert_true(cpu_usage >= 0.0)
  assert_true(cpu_usage <= 100.0)
  
  // 测试文件描述符监控
  let fd_count = ResourceMonitor::file_descriptor_count(monitor)
  assert_true(fd_count > 0)
  assert_true(fd_count < 1000)
  
  // 测试网络连接监控
  let conn_count = ResourceMonitor::network_connection_count(monitor)
  assert_true(conn_count >= 0)
  
  // 创建资源池
  let span_pool = ResourcePool::new(fn() { Span::new("pooled.span", Internal, SpanContext::new("", "", false, "")) })
  ResourcePool::set_max_size(span_pool, 100)
  ResourcePool::set_min_size(span_pool, 10)
  
  // 测试资源池
  let pooled_span1 = ResourcePool::acquire(span_pool)
  let pooled_span2 = ResourcePool::acquire(span_pool)
  let pooled_span3 = ResourcePool::acquire(span_pool)
  
  // 验证资源池状态
  assert_eq(ResourcePool::active_count(span_pool), 3)
  assert_true(ResourcePool::available_count(span_pool) >= 7)  // 至少有7个可用资源
  
  // 释放资源
  ResourcePool::release(span_pool, pooled_span1)
  ResourcePool::release(span_pool, pooled_span2)
  ResourcePool::release(span_pool, pooled_span3)
  
  // 验证资源释放
  assert_eq(ResourcePool::active_count(span_pool), 0)
  assert_true(ResourcePool::available_count(span_pool) >= 10)  // 至少有10个可用资源
  
  // 测试资源限制
  let limited_resource_manager = ResourceManager::new()
  ResourceManager::set_max_memory_usage(limited_resource_manager, 1024)  // 1KB限制
  
  let limited_monitor = ResourceMonitor::new(limited_resource_manager)
  
  // 尝试分配超过限制的内存
  let allocation_result = ResourceManager::try_allocate_memory(limited_resource_manager, 2048)
  assert_false(allocation_result)  // 应该失败
  
  // 尝试分配在限制内的内存
  let small_allocation_result = ResourceManager::try_allocate_memory(limited_resource_manager, 512)
  assert_true(small_allocation_result)  // 应该成功
  
  // 测试资源清理
  let cleanup_manager = CleanupManager::new()
  CleanupManager::register_cleanup_task(cleanup_manager, fn() {
    // 清理临时文件
    TempFileManager::cleanup_all()
  })
  
  CleanupManager::register_cleanup_task(cleanup_manager, fn() {
    // 清理缓存
    CacheManager::cleanup_expired()
  })
  
  CleanupManager::register_cleanup_task(cleanup_manager, fn() {
    // 清理网络连接
    ConnectionPool::cleanup_idle()
  })
  
  // 执行清理任务
  CleanupManager::execute_all(cleanup_manager)
  
  // 验证清理结果
  assert_true(TempFileManager::is_cleaned())
  assert_true(CacheManager::is_expired_cleaned())
  assert_true(ConnectionPool::is_idle_cleaned())
  
  // 测试资源泄漏检测
  let leak_detector = ResourceLeakDetector::new()
  
  // 注册资源
  LeakDetector::register_resource(leak_detector, "test-span-1", "span")
  LeakDetector::register_resource(leak_detector, "test-span-2", "span")
  LeakDetector::register_resource(leak_detector, "test-metric-1", "metric")
  
  // 注销部分资源
  LeakDetector::unregister_resource(leak_detector, "test-span-1")
  
  // 检测泄漏
  let leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(leaks.length(), 2)  // 应该检测到2个泄漏
  
  // 验证泄漏详情
  let leak1 = leaks[0]
  let leak2 = leaks[1]
  
  assert_eq(ResourceLeak::resource_id(leak1), "test-span-2")
  assert_eq(ResourceLeak::resource_type(leak1), "span")
  
  assert_eq(ResourceLeak::resource_id(leak2), "test-metric-1")
  assert_eq(ResourceLeak::resource_type(leak2), "metric")
}

// 测试10: 遥测数据生命周期管理
test "遥测数据生命周期管理测试" {
  // 创建生命周期管理器
  let lifecycle_manager = LifecycleManager::new()
  
  // 配置生命周期策略
  LifecycleManager::set_span_retention_period(lifecycle_manager, 86400000)  // 24小时
  LifecycleManager::set_metric_retention_period(lifecycle_manager, 604800000)  // 7天
  LifecycleManager::set_log_retention_period(lifecycle_manager, 2592000000)  // 30天
  
  // 创建生命周期阶段
  let creation_stage = LifecycleStage::new("creation")
  let processing_stage = LifecycleStage::new("processing")
  let export_stage = LifecycleStage::new("export")
  let archival_stage = LifecycleStage::new("archival")
  let deletion_stage = LifecycleStage::new("deletion")
  
  // 配置阶段转换
  LifecycleManager::add_transition(lifecycle_manager, creation_stage, processing_stage, 
    fn(data) { true })  // 总是转换
  
  LifecycleManager::add_transition(lifecycle_manager, processing_stage, export_stage, 
    fn(data) { LifecycleData::is_ready_for_export(data) })
  
  LifecycleManager::add_transition(lifecycle_manager, export_stage, archival_stage, 
    fn(data) { LifecycleData::export_successful(data) })
  
  LifecycleManager::add_transition(lifecycle_manager, archival_stage, deletion_stage, 
    fn(data) { LifecycleData::is_expired(data) })
  
  // 创建测试数据
  let test_span = Span::new("lifecycle.test.span", Internal, SpanContext::new("trace-123", "span-456", true, ""))
  Span::set_attribute(test_span, "test.data", StringValue("lifecycle test"))
  
  let lifecycle_data = LifecycleData::new(test_span)
  LifecycleData::set_timestamp(lifecycle_data, 1640995200)  // 2022-01-01
  
  // 测试生命周期阶段
  assert_eq(LifecycleData::current_stage(lifecycle_data), creation_stage)
  
  // 转换到处理阶段
  let processed_data = LifecycleManager::transition(lifecycle_manager, lifecycle_data)
  assert_eq(LifecycleData::current_stage(processed_data), processing_stage)
  
  // 标记为可导出
  LifecycleData::mark_ready_for_export(processed_data)
  
  // 转换到导出阶段
  let exporting_data = LifecycleManager::transition(lifecycle_manager, processed_data)
  assert_eq(LifecycleData::current_stage(exporting_data), export_stage)
  
  // 模拟成功导出
  LifecycleData::mark_export_successful(exporting_data)
  
  // 转换到归档阶段
  let archived_data = LifecycleManager::transition(lifecycle_manager, exporting_data)
  assert_eq(LifecycleData::current_stage(archived_data), archival_stage)
  
  // 测试数据过期
  let current_time = 1640995200 + 86400000 + 1000  // 超过保留期
  LifecycleData::set_current_time(archived_data, current_time)
  
  // 转换到删除阶段
  let expired_data = LifecycleManager::transition(lifecycle_manager, archived_data)
  assert_eq(LifecycleData::current_stage(expired_data), deletion_stage)
  
  // 测试生命周期策略
  let retention_policy = RetentionPolicy::new()
  RetentionPolicy::add_rule(retention_policy, "span", RetentionRule::time_based(86400000))  // 24小时
  RetentionPolicy::add_rule(retention_policy, "metric", RetentionRule::time_based(604800000))  // 7天
  RetentionPolicy::add_rule(retention_policy, "log", RetentionRule::time_based(2592000000))  // 30天
  RetentionPolicy::add_rule(retention_policy, "error", RetentionRule::count_based(1000))  // 最多1000个错误
  
  // 测试保留策略
  let span_data = LifecycleData::new(test_span)
  let should_retain_span = RetentionPolicy::should_retain(retention_policy, span_data)
  assert_true(should_retain_span)  // 新数据应该保留
  
  // 创建过期数据
  let old_span_data = LifecycleData::new(test_span)
  LifecycleData::set_timestamp(old_span_data, 1640995200 - 86400000 - 1000)  // 超过24小时
  let should_retain_old_span = RetentionPolicy::should_retain(retention_policy, old_span_data)
  assert_false(should_retain_old_span)  // 过期数据不应该保留
  
  // 测试生命周期统计
  let lifecycle_stats = LifecycleManager::statistics(lifecycle_manager)
  assert_eq(LifecycleStatistics::transitions(lifecycle_stats), 4)  // 4次转换
  assert_eq(LifecycleStatistics::created_count(lifecycle_stats), 1)  // 1个创建
  assert_eq(LifecycleStatistics::processed_count(lifecycle_stats), 1)  // 1个处理
  assert_eq(LifecycleStatistics::exported_count(lifecycle_stats), 1)  // 1个导出
  assert_eq(LifecycleStatistics::archived_count(lifecycle_stats), 1)  // 1个归档
  assert_eq(LifecycleStatistics::deleted_count(lifecycle_stats), 1)  // 1个删除
  
  // 测试生命周期清理
  LifecycleManager::schedule_cleanup(lifecycle_manager, 3600000)  // 每小时清理一次
  
  // 模拟清理任务
  let cleanup_result = LifecycleManager::execute_cleanup(lifecycle_manager)
  assert_true(CleanupResult::success(cleanup_result))
  assert_true(CleanupResult::cleaned_count(cleanup_result) > 0)
  
  // 测试生命周期事件
  let event_listener = LifecycleEventListener::new()
  LifecycleEventListener::on_transition(event_listener, fn(from_stage, to_stage, data) {
    // 记录转换事件
    EventLogger::log("Lifecycle transition: " + from_stage.name() + " -> " + to_stage.name())
  })
  
  LifecycleEventListener::on_expiry(event_listener, fn(data) {
    // 记录过期事件
    EventLogger::log("Data expired: " + LifecycleData::id(data))
  })
  
  LifecycleEventListener::on_deletion(event_listener, fn(data) {
    // 记录删除事件
    EventLogger::log("Data deleted: " + LifecycleData::id(data))
  })
  
  // 注册事件监听器
  LifecycleManager::register_event_listener(lifecycle_manager, event_listener)
  
  // 测试事件触发
  let test_data = LifecycleData::new(test_span)
  LifecycleData::mark_expired(test_data)
  LifecycleManager::schedule_deletion(lifecycle_manager, test_data)
  
  // 验证事件被触发
  let event_log = EventLogger::get_log()
  assert_true(event_log.length() > 0)
  assert_true(event_log.contains("Data expired"))
  assert_true(event_log.contains("Data deleted"))
}