// Azimuth 高级遥测分析测试用例
// 专注于遥测数据分析、性能优化和高级功能

// 测试1: 遥测数据聚合和分析
test "遥测数据聚合和分析测试" {
  // 创建数据聚合器
  let aggregator = TelemetryAggregator::new("service.metrics")
  
  // 添加指标数据点
  let data_points = [
    MetricDataPoint::new("response_time", 120.5, "2023-01-01T10:00:00Z"),
    MetricDataPoint::new("response_time", 85.2, "2023-01-01T10:01:00Z"),
    MetricDataPoint::new("response_time", 150.8, "2023-01-01T10:02:00Z"),
    MetricDataPoint::new("response_time", 95.3, "2023-01-01T10:03:00Z"),
    MetricDataPoint::new("response_time", 110.7, "2023-01-01T10:04:00Z")
  ]
  
  // 聚合数据
  for point in data_points {
    TelemetryAggregator::add_data_point(aggregator, point)
  }
  
  // 计算统计指标
  let stats = TelemetryAggregator::calculate_statistics(aggregator, "response_time")
  
  // 验证统计结果
  assert_eq(Statistics::count(stats), 5)
  assert_eq(Statistics::min(stats), 85.2)
  assert_eq(Statistics::max(stats), 150.8)
  assert_eq(Statistics::average(stats), 112.5)
  
  // 计算百分位数
  let p95 = Statistics::percentile(stats, 95.0)
  let p99 = Statistics::percentile(stats, 99.0)
  
  assert_true(p95 >= 150.8)  // 95%应该小于等于最大值
  assert_true(p99 >= 150.8)  // 99%应该小于等于最大值
}

// 测试2: 遥测数据趋势分析
test "遥测数据趋势分析测试" {
  // 创建趋势分析器
  let trend_analyzer = TrendAnalyzer::new("hourly.analysis")
  
  // 添加时间序列数据
  let time_series_data = [
    ("2023-01-01T08:00:00Z", 100.0),
    ("2023-01-01T09:00:00Z", 120.0),
    ("2023-01-01T10:00:00Z", 115.0),
    ("2023-01-01T11:00:00Z", 130.0),
    ("2023-01-01T12:00:00Z", 125.0),
    ("2023-01-01T13:00:00Z", 140.0),
    ("2023-01-01T14:00:00Z", 135.0)
  ]
  
  // 加载数据到分析器
  for (timestamp, value) in time_series_data {
    TrendAnalyzer::add_data_point(trend_analyzer, timestamp, value)
  }
  
  // 计算趋势
  let trend = TrendAnalyzer::calculate_trend(trend_analyzer)
  
  // 验证趋势分析结果
  assert_eq(Trend::direction(trend), "increasing")  // 总体趋势上升
  assert_true(Trend::slope(trend) > 0.0)  // 斜率应该为正
  assert_true(Trend::correlation_coefficient(trend) > 0.8)  // 强正相关
  
  // 预测下一个值
  let predicted_value = TrendAnalyzer::predict_next(trend_analyzer)
  assert_true(predicted_value > 140.0)  // 预测值应该大于最后一个实际值
  
  // 检测异常点
  let anomalies = TrendAnalyzer::detect_anomalies(trend_analyzer, 2.0)  // 2个标准差
  assert_eq(anomalies.length(), 0)  // 在这个数据集中没有异常点
}

// 测试3: 智能采样策略
test "智能采样策略测试" {
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new()
  
  // 配置采样参数
  AdaptiveSampler::set_target_rate(adaptive_sampler, 0.1)  // 目标采样率10%
  AdaptiveSampler::set_max_sample_rate(adaptive_sampler, 0.5)  // 最大采样率50%
  AdaptiveSampler::set_min_sample_rate(adaptive_sampler, 0.01)  // 最小采样率1%
  
  // 模拟不同优先级的请求
  let high_priority_trace = TraceContext::new("high-priority-trace", "span-1", [])
  let normal_priority_trace = TraceContext::new("normal-priority-trace", "span-2", [])
  let low_priority_trace = TraceContext::new("low-priority-trace", "span-3", [])
  
  // 设置采样属性
  TraceContext::set_attribute(high_priority_trace, "trace.priority", "high")
  TraceContext::set_attribute(normal_priority_trace, "trace.priority", "normal")
  TraceContext::set_attribute(low_priority_trace, "trace.priority", "low")
  
  // 测试采样决策
  let high_sampled = AdaptiveSampler::should_sample(adaptive_sampler, high_priority_trace)
  let normal_sampled = AdaptiveSampler::should_sample(adaptive_sampler, normal_priority_trace)
  let low_sampled = AdaptiveSampler::should_sample(adaptive_sampler, low_priority_trace)
  
  // 验证采样结果
  assert_true(high_sampled)  // 高优先级应该总是被采样
  // 正常和低优先级的采样率应该不同
  assert_ne(normal_sampled, low_sampled)
  
  // 测试采样率自适应调整
  let initial_rate = AdaptiveSampler::get_current_rate(adaptive_sampler)
  
  // 模拟高负载情况
  for i in 0..1000 {
    let trace = TraceContext::new("trace-" + i.to_string(), "span-" + i.to_string(), [])
    AdaptiveSampler::record_sample_decision(adaptive_sampler, trace, true)  // 记录采样
  }
  
  // 调整采样率
  AdaptiveSampler::adjust_rate(adaptive_sampler)
  let adjusted_rate = AdaptiveSampler::get_current_rate(adaptive_sampler)
  
  // 验证采样率调整
  assert_true(adjusted_rate <= initial_rate)  // 高负载时采样率应该降低
}

// 测试4: 遥测数据缓存机制
test "遥测数据缓存机制测试" {
  // 创建LRU缓存
  let cache = LRUCache::new(100)  // 最大100项
  
  // 添加缓存项
  let cache_keys = ["metric-1", "metric-2", "metric-3", "metric-4", "metric-5"]
  let cache_values = [100.0, 200.0, 300.0, 400.0, 500.0]
  
  for i in 0..cache_keys.length() {
    LRUCache::put(cache, cache_keys[i], cache_values[i])
  }
  
  // 验证缓存命中
  let value1 = LRUCache::get(cache, "metric-1")
  let value3 = LRUCache::get(cache, "metric-3")
  let value5 = LRUCache::get(cache, "metric-5")
  
  assert_eq(value1, Some(100.0))
  assert_eq(value3, Some(300.0))
  assert_eq(value5, Some(500.0))
  
  // 验证缓存未命中
  let missing_value = LRUCache::get(cache, "non-existent")
  assert_eq(missing_value, None)
  
  // 测试缓存淘汰
  // 添加更多项超过缓存容量
  for i in 6..150 {
    LRUCache::put(cache, "metric-" + i.to_string(), i.to_float() * 100.0)
  }
  
  // 最早的项应该被淘汰
  let evicted_value = LRUCache::get(cache, "metric-1")
  assert_eq(evicted_value, None)
  
  // 最新的项应该存在
  let latest_value = LRUCache::get(cache, "metric-149")
  assert_eq(latest_value, Some(14900.0))
  
  // 验证缓存大小
  assert_eq(LRUCache::size(cache), 100)
}

// 测试5: 遥测数据压缩和传输优化
test "遥测数据压缩和传输优化测试" {
  // 创建测试数据集
  let telemetry_batch = TelemetryBatch::new()
  
  // 添加大量span数据
  for i in 0..1000 {
    let span = Span::new("operation-" + (i % 10).to_string(), Server, 
                        SpanContext::new("trace-" + (i % 100).to_string(), 
                                       "span-" + i.to_string(), true, ""))
    
    // 添加属性
    Span::set_attribute(span, "http.method", StringValue("GET"))
    Span::set_attribute(span, "http.status_code", IntValue(200))
    Span::set_attribute(span, "service.name", StringValue("api.service"))
    Span::set_attribute(span, "operation.index", IntValue(i))
    
    TelemetryBatch::add_span(telemetry_batch, span)
  }
  
  // 测试原始数据大小
  let original_size = TelemetryBatch::calculate_size(telemetry_batch)
  assert_true(original_size > 0)
  
  // 压缩数据
  let compressed_data = TelemetryBatch::compress(telemetry_batch)
  let compressed_size = compressed_data.length()
  
  // 验证压缩效果
  assert_true(compressed_size < original_size)
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  assert_true(compression_ratio < 0.8)  // 压缩率应该小于80%
  
  // 解压缩数据
  let decompressed_batch = TelemetryBatch::decompress(compressed_data)
  
  // 验证解压缩后的数据完整性
  assert_eq(TelemetryBatch::span_count(decompressed_batch), 1000)
  assert_eq(TelemetryBatch::span_count(telemetry_batch), TelemetryBatch::span_count(decompressed_batch))
  
  // 验证特定span的属性
  let first_span = TelemetryBatch::get_span_at(decompressed_batch, 0)
  let method_attr = Span::get_attribute(first_span, "http.method")
  assert_eq(method_attr, Some(StringValue("GET")))
  
  let index_attr = Span::get_attribute(first_span, "operation.index")
  assert_eq(index_attr, Some(IntValue(0)))
}

// 测试6: 遥测数据质量验证
test "遥测数据质量验证测试" {
  // 创建数据质量验证器
  let validator = DataQualityValidator::new()
  
  // 添加验证规则
  DataQualityValidator::add_rule(validator, "span.name", ValidationRule::required())
  DataQualityValidator::add_rule(validator, "span.duration", ValidationRule::min_value(0))
  DataQualityValidator::add_rule(validator, "http.status_code", ValidationRule::range(100, 599))
  DataQualityValidator::add_rule(validator, "service.name", ValidationRule::allowed_values(["api.service", "web.service", "db.service"]))
  
  // 创建有效数据
  let valid_span = Span::new("valid.operation", Server, SpanContext::new("trace-1", "span-1", true, ""))
  Span::set_duration(valid_span, 100)  // 有效持续时间
  Span::set_attribute(valid_span, "http.status_code", IntValue(200))
  Span::set_attribute(valid_span, "service.name", StringValue("api.service"))
  
  // 验证有效数据
  let valid_result = DataQualityValidator::validate_span(validator, valid_span)
  assert_true(DataQualityValidationResult::is_valid(valid_result))
  assert_eq(DataQualityValidationResult::error_count(valid_result), 0)
  
  // 创建无效数据
  let invalid_span = Span::new("", Server, SpanContext::new("trace-2", "span-2", true, ""))  // 空名称
  Span::set_duration(invalid_span, -50)  // 负持续时间
  Span::set_attribute(invalid_span, "http.status_code", IntValue(999))  // 无效状态码
  Span::set_attribute(invalid_span, "service.name", StringValue("unknown.service"))  // 不允许的服务名
  
  // 验证无效数据
  let invalid_result = DataQualityValidator::validate_span(validator, invalid_span)
  assert_false(DataQualityValidationResult::is_valid(invalid_result))
  assert_true(DataQualityValidationResult::error_count(invalid_result) > 0)
  
  // 验证具体错误
  let errors = DataQualityValidationResult::get_errors(invalid_result)
  assert_true(errors.any(fn(error) { error.contains("span.name") }))
  assert_true(errors.any(fn(error) { error.contains("span.duration") }))
  assert_true(errors.any(fn(error) { error.contains("http.status_code") }))
  assert_true(errors.any(fn(error) { error.contains("service.name") }))
}

// 测试7: 遥测数据关联和依赖分析
test "遥测数据关联和依赖分析测试" {
  // 创建依赖分析器
  let dependency_analyzer = DependencyAnalyzer::new()
  
  // 添加服务调用链
  let service_calls = [
    ServiceCall::new("frontend", "api-gateway", "GET /api/users"),
    ServiceCall::new("api-gateway", "user-service", "GET /users"),
    ServiceCall::new("user-service", "database", "SELECT * FROM users"),
    ServiceCall::new("user-service", "cache", "GET user:123"),
    ServiceCall::new("api-gateway", "auth-service", "POST /auth/validate"),
    ServiceCall::new("auth-service", "database", "SELECT * FROM tokens")
  ]
  
  // 加载调用数据
  for call in service_calls {
    DependencyAnalyzer::add_service_call(dependency_analyzer, call)
  }
  
  // 分析服务依赖
  let dependencies = DependencyAnalyzer::analyze_dependencies(dependency_analyzer)
  
  // 验证依赖关系
  assert_true(dependencies.contains(("frontend", "api-gateway")))
  assert_true(dependencies.contains(("api-gateway", "user-service")))
  assert_true(dependencies.contains(("api-gateway", "auth-service")))
  assert_true(dependencies.contains(("user-service", "database")))
  assert_true(dependencies.contains(("user-service", "cache")))
  assert_true(dependencies.contains(("auth-service", "database")))
  
  // 分析调用频率
  let call_frequencies = DependencyAnalyzer::analyze_call_frequencies(dependency_analyzer)
  
  // 验证频率分析
  assert_eq(call_frequencies.get(("user-service", "database")), Some(1))
  assert_eq(call_frequencies.get(("api-gateway", "user-service")), Some(1))
  
  // 检测关键路径
  let critical_paths = DependencyAnalyzer::find_critical_paths(dependency_analyzer, "frontend")
  
  // 验证关键路径
  assert_true(critical_paths.length() > 0)
  
  // 应该包含从frontend到database的路径
  let frontend_to_db_path = critical_paths.find(fn(path) {
    path.last() == "database" and path.first() == "frontend"
  })
  assert_true(frontend_to_db_path.is_some())
  
  // 检测循环依赖
  let circular_deps = DependencyAnalyzer::detect_circular_dependencies(dependency_analyzer)
  assert_eq(circular_deps.length(), 0)  // 当前数据没有循环依赖
}

// 测试8: 遥测数据异常检测
test "遥测数据异常检测测试" {
  // 创建异常检测器
  let anomaly_detector = AnomalyDetector::new()
  
  // 配置检测算法
  AnomalyDetector::set_algorithm(anomaly_detector, "statistical")  // 使用统计方法
  AnomalyDetector::set_threshold(anomaly_detector, 2.5)  // 2.5个标准差
  AnomalyDetector::set_window_size(anomaly_detector, 100)  // 滑动窗口大小
  
  // 添加正常数据点
  let normal_data = [
    100.0, 102.0, 98.0, 101.0, 99.0, 103.0, 97.0, 100.0, 101.0, 99.0,
    100.0, 98.0, 102.0, 99.0, 101.0, 100.0, 99.0, 102.0, 98.0, 100.0,
    101.0, 99.0, 100.0, 98.0, 102.0, 99.0, 101.0, 100.0, 99.0, 98.0,
    102.0, 100.0, 99.0, 101.0, 98.0, 100.0, 99.0, 102.0, 100.0, 99.0,
    101.0, 98.0, 100.0, 99.0, 102.0, 100.0, 99.0, 101.0, 98.0, 100.0
  ]
  
  for value in normal_data {
    AnomalyDetector::add_data_point(anomaly_detector, value)
  }
  
  // 添加异常数据点
  let anomaly_data = [150.0, 200.0, 20.0, 250.0, 10.0]
  
  for value in anomaly_data {
    AnomalyDetector::add_data_point(anomaly_detector, value)
  }
  
  // 检测异常
  let anomalies = AnomalyDetector::detect_anomalies(anomaly_detector)
  
  // 验证异常检测结果
  assert_true(anomalies.length() >= 5)  // 至少应该检测到5个异常点
  
  // 验证异常值
  for anomaly in anomalies {
    let value = AnomalyPoint::value(anomaly)
    assert_true(value >= 150.0 or value <= 50.0)  // 异常值应该远离正常范围
  }
  
  // 测试异常评分
  let anomaly_scores = AnomalyDetector::calculate_anomaly_scores(anomaly_detector)
  
  // 验证异常评分
  for i in 0..anomaly_scores.length() {
    let score = anomaly_scores[i]
    let value = AnomalyDetector::get_data_point_at(anomaly_detector, i)
    
    // 异常值的评分应该更高
    if value >= 150.0 or value <= 50.0 {
      assert_true(score > 0.7)  // 异常评分应该大于0.7
    } else {
      assert_true(score < 0.5)  // 正常值评分应该小于0.5
    }
  }
  
  // 测试异常趋势检测
  let trend_anomalies = AnomalyDetector::detect_trend_anomalies(anomaly_detector)
  
  // 验证趋势异常
  assert_true(trend_anomalies.length() >= 1)  // 应该检测到趋势异常
}

// 测试9: 遥测数据实时流处理
test "遥测数据实时流处理测试" {
  // 创建流处理器
  let stream_processor = StreamProcessor::new()
  
  // 配置流处理管道
  StreamProcessor::add_source(stream_processor, "telemetry.source")
  StreamProcessor::add_filter(stream_processor, fn(data) {
    // 过滤掉无效数据
    data.duration > 0 and data.status_code >= 200 and data.status_code < 600
  })
  StreamProcessor::add_transformer(stream_processor, fn(data) {
    // 转换数据格式
    {
      service: data.service_name,
      operation: data.operation_name,
      duration_ms: data.duration,
      success: data.status_code < 400,
      timestamp: data.timestamp
    }
  })
  StreamProcessor::add_aggregator(stream_processor, "window", 60)  // 60秒窗口聚合
  StreamProcessor::add_sink(stream_processor, "metrics.output")
  
  // 启动流处理器
  StreamProcessor::start(stream_processor)
  
  // 模拟实时数据流
  let real_time_data = [
    TelemetryData::new("api.service", "get_user", 120, 200, "2023-01-01T10:00:00Z"),
    TelemetryData::new("api.service", "create_user", 250, 201, "2023-01-01T10:00:01Z"),
    TelemetryData::new("api.service", "get_user", 95, 200, "2023-01-01T10:00:02Z"),
    TelemetryData::new("api.service", "update_user", 180, 200, "2023-01-01T10:00:03Z"),
    TelemetryData::new("api.service", "delete_user", 300, 204, "2023-01-01T10:00:04Z"),
    TelemetryData::new("api.service", "get_user", 85, 200, "2023-01-01T10:00:05Z"),
    TelemetryData::new("api.service", "invalid_operation", -10, 500, "2023-01-01T10:00:06Z"),  // 无效数据，应该被过滤
    TelemetryData::new("api.service", "get_user", 110, 200, "2023-01-01T10:00:07Z")
  ]
  
  // 处理数据流
  for data in real_time_data {
    StreamProcessor::process(stream_processor, data)
  }
  
  // 等待处理完成
  StreamProcessor::flush(stream_processor)
  
  // 验证处理结果
  let processed_count = StreamProcessor::get_processed_count(stream_processor)
  assert_eq(processed_count, 7)  // 应该处理了7个有效数据点（1个被过滤）
  
  // 验证聚合结果
  let aggregated_metrics = StreamProcessor::get_aggregated_metrics(stream_processor)
  assert_true(aggregated_metrics.contains("api.service.get_user"))
  
  let get_user_metrics = aggregated_metrics.get("api.service.get_user")
  assert_eq(get_user_metrics.count, 4)  // get_user被调用了4次
  assert_eq(get_user_metrics.avg_duration, 102.5)  // 平均延迟
  
  // 停止流处理器
  StreamProcessor::stop(stream_processor)
}

// 测试10: 遥测数据自动清理和归档
test "遥测数据自动清理和归档测试" {
  // 创建数据管理器
  let data_manager = DataRetentionManager::new()
  
  // 配置保留策略
  DataRetentionManager::set_retention_policy(data_manager, "spans", 7)      // Span保留7天
  DataRetentionManager::set_retention_policy(data_manager, "metrics", 30)  // 指标保留30天
  DataRetentionManager::set_retention_policy(data_manager, "logs", 14)     // 日志保留14天
  
  // 添加测试数据（带有不同时间戳）
  let current_time = "2023-01-15T00:00:00Z"  // 当前时间
  let old_data_time = "2023-01-01T00:00:00Z"  // 14天前的数据
  let very_old_data_time = "2022-12-01T00:00:00Z"  // 45天前的数据
  
  // 添加当前数据
  let current_span = Span::new("current.operation", Server, SpanContext::new("trace-current", "span-current", true, ""))
  Span::set_timestamp(current_span, current_time)
  DataRetentionManager::add_span(data_manager, current_span)
  
  let current_metric = Metric::new("current.metric", 100.0, current_time)
  DataRetentionManager::add_metric(data_manager, current_metric)
  
  let current_log = Log::new("Current log message", Info, current_time)
  DataRetentionManager::add_log(data_manager, current_log)
  
  // 添加旧数据
  let old_span = Span::new("old.operation", Server, SpanContext::new("trace-old", "span-old", true, ""))
  Span::set_timestamp(old_span, old_data_time)
  DataRetentionManager::add_span(data_manager, old_span)
  
  let old_metric = Metric::new("old.metric", 200.0, old_data_time)
  DataRetentionManager::add_metric(data_manager, old_metric)
  
  let old_log = Log::new("Old log message", Info, old_data_time)
  DataRetentionManager::add_log(data_manager, old_log)
  
  // 添加很旧的数据
  let very_old_span = Span::new("very.old.operation", Server, SpanContext::new("trace-very-old", "span-very-old", true, ""))
  Span::set_timestamp(very_old_span, very_old_data_time)
  DataRetentionManager::add_span(data_manager, very_old_span)
  
  let very_old_metric = Metric::new("very.old.metric", 300.0, very_old_data_time)
  DataRetentionManager::add_metric(data_manager, very_old_metric)
  
  let very_old_log = Log::new("Very old log message", Info, very_old_data_time)
  DataRetentionManager::add_log(data_manager, very_old_log)
  
  // 验证初始数据量
  assert_eq(DataRetentionManager::get_span_count(data_manager), 3)
  assert_eq(DataRetentionManager::get_metric_count(data_manager), 3)
  assert_eq(DataRetentionManager::get_log_count(data_manager), 3)
  
  // 执行清理（基于当前时间2023-01-15）
  let cleanup_result = DataRetentionManager::cleanup_expired_data(data_manager, current_time)
  
  // 验证清理结果
  assert_eq(cleanup_result.deleted_spans, 1)  // 应该删除1个span（45天前的）
  assert_eq(cleanup_result.deleted_metrics, 1)  // 应该删除1个metric（45天前的）
  assert_eq(cleanup_result.deleted_logs, 1)  // 应该删除1个log（45天前的）
  
  // 验证剩余数据
  assert_eq(DataRetentionManager::get_span_count(data_manager), 2)  // 剩余2个span
  assert_eq(DataRetentionManager::get_metric_count(data_manager), 2)  // 剩余2个metric
  assert_eq(DataRetentionManager::get_log_count(data_manager), 2)  // 剩余2个log
  
  // 验证归档数据
  let archived_data = DataRetentionManager::get_archived_data(data_manager)
  assert_eq(archived_data.spans.length(), 1)
  assert_eq(archived_data.metrics.length(), 1)
  assert_eq(archived_data.logs.length(), 1)
  
  // 验证归档数据的正确性
  let archived_span = archived_data.spans[0]
  assert_eq(Span::name(archived_span), "very.old.operation")
  
  let archived_metric = archived_data.metrics[0]
  assert_eq(Metric::name(archived_metric), "very.old.metric")
  assert_eq(Metric::value(archived_metric), 300.0)
  
  let archived_log = archived_data.logs[0]
  assert_eq(Log::message(archived_log), "Very old log message")
}