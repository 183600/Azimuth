// Azimuth Premium Quality and Validation Test Suite
// 高级质量和验证测试套件 - 专注于遥测数据质量和准确性验证

// Test 1: 遥测数据完整性验证
test "telemetry data integrity validation" {
  // 创建数据完整性验证器
  let integrity_validator = @azimuth.IntegrityValidator::new()
  
  // 创建测试数据集
  let test_trace_id = @azimuth.TraceId::generate()
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  let mut telemetry_batch = []
  for i in 0..=100 {
    let span_data = @azimuth.SpanData::new(
      test_trace_id,
      @azimuth.SpanId::generate(),
      if i > 0 { Some(@azimuth.SpanId::generate()) } else { None },
      "operation-#{i}",
      base_timestamp + (i * 1000000), // 每个span间隔1ms
      base_timestamp + (i * 1000000) + 500000, // 每个span持续0.5ms
      @azimuth.SpanStatus::OK,
      [
        ("service.name", @azimuth.StringValue("quality-test-service")),
        ("operation.index", @azimuth.IntValue(i)),
        ("operation.type", @azimuth.StringValue(if i % 3 == 0 { "critical" } else { "normal" }))
      ]
    )
    telemetry_batch = telemetry_batch.push(span_data)
  }
  
  // 执行完整性验证
  let validation_result = @azimuth.IntegrityValidator::validate_batch(integrity_validator, telemetry_batch)
  
  // 验证结果
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.total_spans, 101)
  assert_eq(validation_result.trace_consistency_errors, 0)
  assert_eq(validation_result.timestamp_ordering_errors, 0)
  assert_true(validation_result.completeness_score > 0.95)
  
  // 测试损坏数据的检测
  let corrupted_batch = telemetry_batch.map(fn(span) {
    if @azimuth.SpanData::operation_index(span) % 10 == 0 {
      // 故意损坏一些数据
      @azimuth.SpanData::with_invalid_timestamp(span, base_timestamp - 1000000)
    } else {
      span
    }
  })
  
  let corruption_validation = @azimuth.IntegrityValidator::validate_batch(integrity_validator, corrupted_batch)
  assert_false(corruption_validation.is_valid)
  assert_true(corruption_validation.timestamp_ordering_errors > 0)
  assert_true(corruption_validation.completeness_score < 0.9)
}

// Test 2: 数据准确性度量
test "data accuracy metrics and measurement" {
  // 创建准确性度量器
  let accuracy_measurer = @azimuth.AccuracyMeasurer::new()
  
  // 创建基准数据集（已知准确的数据）
  let baseline_dataset = @azimuth.BaselineDataset::new([
    @azimuth.MetricPoint::new("cpu.usage", 45.2, @azimuth.Time::now_unix_nanos()),
    @azimuth.MetricPoint::new("memory.usage", 67.8, @azimuth.Time::now_unix_nanos()),
    @azimuth.MetricPoint::new("disk.io", 123.4, @azimuth.Time::now_unix_nanos()),
    @azimuth.MetricPoint::new("network.throughput", 89.1, @azimuth.Time::now_unix_nanos())
  ])
  
  // 创建测试数据集（包含一些偏差）
  let test_dataset = @azimuth.TestDataset::new([
    @azimuth.MetricPoint::new("cpu.usage", 45.5, @azimuth.Time::now_unix_nanos()), // 0.3偏差
    @azimuth.MetricPoint::new("memory.usage", 66.2, @azimuth.Time::now_unix_nanos()), // 1.6偏差
    @azimuth.MetricPoint::new("disk.io", 123.1, @azimuth.Time::now_unix_nanos()), // 0.3偏差
    @azimuth.MetricPoint::new("network.throughput", 91.3, @azimuth.Time::now_unix_nanos()) // 2.2偏差
  ])
  
  // 计算准确性指标
  let accuracy_metrics = @azimuth.AccuracyMeasurer::calculate_metrics(accuracy_measurer, baseline_dataset, test_dataset)
  
  // 验证准确性指标
  assert_true(accuracy_metrics.mean_absolute_error >= 0.0)
  assert_true(accuracy_metrics.mean_absolute_error < 3.0)
  assert_true(accuracy_metrics.root_mean_square_error >= 0.0)
  assert_true(accuracy_metrics.root_mean_square_error < 5.0)
  assert_true(accuracy_metrics.mean_absolute_percentage_error >= 0.0)
  assert_true(accuracy_metrics.mean_absolute_percentage_error < 10.0)
  assert_true(accuracy_metrics.accuracy_score > 0.9)
  
  // 测试时间序列准确性
  let time_series_baseline = @azimuth.TimeSeries::new([
    (1000, 10.5),
    (2000, 12.3),
    (3000, 11.8),
    (4000, 13.2),
    (5000, 14.1)
  ])
  
  let time_series_test = @azimuth.TimeSeries::new([
    (1000, 10.7),
    (2000, 12.1),
    (3000, 12.0),
    (4000, 13.0),
    (5000, 14.3)
  ])
  
  let time_series_accuracy = @azimuth.AccuracyMeasurer::calculate_time_series_accuracy(
    accuracy_measurer, 
    time_series_baseline, 
    time_series_test
  )
  
  assert_true(time_series_accuracy.correlation_coefficient > 0.95)
  assert_true(time_series_accuracy.trend_accuracy > 0.8)
  assert_true(time_series_accuracy.seasonality_detection_accuracy > 0.7)
}

// Test 3: 数据一致性检查
test "data consistency checks across distributed systems" {
  // 创建分布式一致性检查器
  let consistency_checker = @azimuth.DistributedConsistencyChecker::new()
  
  // 模拟多个数据源的遥测数据
  let source_a_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("source", @azimuth.StringValue("service-a")),
      ("operation", @azimuth.StringValue("user-login")),
      ("user.id", @azimuth.StringValue("user-123")),
      ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos())),
      ("duration", @azimuth.IntValue(250)),
      ("status", @azimuth.StringValue("success"))
    ]
  )
  
  let source_b_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("source", @azimuth.StringValue("service-b")),
      ("operation", @azimuth.StringValue("user-authentication")),
      ("user.id", @azimuth.StringValue("user-123")),
      ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos() + 100)),
      ("auth.method", @azimuth.StringValue("oauth")),
      ("status", @azimuth.StringValue("approved"))
    ]
  )
  
  let source_c_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("source", @azimuth.StringValue("service-c")),
      ("operation", @azimuth.StringValue("user-profile-load")),
      ("user.id", @azimuth.StringValue("user-123")),
      ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos() + 200)),
      ("profile.loaded", @azimuth.BoolValue(true)),
      ("cache.hit", @azimuth.BoolValue(false))
    ]
  )
  
  // 创建数据关联规则
  let correlation_rules = [
    @azimuth.CorrelationRule::new("user.id", "exact_match"),
    @azimuth.CorrelationRule::new("timestamp", "temporal_sequence", 5000), // 5秒内的时间窗口
    @azimuth.CorrelationRule::new("status", "semantic_consistency")
  ]
  
  // 执行一致性检查
  let consistency_result = @azimuth.DistributedConsistencyChecker::check_consistency(
    consistency_checker,
    [source_a_data, source_b_data, source_c_data],
    correlation_rules
  )
  
  // 验证一致性结果
  assert_true(consistency_result.overall_consistency_score > 0.8)
  assert_eq(consistency_result.correlated_events, 3)
  assert_true(consistency_result.temporal_violations == 0)
  assert_true(consistency_result.semantic_violations == 0)
  
  // 测试不一致数据的检测
  let inconsistent_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("source", @azimuth.StringValue("service-d")),
      ("operation", @azimuth.StringValue("user-logout")),
      ("user.id", @azimuth.StringValue("user-456")), // 不同的用户ID
      ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos() - 10000)), // 时间戳超出范围
      ("status", @azimuth.StringValue("failed"))
    ]
  )
  
  let inconsistent_result = @azimuth.DistributedConsistencyChecker::check_consistency(
    consistency_checker,
    [source_a_data, source_b_data, source_c_data, inconsistent_data],
    correlation_rules
  )
  
  assert_true(inconsistent_result.overall_consistency_score < consistency_result.overall_consistency_score)
  assert_true(inconsistent_result.correlation_violations > 0)
  assert_true(inconsistent_result.temporal_violations > 0)
}

// Test 4: 数据质量评分系统
test "data quality scoring system" {
  // 创建数据质量评分器
  let quality_scorer = @azimuth.DataQualityScorer::new()
  
  // 配置质量评分权重
  let scoring_weights = @azimuth.ScoringWeights::new([
    ("completeness", 0.25),
    ("accuracy", 0.20),
    ("consistency", 0.15),
    ("timeliness", 0.15),
    ("validity", 0.15),
    ("uniqueness", 0.10)
  ])
  
  @azimuth.DataQualityScorer::configure_weights(quality_scorer, scoring_weights)
  
  // 创建测试数据集
  let test_dataset = @azimuth.QualityTestDataset::new([
    // 高质量数据
    @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("service.name", @azimuth.StringValue("payment-service")),
        ("operation", @azimuth.StringValue("process-payment")),
        ("user.id", @azimuth.StringValue("user-001")),
        ("amount", @azimuth.FloatValue(99.99)),
        ("currency", @azimuth.StringValue("USD")),
        ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos())),
        ("status", @azimuth.StringValue("success"))
      ]
    ),
    // 中等质量数据（缺少一些字段）
    @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("service.name", @azimuth.StringValue("payment-service")),
        ("operation", @azimuth.StringValue("process-payment")),
        ("user.id", @azimuth.StringValue("user-002")),
        ("amount", @azimuth.FloatValue(49.99)),
        ("status", @azimuth.StringValue("pending"))
      ]
    ),
    // 低质量数据（格式错误和不一致）
    @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("service.name", @azimuth.StringValue("")),
        ("operation", @azimuth.StringValue("process-payment")),
        ("user.id", @azimuth.StringValue("")),
        ("amount", @azimuth.FloatValue(-10.0)),
        ("currency", @azimuth.StringValue("INVALID")),
        ("timestamp", @azimuth.IntValue(0)),
        ("status", @azimuth.StringValue("unknown"))
      ]
    )
  ])
  
  // 计算质量评分
  let quality_scores = @azimuth.DataQualityScorer::score_dataset(quality_scorer, test_dataset)
  
  // 验证评分结果
  assert_eq(quality_scores.total_records, 3)
  assert_true(quality_scores.overall_quality_score >= 0.0)
  assert_true(quality_scores.overall_quality_score <= 1.0)
  
  // 验证各维度评分
  assert_true(quality_scores.dimension_scores.get("completeness") >= 0.0)
  assert_true(quality_scores.dimension_scores.get("completeness") <= 1.0)
  assert_true(quality_scores.dimension_scores.get("accuracy") >= 0.0)
  assert_true(quality_scores.dimension_scores.get("accuracy") <= 1.0)
  assert_true(quality_scores.dimension_scores.get("consistency") >= 0.0)
  assert_true(quality_scores.dimension_scores.get("consistency") <= 1.0)
  
  // 验证质量分级
  match quality_scores.quality_grade {
    @azimuth.QualityGrade::Excellent => assert_true(quality_scores.overall_quality_score >= 0.9)
    @azimuth.QualityGrade::Good => assert_true(quality_scores.overall_quality_score >= 0.7 && quality_scores.overall_quality_score < 0.9)
    @azimuth.QualityGrade::Fair => assert_true(quality_scores.overall_quality_score >= 0.5 && quality_scores.overall_quality_score < 0.7)
    @azimuth.QualityGrade::Poor => assert_true(quality_scores.overall_quality_score < 0.5)
  }
  
  // 测试质量改进建议
  let improvement_suggestions = @azimuth.DataQualityScorer::get_improvement_suggestions(quality_scorer, quality_scores)
  assert_true(improvement_suggestions.length() > 0)
  
  // 验证建议包含具体的问题和解决方案
  let has_completeness_suggestion = improvement_suggestions.any(fn(suggestion) {
    suggestion.dimension == "completeness"
  })
  assert_true(has_completeness_suggestion)
}

// Test 5: 异常数据检测和清洗
test "anomaly detection and data cleaning" {
  // 创建异常检测器
  let anomaly_detector = @azimuth.AnomalyDetector::new()
  
  // 配置检测算法
  let detection_config = @azimuth.AnomalyDetectionConfig::new([
    @azimuth.DetectionAlgorithm::StatisticalOutlier,
    @azimuth.DetectionAlgorithm::TimeSeriesAnomaly,
    @azimuth.DetectionAlgorithm::PatternViolation
  ])
  
  @azimuth.AnomalyDetector::configure(anomaly_detector, detection_config)
  
  // 创建包含异常的测试数据
  let normal_data_points = [
    @azimuth.MetricPoint::new("response.time", 120.5, @azimuth.Time::now_unix_nanos()),
    @azimuth.MetricPoint::new("response.time", 115.3, @azimuth.Time::now_unix_nanos() + 1000),
    @azimuth.MetricPoint::new("response.time", 122.1, @azimuth.Time::now_unix_nanos() + 2000),
    @azimuth.MetricPoint::new("response.time", 118.7, @azimuth.Time::now_unix_nanos() + 3000),
    @azimuth.MetricPoint::new("response.time", 121.4, @azimuth.Time::now_unix_nanos() + 4000)
  ]
  
  let anomaly_data_points = [
    @azimuth.MetricPoint::new("response.time", 5000.0, @azimuth.Time::now_unix_nanos() + 5000), // 极端值
    @azimuth.MetricPoint::new("response.time", -50.0, @azimuth.Time::now_unix_nanos() + 6000),  // 负值
    @azimuth.MetricPoint::new("response.time", 0.0, @azimuth.Time::now_unix_nanos() + 7000)     // 零值异常
  ]
  
  let all_data_points = normal_data_points.concat(anomaly_data_points)
  
  // 执行异常检测
  let detection_result = @azimuth.AnomalyDetector::detect_anomalies(anomaly_detector, all_data_points)
  
  // 验证检测结果
  assert_eq(detection_result.total_points, 8)
  assert_true(detection_result.anomaly_count >= 2)
  assert_true(detection_result.normal_count >= 5)
  assert_eq(detection_result.anomaly_count + detection_result.normal_count, detection_result.total_points)
  
  // 验证异常详情
  let anomalies = detection_result.anomalies
  assert_true(anomalies.length() >= 2)
  
  // 检查是否检测到了极端值
  let has_extreme_value = anomalies.any(fn(anomaly) {
    anomaly.anomaly_type == @azimuth.AnomalyType::StatisticalOutlier && 
    anomaly.value > 1000.0
  })
  assert_true(has_extreme_value)
  
  // 检查是否检测到了负值
  let has_negative_value = anomalies.any(fn(anomaly) {
    anomaly.anomaly_type == @azimuth.AnomalyType::PatternViolation && 
    anomaly.value < 0.0
  })
  assert_true(has_negative_value)
  
  // 测试数据清洗
  let data_cleaner = @azimuth.DataCleaner::new()
  let cleaning_config = @azimuth.CleaningConfig::new([
    @azimuth.CleaningStrategy::RemoveAnomalies,
    @azimuth.CleaningStrategy::InterpolateMissing,
    @azimuth.CleaningStrategy::SmoothOutliers
  ])
  
  @azimuth.DataCleaner::configure(data_cleaner, cleaning_config)
  
  let cleaned_data = @azimuth.DataCleaner::clean_dataset(data_cleaner, all_data_points, detection_result.anomalies)
  
  // 验证清洗结果
  assert_true(cleaned_data.length() < all_data_points.length())
  assert_true(cleaned_data.length() >= normal_data_points.length())
  
  // 验证清洗后的数据不再包含异常
  let cleaned_detection = @azimuth.AnomalyDetector::detect_anomalies(anomaly_detector, cleaned_data)
  assert_true(cleaned_detection.anomaly_count < detection_result.anomaly_count)
  
  // 验证数据统计特性
  let original_stats = @azimuth.DataStatistics::calculate(all_data_points)
  let cleaned_stats = @azimuth.DataStatistics::calculate(cleaned_data)
  
  assert_true(cleaned_stats.mean >= original_stats.min && cleaned_stats.mean <= original_stats.max)
  assert_true(cleaned_stats.standard_deviation <= original_stats.standard_deviation)
}