// 遥测数据质量保证测试用例
// 测试Azimuth遥测数据的质量保证机制

test "遥测数据完整性验证测试" {
  // 测试遥测数据的完整性验证
  let integrity_validator = IntegrityValidator::new()
  
  // 配置完整性规则
  IntegrityValidator::add_required_field(integrity_validator, "span", "trace_id")
  IntegrityValidator::add_required_field(integrity_validator, "span", "span_id")
  IntegrityValidator::add_required_field(integrity_validator, "span", "operation_name")
  IntegrityValidator::add_required_field(integrity_validator, "span", "start_time")
  IntegrityValidator::add_required_field(integrity_validator, "span", "end_time")
  
  IntegrityValidator::add_required_field(integrity_validator, "metric", "name")
  IntegrityValidator::add_required_field(integrity_validator, "metric", "value")
  IntegrityValidator::add_required_field(integrity_validator, "metric", "timestamp")
  
  IntegrityValidator::add_required_field(integrity_validator, "log", "timestamp")
  IntegrityValidator::add_required_field(integrity_validator, "log", "severity")
  IntegrityValidator::add_required_field(integrity_validator, "log", "message")
  
  // 创建测试数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "integrity.test")
  
  // 创建完整的span
  let complete_span = Tracer::start_span(tracer, "complete.operation")
  Span::set_attribute(complete_span, "user.id", "user-123")
  Span::set_attribute(complete_span, "operation.type", "test")
  Span::end(complete_span)
  
  // 验证完整span
  let span_validation_result = IntegrityValidator::validate_span(integrity_validator, complete_span)
  assert_true(ValidationResult::is_valid(span_validation_result))
  
  // 创建不完整的span（缺少必要字段）
  let incomplete_span = IncompleteSpan::new("incomplete.operation")
  // 故意不设置end_time
  
  // 验证不完整span
  let incomplete_span_result = IntegrityValidator::validate_span(integrity_validator, incomplete_span)
  assert_false(ValidationResult::is_valid(incomplete_span_result))
  
  // 检查缺失字段
  let missing_fields = ValidationResult::get_missing_fields(incomplete_span_result)
  assert_true(missing_fields.contains("end_time"))
  
  // 测试度量数据完整性
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "integrity.metrics.test")
  
  let counter = Meter::create_counter(meter, "test.operations", Some("Test operations"), Some("count"))
  Counter::add_with_attributes(counter, 5.0, [("operation.type", "test")])
  
  // 验证度量数据
  let metric_validation_result = IntegrityValidator::validate_metric(integrity_validator, counter)
  assert_true(ValidationResult::is_valid(metric_validation_result))
  
  // 测试日志数据完整性
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "integrity.logger")
  
  let complete_log = LogRecord::new(Info, "Complete log message")
  LogRecord::add_attribute(complete_log, "component", "test")
  LogRecord::add_attribute(complete_log, "operation.id", "op-123")
  
  // 验证完整日志
  let log_validation_result = IntegrityValidator::validate_log(integrity_validator, complete_log)
  assert_true(ValidationResult::is_valid(log_validation_result))
  
  // 创建不完整的日志（缺少严重性）
  let incomplete_log = IncompleteLogRecord::new("Incomplete log message")
  // 故意不设置严重性
  
  // 验证不完整日志
  let incomplete_log_result = IntegrityValidator::validate_log(integrity_validator, incomplete_log)
  assert_false(ValidationResult::is_valid(incomplete_log_result))
  
  // 检查缺失字段
  let log_missing_fields = ValidationResult::get_missing_fields(incomplete_log_result)
  assert_true(log_missing_fields.contains("severity"))
}

test "遥测数据一致性验证测试" {
  // 测试遥测数据的一致性验证
  let consistency_validator = ConsistencyValidator::new()
  
  // 配置一致性规则
  ConsistencyValidator::add_time_consistency_rule(consistency_validator, "span", "start_time", "end_time")
  ConsistencyValidator::add_value_range_rule(consistency_validator, "metric", "duration_ms", 0, 3600000) // 0-1小时
  ConsistencyValidator::add_format_rule(consistency_validator, "span", "trace_id", "^[a-f0-9]{32}$") // 32位十六进制
  ConsistencyValidator::add_format_rule(consistency_validator, "span", "span_id", "^[a-f0-9]{16}$") // 16位十六进制
  
  // 创建测试数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "consistency.test")
  
  // 创建时间一致的span
  let consistent_span = Tracer::start_span(tracer, "consistent.operation")
  Span::set_attribute(consistent_span, "trace_id", "1234567890abcdef1234567890abcdef")
  Span::set_attribute(consistent_span, "span_id", "1234567890abcdef")
  Time::sleep(100) // 确保 end_time > start_time
  Span::end(consistent_span)
  
  // 验证一致性span
  let consistent_result = ConsistencyValidator::validate_span(consistency_validator, consistent_span)
  assert_true(ValidationResult::is_valid(consistent_result))
  
  // 创建时间不一致的span（end_time < start_time）
  let inconsistent_span = InconsistentSpan::new("inconsistent.operation")
  InconsistentSpan::set_start_time(inconsistent_span, 1000)
  InconsistentSpan::set_end_time(inconsistent_span, 500) // 结束时间早于开始时间
  
  // 验证不一致span
  let inconsistent_result = ConsistencyValidator::validate_span(consistency_validator, inconsistent_span)
  assert_false(ValidationResult::is_valid(inconsistent_result))
  
  // 检查一致性错误
  let consistency_errors = ValidationResult::get_consistency_errors(inconsistent_result)
  assert_true(consistency_errors.contains("time_consistency_violation"))
  
  // 测试度量数据值范围一致性
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "consistency.metrics.test")
  
  let histogram = Meter::create_histogram(meter, "operation.duration", Some("Operation duration"), Some("ms"))
  
  // 记录有效值
  Histogram::record(histogram, 1000.0) // 1秒，在范围内
  Histogram::record(histogram, 60000.0) // 1分钟，在范围内
  
  // 验证有效度量
  let valid_metric_result = ConsistencyValidator::validate_metric(consistency_validator, histogram)
  assert_true(ValidationResult::is_valid(valid_metric_result))
  
  // 记录无效值
  Histogram::record(histogram, -100.0) // 负值，超出范围
  Histogram::record(histogram, 7200000.0) // 2小时，超出范围
  
  // 验证无效度量
  let invalid_metric_result = ConsistencyValidator::validate_metric(consistency_validator, histogram)
  assert_false(ValidationResult::is_valid(invalid_metric_result))
  
  // 检查值范围错误
  let range_errors = ValidationResult::get_range_errors(invalid_metric_result)
  assert_true(range_errors.contains("value_out_of_range"))
  
  // 测试ID格式一致性
  let format_span = Tracer::start_span(tracer, "format.test")
  Span::set_attribute(format_span, "trace_id", "invalid-trace-id") // 无效格式
  Span::set_attribute(format_span, "span_id", "invalid-span-id") // 无效格式
  Span::end(format_span)
  
  // 验证格式不一致span
  let format_result = ConsistencyValidator::validate_span(consistency_validator, format_span)
  assert_false(ValidationResult::is_valid(format_result))
  
  // 检查格式错误
  let format_errors = ValidationResult::get_format_errors(format_result)
  assert_true(format_errors.contains("trace_id_format_invalid"))
  assert_true(format_errors.contains("span_id_format_invalid"))
  
  // 测试跨数据类型一致性
  let cross_type_validator = CrossTypeValidator::new()
  
  // 添加跨类型一致性规则
  CrossTypeValidator::add_correlation_rule(cross_type_validator, "span.trace_id", "log.trace_id")
  CrossTypeValidator::add_correlation_rule(cross_type_validator, "span.user_id", "metric.user_id")
  
  // 创建相关的遥测数据
  let correlated_span = Tracer::start_span(tracer, "correlated.operation")
  Span::set_attribute(correlated_span, "trace_id", "abcdef1234567890abcdef1234567890")
  Span::set_attribute(correlated_span, "user_id", "user-12345")
  Span::end(correlated_span)
  
  let correlated_log = LogRecord::new(Info, "Correlated log message")
  LogRecord::add_attribute(correlated_log, "trace_id", "abcdef1234567890abcdef1234567890") // 相同的trace_id
  LogRecord::add_attribute(correlated_log, "user_id", "user-12345") // 相同的user_id
  
  let correlated_metric = Meter::create_histogram(meter, "user.operations", Some("User operations"), Some("count"))
  Histogram::record_with_attributes(correlated_metric, 1.0, [("user_id", "user-12345")]) // 相同的user_id
  
  // 验证跨类型一致性
  let telemetry_set = TelemetrySet::new()
  TelemetrySet::add_span(telemetry_set, correlated_span)
  TelemetrySet::add_log(telemetry_set, correlated_log)
  TelemetrySet::add_metric(telemetry_set, correlated_metric)
  
  let cross_type_result = CrossTypeValidator::validate_telemetry_set(cross_type_validator, telemetry_set)
  assert_true(ValidationResult::is_valid(cross_type_result))
  
  // 创建不一致的相关数据
  let uncorrelated_log = LogRecord::new(Info, "Uncorrelated log message")
  LogRecord::add_attribute(uncorrelated_log, "trace_id", "different-trace-id") // 不同的trace_id
  LogRecord::add_attribute(uncorrelated_log, "user_id", "user-67890") // 不同的user_id
  
  let uncorrelated_telemetry_set = TelemetrySet::new()
  TelemetrySet::add_span(uncorrelated_telemetry_set, correlated_span)
  TelemetrySet::add_log(uncorrelated_telemetry_set, uncorrelated_log)
  TelemetrySet::add_metric(uncorrelated_telemetry_set, correlated_metric)
  
  // 验证不一致的跨类型数据
  let uncorrelated_result = CrossTypeValidator::validate_telemetry_set(cross_type_validator, uncorrelated_telemetry_set)
  assert_false(ValidationResult::is_valid(uncorrelated_result))
  
  // 检查跨类型错误
  let cross_type_errors = ValidationResult::get_cross_type_errors(uncorrelated_result)
  assert_true(cross_type_errors.contains("trace_id_mismatch"))
  assert_true(cross_type_errors.contains("user_id_mismatch"))
}

test "遥测数据准确性验证测试" {
  // 测试遥测数据的准确性验证
  let accuracy_validator = AccuracyValidator::new()
  
  // 配置准确性规则
  AccuracyValidator::add_timestamp_accuracy_rule(accuracy_validator, 5000) // 5秒误差范围
  AccuracyValidator::add_counter_accuracy_rule(accuracy_validator, "monotonic") // 计数器必须单调递增
  AccuracyValidator::add_distribution_accuracy_rule(accuracy_validator, "statistical") // 分布数据统计准确性
  
  // 创建测试数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "accuracy.test")
  
  // 创建时间准确的span
  let accurate_span = Tracer::start_span(tracer, "accurate.operation")
  let start_time = Time::now()
  Time::sleep(1000) // 等待1秒
  Span::set_attribute(accurate_span, "manual_timestamp", start_time + 1000) // 准确的时间戳
  Span::end(accurate_span)
  
  // 验证时间准确性
  let timestamp_result = AccuracyValidator::validate_timestamp_accuracy(accuracy_validator, accurate_span)
  assert_true(ValidationResult::is_valid(timestamp_result))
  
  // 创建时间不准确的span
  let inaccurate_span = Tracer::start_span(tracer, "inaccurate.operation")
  let inaccurate_start_time = Time::now()
  Time::sleep(1000) // 等待1秒
  Span::set_attribute(inaccurate_span, "manual_timestamp", inaccurate_start_time + 10000) // 不准确的时间戳（偏差9秒）
  Span::end(inaccurate_span)
  
  // 验证时间不准确
  let inaccurate_timestamp_result = AccuracyValidator::validate_timestamp_accuracy(accuracy_validator, inaccurate_span)
  assert_false(ValidationResult::is_valid(inaccurate_timestamp_result))
  
  // 检查时间准确性错误
  let timestamp_errors = ValidationResult::get_timestamp_errors(inaccurate_timestamp_result)
  assert_true(timestamp_errors.contains("timestamp_deviation_exceeded"))
  
  // 测试计数器单调性
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "accuracy.metrics.test")
  
  let monotonic_counter = Meter::create_counter(meter, "mononic.operations", Some("Mononic operations"), Some("count"))
  
  // 单调递增计数器
  Counter::add(monotonic_counter, 1.0)
  Counter::add(monotonic_counter, 2.0)
  Counter::add(monotonic_counter, 3.0)
  
  // 验证单调性
  let monotonic_result = AccuracyValidator::validate_counter_monotonicity(accuracy_validator, monotonic_counter)
  assert_true(ValidationResult::is_valid(monotonic_result))
  
  // 创建非单调计数器（模拟）
  let non_monotonic_counter = NonMonotonicCounter::new("non.monotonic.operations")
  NonMonotonicCounter::add(non_monotonic_counter, 1.0)
  NonMonotonicCounter::add(non_monotonic_counter, 2.0)
  NonMonotonicCounter::subtract(non_monotonic_counter, 0.5) // 减少，破坏单调性
  
  // 验证非单调性
  let non_monotonic_result = AccuracyValidator::validate_counter_monotonicity(accuracy_validator, non_monotonic_counter)
  assert_false(ValidationResult::is_valid(non_monotonic_result))
  
  // 检查单调性错误
  let monotonic_errors = ValidationResult::get_monotonic_errors(non_monotonic_result)
  assert_true(monotonic_errors.contains("counter_not_monotonic"))
  
  // 测试分布数据统计准确性
  let distribution_histogram = Meter::create_histogram(meter, "response.time", Some("Response time"), Some("ms"))
  
  // 添加已知分布的数据
  let known_values = [10.0, 20.0, 30.0, 40.0, 50.0]
  for value in known_values {
    Histogram::record(distribution_histogram, value)
  }
  
  // 验证分布统计准确性
  let distribution_result = AccuracyValidator::validate_distribution_accuracy(accuracy_validator, distribution_histogram)
  assert_true(ValidationResult::is_valid(distribution_result))
  
  // 验证统计值
  let calculated_mean = Histogram::get_mean(distribution_histogram)
  let expected_mean = 30.0 // (10+20+30+40+50)/5
  assert_true(Math::abs(calculated_mean - expected_mean) < 0.01)
  
  let calculated_percentiles = Histogram::get_percentiles(distribution_histogram, [50.0, 90.0, 99.0])
  assert_true(Math::abs(calculated_percentiles[0] - 30.0) < 0.01) // 50th percentile should be 30
  assert_true(Math::abs(calculated_percentiles[1] - 50.0) < 0.01) // 90th percentile should be 50
  
  // 测试数据漂移检测
  let drift_detector = DataDriftDetector::new()
  DriftDetector::set_baseline_window(drift_detector, 1000) // 基线窗口1000个数据点
  DriftDetector::set_detection_window(drift_detector, 100) // 检测窗口100个数据点
  DriftDetector::set_drift_threshold(drift_detector, 0.1) // 10%漂移阈值
  
  // 创建基线数据
  let baseline_histogram = Meter::create_histogram(meter, "baseline.metric", Some("Baseline metric"), Some("count"))
  for i in 0..1000 {
    Histogram::record(baseline_histogram, NormalDistribution::sample(100.0, 10.0)) // 均值100，标准差10
  }
  
  // 设置基线
  DriftDetector::set_baseline(drift_detector, baseline_histogram)
  
  // 创建正常数据（无漂移）
  let normal_histogram = Meter::create_histogram(meter, "normal.metric", Some("Normal metric"), Some("count"))
  for i in 0..100 {
    Histogram::record(normal_histogram, NormalDistribution::sample(100.0, 10.0)) // 相同分布
  }
  
  // 检测漂移
  let no_drift_result = DriftDetector::detect_drift(drift_detector, normal_histogram)
  assert_false(DriftResult::is_drift_detected(no_drift_result))
  
  // 创建漂移数据（均值偏移）
  let drift_histogram = Meter::create_histogram(meter, "drift.metric", Some("Drift metric"), Some("count"))
  for i in 0..100 {
    Histogram::record(drift_histogram, NormalDistribution::sample(120.0, 10.0)) // 均值120，漂移20%
  }
  
  // 检测漂移
  let drift_result = DriftDetector::detect_drift(drift_detector, drift_histogram)
  assert_true(DriftResult::is_drift_detected(drift_result))
  
  // 检查漂移信息
  let drift_magnitude = DriftResult::get_drift_magnitude(drift_result)
  assert_true(drift_magnitude > 0.1) // 漂移应该超过阈值
  
  let drift_type = DriftResult::get_drift_type(drift_result)
  assert_eq(drift_type, "mean_shift")
}

test "遥测数据质量监控和报告测试" {
  // 测试遥测数据质量监控和报告
  let quality_monitor = DataQualityMonitor::new()
  
  // 配置质量监控
  QualityMonitor::set_monitoring_interval(quality_monitor, 10000) // 10秒监控间隔
  QualityMonitor::enable_real_time_monitoring(quality_monitor, true)
  
  // 配置质量指标
  QualityMonitor::add_quality_metric(quality_monitor, "completeness_score", 0.95) // 95%完整性阈值
  QualityMonitor::add_quality_metric(quality_monitor, "consistency_score", 0.90) // 90%一致性阈值
  QualityMonitor::add_quality_metric(quality_monitor, "accuracy_score", 0.95) // 95%准确性阈值
  QualityMonitor::add_quality_metric(quality_monitor, "timeliness_score", 0.80) // 80%及时性阈值
  
  // 启动质量监控
  QualityMonitor::start_monitoring(quality_monitor)
  
  // 创建测试数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "quality.monitor.test")
  
  // 创建高质量数据
  let high_quality_spans = []
  for i in 0..100 {
    let span = Tracer::start_span(tracer, "high.quality.operation.\{i}")
    Span::set_attribute(span, "trace_id", "1234567890abcdef1234567890abcdef")
    Span::set_attribute(span, "span_id", "1234567890abcdef")
    Span::set_attribute(span, "operation.type", "high-quality")
    Span::set_attribute(span, "user.id", "user-\{i}")
    Span::end(span)
    high_quality_spans = high_quality_spans.push(span)
  }
  
  // 添加到质量监控
  for span in high_quality_spans {
    QualityMonitor::add_telemetry_data(quality_monitor, span)
  }
  
  // 创建低质量数据
  let low_quality_spans = []
  for i in 0..20 {
    let span = IncompleteSpan::new("low.quality.operation.\{i}")
    // 故意缺少一些必要字段
    if i % 2 == 0 {
      Span::set_attribute(span, "trace_id", "1234567890abcdef1234567890abcdef")
    }
    if i % 3 == 0 {
      Span::set_attribute(span, "span_id", "1234567890abcdef")
    }
    low_quality_spans = low_quality_spans.push(span)
  }
  
  // 添加到质量监控
  for span in low_quality_spans {
    QualityMonitor::add_telemetry_data(quality_monitor, span)
  }
  
  // 等待质量监控处理数据
  Time::sleep(2000) // 等待2秒
  
  // 获取质量报告
  let quality_report = QualityMonitor::generate_report(quality_monitor)
  
  // 验证质量报告
  let completeness_score = QualityReport::get_completeness_score(quality_report)
  assert_true(completeness_score >= 0.80 && completeness_score <= 1.0) // 应该在80%-100%之间
  
  let consistency_score = QualityReport::get_consistency_score(quality_report)
  assert_true(consistency_score >= 0.80 && consistency_score <= 1.0) // 应该在80%-100%之间
  
  let accuracy_score = QualityReport::get_accuracy_score(quality_report)
  assert_true(accuracy_score >= 0.80 && accuracy_score <= 1.0) // 应该在80%-100%之间
  
  let overall_score = QualityReport::get_overall_score(quality_report)
  assert_true(overall_score >= 0.75 && overall_score <= 1.0) // 总体分数应该合理
  
  // 测试质量趋势分析
  let trend_analyzer = QualityTrendAnalyzer::new()
  TrendAnalyzer::set_analysis_window(trend_analyzer, 7) // 7天分析窗口
  TrendAnalyzer::set_trend_threshold(trend_analyzer, 0.05) // 5%趋势阈值
  
  // 添加历史质量数据
  for day in 0..7 {
    let daily_score = 0.90 - (day * 0.02) // 模拟质量下降趋势
    TrendAnalyzer::add_daily_score(trend_analyzer, day, daily_score)
  }
  
  // 分析趋势
  let trend_analysis = TrendAnalyzer::analyze_trend(trend_analyzer)
  
  // 验证趋势分析
  let trend_direction = TrendAnalysis::get_direction(trend_analysis)
  assert_eq(trend_direction, "declining") // 应该检测到下降趋势
  
  let trend_magnitude = TrendAnalysis::get_magnitude(trend_analysis)
  assert_true(trend_magnitude > 0.05) // 趋势应该超过阈值
  
  // 测试质量警报
  let quality_alert_manager = QualityAlertManager::new()
  AlertManager::add_alert_rule(quality_alert_manager, "completeness_below_threshold", 0.90)
  AlertManager::add_alert_rule(quality_alert_manager, "consistency_below_threshold", 0.85)
  AlertManager::add_alert_rule(quality_alert_manager, "accuracy_below_threshold", 0.90)
  AlertManager::add_alert_rule(quality_alert_manager, "quality_declining_trend", 0.05)
  
  // 检查警报
  let alerts = AlertManager::check_alerts(quality_alert_manager, quality_report, trend_analysis)
  
  // 验证警报
  let completeness_alert = AlertManager::get_alert(alerts, "completeness_below_threshold")
  if completeness_score < 0.90 {
    assert_true(Alert::is_triggered(completeness_alert))
  }
  
  let trend_alert = AlertManager::get_alert(alerts, "quality_declining_trend")
  assert_true(Alert::is_triggered(trend_alert)) // 趋势下降应该触发警报
  
  // 测试质量改进建议
  let improvement_advisor = QualityImprovementAdvisor::new()
  
  // 生成改进建议
  let suggestions = ImprovementAdvisor::generate_suggestions(improvement_advisor, quality_report, trend_analysis)
  
  // 验证改进建议
  let completeness_suggestion = ImprovementAdvisor::get_suggestion(suggestions, "completeness")
  if completeness_score < 0.95 {
    assert_true(Suggestion::is_present(completeness_suggestion))
    assert_true(Suggestion::get_description(completeness_suggestion).contains("required_fields"))
  }
  
  let trend_suggestion = ImprovementAdvisor::get_suggestion(suggestions, "trend")
  assert_true(Suggestion::is_present(trend_suggestion))
  assert_true(Suggestion::get_description(trend_suggestion).contains("declining_trend"))
  
  // 测试质量仪表板
  let quality_dashboard = QualityDashboard::new()
  Dashboard::configure(quality_dashboard, [
    ("completeness_gauge", "gauge"),
    ("consistency_gauge", "gauge"),
    ("accuracy_gauge", "gauge"),
    ("trend_chart", "line_chart"),
    ("alert_panel", "alert_panel")
  ])
  
  // 更新仪表板数据
  Dashboard::update_quality_scores(quality_dashboard, quality_report)
  Dashboard::update_trend_data(quality_dashboard, trend_analysis)
  Dashboard::update_alerts(quality_dashboard, alerts)
  
  // 生成仪表板报告
  let dashboard_report = Dashboard::generate_report(quality_dashboard)
  
  // 验证仪表板报告
  assert_true(DashboardReport::contains_quality_gauges(dashboard_report))
  assert_true(DashboardReport::contains_trend_chart(dashboard_report))
  assert_true(DashboardReport::contains_alert_panel(dashboard_report))
  
  // 停止质量监控
  QualityMonitor::stop_monitoring(quality_monitor)
}