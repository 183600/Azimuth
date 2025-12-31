// 遥测数据质量验证测试用例

test "telemetry_data_completeness_validation" {
  // 测试遥测数据完整性验证
  
  // 创建数据质量验证器
  let quality_validator = azimuth::telemetry::api::quality::DataQualityValidator::new()
    .with_required_fields(["trace_id", "span_id", "timestamp", "service_name"])
    .with_optional_fields(["operation_name", "duration", "status"])
    .with_field_validations({
      "trace_id": {"pattern": "^[a-f0-9]{32}$", "max_length": 32},
      "span_id": {"pattern": "^[a-f0-9]{16}$", "max_length": 16},
      "timestamp": {"min_value": 0, "max_value": 4102444800000},  // 合理的时间戳范围
      "duration": {"min_value": 0, "max_value": 3600000}  // 最大1小时
    })
  
  // 创建完整的遥测数据
  let complete_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_trace_id("1234567890abcdef1234567890abcdef")
    .with_span_id("1234567890abcdef")
    .with_timestamp(1640995200000)
    .with_service_name("user-service")
    .with_operation_name("authenticate")
    .with_duration(150)
    .with_status("success")
  
  // 验证完整数据
  let complete_validation = quality_validator.validate(complete_data)
  assert_eq(complete_validation.is_valid, true)
  assert_eq(complete_validation.errors.length(), 0)
  assert_eq(complete_validation.warnings.length(), 0)
  assert_eq(complete_validation.completeness_score, 1.0)
  
  // 创建缺少必需字段的数据
  let incomplete_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_trace_id("1234567890abcdef1234567890abcdef")
    // 缺少span_id
    .with_timestamp(1640995200000)
    .with_service_name("user-service")
  
  // 验证不完整数据
  let incomplete_validation = quality_validator.validate(incomplete_data)
  assert_eq(incomplete_validation.is_valid, false)
  assert_eq(incomplete_validation.errors.length() > 0, true)
  assert_eq(incomplete_validation.errors.any(|err| err.contains("span_id")), true)
  assert_eq(incomplete_validation.completeness_score < 1.0)
  
  // 创建部分字段无效的数据
  let invalid_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_trace_id("invalid-trace-id")  // 不符合格式要求
    .with_span_id("1234567890abcdef")
    .with_timestamp(-1000)  // 无效时间戳
    .with_service_name("user-service")
    .with_duration(-50)  // 无效持续时间
  
  // 验证无效数据
  let invalid_validation = quality_validator.validate(invalid_data)
  assert_eq(invalid_validation.is_valid, false)
  assert_eq(invalid_validation.errors.length() >= 3, true)  // 至少3个错误
  assert_eq(incomplete_validation.errors.any(|err| err.contains("trace_id")), true)
  assert_eq(incomplete_validation.errors.any(|err| err.contains("timestamp")), true)
  assert_eq(incomplete_validation.errors.any(|err| err.contains("duration")), true)
}

test "telemetry_data_consistency_validation" {
  // 测试遥测数据一致性验证
  
  // 创建一致性验证器
  let consistency_validator = azimuth::telemetry::api::quality::ConsistencyValidator::new()
    .with_temporal_consistency_rules([
      {"field": "timestamp", "rule": "monotonic_increasing"},  // 时间戳应该单调递增
      {"field": "duration", "rule": "non_negative"}           // 持续时间应该非负
    ])
    .with_logical_consistency_rules([
      {"rule": "end_time >= start_time"},
      {"rule": "parent_span_timestamp <= child_span_timestamp"},
      {"rule": "status_code matches duration_range"}  // 状态码与持续时间范围匹配
    ])
    .with_cross_field_consistency_rules([
      {"fields": ["service_name", "operation_name"], "rule": "valid_combination"},
      {"fields": ["http.method", "http.status_code"], "rule": "method_status_consistency"}
    ])
  
  // 创建一致的时间序列数据
  let consistent_time_series = [
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("consistent-trace")
      .with_span_id("span-001")
      .with_timestamp(1640995200000)
      .with_duration(100),
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("consistent-trace")
      .with_span_id("span-002")
      .with_timestamp(1640995300000)  // 10秒后
      .with_duration(150),
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("consistent-trace")
      .with_span_id("span-003")
      .with_timestamp(1640995400000)  // 20秒后
      .with_duration(80)
  ]
  
  // 验证时间序列一致性
  let time_series_validation = consistency_validator.validate_time_series(consistent_time_series)
  assert_eq(time_series_validation.is_consistent, true)
  assert_eq(time_series_validation.violations.length(), 0)
  
  // 创建不一致的时间序列数据
  let inconsistent_time_series = [
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("inconsistent-trace")
      .with_span_id("span-001")
      .with_timestamp(1640995400000)  // 较晚的时间
      .with_duration(100),
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("inconsistent-trace")
      .with_span_id("span-002")
      .with_timestamp(1640995300000)  // 较早的时间（违反单调递增）
      .with_duration(150),
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("inconsistent-trace")
      .with_span_id("span-003")
      .with_timestamp(1640995500000)  // 晚于第一个
      .with_duration(-50)  // 负持续时间
  ]
  
  // 验证不一致的时间序列
  let inconsistent_validation = consistency_validator.validate_time_series(inconsistent_time_series)
  assert_eq(inconsistent_validation.is_consistent, false)
  assert_eq(inconsistent_validation.violations.length() >= 2, true)
  assert_eq(inconsistent_validation.violations.any(|v| v.contains("monotonic_increasing")), true)
  assert_eq(inconsistent_validation.violations.any(|v| v.contains("non_negative")), true)
  
  // 创建跨字段一致性测试数据
  let cross_field_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_service_name("payment-service")
    .with_operation_name("process_payment")
    .with_attributes({
      "http.method": "POST",
      "http.status_code": "404",  // POST请求返回404不太合理
      "payment.amount": "99.99"
    })
  
  // 验证跨字段一致性
  let cross_field_validation = consistency_validator.validate_cross_field_consistency(cross_field_data)
  assert_eq(cross_field_validation.is_consistent, false)
  assert_eq(cross_field_validation.violations.length() > 0, true)
}

test "telemetry_data_accuracy_validation" {
  // 测试遥测数据准确性验证
  
  // 创建准确性验证器
  let accuracy_validator = azimuth::telemetry::api::quality::AccuracyValidator::new()
    .with_range_validations({
      "cpu_usage": {"min": 0.0, "max": 100.0},
      "memory_usage": {"min": 0.0, "max": "total_memory"},
      "response_time": {"min": 0.0, "max": 300000.0},  // 最大5分钟
      "error_rate": {"min": 0.0, "max": 1.0}
    })
    .with_statistical_validations({
      "outlier_detection": {"method": "iqr", "threshold": 1.5},
      "distribution_validation": {"method": "ks_test", "alpha": 0.05}
    })
    .with_business_rule_validations([
      {"rule": "payment_amount <= 10000", "description": "支付金额不应超过10000"},
      {"rule": "user_age >= 0 and user_age <= 150", "description": "用户年龄应在合理范围内"}
    ])
  
  // 创建准确的指标数据
  let accurate_metrics = [
    azimuth::telemetry::api::metrics::Metric::new("cpu_usage", 45.5, "percent"),
    azimuth::telemetry::api::metrics::Metric::new("memory_usage", 2048.0, "MB"),
    azimuth::telemetry::api::metrics::Metric::new("response_time", 150.0, "ms"),
    azimuth::telemetry::api::metrics::Metric::new("error_rate", 0.02, "ratio")
  ]
  
  // 验证准确指标
  let accurate_validation = accuracy_validator.validate_metrics(accurate_metrics)
  assert_eq(accurate_validation.is_accurate, true)
  assert_eq(accurate_validation.errors.length(), 0)
  
  // 创建不准确的指标数据
  let inaccurate_metrics = [
    azimuth::telemetry::api::metrics::Metric::new("cpu_usage", 150.0, "percent"),  // 超过100%
    azimuth::telemetry::api::metrics::Metric::new("memory_usage", -512.0, "MB"),  // 负值
    azimuth::telemetry::api::metrics::Metric::new("response_time", 500000.0, "ms"),  // 超过5分钟
    azimuth::telemetry::api::metrics::Metric::new("error_rate", 1.5, "ratio")  // 超过1.0
  ]
  
  // 验证不准确指标
  let inaccurate_validation = accuracy_validator.validate_metrics(inaccurate_metrics)
  assert_eq(inaccurate_validation.is_accurate, false)
  assert_eq(inaccurate_validation.errors.length() >= 4, true)
  
  // 创建包含异常值的数据集
  let data_with_outliers = [
    10.0, 12.0, 11.5, 13.0, 9.5, 10.5, 11.0, 12.5, 10.8, 11.2,
    150.0  // 明显的异常值
  ]
  
  // 检测异常值
  let outlier_detection = accuracy_validator.detect_outliers(data_with_outliers)
  assert_eq(outlier_detection.outliers_detected, true)
  assert_eq(outlier_detection.outlier_indices.length() >= 1, true)
  assert_eq(outlier_detection.outlier_indices.contains(10), true)  // 第11个数据点
  
  // 验证业务规则
  let business_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_attributes({
      "payment_amount": "15000.00",  // 超过限制
      "user_age": "200",             // 超过限制
      "transaction_id": "tx-12345"
    })
  
  let business_validation = accuracy_validator.validate_business_rules(business_data)
  assert_eq(business_validation.complies_with_rules, false)
  assert_eq(business_validation.violations.length() >= 2, true)
}

test "telemetry_data_timeliness_validation" {
  // 测试遥测数据及时性验证
  
  // 创建及时性验证器
  let timeliness_validator = azimuth::telemetry::api::quality::TimelinessValidator::new()
    .with_latency_thresholds({
      "critical": {"max_delay": 5000},      // 5秒
      "important": {"max_delay": 30000},    // 30秒
      "normal": {"max_delay": 300000}       // 5分钟
    })
    .with_freshness_requirements({
      "metrics": {"max_age": 60000},        // 1分钟
      "traces": {"max_age": 300000},        // 5分钟
      "logs": {"max_age": 1800000}          // 30分钟
    })
    .with_drift_detection({
      "clock_drift_threshold": 1000,        // 1秒
      "processing_delay_threshold": 10000   // 10秒
    })
  
  // 获取当前时间
  let current_time = azimuth::telemetry::api::time::Timestamp::now()
  
  // 创建及时的遥测数据
  let timely_data = [
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("timely-trace-1")
      .with_timestamp(current_time.subtract_millis(1000))  // 1秒前
      .with_priority("critical"),
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("timely-trace-2")
      .with_timestamp(current_time.subtract_millis(10000))  // 10秒前
      .with_priority("important"),
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("timely-trace-3")
      .with_timestamp(current_time.subtract_millis(100000))  // 100秒前
      .with_priority("normal")
  ]
  
  // 验证及时数据
  let timely_validation = timeliness_validator.validate_timeliness(timely_data, current_time)
  assert_eq(timely_validation.is_timely, true)
  assert_eq(timely_validation.violations.length(), 0)
  
  // 创建延迟的遥测数据
  let delayed_data = [
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("delayed-trace-1")
      .with_timestamp(current_time.subtract_millis(10000))  // 10秒前
      .with_priority("critical"),  // 超过5秒阈值
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("delayed-trace-2")
      .with_timestamp(current_time.subtract_millis(60000))  // 1分钟前
      .with_priority("important"),  // 超过30秒阈值
    azimuth::telemetry::api::trace::TraceData::new()
      .with_trace_id("delayed-trace-3")
      .with_timestamp(current_time.subtract_millis(400000))  // 400秒前
      .with_priority("normal")  // 超过5分钟阈值
  ]
  
  // 验证延迟数据
  let delayed_validation = timeliness_validator.validate_timeliness(delayed_data, current_time)
  assert_eq(delayed_validation.is_timely, false)
  assert_eq(delayed_validation.violations.length() >= 3, true)
  
  // 测试数据新鲜度
  let stale_metrics = azimuth::telemetry::api::metrics::Metric::new("cpu_usage", 50.0, "percent")
    .with_timestamp(current_time.subtract_millis(120000))  // 2分钟前，超过1分钟阈值
  
  let fresh_traces = azimuth::telemetry::api::trace::TraceData::new()
    .with_timestamp(current_time.subtract_millis(100000))  // 100秒前，在5分钟阈值内
  
  let freshness_validation = timeliness_validator.validate_freshness([
    {"type": "metrics", "data": stale_metrics},
    {"type": "traces", "data": fresh_traces}
  ], current_time)
  
  assert_eq(freshness_validation.metrics_fresh, false)
  assert_eq(freshness_validation.traces_fresh, true)
  
  // 测试时钟漂移检测
  let clock_drift_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_timestamp(current_time.add_millis(5000))  // 未来5秒（明显时钟漂移）
    .with_clock_offset(5000)  // 5秒偏移
  
  let drift_validation = timeliness_validator.detect_clock_drift(clock_drift_data, current_time)
  assert_eq(drift_validation.drift_detected, true)
  assert_eq(drift_validation.drift_amount > 1000, true)  // 超过1秒阈值
}

test "telemetry_data_quality_scoring" {
  // 测试遥测数据质量评分
  
  // 创建数据质量评分器
  let quality_scorer = azimuth::telemetry::api::quality::QualityScorer::new()
    .with_dimension_weights({
      "completeness": 0.25,
      "accuracy": 0.25,
      "consistency": 0.20,
      "timeliness": 0.15,
      "validity": 0.15
    })
    .with_quality_thresholds({
      "excellent": 0.9,
      "good": 0.7,
      "acceptable": 0.5,
      "poor": 0.3
    })
  
  // 创建高质量数据集
  let high_quality_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_trace_id("1234567890abcdef1234567890abcdef")
    .with_span_id("1234567890abcdef")
    .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().subtract_millis(1000))
    .with_service_name("high-quality-service")
    .with_operation_name("process_data")
    .with_duration(150)
    .with_status("success")
    .with_attributes({
      "user_id": "user123",
      "request_id": "req-456",
      "cpu_usage": "45.5",
      "memory_usage": "1024"
    })
  
  // 评估高质量数据
  let high_quality_score = quality_scorer.score_data_quality(high_quality_data)
  assert_eq(high_quality_score.overall_score >= 0.9, true)
  assert_eq(high_quality_score.quality_grade, "excellent")
  assert_eq(high_quality_score.dimension_scores.get("completeness").unwrap() >= 0.95, true)
  assert_eq(high_quality_score.dimension_scores.get("accuracy").unwrap() >= 0.95, true)
  
  // 创建中等质量数据集
  let medium_quality_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_trace_id("1234567890abcdef1234567890abcdef")
    .with_span_id("1234567890abcdef")
    .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().subtract_millis(60000))
    .with_service_name("medium-quality-service")
    // 缺少operation_name
    .with_duration(150)
    .with_status("success")
    .with_attributes({
      "user_id": "user123",
      "cpu_usage": "45.5"
      // 缺少一些属性
    })
  
  // 评估中等质量数据
  let medium_quality_score = quality_scorer.score_data_quality(medium_quality_data)
  assert_eq(medium_quality_score.overall_score >= 0.5, true)
  assert_eq(medium_quality_score.overall_score < 0.9, true)
  assert_eq(medium_quality_score.quality_grade == "good" || medium_quality_score.quality_grade == "acceptable", true)
  
  // 创建低质量数据集
  let low_quality_data = azimuth::telemetry::api::trace::TraceData::new()
    .with_trace_id("invalid")  // 格式错误
    // 缺少span_id
    .with_timestamp(azimuth::telemetry::api::time::Timestamp::now().subtract_millis(600000))  // 10分钟前
    .with_service_name("low-quality-service")
    .with_duration(-50)  // 无效值
    .with_status("unknown")  // 未知状态
    .with_attributes({
      "cpu_usage": "150.0"  // 超过100%
    })
  
  // 评估低质量数据
  let low_quality_score = quality_scorer.score_data_quality(low_quality_data)
  assert_eq(low_quality_score.overall_score < 0.5, true)
  assert_eq(low_quality_score.quality_grade == "poor" || low_quality_score.quality_grade == "unacceptable", true)
  
  // 批量数据质量评估
  let batch_data = [high_quality_data, medium_quality_data, low_quality_data]
  let batch_quality_report = quality_scorer.score_batch_quality(batch_data)
  
  assert_eq(batch_quality_report.total_items, 3)
  assert_eq(batch_quality_report.quality_distribution.get("excellent").unwrap() >= 1, true)
  assert_eq(batch_quality_report.quality_distribution.get("poor").unwrap() >= 1, true)
  assert_eq(batch_quality_report.average_score >= 0.3, true)
  assert_eq(batch_quality_report.average_score <= 1.0, true)
}