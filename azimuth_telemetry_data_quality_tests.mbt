// Azimuth Telemetry Data Quality Validation Tests
// 遥测数据质量验证测试用例，专注于数据完整性、一致性和准确性验证

// Test 1: 遥测数据完整性验证
test "telemetry data integrity validation" {
  // 创建数据质量验证器
  let validator = azimuth::quality::Validator::new()
  
  // 配置完整性规则
  azimuth::quality::add_integrity_rule(validator, "trace_id", "required", true)
  azimuth::quality::add_integrity_rule(validator, "span_id", "required", true)
  azimuth::quality::add_integrity_rule(validator, "operation_name", "required", true)
  azimuth::quality::add_integrity_rule(validator, "start_time", "required", true)
  azimuth::quality::add_integrity_rule(validator, "end_time", "required", true)
  
  // 创建有效遥测数据
  let valid_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(valid_data, "trace_id", "abc123def45678901234567890123456")
  azimuth::quality::set_attribute(valid_data, "span_id", "1234567890abcdef")
  azimuth::quality::set_attribute(valid_data, "operation_name", "get_user")
  azimuth::quality::set_attribute(valid_data, "start_time", "1640995200000000")
  azimuth::quality::set_attribute(valid_data, "end_time", "1640995201000000")
  
  // 验证有效数据
  let valid_result = azimuth::quality::validate_integrity(validator, valid_data)
  assert_true(azimuth::quality::is_valid(valid_result))
  
  // 创建无效遥测数据（缺少必需字段）
  let invalid_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(invalid_data, "trace_id", "def456abc12378901234567890123456")
  azimuth::quality::set_attribute(invalid_data, "span_id", "234567890abcdef1")
  // 缺少 operation_name, start_time, end_time
  
  // 验证无效数据
  let invalid_result = azimuth::quality::validate_integrity(validator, invalid_data)
  assert_false(azimuth::quality::is_valid(invalid_result))
  
  // 检查缺失字段
  let missing_fields = azimuth::quality::get_missing_fields(invalid_result)
  assert_eq(missing_fields.length(), 3)
  assert_true(missing_fields.contains("operation_name"))
  assert_true(missing_fields.contains("start_time"))
  assert_true(missing_fields.contains("end_time"))
}

// Test 2: 遥测数据格式验证
test "telemetry data format validation" {
  // 创建格式验证器
  let validator = azimuth::quality::FormatValidator::new()
  
  // 配置格式规则
  azimuth::quality::add_format_rule(validator, "trace_id", "pattern", "^[a-f0-9]{32}$")
  azimuth::quality::add_format_rule(validator, "span_id", "pattern", "^[a-f0-9]{16}$")
  azimuth::quality::add_format_rule(validator, "timestamp", "type", "integer")
  azimuth::quality::add_format_rule(validator, "duration_ms", "type", "integer")
  azimuth::quality::add_format_rule(validator, "status_code", "range", "200..599")
  
  // 创建符合格式的数据
  let valid_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(valid_data, "trace_id", "abc123def45678901234567890123456")
  azimuth::quality::set_attribute(valid_data, "span_id", "1234567890abcdef")
  azimuth::quality::set_attribute(valid_data, "timestamp", "1640995200000000")
  azimuth::quality::set_attribute(valid_data, "duration_ms", "1000")
  azimuth::quality::set_attribute(valid_data, "status_code", "200")
  
  // 验证有效数据
  let valid_result = azimuth::quality::validate_format(validator, valid_data)
  assert_true(azimuth::quality::is_valid(valid_result))
  
  // 创建不符合格式的数据
  let invalid_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(invalid_data, "trace_id", "invalid-trace-id") // 不符合格式
  azimuth::quality::set_attribute(invalid_data, "span_id", "123") // 太短
  azimuth::quality::set_attribute(invalid_data, "timestamp", "not-a-number") // 不是整数
  azimuth::quality::set_attribute(invalid_data, "duration_ms", "1000.5") // 不是整数
  azimuth::quality::set_attribute(invalid_data, "status_code", "999") // 超出范围
  
  // 验证无效数据
  let invalid_result = azimuth::quality::validate_format(validator, invalid_data)
  assert_false(azimuth::quality::is_valid(invalid_result))
  
  // 检查格式错误
  let format_errors = azimuth::quality::get_format_errors(invalid_result)
  assert_eq(format_errors.length(), 5)
}

// Test 3: 遥测数据一致性验证
test "telemetry data consistency validation" {
  // 创建一致性验证器
  let validator = azimuth::quality::ConsistencyValidator::new()
  
  // 配置一致性规则
  azimuth::quality::add_consistency_rule(validator, "time_order", "start_time < end_time")
  azimuth::quality::add_consistency_rule(validator, "duration_calculation", "duration_ms = (end_time - start_time) / 1000000")
  azimuth::quality::add_consistency_rule(validator, "parent_child", "parent_span_id != span_id")
  
  // 创建一致的数据
  let consistent_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(consistent_data, "start_time", "1640995200000000")
  azimuth::quality::set_attribute(consistent_data, "end_time", "1640995201000000")
  azimuth::quality::set_attribute(consistent_data, "duration_ms", "1000")
  azimuth::quality::set_attribute(consistent_data, "span_id", "1234567890abcdef")
  azimuth::quality::set_attribute(consistent_data, "parent_span_id", "0123456789abcdef")
  
  // 验证一致性数据
  let consistent_result = azimuth::quality::validate_consistency(validator, consistent_data)
  assert_true(azimuth::quality::is_valid(consistent_result))
  
  // 创建不一致的数据
  let inconsistent_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(inconsistent_data, "start_time", "1640995201000000") // 开始时间晚于结束时间
  azimuth::quality::set_attribute(inconsistent_data, "end_time", "1640995200000000")
  azimuth::quality::set_attribute(inconsistent_data, "duration_ms", "2000") // 持续时间不匹配
  azimuth::quality::set_attribute(inconsistent_data, "span_id", "1234567890abcdef")
  azimuth::quality::set_attribute(inconsistent_data, "parent_span_id", "1234567890abcdef") // 父span ID等于自身span ID
  
  // 验证不一致数据
  let inconsistent_result = azimuth::quality::validate_consistency(validator, inconsistent_data)
  assert_false(azimuth::quality::is_valid(inconsistent_result))
  
  // 检查一致性错误
  let consistency_errors = azimuth::quality::get_consistency_errors(inconsistent_result)
  assert_eq(consistency_errors.length(), 3)
}

// Test 4: 遥测数据异常值检测
test "telemetry data anomaly detection" {
  // 创建异常检测器
  let detector = azimuth::quality::AnomalyDetector::new()
  
  // 配置异常检测规则
  azimuth::quality::add_anomaly_rule(detector, "response_time", "statistical", 3.0) // 3个标准差
  azimuth::quality::add_anomaly_rule(detector, "error_rate", "threshold", 0.5) // 50%错误率阈值
  azimuth::quality::add_anomaly_rule(detector, "memory_usage", "range", "0..100") // 0-100%范围
  
  // 创建正常数据集
  let normal_data = [
    ("response_time", "150"), ("response_time", "200"), ("response_time", "180"),
    ("response_time", "220"), ("response_time", "190"), ("response_time", "210"),
    ("error_rate", "0.01"), ("error_rate", "0.02"), ("error_rate", "0.015"),
    ("memory_usage", "45"), ("memory_usage", "50"), ("memory_usage", "48")
  ]
  
  // 训练异常检测模型
  for (metric, value) in normal_data {
    azimuth::quality::train_detector(detector, metric, value)
  }
  
  // 测试正常数据
  let test_data1 = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(test_data1, "response_time", "195")
  azimuth::quality::set_attribute(test_data1, "error_rate", "0.018")
  azimuth::quality::set_attribute(test_data1, "memory_usage", "52")
  
  let normal_result = azimuth::quality::detect_anomalies(detector, test_data1)
  assert_false(azimuth::quality::has_anomalies(normal_result))
  
  // 测试异常数据
  let test_data2 = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(test_data2, "response_time", "5000") // 异常高的响应时间
  azimuth::quality::set_attribute(test_data2, "error_rate", "0.8") // 异常高的错误率
  azimuth::quality::set_attribute(test_data2, "memory_usage", "150") // 超出范围的内存使用
  
  let anomaly_result = azimuth::quality::detect_anomalies(detector, test_data2)
  assert_true(azimuth::quality::has_anomalies(anomaly_result))
  
  // 检查异常指标
  let anomaly_metrics = azimuth::quality::get_anomaly_metrics(anomaly_result)
  assert_eq(anomaly_metrics.length(), 3)
  assert_true(anomaly_metrics.contains("response_time"))
  assert_true(anomaly_metrics.contains("error_rate"))
  assert_true(anomaly_metrics.contains("memory_usage"))
}

// Test 5: 遥测数据重复检测
test "telemetry data duplication detection" {
  // 创建重复检测器
  let detector = azimuth::quality::DuplicationDetector::new()
  
  // 配置重复检测键
  azimuth::quality::set_deduplication_key(detector, ["trace_id", "span_id", "start_time"])
  
  // 创建原始数据
  let original_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(original_data, "trace_id", "abc123def45678901234567890123456")
  azimuth::quality::set_attribute(original_data, "span_id", "1234567890abcdef")
  azimuth::quality::set_attribute(original_data, "start_time", "1640995200000000")
  azimuth::quality::set_attribute(original_data, "operation_name", "get_user")
  
  // 添加到检测器
  azimuth::quality::add_data(detector, original_data)
  
  // 创建相同的数据（完全重复）
  let duplicate_data1 = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(duplicate_data1, "trace_id", "abc123def45678901234567890123456")
  azimuth::quality::set_attribute(duplicate_data1, "span_id", "1234567890abcdef")
  azimuth::quality::set_attribute(duplicate_data1, "start_time", "1640995200000000")
  azimuth::quality::set_attribute(duplicate_data1, "operation_name", "get_user")
  
  // 检测重复
  let is_duplicate1 = azimuth::quality::is_duplicate(detector, duplicate_data1)
  assert_true(is_duplicate1)
  
  // 创建部分相同的数据（非完全重复）
  let similar_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(similar_data, "trace_id", "abc123def45678901234567890123456")
  azimuth::quality::set_attribute(similar_data, "span_id", "1234567890abcdef")
  azimuth::quality::set_attribute(similar_data, "start_time", "1640995200000001") // 不同的时间戳
  azimuth::quality::set_attribute(similar_data, "operation_name", "get_user")
  
  // 检测重复
  let is_duplicate2 = azimuth::quality::is_duplicate(detector, similar_data)
  assert_false(is_duplicate2)
  
  // 获取重复统计
  let duplicate_stats = azimuth::quality::get_duplicate_statistics(detector)
  assert_eq(azimuth::quality::get_total_processed(duplicate_stats), 2)
  assert_eq(azimuth::quality::get_duplicates_found(duplicate_stats), 1)
}

// Test 6: 遥测数据质量评分
test "telemetry data quality scoring" {
  // 创建质量评分器
  let scorer = azimuth::quality::Scorer::new()
  
  // 配置评分权重
  azimuth::quality::set_weight(scorer, "completeness", 0.3) // 完整性权重30%
  azimuth::quality::set_weight(scorer, "accuracy", 0.3) // 准确性权重30%
  azimuth::quality::set_weight(scorer, "consistency", 0.2) // 一致性权重20%
  azimuth::quality::set_weight(scorer, "timeliness", 0.2) // 及时性权重20%
  
  // 创建高质量数据
  let high_quality_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(high_quality_data, "trace_id", "abc123def45678901234567890123456")
  azimuth::quality::set_attribute(high_quality_data, "span_id", "1234567890abcdef")
  azimuth::quality::set_attribute(high_quality_data, "operation_name", "get_user")
  azimuth::quality::set_attribute(high_quality_data, "start_time", "1640995200000000")
  azimuth::quality::set_attribute(high_quality_data, "end_time", "1640995201000000")
  azimuth::quality::set_attribute(high_quality_data, "status_code", "200")
  azimuth::quality::set_attribute(high_quality_data, "timestamp", "1640995200000000") // 当前时间戳
  
  // 评分高质量数据
  let high_quality_score = azimuth::quality::calculate_score(scorer, high_quality_data)
  assert_true(high_quality_score >= 0.9) // 高质量数据得分应该大于90%
  
  // 创建低质量数据
  let low_quality_data = azimuth::quality::TelemetryData::new()
  azimuth::quality::set_attribute(low_quality_data, "trace_id", "abc123def45678901234567890123456")
  // 缺少 span_id, operation_name
  azimuth::quality::set_attribute(low_quality_data, "start_time", "1640995201000000") // 开始时间晚于结束时间
  azimuth::quality::set_attribute(low_quality_data, "end_time", "1640995200000000")
  azimuth::quality::set_attribute(low_quality_data, "status_code", "200")
  azimuth::quality::set_attribute(low_quality_data, "timestamp", "1640995000000000") // 旧时间戳
  
  // 评分低质量数据
  let low_quality_score = azimuth::quality::calculate_score(scorer, low_quality_data)
  assert_true(low_quality_score <= 0.6) // 低质量数据得分应该小于60%
  
  // 获取详细评分
  let detailed_score = azimuth::quality::get_detailed_score(scorer, low_quality_data)
  let completeness_score = azimuth::quality::get_completeness_score(detailed_score)
  let accuracy_score = azimuth::quality::get_accuracy_score(detailed_score)
  let consistency_score = azimuth::quality::get_consistency_score(detailed_score)
  let timeliness_score = azimuth::quality::get_timeliness_score(detailed_score)
  
  assert_true(completeness_score < 1.0) // 完整性得分应该小于100%
  assert_true(accuracy_score < 1.0) // 准确性得分应该小于100%
  assert_true(consistency_score < 1.0) // 一致性得分应该小于100%
  assert_true(timeliness_score < 1.0) // 及时性得分应该小于100%
}

// Test 7: 遥测数据质量趋势分析
test "telemetry data quality trend analysis" {
  // 创建趋势分析器
  let analyzer = azimuth::quality::TrendAnalyzer::new()
  
  // 模拟不同时间点的数据质量
  let quality_scores = [
    (1640995200000, 0.95), // 高质量
    (1640995260000, 0.92), // 略有下降
    (1640995320000, 0.88), // 继续下降
    (1640995380000, 0.85), // 继续下降
    (1640995440000, 0.87), // 略有回升
    (1640995500000, 0.90), // 继续回升
    (1640995560000, 0.93), // 接近初始水平
    (1640995620000, 0.96)  // 超过初始水平
  ]
  
  // 添加质量数据点
  for (timestamp, score) in quality_scores {
    azimuth::quality::add_quality_point(analyzer, timestamp, score)
  }
  
  // 分析趋势
  let trend = azimuth::quality::analyze_trend(analyzer)
  let trend_direction = azimuth::quality::get_trend_direction(trend)
  let trend_strength = azimuth::quality::get_trend_strength(trend)
  
  // 验证趋势分析结果
  assert_eq(trend_direction, "improving") // 整体趋势应该是改善的
  assert_true(trend_strength > 0.5) // 趋势强度应该大于50%
  
  // 获取质量变化率
  let change_rate = azimuth::quality::get_change_rate(analyzer, 3600) // 1小时内的变化率
  assert_true(change_rate > 0) // 变化率应该是正数
  
  // 预测质量趋势
  let predicted_score = azimuth::quality::predict_quality(analyzer, 1640995680000) // 预测下一个时间点
  assert_true(predicted_score > 0.9) // 预测质量应该保持在高水平
}

// Test 8: 遥测数据质量报告生成
test "telemetry data quality report generation" {
  // 创建报告生成器
  let reporter = azimuth::quality::Reporter::new()
  
  // 配置报告内容
  azimuth::quality::include_completeness_metrics(reporter, true)
  azimuth::quality::include_accuracy_metrics(reporter, true)
  azimuth::quality::include_consistency_metrics(reporter, true)
  azimuth::quality::include_anomaly_summary(reporter, true)
  azimuth::quality::include_trend_analysis(reporter, true)
  
  // 添加测试数据
  let test_data = [
    ("trace1", "valid", 0.95),
    ("trace2", "invalid", 0.45),
    ("trace3", "valid", 0.88),
    ("trace4", "anomaly", 0.72),
    ("trace5", "valid", 0.91)
  ]
  
  for (trace_id, status, score) in test_data {
    let data = azimuth::quality::TelemetryData::new()
    azimuth::quality::set_attribute(data, "trace_id", trace_id)
    azimuth::quality::set_attribute(data, "status", status)
    azimuth::quality::set_quality_score(data, score)
    azimuth::quality::add_to_report(reporter, data)
  }
  
  // 生成质量报告
  let report = azimuth::quality::generate_report(reporter)
  
  // 验证报告内容
  let total_traces = azimuth::quality::report::get_total_traces(report)
  let valid_traces = azimuth::quality::report::get_valid_traces(report)
  let invalid_traces = azimuth::quality::report::get_invalid_traces(report)
  let anomaly_traces = azimuth::quality::report::get_anomaly_traces(report)
  let average_quality = azimuth::quality::report::get_average_quality(report)
  
  assert_eq(total_traces, 5)
  assert_eq(valid_traces, 3)
  assert_eq(invalid_traces, 1)
  assert_eq(anomaly_traces, 1)
  assert_true(average_quality >= 0.7 && average_quality <= 0.8)
  
  // 生成报告摘要
  let summary = azimuth::quality::report::generate_summary(report)
  assert_true(summary.contains("Total traces: 5"))
  assert_true(summary.contains("Valid traces: 3"))
  assert_true(summary.contains("Invalid traces: 1"))
  assert_true(summary.contains("Anomaly traces: 1"))
}