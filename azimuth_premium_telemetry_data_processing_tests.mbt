// Azimuth Premium Telemetry Data Processing Tests
// 高质量遥测数据处理测试用例 - 专注于数据转换、聚合和分析功能

test "遥测数据批量处理和聚合" {
  // 创建模拟的遥测数据点
  let data_points = [
    TelemetryDataPoint::new("cpu_usage", 45.2, 1234567890L),
    TelemetryDataPoint::new("cpu_usage", 52.8, 1234567891L),
    TelemetryDataPoint::new("cpu_usage", 38.1, 1234567892L),
    TelemetryDataPoint::new("memory_usage", 67.3, 1234567890L),
    TelemetryDataPoint::new("memory_usage", 71.2, 1234567891L),
    TelemetryDataPoint::new("memory_usage", 69.8, 1234567892L)
  ]
  
  // 测试按指标名称分组
  let grouped_data = TelemetryProcessor::group_by_metric(data_points)
  assert_eq(grouped_data.length(), 2)
  
  let cpu_data = grouped_data["cpu_usage"]
  assert_eq(cpu_data.length(), 3)
  assert_eq(cpu_data[0].value, 45.2)
  assert_eq(cpu_data[1].value, 52.8)
  assert_eq(cpu_data[2].value, 38.1)
  
  let memory_data = grouped_data["memory_usage"]
  assert_eq(memory_data.length(), 3)
  assert_eq(memory_data[0].value, 67.3)
  assert_eq(memory_data[1].value, 71.2)
  assert_eq(memory_data[2].value, 69.8)
  
  // 测试计算聚合统计信息
  let cpu_stats = TelemetryProcessor::calculate_stats(cpu_data)
  assert_eq(cpu_stats.count, 3)
  assert_eq(cpu_stats.min, 38.1)
  assert_eq(cpu_stats.max, 52.8)
  assert_eq(cpu_stats.average, 45.36666666666667)
  assert_eq(cpu_stats.sum, 136.1)
  
  let memory_stats = TelemetryProcessor::calculate_stats(memory_data)
  assert_eq(memory_stats.count, 3)
  assert_eq(memory_stats.min, 67.3)
  assert_eq(memory_stats.max, 71.2)
  assert_eq(memory_stats.average, 69.43333333333334)
  assert_eq(memory_stats.sum, 208.3)
}

test "遥测数据时间窗口分析" {
  // 创建跨时间窗口的遥测数据
  let time_series_data = [
    TelemetryDataPoint::new("response_time", 120.5, 1234567800L),
    TelemetryDataPoint::new("response_time", 135.2, 1234567860L),
    TelemetryDataPoint::new("response_time", 98.7, 1234567920L),
    TelemetryDataPoint::new("response_time", 142.3, 1234567980L),
    TelemetryDataPoint::new("response_time", 115.8, 1234568040L),
    TelemetryDataPoint::new("response_time", 128.9, 1234568100L)
  ]
  
  // 测试时间窗口分割（60秒窗口）
  let window_size = 60L
  let time_windows = TelemetryProcessor::split_by_time_window(time_series_data, window_size)
  assert_eq(time_windows.length(), 2)
  
  // 第一个窗口应该包含前3个数据点
  let first_window = time_windows[0]
  assert_eq(first_window.length(), 3)
  assert_eq(first_window[0].timestamp, 1234567800L)
  assert_eq(first_window[2].timestamp, 1234567920L)
  
  // 第二个窗口应该包含后3个数据点
  let second_window = time_windows[1]
  assert_eq(second_window.length(), 3)
  assert_eq(second_window[0].timestamp, 1234567980L)
  assert_eq(second_window[2].timestamp, 1234568100L)
  
  // 测试每个窗口的统计信息
  let first_window_stats = TelemetryProcessor::calculate_stats(first_window)
  assert_eq(first_window_stats.average, 118.13333333333334)
  
  let second_window_stats = TelemetryProcessor::calculate_stats(second_window)
  assert_eq(second_window_stats.average, 129.0)
}

test "遥测数据异常检测" {
  // 创建包含异常值的遥测数据
  let normal_data = [
    TelemetryDataPoint::new("latency", 45.2, 1234567890L),
    TelemetryDataPoint::new("latency", 47.8, 1234567891L),
    TelemetryDataPoint::new("latency", 44.1, 1234567892L),
    TelemetryDataPoint::new("latency", 46.5, 1234567893L),
    TelemetryDataPoint::new("latency", 48.2, 1234567894L),
    TelemetryDataPoint::new("latency", 125.7, 1234567895L), // 异常值
    TelemetryDataPoint::new("latency", 45.9, 1234567896L),
    TelemetryDataPoint::new("latency", 47.3, 1234567897L)
  ]
  
  // 使用标准差方法检测异常值
  let anomalies = TelemetryProcessor::detect_anomalies(normal_data, "stddev", 2.0)
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].value, 125.7)
  assert_eq(anomalies[0].timestamp, 1234567895L)
  
  // 使用四分位距方法检测异常值
  let iqr_anomalies = TelemetryProcessor::detect_anomalies(normal_data, "iqr", 1.5)
  assert_eq(iqr_anomalies.length(), 1)
  assert_eq(iqr_anomalies[0].value, 125.7)
  
  // 测试无异常值的数据
  let clean_data = [
    TelemetryDataPoint::new("latency", 45.2, 1234567890L),
    TelemetryDataPoint::new("latency", 47.8, 1234567891L),
    TelemetryDataPoint::new("latency", 44.1, 1234567892L),
    TelemetryDataPoint::new("latency", 46.5, 1234567893L),
    TelemetryDataPoint::new("latency", 48.2, 1234567894L)
  ]
  
  let no_anomalies = TelemetryProcessor::detect_anomalies(clean_data, "stddev", 2.0)
  assert_eq(no_anomalies.length(), 0)
}

test "遥测数据趋势分析" {
  // 创建具有明显趋势的遥测数据
  let trend_data = [
    TelemetryDataPoint::new("error_rate", 2.1, 1234567890L),
    TelemetryDataPoint::new("error_rate", 2.5, 1234567891L),
    TelemetryDataPoint::new("error_rate", 3.2, 1234567892L),
    TelemetryDataPoint::new("error_rate", 3.8, 1234567893L),
    TelemetryDataPoint::new("error_rate", 4.5, 1234567894L),
    TelemetryDataPoint::new("error_rate", 5.1, 1234567895L),
    TelemetryDataPoint::new("error_rate", 5.8, 1234567896L),
    TelemetryDataPoint::new("error_rate", 6.3, 1234567897L)
  ]
  
  // 计算趋势斜率
  let trend_analysis = TelemetryProcessor::analyze_trend(trend_data)
  assert_true(trend_analysis.slope > 0.5) // 上升趋势
  assert_eq(trend_analysis.direction, "increasing")
  assert_true(trend_analysis.correlation > 0.9) // 强相关性
  
  // 创建下降趋势的数据
  let decreasing_data = [
    TelemetryDataPoint::new("throughput", 1000.5, 1234567890L),
    TelemetryDataPoint::new("throughput", 950.2, 1234567891L),
    TelemetryDataPoint::new("throughput", 890.7, 1234567892L),
    TelemetryDataPoint::new("throughput", 845.3, 1234567893L),
    TelemetryDataPoint::new("throughput", 790.8, 1234567894L),
    TelemetryDataPoint::new("throughput", 735.4, 1234567895L)
  ]
  
  let decreasing_trend = TelemetryProcessor::analyze_trend(decreasing_data)
  assert_true(decreasing_trend.slope < -40.0) // 下降趋势
  assert_eq(decreasing_trend.direction, "decreasing")
  assert_true(decreasing_trend.correlation < -0.9) // 强负相关性
  
  // 创建平稳数据
  let stable_data = [
    TelemetryDataPoint::new("cpu_temp", 65.2, 1234567890L),
    TelemetryDataPoint::new("cpu_temp", 64.8, 1234567891L),
    TelemetryDataPoint::new("cpu_temp", 65.5, 1234567892L),
    TelemetryDataPoint::new("cpu_temp", 64.9, 1234567893L),
    TelemetryDataPoint::new("cpu_temp", 65.1, 1234567894L)
  ]
  
  let stable_trend = TelemetryProcessor::analyze_trend(stable_data)
  assert_eq(stable_trend.direction, "stable")
  assert_true(stable_trend.correlation.abs() < 0.3) // 弱相关性
}

test "遥测数据预测和预报" {
  // 创建历史数据用于预测
  let historical_data = [
    TelemetryDataPoint::new("disk_usage", 45.2, 1234567000L),
    TelemetryDataPoint::new("disk_usage", 46.8, 1234567060L),
    TelemetryDataPoint::new("disk_usage", 48.1, 1234567120L),
    TelemetryDataPoint::new("disk_usage", 49.5, 1234567180L),
    TelemetryDataPoint::new("disk_usage", 51.2, 1234567240L),
    TelemetryDataPoint::new("disk_usage", 52.7, 1234567300L),
    TelemetryDataPoint::new("disk_usage", 54.3, 1234567360L),
    TelemetryDataPoint::new("disk_usage", 55.8, 1234567420L)
  ]
  
  // 使用线性回归预测未来值
  let prediction_model = TelemetryProcessor::create_prediction_model(historical_data, "linear")
  let future_predictions = TelemetryProcessor::predict(prediction_model, 1234567480L, 3) // 预测未来3个时间点
  
  assert_eq(future_predictions.length(), 3)
  assert_true(future_predictions[0] > 55.8) // 预测值应该大于最后一个历史值
  assert_true(future_predictions[1] > future_predictions[0]) // 上升趋势
  assert_true(future_predictions[2] > future_predictions[1])
  
  // 测试预测准确性（使用历史数据的一部分进行验证）
  let training_data = historical_data.slice(0, 6)
  let validation_data = historical_data.slice(6, 2)
  
  let validation_model = TelemetryProcessor::create_prediction_model(training_data, "linear")
  let validation_predictions = TelemetryProcessor::predict(validation_model, 1234567360L, 2)
  
  // 验证预测误差在可接受范围内
  let error1 = (validation_predictions[0] - validation_data[0].value).abs()
  let error2 = (validation_predictions[1] - validation_data[1].value).abs()
  assert_true(error1 < 2.0) // 误差小于2个单位
  assert_true(error2 < 2.0)
}

test "遥测数据质量验证" {
  // 创建包含各种质量问题的数据
  let problematic_data = [
    TelemetryDataPoint::new("metric1", -1.0, 1234567890L), // 负值（可能无效）
    TelemetryDataPoint::new("metric1", 45.2, 1234567891L),
    TelemetryDataPoint::new("metric1", 0.0, 1234567892L), // 零值
    TelemetryDataPoint::new("metric1", 999999.9, 1234567893L), // 极大值
    TelemetryDataPoint::new("metric1", 47.8, 1234567894L),
    TelemetryDataPoint::new("metric1", 0.0, 1234567895L), // 重复零值
    TelemetryDataPoint::new("metric1", 48.1, 1234567896L)
  ]
  
  // 定义数据质量规则
  let quality_rules = DataQualityRules::new()
  DataQualityRules::add_range_rule(quality_rules, "metric1", 0.0, 100.0) // 值范围0-100
  DataQualityRules::add_outlier_rule(quality_rules, "metric1", 3.0) // 3倍标准差异常检测
  DataQualityRules::add_monotonic_rule(quality_rules, "metric1", false) // 不要求单调
  
  // 执行数据质量检查
  let quality_report = TelemetryProcessor::validate_data_quality(problematic_data, quality_rules)
  assert_eq(quality_report.total_records, 7)
  assert_eq(quality_report.valid_records, 5)
  assert_eq(quality_report.invalid_records, 2)
  
  // 检查具体问题
  let negative_issues = quality_report.issues.filter(fn(i) { i.issue_type == "negative_value" })
  let outlier_issues = quality_report.issues.filter(fn(i) { i.issue_type == "outlier" })
  
  assert_eq(negative_issues.length(), 1)
  assert_eq(outlier_issues.length(), 1)
  
  // 测试数据清洗功能
  let cleaned_data = TelemetryProcessor::clean_data(problematic_data, quality_rules)
  assert_eq(cleaned_data.length(), 5) // 移除了无效数据点
  
  // 验证清洗后的数据
  for data_point in cleaned_data {
    assert_true(data_point.value >= 0.0)
    assert_true(data_point.value <= 100.0)
  }
}