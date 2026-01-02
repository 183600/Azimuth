// Azimuth 高级时间序列操作测试用例
// 专注于验证遥测系统的时间序列数据分析功能

// 测试1: 时间序列数据聚合
test "时间序列数据聚合" {
  let time_series_analyzer = TimeSeriesAnalyzer::new()
  
  // 创建时间序列数据点
  let data_points = []
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  // 生成24小时的数据点，每小时一个
  for i = 0; i < 24; i = i + 1 {
    let timestamp = base_timestamp + (i * 3600000) // 每小时
    let value = 100.0 + (i.to_float() * 2.5) + (Random::float() * 20.0 - 10.0) // 基础值 + 增长 + 随机波动
    data_points.push({
      "timestamp": timestamp,
      "value": value,
      "tags": {
        "service": "api-gateway",
        "metric": "request.count",
        "hour": i.to_string()
      }
    })
  }
  
  // 添加时间序列数据
  TimeSeriesAnalyzer::add_series(time_series_analyzer, "gateway.requests", data_points)
  
  // 测试1小时聚合
  let hourly_avg = TimeSeriesAnalyzer::aggregate(time_series_analyzer, "gateway.requests", "1h", "avg")
  let hourly_sum = TimeSeriesAnalyzer::aggregate(time_series_analyzer, "gateway.requests", "1h", "sum")
  let hourly_max = TimeSeriesAnalyzer::aggregate(time_series_analyzer, "gateway.requests", "1h", "max")
  let hourly_min = TimeSeriesAnalyzer::aggregate(time_series_analyzer, "gateway.requests", "1h", "min")
  
  // 验证聚合结果
  assert_eq(hourly_avg.length(), 24)
  assert_eq(hourly_sum.length(), 24)
  assert_eq(hourly_max.length(), 24)
  assert_eq(hourly_min.length(), 24)
  
  // 测试6小时聚合
  let six_hourly_avg = TimeSeriesAnalyzer::aggregate(time_series_analyzer, "gateway.requests", "6h", "avg")
  assert_eq(six_hourly_avg.length(), 4) // 24小时 / 6小时 = 4个数据点
  
  // 验证聚合值的正确性
  for i = 0; i < six_hourly_avg.length(); i = i + 1 {
    let aggregated_value = six_hourly_avg[i].value
    let expected_range_lower = 100.0 + (i.to_float() * 6 * 2.5) - 10.0
    let expected_range_upper = 100.0 + ((i.to_float() + 1.0) * 6 * 2.5) + 10.0
    assert_true(aggregated_value >= expected_range_lower)
    assert_true(aggregated_value <= expected_range_upper)
  }
}

// 测试2: 时间序列趋势分析
test "时间序列趋势分析" {
  let time_series_analyzer = TimeSeriesAnalyzer::new()
  
  // 创建有明显趋势的时间序列数据
  let trend_data = []
  let base_timestamp = 1640995200000
  
  // 生成30天数据，呈线性增长趋势
  for i = 0; i < 30; i = i + 1 {
    let timestamp = base_timestamp + (i * 86400000) // 每天
    let trend_value = 50.0 + (i.to_float() * 2.0) // 每天增长2.0
    let noise = Random::float() * 10.0 - 5.0 // 添加噪声
    let value = trend_value + noise
    
    trend_data.push({
      "timestamp": timestamp,
      "value": value,
      "tags": {
        "service": "user-service",
        "metric": "active.users"
      }
    })
  }
  
  TimeSeriesAnalyzer::add_series(time_series_analyzer, "user.active", trend_data)
  
  // 分析趋势
  let trend_analysis = TimeSeriesAnalyzer::analyze_trend(time_series_analyzer, "user.active")
  
  // 验证趋势分析结果
  assert_true(trend_analysis.has_trend)
  assert_true(trend_analysis.trend_direction == "increasing")
  assert_true(trend_analysis.trend_slope > 1.5) // 斜率应该接近2.0
  assert_true(trend_analysis.trend_slope < 2.5)
  
  // 计算趋势强度
  let trend_strength = TimeSeriesAnalyzer::calculate_trend_strength(time_series_analyzer, "user.active")
  assert_true(trend_strength > 0.7) // 强趋势
  
  // 预测未来值
  let forecast = TimeSeriesAnalyzer::forecast(time_series_analyzer, "user.active", 5) // 预测5天
  assert_eq(forecast.length(), 5)
  
  // 验证预测值的合理性
  let last_actual_value = trend_data[trend_data.length() - 1].value
  for i = 0; i < forecast.length(); i = i + 1 {
    assert_true(forecast[i].value > last_actual_value) // 预测值应该大于最后一个实际值
  }
}

// 测试3: 时间序列异常检测
test "时间序列异常检测" {
  let time_series_analyzer = TimeSeriesAnalyzer::new()
  
  // 创建包含异常值的时间序列数据
  let anomaly_data = []
  let base_timestamp = 1640995200000
  let base_value = 100.0
  
  // 生成100个数据点，其中包含几个异常值
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_timestamp + (i * 60000) // 每分钟
    let mut value = base_value + (Random::float() * 20.0 - 10.0) // 正常波动
    
    // 插入异常值
    if (i == 20) { value = 200.0 } // 突然峰值
    if (i == 45) { value = 10.0 }  // 突然谷值
    if (i == 70) { value = 180.0 } // 另一个峰值
    if (i == 85) { value = 15.0 }  // 另一个谷值
    
    anomaly_data.push({
      "timestamp": timestamp,
      "value": value,
      "tags": {
        "service": "payment-service",
        "metric": "transaction.amount"
      }
    })
  }
  
  TimeSeriesAnalyzer::add_series(time_series_analyzer, "payment.amount", anomaly_data)
  
  // 检测异常
  let anomalies = TimeSeriesAnalyzer::detect_anomalies(time_series_analyzer, "payment.amount", "zscore", 2.5)
  
  // 验证异常检测结果
  assert_true(anomalies.length() >= 4) // 至少应该检测到4个异常值
  
  // 验证检测到的异常点位置
  let anomaly_indices = []
  for i = 0; i < anomalies.length(); i = i + 1 {
    anomaly_indices.push(anomalies[i].index)
  }
  
  assert_true(anomaly_indices.contains(20))
  assert_true(anomaly_indices.contains(45))
  assert_true(anomaly_indices.contains(70))
  assert_true(anomaly_indices.contains(85))
  
  // 测试不同的异常检测算法
  let iqr_anomalies = TimeSeriesAnalyzer::detect_anomalies(time_series_analyzer, "payment.amount", "iqr", 1.5)
  assert_true(iqr_anomalies.length() >= 3) // IQR方法可能检测到不同的异常点
  
  // 验证异常值的详细信息
  for i = 0; i < anomalies.length(); i = i + 1 {
    let anomaly = anomalies[i]
    assert_true(anomaly.zscore > 2.5 || anomaly.zscore < -2.5)
    assert_true(anomaly.timestamp > 0)
    assert_true(anomaly.value > 0)
  }
}

// 测试4: 时间序列季节性分析
test "时间序列季节性分析" {
  let time_series_analyzer = TimeSeriesAnalyzer::new()
  
  // 创建具有季节性的时间序列数据
  let seasonal_data = []
  let base_timestamp = 1640995200000 // 2022-01-01
  
  // 生成一年的数据，具有周季节性和月季节性
  for i = 0; i < 365; i = i + 1 {
    let timestamp = base_timestamp + (i * 86400000) // 每天
    let day_of_week = i % 7
    let day_of_month = i % 30
    
    // 基础值 + 周季节性 + 月季节性 + 随机噪声
    let base_value = 100.0
    let weekly_pattern = 20.0 * Math::sin((day_of_week.to_float() / 7.0) * 2.0 * Math::PI)
    let monthly_pattern = 15.0 * Math::cos((day_of_month.to_float() / 30.0) * 2.0 * Math::PI)
    let noise = Random::float() * 10.0 - 5.0
    
    let value = base_value + weekly_pattern + monthly_pattern + noise
    
    seasonal_data.push({
      "timestamp": timestamp,
      "value": value,
      "tags": {
        "service": "retail-service",
        "metric": "sales.amount"
      }
    })
  }
  
  TimeSeriesAnalyzer::add_series(time_series_analyzer, "retail.sales", seasonal_data)
  
  // 分析季节性
  let seasonality = TimeSeriesAnalyzer::analyze_seasonality(time_series_analyzer, "retail.sales")
  
  // 验证季节性分析结果
  assert_true(seasonality.has_seasonality)
  assert_true(seasonality.weekly_strength > 0.5) // 应该检测到周季节性
  assert_true(seasonality.monthly_strength > 0.3) // 应该检测到月季节性
  
  // 获取季节性模式
  let weekly_pattern = TimeSeriesAnalyzer::get_seasonal_pattern(time_series_analyzer, "retail.sales", "weekly")
  let monthly_pattern = TimeSeriesAnalyzer::get_seasonal_pattern(time_series_analyzer, "retail.sales", "monthly")
  
  // 验证季节性模式
  assert_eq(weekly_pattern.length(), 7) // 7天的周模式
  assert_eq(monthly_pattern.length(), 30) // 30天的月模式
  
  // 验证周模式的合理性（应该有高峰和低谷）
  let weekly_values = []
  for i = 0; i < weekly_pattern.length(); i = i + 1 {
    weekly_values.push(weekly_pattern[i].value)
  }
  
  let weekly_max = Math::max(weekly_values)
  let weekly_min = Math::min(weekly_values)
  assert_true(weekly_max - weekly_min > 10.0) // 应该有明显的波动
}

// 测试5: 时间序列相关性分析
test "时间序列相关性分析" {
  let time_series_analyzer = TimeSeriesAnalyzer::new()
  
  // 创建两个相关的的时间序列
  let series1_data = []
  let series2_data = []
  let base_timestamp = 1640995200000
  
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_timestamp + (i * 3600000) // 每小时
    
    // 第一个序列：基础值 + 趋势
    let value1 = 50.0 + (i.to_float() * 0.5) + (Random::float() * 10.0 - 5.0)
    
    // 第二个序列：与第一个序列相关，但有延迟和噪声
    let delayed_influence = if (i >= 5) { series1_data[i-5].value * 0.8 } else { 0.0 }
    let value2 = 30.0 + delayed_influence + (Random::float() * 8.0 - 4.0)
    
    series1_data.push({
      "timestamp": timestamp,
      "value": value1,
      "tags": { "metric": "cpu.usage" }
    })
    
    series2_data.push({
      "timestamp": timestamp,
      "value": value2,
      "tags": { "metric": "memory.usage" }
    })
  }
  
  TimeSeriesAnalyzer::add_series(time_series_analyzer, "system.cpu", series1_data)
  TimeSeriesAnalyzer::add_series(time_series_analyzer, "system.memory", series2_data)
  
  // 计算相关性
  let correlation = TimeSeriesAnalyzer::calculate_correlation(time_series_analyzer, "system.cpu", "system.memory")
  
  // 验证相关性分析结果
  assert_true(correlation.correlation_coefficient > 0.5) // 应该有中等程度的正相关
  assert_true(correlation.correlation_coefficient < 1.0) // 但不是完全相关
  assert_true(correlation.p_value < 0.05) // 相关性应该显著
  
  // 计算交叉相关性（检测延迟）
  let cross_correlation = TimeSeriesAnalyzer::calculate_cross_correlation(time_series_analyzer, "system.cpu", "system.memory", 10)
  
  // 验证交叉相关性结果
  assert_eq(cross_correlation.length(), 21) // -10到+10的延迟
  
  // 找到最大相关性的延迟
  let max_corr = 0.0
  let best_lag = 0
  for i = 0; i < cross_correlation.length(); i = i + 1 {
    if (cross_correlation[i].correlation > max_corr) {
      max_corr = cross_correlation[i].correlation
      best_lag = cross_correlation[i].lag
    }
  }
  
  // 验证检测到的延迟（应该接近5）
  assert_true(best_lag >= 3 && best_lag <= 7) // 允许一定的误差
}

// 测试6: 时间序列降采样和上采样
test "时间序列降采样和上采样" {
  let time_series_analyzer = TimeSeriesAnalyzer::new()
  
  // 创建高频时间序列数据（每分钟一个数据点）
  let high_freq_data = []
  let base_timestamp = 1640995200000
  
  for i = 0; i < 1440; i = i + 1 { // 24小时，每分钟
    let timestamp = base_timestamp + (i * 60000) // 每分钟
    let value = 100.0 + 10.0 * Math::sin((i.to_float() / 60.0) * 2.0 * Math::PI) + (Random::float() * 5.0 - 2.5)
    
    high_freq_data.push({
      "timestamp": timestamp,
      "value": value,
      "tags": { "metric": "temperature" }
    })
  }
  
  TimeSeriesAnalyzer::add_series(time_series_analyzer, "sensor.temperature", high_freq_data)
  
  // 测试降采样（从分钟级到小时级）
  let downsampled_hourly = TimeSeriesAnalyzer::downsample(time_series_analyzer, "sensor.temperature", "1h", "avg")
  assert_eq(downsampled_hourly.length(), 24) // 24小时
  
  // 验证降采样数据的正确性
  for i = 0; i < downsampled_hourly.length(); i = i + 1 {
    let downsampled_point = downsampled_hourly[i]
    let expected_timestamp = base_timestamp + (i * 3600000)
    assert_eq(downsampled_point.timestamp, expected_timestamp)
    
    // 验证降采样值在合理范围内
    assert_true(downsampled_point.value >= 85.0)
    assert_true(downsampled_point.value <= 115.0)
  }
  
  // 测试不同的降采样方法
  let downsampled_max = TimeSeriesAnalyzer::downsample(time_series_analyzer, "sensor.temperature", "1h", "max")
  let downsampled_min = TimeSeriesAnalyzer::downsample(time_series_analyzer, "sensor.temperature", "1h", "min")
  
  // 验证最大值和最小值的关系
  for i = 0; i < downsampled_hourly.length(); i = i + 1 {
    assert_true(downsampled_max[i].value >= downsampled_hourly[i].value)
    assert_true(downsampled_min[i].value <= downsampled_hourly[i].value)
  }
  
  // 测试上采样（从小时级到分钟级）
  let upsampled = TimeSeriesAnalyzer::upsample(time_series_analyzer, "sensor.temperature", downsampled_hourly, "1m", "linear")
  assert_eq(upsampled.length(), 1440) // 应该恢复到原始数据点数量
  
  // 验证上采样数据的合理性
  for i = 0; i < upsampled.length(); i = i + 1 {
    let upsampled_point = upsampled[i]
    let expected_timestamp = base_timestamp + (i * 60000)
    assert_eq(upsampled_point.timestamp, expected_timestamp)
    
    // 上采样值应该在合理范围内
    assert_true(upsampled_point.value >= 85.0)
    assert_true(upsampled_point.value <= 115.0)
  }
}

// 测试7: 时间序列窗口函数
test "时间序列窗口函数" {
  let time_series_analyzer = TimeSeriesAnalyzer::new()
  
  // 创建时间序列数据
  let window_data = []
  let base_timestamp = 1640995200000
  
  for i = 0; i < 100; i = i + 1 {
    let timestamp = base_timestamp + (i * 60000) // 每分钟
    let value = 100.0 + (Random::float() * 50.0) // 100-150之间的随机值
    
    window_data.push({
      "timestamp": timestamp,
      "value": value,
      "tags": { "metric": "response.time" }
    })
  }
  
  TimeSeriesAnalyzer::add_series(time_series_analyzer, "api.response_time", window_data)
  
  // 测试移动平均
  let moving_avg_5 = TimeSeriesAnalyzer::moving_average(time_series_analyzer, "api.response_time", 5)
  let moving_avg_10 = TimeSeriesAnalyzer::moving_average(time_series_analyzer, "api.response_time", 10)
  
  // 验证移动平均结果
  assert_eq(moving_avg_5.length(), 100)
  assert_eq(moving_avg_10.length(), 100)
  
  // 移动平均应该更平滑（方差更小）
  let original_variance = TimeSeriesAnalyzer::calculate_variance(window_data)
  let avg_5_variance = TimeSeriesAnalyzer::calculate_variance(moving_avg_5)
  let avg_10_variance = TimeSeriesAnalyzer::calculate_variance(moving_avg_10)
  
  assert_true(avg_5_variance < original_variance)
  assert_true(avg_10_variance < avg_5_variance)
  
  // 测试移动标准差
  let moving_std = TimeSeriesAnalyzer::moving_std(time_series_analyzer, "api.response_time", 10)
  assert_eq(moving_std.length(), 100)
  
  // 验证移动标准差的合理性
  for i = 0; i < moving_std.length(); i = i + 1 {
    assert_true(moving_std[i].value >= 0.0)
    assert_true(moving_std[i].value <= 30.0) // 标准差应该在合理范围内
  }
  
  // 测试指数加权移动平均
  let ewma = TimeSeriesAnalyzer::ewma(time_series_analyzer, "api.response_time", 0.3)
  assert_eq(ewma.length(), 100)
  
  // 验证EWMA的平滑效果
  let ewma_variance = TimeSeriesAnalyzer::calculate_variance(ewma)
  assert_true(ewma_variance < original_variance)
}

// 测试8: 时间序列指标计算
test "时间序列指标计算" {
  let time_series_analyzer = TimeSeriesAnalyzer::new()
  
  // 创建已知特性的时间序列数据
  let metrics_data = []
  let base_timestamp = 1640995200000
  
  // 生成数据：前半部分稳定，后半部分有趋势
  for i = 0; i < 200; i = i + 1 {
    let timestamp = base_timestamp + (i * 60000) // 每分钟
    let mut value = 100.0
    
    if (i < 100) {
      // 前半部分：稳定值 + 小噪声
      value = value + (Random::float() * 10.0 - 5.0)
    } else {
      // 后半部分：线性增长 + 噪声
      value = value + ((i - 100).to_float() * 0.5) + (Random::float() * 10.0 - 5.0)
    }
    
    metrics_data.push({
      "timestamp": timestamp,
      "value": value,
      "tags": { "metric": "system.load" }
    })
  }
  
  TimeSeriesAnalyzer::add_series(time_series_analyzer, "system.load", metrics_data)
  
  // 计算基本统计指标
  let stats = TimeSeriesAnalyzer::calculate_statistics(time_series_analyzer, "system.load")
  
  // 验证统计指标
  assert_true(stats.mean > 100.0) // 平均值应该大于基础值
  assert_true(stats.median > 100.0)
  assert_true(stats.std_dev > 10.0) // 应该有明显的标准差
  assert_true(stats.min > 80.0) // 最小值应该在合理范围内
  assert_true(stats.max < 200.0) // 最大值应该在合理范围内
  
  // 计算分位数
  let percentiles = TimeSeriesAnalyzer::calculate_percentiles(time_series_analyzer, "system.load", [25, 50, 75, 90, 95, 99])
  
  // 验证分位数的顺序
  assert_true(percentiles[25] <= percentiles[50])
  assert_true(percentiles[50] <= percentiles[75])
  assert_true(percentiles[75] <= percentiles[90])
  assert_true(percentiles[90] <= percentiles[95])
  assert_true(percentiles[95] <= percentiles[99])
  
  // 计算变化率
  let rate_of_change = TimeSeriesAnalyzer::calculate_rate_of_change(time_series_analyzer, "system.load")
  assert_eq(rate_of_change.length(), 199) // 比原始数据少一个点
  
  // 验证变化率的特征
  let positive_changes = 0
  let negative_changes = 0
  for i = 0; i < rate_of_change.length(); i = i + 1 {
    if (rate_of_change[i].value > 0) {
      positive_changes = positive_changes + 1
    } else {
      negative_changes = negative_changes + 1
    }
  }
  
  // 后半部分有上升趋势，所以正变化应该更多
  assert_true(positive_changes > negative_changes)
  
  // 计算累积和
  let cumulative_sum = TimeSeriesAnalyzer::cumulative_sum(time_series_analyzer, "system.load")
  assert_eq(cumulative_sum.length(), 200)
  
  // 验证累积和的单调递增
  for i = 1; i < cumulative_sum.length(); i = i + 1 {
    assert_true(cumulative_sum[i].value > cumulative_sum[i-1].value)
  }
}