// Azimuth 时序数据处理测试用例
// 测试时序数据的处理、聚合和分析功能

test "时序数据点创建和验证" {
  // 创建基本时序数据点
  let data_point = azimuth::TimeSeriesDataPoint::new(
    "cpu.usage",
    75.5,
    1609459200000L, // 2021-01-01 00:00:00 UTC
    [("host", "server-01"), ("region", "us-west-1")]
  )
  
  assert_eq(data_point.metric_name, "cpu.usage")
  assert_eq(data_point.value, 75.5)
  assert_eq(data_point.timestamp, 1609459200000L)
  assert_eq(data_point.attributes.length(), 2)
  assert_eq(data_point.attributes[0], ("host", "server-01"))
  assert_eq(data_point.attributes[1], ("region", "us-west-1"))
}

test "时序数据序列创建和管理" {
  // 创建时序数据序列
  let series = azimuth::TimeSeries::new("memory.usage", [("service", "api-gateway")])
  
  // 添加数据点
  let base_timestamp = 1609459200000L
  for i in 0..=10 {
    let timestamp = base_timestamp + (i * 60000L) // 每分钟一个数据点
    let value = 50.0 + (i.to_int() * 2.5).to_float()
    series.add_point(timestamp, value)
  }
  
  // 验证数据点数量
  assert_eq(series.points.length(), 11)
  
  // 验证第一个和最后一个数据点
  assert_eq(series.points[0].timestamp, base_timestamp)
  assert_eq(series.points[0].value, 50.0)
  assert_eq(series.points[10].timestamp, base_timestamp + 600000L)
  assert_eq(series.points[10].value, 75.0)
}

test "时序数据窗口聚合" {
  // 创建时序数据
  let series = azimuth::TimeSeries::new("request.count", [("endpoint", "/api/users")])
  let base_timestamp = 1609459200000L
  
  // 添加15分钟的数据点（每分钟一个）
  for i in 0..=14 {
    let timestamp = base_timestamp + (i * 60000L)
    let value = (i % 5 + 1).to_float() // 1到5的循环值
    series.add_point(timestamp, value)
  }
  
  // 测试5分钟窗口聚合
  let window_size = 300000L // 5分钟
  let windows = series.aggregate_by_time_window(window_size)
  
  // 应该有3个5分钟窗口
  assert_eq(windows.length(), 3)
  
  // 验证第一个窗口（0-4分钟）
  assert_eq(windows[0].start_time, base_timestamp)
  assert_eq(windows[0].end_time, base_timestamp + 240000L)
  assert_eq(windows[0].point_count, 5)
  assert_eq(windows[0].sum, 15.0) // 1+2+3+4+5
  assert_eq(windows[0].average, 3.0)
  assert_eq(windows[0].min, 1.0)
  assert_eq(windows[0].max, 5.0)
}

test "时序数据降采样" {
  // 创建高频时序数据（每秒一个点）
  let series = azimuth::TimeSeries::new("network.throughput", [("interface", "eth0")])
  let base_timestamp = 1609459200000L
  
  // 添加60秒的数据
  for i in 0..=59 {
    let timestamp = base_timestamp + (i * 1000L)
    let value = 100.0 + (i.to_int() % 10).to_float() * 10.0
    series.add_point(timestamp, value)
  }
  
  // 降采样到每分钟一个点
  let downsampled = series.downsample(60000L, azimuth::AggregationType::Average)
  
  // 应该有1个降采样点
  assert_eq(downsampled.points.length(), 1)
  assert_eq(downsampled.points[0].timestamp, base_timestamp)
  assert_eq(downsampled.points[0].value, 145.0) // 平均值
}

test "时序数据插值处理" {
  // 创建有缺失数据的时序
  let series = azimuth::TimeSeries::new("disk.io", [("device", "/dev/sda1")])
  
  // 添加不连续的数据点
  series.add_point(1609459200000L, 100.0) // 00:00:00
  series.add_point(1609459260000L, 200.0) // 00:01:00
  series.add_point(1609459380000L, 300.0) // 00:03:00 (跳过了00:02:00)
  
  // 线性插值填充缺失数据
  let interpolated = series.interpolate(azimuth::InterpolationMethod::Linear, 60000L)
  
  // 应该有4个数据点（包括插值点）
  assert_eq(interpolated.points.length(), 4)
  
  // 验证插值点（00:02:00）
  assert_eq(interpolated.points[2].timestamp, 1609459320000L)
  assert_eq(interpolated.points[2].value, 250.0) // (200+300)/2
}

test "时序数据趋势分析" {
  // 创建有明显趋势的时序数据
  let series = azimuth::TimeSeries::new("user.active", [("app", "mobile")])
  let base_timestamp = 1609459200000L
  
  // 添加递增趋势的数据
  for i in 0..=23 {
    let timestamp = base_timestamp + (i * 3600000L) // 每小时一个点
    let value = 1000.0 + (i.to_int() * 50).to_float()
    series.add_point(timestamp, value)
  }
  
  // 计算趋势
  let trend = series.calculate_trend()
  
  // 验证趋势分析结果
  assert_eq(trend.direction, azimuth::TrendDirection::Increasing)
  assert_eq(trend.slope, 50.0) // 每小时增加50
  assert_eq(trend.correlation, 1.0) // 完全正相关
}

test "时序数据异常检测" {
  // 创建包含异常值的时序数据
  let series = azimuth::TimeSeries::new("error.rate", [("service", "auth")])
  let base_timestamp = 1609459200000L
  
  // 添加正常数据（1-5%错误率）
  for i in 0..=19 {
    let timestamp = base_timestamp + (i * 300000L) // 每5分钟一个点
    let value = 1.0 + (i.to_int() % 5).to_float()
    series.add_point(timestamp, value)
  }
  
  // 添加异常值
  series.add_point(base_timestamp + 6000000L, 25.0) // 25%错误率（异常）
  
  // 检测异常
  let anomalies = series.detect_anomalies(azimuth::AnomalyDetectionMethod::ZScore, 2.0)
  
  // 应该检测到1个异常
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].timestamp, base_timestamp + 6000000L)
  assert_eq(anomalies[0].value, 25.0)
  assert_true(anomalies[0].anomaly_score > 2.0)
}

test "时序数据季节性分析" {
  // 创建具有季节性模式的数据
  let series = azimuth::TimeSeries::new("traffic.volume", [("website", "example.com")])
  let base_timestamp = 1609459200000L // 2021-01-01 00:00:00
  
  // 添加7天的数据（模拟每周模式）
  for day in 0..=6 {
    for hour in 0..=23 {
      let timestamp = base_timestamp + ((day * 24 + hour) * 3600000L)
      // 模拟工作日高峰模式（9-17点流量高）
      let base_value = if hour >= 9 && hour <= 17 { 1000.0 } else { 200.0 }
      let noise = (hour.to_int() % 3).to_float() * 50.0
      let value = base_value + noise
      series.add_point(timestamp, value)
    }
  }
  
  // 分析季节性
  let seasonality = series.analyze_seasonality(azimuth::SeasonalityPeriod::Daily)
  
  // 验证季节性分析结果
  assert_eq(seasonality.period, azimuth::SeasonalityPeriod::Daily)
  assert_eq(seasonality.cycle_length, 24) // 24小时周期
  assert_true(seasonality.strength > 0.5) // 季节性强度较高
}

test "时序数据预测" {
  // 创建历史数据
  let series = azimuth::TimeSeries::new("revenue.daily", [("product", "premium")])
  let base_timestamp = 1609459200000L
  
  // 添加30天的历史数据
  for day in 0..=29 {
    let timestamp = base_timestamp + (day * 86400000L) // 每天一个点
    // 模拟增长趋势 + 周末效应
    let day_of_week = day % 7
    let weekend_factor = if day_of_week == 0 || day_of_week == 6 { 0.7 } else { 1.0 }
    let trend_value = 10000.0 + (day.to_int() * 100).to_float()
    let value = trend_value * weekend_factor
    series.add_point(timestamp, value)
  }
  
  // 预测未来7天
  let forecast = series.forecast(azimuth::ForecastingMethod::LinearRegression, 7)
  
  // 验证预测结果
  assert_eq(forecast.points.length(), 7)
  
  // 验证预测时间戳
  let first_forecast_timestamp = base_timestamp + (30 * 86400000L)
  assert_eq(forecast.points[0].timestamp, first_forecast_timestamp)
  
  // 验证预测值趋势（应该继续增长）
  assert_true(forecast.points[6].value > forecast.points[0].value)
}

test "时序数据压缩和存储" {
  // 创建大量时序数据点
  let series = azimuth::TimeSeries::new("sensor.temperature", [("sensor_id", "temp-001")])
  let base_timestamp = 1609459200000L
  
  // 添加1000个数据点（约16.7分钟的数据，每秒一个点）
  for i in 0..=999 {
    let timestamp = base_timestamp + (i * 1000L)
    let value = 20.0 + (i.to_int() % 10).to_float() * 0.5
    series.add_point(timestamp, value)
  }
  
  // 测试数据压缩
  let compressed = series.compress(azimuth::CompressionAlgorithm::Gorilla)
  
  // 验证压缩效果
  assert_true(compressed.size < series.size * 0.3) // 压缩率应小于30%
  
  // 解压缩并验证数据完整性
  let decompressed = compressed.decompress()
  assert_eq(decompressed.points.length(), series.points.length())
  
  // 验证前几个数据点
  for i in 0..=9 {
    assert_eq(decompressed.points[i].timestamp, series.points[i].timestamp)
    assert_eq(decompressed.points[i].value, series.points[i].value)
  }
}