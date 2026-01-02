// Azimuth时间序列数据处理测试用例
// 测试时间序列数据的收集、处理和分析功能

test "时间序列数据创建和基本操作" {
  // 创建时间序列数据点
  let timestamp1 = 1735689600000000000L  // 2025-01-02T10:00:00Z
  let timestamp2 = 1735689660000000000L  // 2025-01-02T10:01:00Z
  let timestamp3 = 1735689720000000000L  // 2025-01-02T10:02:00Z
  
  let data_point1 = TimeSeriesDataPoint::new(timestamp1, 42.5)
  let data_point2 = TimeSeriesDataPoint::new(timestamp2, 45.8)
  let data_point3 = TimeSeriesDataPoint::new(timestamp3, 41.2)
  
  // 创建时间序列
  let time_series = TimeSeries::new("cpu.usage", "percent")
  TimeSeries::add_point(time_series, data_point1)
  TimeSeries::add_point(time_series, data_point2)
  TimeSeries::add_point(time_series, data_point3)
  
  // 验证时间序列属性
  assert_eq(TimeSeries::metric_name(time_series), "cpu.usage")
  assert_eq(TimeSeries::unit(time_series), "percent")
  assert_eq(TimeSeries::point_count(time_series), 3)
  
  // 验证数据点
  let points = TimeSeries::get_points(time_series)
  assert_eq(points.length(), 3)
  assert_eq(points[0].value, 42.5)
  assert_eq(points[1].value, 45.8)
  assert_eq(points[2].value, 41.2)
}

test "时间序列数据聚合操作" {
  // 创建多个数据点用于聚合测试
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("response.time", "milliseconds")
  
  // 添加数据点
  for i in 0..=10 {
    let timestamp = base_timestamp + (i * 60000000000L)  // 每分钟一个点
    let value = 100.0 + (i.to_float() * 5.5)  // 递增值
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 测试平均值计算
  let avg_value = TimeSeries::average(time_series)
  assert_true(avg_value > 125.0 && avg_value < 130.0)
  
  // 测试最大值和最小值
  let max_value = TimeSeries::maximum(time_series)
  let min_value = TimeSeries::minimum(time_series)
  assert_eq(max_value, 100.0 + (10.0 * 5.5))
  assert_eq(min_value, 100.0)
  
  // 测试求和
  let sum_value = TimeSeries::sum(time_series)
  let expected_sum = 0.0
  for i in 0..=10 {
    expected_sum = expected_sum + (100.0 + (i.to_float() * 5.5))
  }
  assert_eq(sum_value, expected_sum)
}

test "时间序列数据窗口操作" {
  // 创建时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("memory.usage", "percent")
  
  // 添加24小时的数据点（每小时一个）
  for i in 0..=23 {
    let timestamp = base_timestamp + (i * 3600000000000L)  // 每小时一个点
    let value = 50.0 + (10.0 * (@sin(i.to_float() * 0.5)))  // 模拟内存使用波动
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 测试时间窗口查询
  let window_start = base_timestamp + (6 * 3600000000000L)  // 6小时后
  let window_end = base_timestamp + (12 * 3600000000000L)   // 12小时后
  
  let window_series = TimeSeries::get_window(time_series, window_start, window_end)
  assert_eq(TimeSeries::point_count(window_series), 7)  // 包含边界点
  
  // 验证窗口内的数据点时间戳
  let window_points = TimeSeries::get_points(window_series)
  for point in window_points {
    assert_true(point.timestamp >= window_start && point.timestamp <= window_end)
  }
  
  // 测试滑动窗口平均值
  let sliding_avg = TimeSeries::sliding_window_average(time_series, 4)  // 4小时滑动窗口
  assert_eq(sliding_avg.length(), 20)  // 24-4个数据点
  
  // 验证第一个滑动平均值
  let first_avg = sliding_avg[0]
  let expected_first_avg = (50.0 + (10.0 * @sin(0.0 * 0.5)) + 
                           50.0 + (10.0 * @sin(1.0 * 0.5)) + 
                           50.0 + (10.0 * @sin(2.0 * 0.5)) + 
                           50.0 + (10.0 * @sin(3.0 * 0.5))) / 4.0
  assert_eq(first_avg, expected_first_avg)
}

test "时间序列数据压缩和采样" {
  // 创建高频时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("high.frequency.metric", "value")
  
  // 添加每秒一个数据点，持续1小时
  for i in 0..=3599 {
    let timestamp = base_timestamp + (i * 1000000000L)  // 每秒一个点
    let value = 100.0 + (10.0 * @sin(i.to_float() * 0.01))  // 模拟周期性变化
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  assert_eq(TimeSeries::point_count(time_series), 3600)
  
  // 测试时间序列降采样（每分钟取一个点）
  let downsampled_series = TimeSeries::downsample(time_series, 60)  // 60秒间隔
  assert_eq(TimeSeries::point_count(downsampled_series), 60)
  
  // 验证降采样后的数据点时间戳
  let downsampled_points = TimeSeries::get_points(downsampled_series)
  for i in 0..=59 {
    let expected_timestamp = base_timestamp + (i * 60 * 1000000000L)
    assert_eq(downsampled_points[i].timestamp, expected_timestamp)
  }
  
  // 测试时间序列压缩（基于变化阈值）
  let compressed_series = TimeSeries::compress_by_threshold(time_series, 2.0)  // 变化小于2.0的点被压缩
  assert_true(TimeSeries::point_count(compressed_series) < TimeSeries::point_count(time_series))
  
  // 验证压缩后的数据点保留了重要变化
  let compressed_points = TimeSeries::get_points(compressed_series)
  assert_true(compressed_points.length() > 10)  // 至少保留了一些关键点
  assert_eq(compressed_points[0].timestamp, base_timestamp)  // 保留第一个点
  assert_eq(compressed_points[compressed_points.length() - 1].timestamp, 
           base_timestamp + (3599 * 1000000000L))  // 保留最后一个点
}

test "时间序列数据趋势分析" {
  // 创建具有明显趋势的时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("growth.metric", "count")
  
  // 添加具有线性增长趋势的数据
  for i in 0..=29 {
    let timestamp = base_timestamp + (i * 3600000000000L)  // 每小时一个点
    let trend_value = 100.0 + (i.to_float() * 5.0)  // 线性增长
    let noise = (@random() * 10.0) - 5.0  // 添加随机噪声
    let value = trend_value + noise
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 测试线性趋势检测
  let trend_analysis = TimeSeries::analyze_trend(time_series)
  assert_eq(trend_analysis.trend_type, "increasing")
  assert_true(trend_analysis.slope > 4.0 && trend_analysis.slope < 6.0)  // 接近5.0
  assert_true(trend_analysis.confidence > 0.7)  // 高置信度
  
  // 测试季节性检测
  let seasonal_series = TimeSeries::new("seasonal.metric", "value")
  for i in 0..=47 {
    let timestamp = base_timestamp + (i * 1800000000000L)  // 每30分钟一个点
    let seasonal_value = 50.0 + (20.0 * @sin(i.to_float() * 0.261799))  // 24小时周期
    let data_point = TimeSeriesDataPoint::new(timestamp, seasonal_value)
    TimeSeries::add_point(seasonal_series, data_point)
  }
  
  let seasonal_analysis = TimeSeries::analyze_seasonality(seasonal_series)
  assert_eq(seasonal_analysis.has_seasonality, true)
  assert_true(seasonal_analysis.period_hours > 22.0 && seasonal_analysis.period_hours < 26.0)  // 接近24小时
}

test "时间序列数据异常检测" {
  // 创建包含异常值的时间序列数据
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("anomaly.test.metric", "value")
  
  // 添加正常数据点
  for i in 0..=19 {
    let timestamp = base_timestamp + (i * 60000000000L)  // 每分钟一个点
    let normal_value = 50.0 + (@random() * 10.0) - 5.0  // 45-55之间的随机值
    let data_point = TimeSeriesDataPoint::new(timestamp, normal_value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 添加异常值
  let anomaly_timestamp1 = base_timestamp + (7 * 60000000000L)
  let anomaly_timestamp2 = base_timestamp + (15 * 60000000000L)
  
  let anomaly_point1 = TimeSeriesDataPoint::new(anomaly_timestamp1, 120.0)  // 高异常值
  let anomaly_point2 = TimeSeriesDataPoint::new(anomaly_timestamp2, -20.0)  // 低异常值
  
  TimeSeries::add_point(time_series, anomaly_point1)
  TimeSeries::add_point(time_series, anomaly_point2)
  
  // 测试异常检测
  let anomalies = TimeSeries::detect_anomalies(time_series, 2.0)  // 2倍标准差阈值
  
  assert_eq(anomalies.length(), 2)  // 应该检测到两个异常值
  
  // 验证异常值的时间戳
  let anomaly_timestamps = anomalies.map(fn(a) { a.timestamp })
  assert_true(anomaly_timestamps.contains(anomaly_timestamp1))
  assert_true(anomaly_timestamps.contains(anomaly_timestamp2))
  
  // 验证异常值的分数
  for anomaly in anomalies {
    assert_true(anomaly.anomaly_score > 2.0)
    assert_eq(anomaly.expected_range.min, 45.0)  // 预期范围
    assert_eq(anomaly.expected_range.max, 55.0)
  }
}

test "时间序列数据预测" {
  // 创建历史数据用于预测测试
  let base_timestamp = 1735689600000000000L
  let time_series = TimeSeries::new("prediction.test.metric", "value")
  
  // 添加具有趋势和季节性的历史数据
  for i in 0..=167 {  // 7天的数据（每小时一个点）
    let timestamp = base_timestamp + (i * 3600000000000L)
    let trend = 100.0 + (i.to_float() * 0.5)  // 缓慢增长趋势
    let seasonal = 10.0 * @sin((i.to_float() / 24.0) * 2.0 * 3.14159)  // 24小时季节性
    let noise = (@random() * 5.0) - 2.5  // 随机噪声
    let value = trend + seasonal + noise
    let data_point = TimeSeriesDataPoint::new(timestamp, value)
    TimeSeries::add_point(time_series, data_point)
  }
  
  // 测试短期预测（未来24小时）
  let forecast = TimeSeries::forecast(time_series, 24)  // 预测24个点
  assert_eq(forecast.length(), 24)
  
  // 验证预测时间戳
  let last_timestamp = base_timestamp + (167 * 3600000000000L)
  for i in 0..=23 {
    let expected_timestamp = last_timestamp + ((i + 1) * 3600000000000L)
    assert_eq(forecast[i].timestamp, expected_timestamp)
  }
  
  // 验证预测值的合理性
  let last_value = 100.0 + (167.0 * 0.5)  // 最后一个趋势值
  for prediction in forecast {
    assert_true(prediction.value > 80.0 && prediction.value < 150.0)  // 合理范围
    assert_true(prediction.confidence > 0.0 && prediction.confidence <= 1.0)  // 有效置信度
  }
  
  // 验证预测趋势延续性
  let first_predicted = forecast[0].value
  let last_predicted = forecast[23].value
  assert_true(last_predicted > first_predicted)  // 应该继续增长趋势
}