// 遥测时间序列数据测试用例

test "time_series_data_structure" {
  // 测试时间序列数据结构
  
  let metric_name = "cpu_usage_percent"
  let timestamps = [1640995200L, 1640995260L, 1640995320L, 1640995380L, 1640995440L]
  let values = [45.2, 47.8, 52.1, 49.6, 51.3]
  let tags = ["service:api", "env:production", "region:us-east-1"]
  
  // 验证指标名称
  assert_eq(metric_name.has_prefix("cpu"), true)
  assert_eq(metric_name.has_suffix("percent"), true)
  assert_eq(metric_name.length(), 17)
  
  // 验证时间戳序列
  assert_eq(timestamps.length(), 5)
  assert_eq(timestamps[1] - timestamps[0], 60L) // 60秒间隔
  assert_eq(timestamps[4] - timestamps[0], 240L) // 总共4分钟
  
  // 验证数值序列
  assert_eq(values.length(), 5)
  assert_eq(values[0] > 40.0, true)
  assert_eq(values[2] < 55.0, true)
  
  // 验证标签
  assert_eq(tags.length(), 3)
  assert_eq(tags[0], "service:api")
  assert_eq(tags[2], "region:us-east-1")
}

test "time_series_aggregation" {
  // 测试时间序列数据聚合
  
  let data_points = [23.5, 27.8, 31.2, 29.6, 33.1, 28.9, 32.4, 30.7]
  
  // 计算平均值
  let mut sum = 0.0
  let mut i = 0
  while i < data_points.length() {
    sum = sum + data_points[i]
    i = i + 1
  }
  let average = sum / data_points.length().to_double()
  
  // 验证聚合结果
  assert_eq(data_points.length(), 8)
  assert_eq(average > 25.0, true)
  assert_eq(average < 35.0, true)
  
  // 计算最大值和最小值
  let mut max_value = data_points[0]
  let mut min_value = data_points[0]
  i = 0
  while i < data_points.length() {
    if data_points[i] > max_value {
      max_value = data_points[i]
    }
    if data_points[i] < min_value {
      min_value = data_points[i]
    }
    i = i + 1
  }
  
  assert_eq(max_value, 33.1)
  assert_eq(min_value, 23.5)
  assert_eq(max_value - min_value, 9.6)
}

test "time_series_windowing" {
  // 测试时间序列窗口处理
  
  let full_series = [10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0]
  let window_size = 3
  let sliding_windows = []
  
  // 创建滑动窗口
  let mut i = 0
  while i <= full_series.length() - window_size {
    let window = []
    let mut j = 0
    while j < window_size {
      window.push(full_series[i + j])
      j = j + 1
    }
    sliding_windows.push(window)
    i = i + 1
  }
  
  // 验证窗口数量
  assert_eq(sliding_windows.length(), 8) // 10 - 3 + 1 = 8
  
  // 验证窗口内容
  assert_eq(sliding_windows[0].length(), 3)
  assert_eq(sliding_windows[0][0], 10.0)
  assert_eq(sliding_windows[0][2], 20.0)
  
  assert_eq(sliding_windows[4].length(), 3)
  assert_eq(sliding_windows[4][0], 30.0)
  assert_eq(sliding_windows[4][2], 40.0)
  
  assert_eq(sliding_windows[7].length(), 3)
  assert_eq(sliding_windows[7][0], 40.0)
  assert_eq(sliding_windows[7][2], 50.0)
}

test "time_series_downsampling" {
  // 测试时间序列数据降采样
  
  let high_freq_data = [1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0]
  let downsample_factor = 3
  let downsampled_data = []
  
  // 执行降采样
  let mut i = 0
  while i < high_freq_data.length() {
    if i % downsample_factor == 0 {
      downsampled_data.push(high_freq_data[i])
    }
    i = i + 1
  }
  
  // 验证降采样结果
  assert_eq(high_freq_data.length(), 11)
  assert_eq(downsampled_data.length(), 4) // ceil(11/3) = 4
  
  assert_eq(downsampled_data[0], 1.0)
  assert_eq(downsampled_data[1], 1.3)
  assert_eq(downsampled_data[2], 1.6)
  assert_eq(downsampled_data[3], 1.9)
}

test "time_series_interpolation" {
  // 测试时间序列数据插值
  
  let known_points = [(0L, 100.0), (10L, 200.0), (20L, 150.0)]
  let interpolation_timestamp = 5L
  
  // 简单线性插值
  let mut interpolated_value = 0.0
  let mut i = 0
  while i < known_points.length() - 1 {
    let (t1, v1) = known_points[i]
    let (t2, v2) = known_points[i + 1]
    
    if interpolation_timestamp >= t1 && interpolation_timestamp <= t2 {
      let ratio = (interpolation_timestamp - t1).to_double() / (t2 - t1).to_double()
      interpolated_value = v1 + ratio * (v2 - v1)
      break
    }
    i = i + 1
  }
  
  // 验证插值结果
  assert_eq(known_points.length(), 3)
  assert_eq(interpolated_value, 150.0) // 100 + 0.5 * (200 - 100)
  assert_eq(interpolated_value > known_points[0].1, true)
  assert_eq(interpolated_value < known_points[1].1, true)
}

test "time_series_anomaly_detection" {
  // 测试时间序列异常检测
  
  let normal_data = [50.0, 52.0, 48.0, 51.0, 49.0, 53.0, 47.0, 50.0]
  let anomaly_data = [50.0, 52.0, 150.0, 51.0, 49.0, -50.0, 47.0, 50.0]
  let threshold = 20.0
  
  // 计算基准统计量
  let mut sum = 0.0
  let mut i = 0
  while i < normal_data.length() {
    sum = sum + normal_data[i]
    i = i + 1
  }
  let mean = sum / normal_data.length().to_double()
  
  // 检测异常
  let detected_anomalies = []
  i = 0
  while i < anomaly_data.length() {
    let deviation = (anomaly_data[i] - mean).abs()
    if deviation > threshold {
      detected_anomalies.push((i, anomaly_data[i], deviation))
    }
    i = i + 1
  }
  
  // 验证异常检测结果
  assert_eq(normal_data.length(), 8)
  assert_eq(mean > 48.0 && mean < 52.0, true)
  
  assert_eq(detected_anomalies.length(), 2)
  assert_eq(detected_anomalies[0].0, 2) // 索引2
  assert_eq(detected_anomalies[0].1, 150.0) // 异常值
  assert_eq(detected_anomalies[1].0, 5) // 索引5
  assert_eq(detected_anomalies[1].1, -50.0) // 异常值
}

test "time_series_compression" {
  // 测试时间序列数据压缩
  
  let raw_data = [100.0, 100.1, 99.9, 100.2, 100.0, 99.8, 100.1, 100.0]
  let compression_threshold = 0.15
  let compressed_data = []
  
  // 简单的差分压缩
  compressed_data.push(raw_data[0]) // 保留第一个值
  let mut i = 1
  while i < raw_data.length() {
    let diff = raw_data[i] - raw_data[i - 1]
    if diff.abs() > compression_threshold {
      compressed_data.push(raw_data[i])
    }
    i = i + 1
  }
  
  // 验证压缩结果
  assert_eq(raw_data.length(), 8)
  assert_eq(compressed_data.length(), 3) // 显著减少
  assert_eq(compressed_data[0], 100.0)
  assert_eq(compressed_data[1], 100.2) // 超过阈值
  assert_eq(compressed_data[2], 99.8) // 超过阈值
  
  // 计算压缩率
  let compression_ratio = compressed_data.length().to_double() / raw_data.length().to_double()
  assert_eq(compression_ratio < 0.5, true) // 压缩率超过50%
}

test "time_series_forecasting" {
  // 测试时间序列预测
  
  let historical_data = [10.0, 12.0, 14.0, 16.0, 18.0, 20.0]
  let forecast_horizon = 3
  let forecasted_values = []
  
  // 简单线性趋势预测
  let mut sum_diff = 0.0
  let mut i = 1
  while i < historical_data.length() {
    sum_diff = sum_diff + (historical_data[i] - historical_data[i - 1])
    i = i + 1
  }
  let avg_diff = sum_diff / (historical_data.length() - 1).to_double()
  
  // 生成预测值
  let last_value = historical_data[historical_data.length() - 1]
  let mut j = 1
  while j <= forecast_horizon {
    let forecasted = last_value + avg_diff * j.to_double()
    forecasted_values.push(forecasted)
    j = j + 1
  }
  
  // 验证预测结果
  assert_eq(historical_data.length(), 6)
  assert_eq(avg_diff, 2.0) // 每次增加2
  assert_eq(forecasted_values.length(), 3)
  assert_eq(forecasted_values[0], 22.0)
  assert_eq(forecasted_values[1], 24.0)
  assert_eq(forecasted_values[2], 26.0)
}