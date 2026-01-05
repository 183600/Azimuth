// Azimuth Advanced Telemetry Data Processing Test Suite
// 高级遥测数据处理测试套件，验证复杂数据处理场景

test "遥测数据聚合与统计计算" {
  // 创建测试数据集
  let telemetry_data = [
    ("metric1", 10.5, "2023-01-01T10:00:00Z"),
    ("metric1", 15.2, "2023-01-01T10:01:00Z"),
    ("metric1", 12.8, "2023-01-01T10:02:00Z"),
    ("metric2", 100.0, "2023-01-01T10:00:00Z"),
    ("metric2", 120.5, "2023-01-01T10:01:00Z"),
    ("metric2", 95.3, "2023-01-01T10:02:00Z"),
    ("metric3", 50.0, "2023-01-01T10:00:00Z"),
    ("metric3", 55.5, "2023-01-01T10:01:00Z"),
    ("metric3", 48.7, "2023-01-01T10:02:00Z")
  ]
  
  // 计算metric1的统计值
  let mut metric1_sum = 0.0
  let mut metric1_count = 0
  let mut metric1_max = 0.0
  let mut metric1_min = 999999.0
  
  for (metric_name, value, _) in telemetry_data {
    if metric_name == "metric1" {
      metric1_sum = metric1_sum + value
      metric1_count = metric1_count + 1
      if value > metric1_max {
        metric1_max = value
      }
      if value < metric1_min {
        metric1_min = value
      }
    }
  }
  
  let metric1_avg = metric1_sum / metric1_count.to_float()
  
  // 验证计算结果
  assert_eq(metric1_count, 3)
  assert_true(abs_float(metric1_sum - 38.5) < 0.001)
  assert_true(abs_float(metric1_avg - 12.8333333333) < 0.001)
  assert_true(abs_float(metric1_max - 15.2) < 0.001)
  assert_true(abs_float(metric1_min - 10.5) < 0.001)
  
  // 计算metric2的统计值
  let mut metric2_sum = 0.0
  let mut metric2_count = 0
  
  for (metric_name, value, _) in telemetry_data {
    if metric_name == "metric2" {
      metric2_sum = metric2_sum + value
      metric2_count = metric2_count + 1
    }
  }
  
  let metric2_avg = metric2_sum / metric2_count.to_float()
  
  // 验证metric2计算结果
  assert_eq(metric2_count, 3)
  assert_true(abs_float(metric2_sum - 315.8) < 0.001)
  assert_true(abs_float(metric2_avg - 105.2666666667) < 0.001)
}

test "遥测数据时间序列分析" {
  // 创建时间序列数据
  let time_series_data = [
    (1640995200L, 10.5),  // 2022-01-01 00:00:00
    (1640995260L, 12.3),  // 2022-01-01 00:01:00
    (1640995320L, 11.8),  // 2022-01-01 00:02:00
    (1640995380L, 13.2),  // 2022-01-01 00:03:00
    (1640995440L, 14.7),  // 2022-01-01 00:04:00
    (1640995500L, 15.1),  // 2022-01-01 00:05:00
    (1640995560L, 13.9),  // 2022-01-01 00:06:00
    (1640995620L, 12.4),  // 2022-01-01 00:07:00
    (1640995680L, 11.6),  // 2022-01-01 00:08:00
    (1640995740L, 10.9)   // 2022-01-01 00:09:00
  ]
  
  // 计算移动平均值（窗口大小为3）
  let mut moving_averages = []
  
  for i in 2..time_series_data.length() {
    let sum = time_series_data[i-2].1 + time_series_data[i-1].1 + time_series_data[i].1
    let avg = sum / 3.0
    moving_averages = moving_averages + [(time_series_data[i].0, avg)]
  }
  
  // 验证移动平均值
  assert_eq(moving_averages.length(), 8)
  assert_true(abs_float(moving_averages[0].1 - 11.5333333333) < 0.001) // (10.5+12.3+11.8)/3
  assert_true(abs_float(moving_averages[1].1 - 12.4333333333) < 0.001) // (12.3+11.8+13.2)/3
  assert_true(abs_float(moving_averages[2].1 - 13.2333333333) < 0.001) // (11.8+13.2+14.7)/3
  assert_true(abs_float(moving_averages[3].1 - 14.3333333333) < 0.001) // (13.2+14.7+15.1)/3
  assert_true(abs_float(moving_averages[4].1 - 14.5666666667) < 0.001) // (14.7+15.1+13.9)/3
  assert_true(abs_float(moving_averages[5].1 - 13.8) < 0.001)        // (15.1+13.9+12.4)/3
  assert_true(abs_float(moving_averages[6].1 - 12.6333333333) < 0.001) // (13.9+12.4+11.6)/3
  assert_true(abs_float(moving_averages[7].1 - 11.6333333333) < 0.001) // (12.4+11.6+10.9)/3
  
  // 计算趋势（简单线性回归）
  let n = time_series_data.length().to_float()
  let mut sum_x = 0.0
  let mut sum_y = 0.0
  let mut sum_xy = 0.0
  let mut sum_x2 = 0.0
  
  for i in 0..time_series_data.length() {
    let x = i.to_float()
    let y = time_series_data[i].1
    sum_x = sum_x + x
    sum_y = sum_y + y
    sum_xy = sum_xy + (x * y)
    sum_x2 = sum_x2 + (x * x)
  }
  
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  let intercept = (sum_y - slope * sum_x) / n
  
  // 验证趋势计算
  assert_true(abs_float(slope - 0.1818181818) < 0.001)
  assert_true(abs_float(intercept - 11.9) < 0.001)
}

test "遥测数据过滤与转换" {
  // 创建原始遥测数据
  let raw_telemetry = [
    ("cpu.usage", 85.2, "host1", "production"),
    ("memory.usage", 62.7, "host1", "production"),
    ("disk.usage", 45.3, "host1", "production"),
    ("cpu.usage", 12.5, "host2", "development"),
    ("memory.usage", 34.8, "host2", "development"),
    ("disk.usage", 28.9, "host2", "development"),
    ("cpu.usage", 92.1, "host3", "production"),
    ("memory.usage", 78.4, "host3", "production"),
    ("disk.usage", 56.7, "host3", "production"),
    ("cpu.usage", 25.6, "host4", "staging"),
    ("memory.usage", 42.3, "host4", "staging"),
    ("disk.usage", 31.2, "host4", "staging")
  ]
  
  // 过滤生产环境的数据
  let mut production_data = []
  for (metric_name, value, host, environment) in raw_telemetry {
    if environment == "production" {
      production_data = production_data + [(metric_name, value, host, environment)]
    }
  }
  
  // 验证过滤结果
  assert_eq(production_data.length(), 6)
  
  // 按主机分组生产环境数据
  let mut host1_metrics = []
  let mut host3_metrics = []
  
  for (metric_name, value, host, environment) in production_data {
    if host == "host1" {
      host1_metrics = host1_metrics + [(metric_name, value)]
    } else if host == "host3" {
      host3_metrics = host3_metrics + [(metric_name, value)]
    }
  }
  
  // 验证分组结果
  assert_eq(host1_metrics.length(), 3)
  assert_eq(host3_metrics.length(), 3)
  
  // 转换数据：将CPU使用率转换为负载等级
  let mut load_levels = []
  for (metric_name, value, host, environment) in production_data {
    if metric_name == "cpu.usage" {
      let load_level = if value > 80.0 {
        "high"
      } else if value > 50.0 {
        "medium"
      } else {
        "low"
      }
      load_levels = load_levels + [(host, load_level, value)]
    }
  }
  
  // 验证转换结果
  assert_eq(load_levels.length(), 2)
  
  // 验证负载等级
  let mut host1_load = ""
  let mut host3_load = ""
  
  for (host, level, value) in load_levels {
    if host == "host1" {
      host1_load = level
      assert_true(abs_float(value - 85.2) < 0.001)
    } else if host == "host3" {
      host3_load = level
      assert_true(abs_float(value - 92.1) < 0.001)
    }
  }
  
  assert_eq(host1_load, "high")
  assert_eq(host3_load, "high")
}

test "遥测数据异常检测" {
  // 创建包含异常值的遥测数据
  let telemetry_with_anomalies = [
    (1640995200L, 25.5),  // 正常值
    (1640995260L, 26.2),  // 正常值
    (1640995320L, 24.8),  // 正常值
    (1640995380L, 95.7),  // 异常值
    (1640995440L, 25.1),  // 正常值
    (1640995500L, 26.8),  // 正常值
    (1640995560L, 24.3),  // 正常值
    (1640995620L, 12.1),  // 异常值
    (1640995680L, 25.7),  // 正常值
    (1640995740L, 26.4)   // 正常值
  ]
  
  // 计算均值和标准差
  let n = telemetry_with_anomalies.length().to_float()
  let mut sum = 0.0
  
  for (_, value) in telemetry_with_anomalies {
    sum = sum + value
  }
  
  let mean = sum / n
  
  // 计算标准差
  let mut variance_sum = 0.0
  for (_, value) in telemetry_with_anomalies {
    let diff = value - mean
    variance_sum = variance_sum + (diff * diff)
  }
  
  let variance = variance_sum / n
  let std_dev = sqrt(variance)
  
  // 使用3σ规则检测异常值
  let mut anomalies = []
  let mut normal_values = []
  
  for (timestamp, value) in telemetry_with_anomalies {
    let z_score = abs_float(value - mean) / std_dev
    if z_score > 3.0 {
      anomalies = anomalies + [(timestamp, value, z_score)]
    } else {
      normal_values = normal_values + [(timestamp, value)]
    }
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 2)
  assert_eq(normal_values.length(), 8)
  
  // 验证检测到的异常值
  let mut anomaly_timestamps = []
  for (timestamp, value, z_score) in anomalies {
    anomaly_timestamps = anomaly_timestamps + [timestamp]
    assert_true(z_score > 3.0)
  }
  
  assert_true(anomaly_timestamps.contains(1640995380L)) // 95.7的异常值
  assert_true(anomaly_timestamps.contains(1640995620L)) // 12.1的异常值
  
  // 验证正常值
  let mut normal_timestamps = []
  for (timestamp, value) in normal_values {
    normal_timestamps = normal_timestamps + [timestamp]
    assert_true(value < 50.0 && value > 20.0)
  }
  
  assert_eq(normal_timestamps.length(), 8)
}

test "遥测数据降采样与聚合" {
  // 创建高频遥测数据（每秒一个数据点）
  let high_frequency_data = []
  let mut base_timestamp = 1640995200L // 2022-01-01 00:00:00
  
  // 生成1小时的数据（每秒一个点）
  for i in 0..3600 {
    let value = 25.0 + (i.to_float() * 0.01) + (sin(i.to_float() * 0.1) * 5.0)
    high_frequency_data = high_frequency_data + [(base_timestamp + i.to_long(), value)]
  }
  
  assert_eq(high_frequency_data.length(), 3600)
  
  // 降采样到每分钟（60秒）
  let mut downsampled_data = []
  let mut i = 0
  
  while i < high_frequency_data.length() {
    let window_start = i
    let window_end = if i + 60 < high_frequency_data.length() {
      i + 60
    } else {
      high_frequency_data.length()
    }
    
    // 计算窗口内的平均值
    let mut sum = 0.0
    let mut count = 0
    
    for j in window_start..window_end {
      sum = sum + high_frequency_data[j].1
      count = count + 1
    }
    
    let avg_value = sum / count.to_float()
    let window_timestamp = high_frequency_data[window_start].0
    
    downsampled_data = downsampled_data + [(window_timestamp, avg_value)]
    i = i + 60
  }
  
  // 验证降采样结果
  assert_eq(downsampled_data.length(), 60) // 3600秒/60秒 = 60分钟
  
  // 验证第一个和最后一个数据点
  assert_eq(downsampled_data[0].0, 1640995200L) // 第一个时间戳
  assert_true(abs_float(downsampled_data[0].1 - 25.465816) < 0.001) // 第一个窗口的平均值
  
  assert_eq(downsampled_data[59].0, 1640998800L) // 最后一个时间戳
  assert_true(abs_float(downsampled_data[59].1 - 61.465816) < 0.001) // 最后一个窗口的平均值
  
  // 计算降采样数据的统计信息
  let mut downsampled_sum = 0.0
  for (_, value) in downsampled_data {
    downsampled_sum = downsampled_sum + value
  }
  
  let downsampled_avg = downsampled_sum / downsampled_data.length().to_float()
  
  // 验证降采样数据的平均值
  assert_true(abs_float(downsampled_avg - 43.465816) < 0.001)
}

// 辅助函数
fn abs_float(x : Float) -> Float {
  if x < 0.0 {
    -x
  } else {
    x
  }
}

fn sqrt(x : Float) -> Float {
  // 简单的平方根实现（实际应该使用数学库）
  if x == 0.0 {
    return 0.0
  }
  
  let mut guess = x / 2.0
  let mut prev = 0.0
  
  while abs_float(guess - prev) > 0.0001 {
    prev = guess
    guess = (guess + x / guess) / 2.0
  }
  
  guess
}

fn sin(x : Float) -> Float {
  // 简单的正弦函数实现（实际应该使用数学库）
  // 使用泰勒级数近似
  let mut result = 0.0
  let mut term = x
  let mut x_squared = x * x
  
  for i in 0..10 {
    result = result + term
    term = term * (-x_squared) / ((2 * i + 2) * (2 * i + 3)).to_float()
  }
  
  result
}