// Azimuth Telemetry System - Time Series Data Aggregation and Analysis Tests
// This file contains test cases for time series data aggregation and analysis functionality

test "时间序列数据基本聚合测试" {
  // 时间序列数据点
  struct TimeSeriesPoint {
    timestamp : Int
    value : Double
    tags : Array[(String, String)]
  }
  
  // 创建测试数据
  let data_points = [
    TimeSeriesPoint(1640995200000, 10.5, [("service", "api"), ("region", "us-east")]),
    TimeSeriesPoint(1640995260000, 12.3, [("service", "api"), ("region", "us-east")]),
    TimeSeriesPoint(1640995320000, 11.8, [("service", "api"), ("region", "us-east")]),
    TimeSeriesPoint(1640995380000, 13.2, [("service", "api"), ("region", "us-east")]),
    TimeSeriesPoint(1640995440000, 12.9, [("service", "api"), ("region", "us-east")])
  ]
  
  // 计算平均值
  func calculate_average(points : Array[TimeSeriesPoint]) -> Double {
    let sum = points.fold(0.0, fn(acc, p) { acc + p.value })
    sum / points.length().to_double()
  }
  
  // 计算最大值和最小值
  func calculate_min_max(points : Array[TimeSeriesPoint]) -> (Double, Double) {
    let min_value = points.fold(Double::max_value(), fn(acc, p) { 
      if p.value < acc { p.value } else { acc } 
    })
    let max_value = points.fold(Double::min_value(), fn(acc, p) { 
      if p.value > acc { p.value } else { acc } 
    })
    (min_value, max_value)
  }
  
  let average = calculate_average(data_points)
  let (min_value, max_value) = calculate_min_max(data_points)
  
  assert_true(average > 11.0 && average < 13.0)
  assert_eq(min_value, 10.5)
  assert_eq(max_value, 13.2)
}

test "时间序列数据时间窗口聚合测试" {
  struct TimeSeriesPoint {
    timestamp : Int
    value : Double
    metric_name : String
  }
  
  // 创建1小时的测试数据（每分钟一个点）
  let mut hourly_data = []
  let base_time = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  for i in 0..60 {
    hourly_data.push(TimeSeriesPoint(
      base_time + (i * 60000), // 每分钟
      10.0 + (i.to_double() * 0.1), // 递增值
      "response_time"
    ))
  }
  
  // 按10分钟窗口聚合
  func aggregate_by_window(points : Array[TimeSeriesPoint], window_size_minutes : Int) -> Array[(Int, Double, Int)] {
    let window_size_ms = window_size_minutes * 60000
    let mut windows = []
    let mut i = 0
    
    while i < points.length() {
      let window_start = points[i].timestamp
      let window_end = window_start + window_size_ms
      
      let mut window_points = []
      let j = i
      while j < points.length() && points[j].timestamp < window_end {
        window_points.push(points[j])
        j = j + 1
      }
      
      if window_points.length() > 0 {
        let avg_value = window_points.fold(0.0, fn(acc, p) { acc + p.value }) / window_points.length().to_double()
        windows.push((window_start, avg_value, window_points.length()))
      }
      
      i = j
    }
    
    windows
  }
  
  let ten_minute_windows = aggregate_by_window(hourly_data, 10)
  assert_eq(ten_minute_windows.length(), 6) // 60分钟 / 10分钟窗口 = 6个窗口
  
  // 验证第一个窗口的数据
  assert_eq(ten_minute_windows[0].2, 10) // 第一个窗口应该有10个数据点
  assert_true(ten_minute_windows[0].1 > 10.0 && ten_minute_windows[0].1 < 11.0)
}

test "时间序列数据下采样测试" {
  struct TimeSeriesPoint {
    timestamp : Int
    value : Double
  }
  
  // 创建高频数据（每秒一个点，共1小时）
  let mut high_freq_data = []
  let base_time = 1640995200000
  
  for i in 0..3600 {
    high_freq_data.push(TimeSeriesPoint(
      base_time + (i * 1000), // 每秒
      100.0 + (i.to_double() * 0.01) + (Double::random() * 5.0) // 基础值 + 增长 + 随机噪声
    ))
  }
  
  // 下采样到每分钟一个点
  func downsample(points : Array[TimeSeriesPoint], target_interval_seconds : Int) -> Array[TimeSeriesPoint] {
    let target_interval_ms = target_interval_seconds * 1000
    let mut downsampled = []
    let mut i = 0
    
    while i < points.length() {
      let window_start = points[i].timestamp
      let window_end = window_start + target_interval_ms
      
      let mut window_points = []
      let j = i
      while j < points.length() && points[j].timestamp < window_end {
        window_points.push(points[j])
        j = j + 1
      }
      
      if window_points.length() > 0 {
        // 使用平均值作为下采样值
        let avg_value = window_points.fold(0.0, fn(acc, p) { acc + p.value }) / window_points.length().to_double()
        downsampled.push(TimeSeriesPoint(window_start, avg_value))
      }
      
      i = j
    }
    
    downsampled
  }
  
  let minute_data = downsample(high_freq_data, 60)
  assert_eq(minute_data.length(), 60) // 1小时下采样到每分钟应该有60个点
  assert_true(minute_data.length() < high_freq_data.length())
}

test "时间序列数据趋势分析测试" {
  struct TimeSeriesPoint {
    timestamp : Int
    value : Double
  }
  
  // 创建具有明显趋势的数据
  let mut trend_data = []
  let base_time = 1640995200000
  
  for i in 0..100 {
    // 线性增长趋势 + 一些随机噪声
    let trend_value = 50.0 + (i.to_double() * 2.0) + (Double::random() * 10.0 - 5.0)
    trend_data.push(TimeSeriesPoint(base_time + (i * 60000), trend_value))
  }
  
  // 计算简单线性趋势
  func calculate_linear_trend(points : Array[TimeSeriesPoint]) -> (Double, Double) {
    let n = points.length().to_double()
    let sum_x = (0..points.length()).fold(0, fn(acc, i) { acc + i }).to_double()
    let sum_y = points.fold(0.0, fn(acc, p) { acc + p.value })
    let sum_xy = points.fold(0.0, fn(acc, p) { 
      let i = points.index_of(p).unwrap_or(0)
      acc + (i.to_double() * p.value) 
    })
    let sum_x2 = (0..points.length()).fold(0, fn(acc, i) { acc + (i * i) }).to_double()
    
    // 计算斜率和截距 (y = mx + b)
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    (slope, intercept)
  }
  
  let (slope, intercept) = calculate_linear_trend(trend_data)
  
  // 验证趋势方向（应该是正增长）
  assert_true(slope > 1.5 && slope < 2.5) // 斜率应该在2左右
  assert_true(intercept > 45.0 && intercept < 55.0) // 截距应该在50左右
}

test "时间序列数据异常检测测试" {
  struct TimeSeriesPoint {
    timestamp : Int
    value : Double
  }
  
  // 创建包含异常值的数据
  let normal_data = [
    TimeSeriesPoint(1640995200000, 10.5),
    TimeSeriesPoint(1640995260000, 11.2),
    TimeSeriesPoint(1640995320000, 10.8),
    TimeSeriesPoint(1640995380000, 11.0),
    TimeSeriesPoint(1640995440000, 10.9),
    TimeSeriesPoint(1640995500000, 50.2), // 异常值
    TimeSeriesPoint(1640995560000, 11.1),
    TimeSeriesPoint(1640995620000, 10.7),
    TimeSeriesPoint(1640995680000, 11.3),
    TimeSeriesPoint(1640995740000, 10.6)
  ]
  
  // 使用Z-score检测异常值
  func detect_anomalies(points : Array[TimeSeriesPoint], threshold : Double) -> Array[Int] {
    let values = points.map(fn(p) { p.value })
    let mean = values.fold(0.0, fn(acc, v) { acc + v }) / values.length().to_double()
    let variance = values.fold(0.0, fn(acc, v) { acc + (v - mean) * (v - mean) }) / values.length().to_double()
    let std_dev = Double::sqrt(variance)
    
    let mut anomaly_indices = []
    for i in 0..points.length() {
      let z_score = Double::abs(points[i].value - mean) / std_dev
      if z_score > threshold {
        anomaly_indices.push(i)
      }
    }
    
    anomaly_indices
  }
  
  let anomalies = detect_anomalies(normal_data, 2.0)
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0], 5) // 第6个点是异常值
}

test "时间序列数据季节性分析测试" {
  struct TimeSeriesPoint {
    timestamp : Int
    value : Double
  }
  
  // 创建具有日季节性的数据（24小时模式）
  let mut seasonal_data = []
  let base_time = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  for day in 0..7 { // 7天数据
    for hour in 0..24 { // 每天24小时
      // 模拟日模式：白天高，夜晚低
      let base_value = if hour >= 8 && hour <= 20 { 80.0 } else { 20.0 }
      let noise = Double::random() * 10.0
      
      seasonal_data.push(TimeSeriesPoint(
        base_time + (day * 24 * 60 * 60 * 1000) + (hour * 60 * 60 * 1000),
        base_value + noise
      ))
    }
  }
  
  // 检测季节性模式
  func detect_daily_seasonality(points : Array[TimeSeriesPoint]) -> Bool {
    if points.length() < 48 { return false } // 至少需要2天数据
    
    // 计算每小时的平均值
    let mut hourly_averages = []
    for hour in 0..24 {
      let mut hour_values = []
      for i in 0..points.length() {
        let point_hour = ((points[i].timestamp / (60 * 60 * 1000)) % 24).to_int()
        if point_hour == hour {
          hour_values.push(points[i].value)
        }
      }
      
      if hour_values.length() > 0 {
        let avg = hour_values.fold(0.0, fn(acc, v) { acc + v }) / hour_values.length().to_double()
        hourly_averages.push((hour, avg))
      }
    }
    
    // 检查是否存在明显的日模式（白天vs夜晚的差异）
    let day_hours = hourly_averages.filter(fn(h) { h.0 >= 8 && h.0 <= 20 })
    let night_hours = hourly_averages.filter(fn(h) { h.0 < 8 || h.0 > 20 })
    
    if day_hours.length() == 0 || night_hours.length() == 0 { return false }
    
    let day_avg = day_hours.fold(0.0, fn(acc, h) { acc + h.1 }) / day_hours.length().to_double()
    let night_avg = night_hours.fold(0.0, fn(acc, h) { acc + h.1 }) / night_hours.length().to_double()
    
    // 如果白天平均值显著高于夜晚平均值，则认为存在季节性
    day_avg > night_avg * 1.5
  }
  
  assert_true(detect_daily_seasonality(seasonal_data))
}