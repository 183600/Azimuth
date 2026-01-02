// Azimuth Time Series Data Processing Test Suite
// 时间序列数据处理测试用例

// 测试1: 时间序列数据结构
test "时间序列数据结构测试" {
  // 时间序列数据点结构
  struct TimeSeriesPoint {
    timestamp : Int64  // 纳秒级时间戳
    value : Double     // 数据值
    labels : Array[(String, String)]  // 标签
  }
  
  // 时间序列结构
  struct TimeSeries {
    name : String
    points : Array[TimeSeriesPoint]
    aggregation : Option[String]  // 聚合类型
    interval : Option[Int64]      // 采样间隔
  }
  
  // 创建时间序列点
  fn TimeSeriesPoint::new(timestamp : Int64, value : Double) -> TimeSeriesPoint {
    TimeSeriesPoint::{ timestamp, value, labels: [] }
  }
  
  fn TimeSeriesPoint::with_labels(timestamp : Int64, value : Double, labels : Array[(String, String)]) -> TimeSeriesPoint {
    TimeSeriesPoint::{ timestamp, value, labels }
  }
  
  // 创建时间序列
  fn TimeSeries::new(name : String) -> TimeSeries {
    TimeSeries::{ name, points: [], aggregation: None, interval: None }
  }
  
  fn TimeSeries::with_aggregation(name : String, aggregation : String, interval : Int64) -> TimeSeries {
    TimeSeries::{ name, points: [], aggregation: Some(aggregation), interval: Some(interval) }
  }
  
  // 添加数据点
  fn TimeSeries::add_point(series : TimeSeries, point : TimeSeriesPoint) -> TimeSeries {
    TimeSeries::{ 
      name: series.name, 
      points: series.points.push(point), 
      aggregation: series.aggregation, 
      interval: series.interval 
    }
  }
  
  // 获取时间范围
  fn TimeSeries::time_range(series : TimeSeries) -> (Option[Int64], Option[Int64]) {
    match series.points {
      [] => (None, None)
      [head, ..tail] => {
        let mut min_time = head.timestamp
        let mut max_time = head.timestamp
        
        for point in tail {
          if point.timestamp < min_time {
            min_time = point.timestamp
          }
          if point.timestamp > max_time {
            max_time = point.timestamp
          }
        }
        
        (Some(min_time), Some(max_time))
      }
    }
  }
  
  // 获取值范围
  fn TimeSeries::value_range(series : TimeSeries) -> (Option[Double], Option[Double]) {
    match series.points {
      [] => (None, None)
      [head, ..tail] => {
        let mut min_value = head.value
        let mut max_value = head.value
        
        for point in tail {
          if point.value < min_value {
            min_value = point.value
          }
          if point.value > max_value {
            max_value = point.value
          }
        }
        
        (Some(min_value), Some(max_value))
      }
    }
  }
  
  // 测试时间序列创建
  let cpu_series = TimeSeries::new("cpu.usage")
  assert_eq(cpu_series.name, "cpu.usage")
  assert_eq(cpu_series.points.length(), 0)
  assert_eq(cpu_series.aggregation, None)
  assert_eq(cpu_series.interval, None)
  
  let memory_series = TimeSeries::with_aggregation("memory.usage", "avg", 60000000000L)  // 1分钟间隔
  assert_eq(memory_series.name, "memory.usage")
  assert_eq(memory_series.aggregation, Some("avg"))
  assert_eq(memory_series.interval, Some(60000000000L))
  
  // 创建测试数据点
  let base_time = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  let point1 = TimeSeriesPoint::new(base_time, 25.5)
  let point2 = TimeSeriesPoint::with_labels(base_time + 60000000000L, 30.2, [("service", "web")])
  let point3 = TimeSeriesPoint::new(base_time + 120000000000L, 28.7)
  
  // 添加数据点到时间序列
  let cpu_series1 = TimeSeries::add_point(cpu_series, point1)
  let cpu_series2 = TimeSeries::add_point(cpu_series1, point2)
  let cpu_series3 = TimeSeries::add_point(cpu_series2, point3)
  
  assert_eq(cpu_series3.points.length(), 3)
  assert_eq(cpu_series3.points[0].value, 25.5)
  assert_eq(cpu_series3.points[1].value, 30.2)
  assert_eq(cpu_series3.points[2].value, 28.7)
  
  // 测试时间范围
  let (min_time, max_time) = TimeSeries::time_range(cpu_series3)
  assert_eq(min_time, Some(base_time))
  assert_eq(max_time, Some(base_time + 120000000000L))
  
  // 测试值范围
  let (min_value, max_value) = TimeSeries::value_range(cpu_series3)
  assert_eq(min_value, Some(25.5))
  assert_eq(max_value, Some(30.2))
}

// 测试2: 时间序列聚合操作
test "时间序列聚合操作测试" {
  // 复用上一测试的结构定义
  
  // 聚合函数
  fn aggregate_sum(series : TimeSeries, window : Int64) -> TimeSeries {
    let base_time = match series.points {
      [] => return series
      [head, ..] => head.timestamp
    }
    
    fn aggregate_window(points : Array[TimeSeriesPoint], window_start : Int64, window_end : Int64) -> Double {
      let mut sum = 0.0
      for point in points {
        if point.timestamp >= window_start && point.timestamp < window_end {
          sum = sum + point.value
        }
      }
      sum
    }
    
    let (min_time, max_time) = TimeSeries::time_range(series)
    match (min_time, max_time) {
      (None, None) | (_, None) => series
      (Some(min), Some(max)) => {
        let mut aggregated_points = []
        let mut current_start = min
        
        while current_start < max {
          let current_end = current_start + window
          let sum = aggregate_window(series.points, current_start, current_end)
          let aggregated_point = TimeSeriesPoint::new(current_start + window / 2, sum)  // 窗口中点
          aggregated_points = aggregated_points.push(aggregated_point)
          current_start = current_end
        }
        
        TimeSeries::{ 
          name: series.name + ".sum", 
          points: aggregated_points, 
          aggregation: Some("sum"), 
          interval: Some(window) 
        }
      }
    }
  }
  
  fn aggregate_avg(series : TimeSeries, window : Int64) -> TimeSeries {
    let base_time = match series.points {
      [] => return series
      [head, ..] => head.timestamp
    }
    
    fn aggregate_window_avg(points : Array[TimeSeriesPoint], window_start : Int64, window_end : Int64) -> Double {
      let mut sum = 0.0
      let mut count = 0
      
      for point in points {
        if point.timestamp >= window_start && point.timestamp < window_end {
          sum = sum + point.value
          count = count + 1
        }
      }
      
      if count > 0 { sum / count.to_double() } else { 0.0 }
    }
    
    let (min_time, max_time) = TimeSeries::time_range(series)
    match (min_time, max_time) {
      (None, None) | (_, None) => series
      (Some(min), Some(max)) => {
        let mut aggregated_points = []
        let mut current_start = min
        
        while current_start < max {
          let current_end = current_start + window
          let avg = aggregate_window_avg(series.points, current_start, current_end)
          let aggregated_point = TimeSeriesPoint::new(current_start + window / 2, avg)
          aggregated_points = aggregated_points.push(aggregated_point)
          current_start = current_end
        }
        
        TimeSeries::{ 
          name: series.name + ".avg", 
          points: aggregated_points, 
          aggregation: Some("avg"), 
          interval: Some(window) 
        }
      }
    }
  }
  
  fn aggregate_max(series : TimeSeries, window : Int64) -> TimeSeries {
    let base_time = match series.points {
      [] => return series
      [head, ..] => head.timestamp
    }
    
    fn aggregate_window_max(points : Array[TimeSeriesPoint], window_start : Int64, window_end : Int64) -> Double {
      let mut max = -999999999.0  // 负无穷大
      
      for point in points {
        if point.timestamp >= window_start && point.timestamp < window_end {
          if point.value > max {
            max = point.value
          }
        }
      }
      
      max
    }
    
    let (min_time, max_time) = TimeSeries::time_range(series)
    match (min_time, max_time) {
      (None, None) | (_, None) => series
      (Some(min), Some(max)) => {
        let mut aggregated_points = []
        let mut current_start = min
        
        while current_start < max {
          let current_end = current_start + window
          let max_val = aggregate_window_max(series.points, current_start, current_end)
          let aggregated_point = TimeSeriesPoint::new(current_start + window / 2, max_val)
          aggregated_points = aggregated_points.push(aggregated_point)
          current_start = current_end
        }
        
        TimeSeries::{ 
          name: series.name + ".max", 
          points: aggregated_points, 
          aggregation: Some("max"), 
          interval: Some(window) 
        }
      }
    }
  }
  
  // 创建测试数据
  let base_time = 1640995200000000000L
  let series = TimeSeries::new("test.metric")
  
  let mut test_series = series
  for i in 0..60 {
    let timestamp = base_time + (i * 1000000000L)  // 每秒一个数据点
    let value = 10.0 + (i % 10).to_double()  // 10-19之间的值
    let point = TimeSeriesPoint::new(timestamp, value)
    test_series = TimeSeries::add_point(test_series, point)
  }
  
  // 测试求和聚合（10秒窗口）
  let sum_series = aggregate_sum(test_series, 10000000000L)
  assert_eq(sum_series.name, "test.metric.sum")
  assert_eq(sum_series.aggregation, Some("sum"))
  assert_eq(sum_series.interval, Some(10000000000L))
  assert_eq(sum_series.points.length(), 6)  // 60秒/10秒 = 6个窗口
  
  // 验证第一个窗口的求和结果
  // 第一个窗口包含10个值: 10, 11, 12, 13, 14, 15, 16, 17, 18, 19
  // 求和 = 145
  assert_true(sum_series.points[0].value > 140.0 && sum_series.points[0].value < 150.0)
  
  // 测试平均值聚合（10秒窗口）
  let avg_series = aggregate_avg(test_series, 10000000000L)
  assert_eq(avg_series.name, "test.metric.avg")
  assert_eq(avg_series.aggregation, Some("avg"))
  assert_eq(avg_series.points.length(), 6)
  
  // 验证第一个窗口的平均值结果
  // 平均值 = 145 / 10 = 14.5
  assert_true(avg_series.points[0].value > 14.0 && avg_series.points[0].value < 15.0)
  
  // 测试最大值聚合（10秒窗口）
  let max_series = aggregate_max(test_series, 10000000000L)
  assert_eq(max_series.name, "test.metric.max")
  assert_eq(max_series.aggregation, Some("max"))
  assert_eq(max_series.points.length(), 6)
  
  // 验证第一个窗口的最大值结果
  // 最大值 = 19
  assert_true(max_series.points[0].value > 18.0 && max_series.points[0].value < 20.0)
}

// 测试3: 时间序列重采样
test "时间序列重采样测试" {
  // 复用结构定义
  
  // 上采样（增加数据点）
  fn upsample_linear(series : TimeSeries, target_interval : Int64) -> TimeSeries {
    let (min_time, max_time) = TimeSeries::time_range(series)
    match (min_time, max_time) {
      (None, None) | (_, None) => series
      (Some(min), Some(max)) => {
        let mut upsampled_points = []
        let mut current_time = min
        
        // 找到当前时间点两侧的原始数据点进行线性插值
        fn interpolate_linear(points : Array[TimeSeriesPoint], timestamp : Int64) -> Double {
          // 找到时间点前后的点
          let mut before_point = None
          let mut after_point = None
          
          for point in points {
            if point.timestamp <= timestamp {
              before_point = Some(point)
            } else if point.timestamp > timestamp && after_point.is_none() {
              after_point = Some(point)
            }
          }
          
          match (before_point, after_point) {
            (Some(before), Some(after)) => {
              let ratio = (timestamp - before.timestamp).to_double() / (after.timestamp - before.timestamp).to_double()
              before.value + ratio * (after.value - before.value)
            }
            (Some(point), None) => point.value
            (None, Some(point)) => point.value
            (None, None) => 0.0
          }
        }
        
        while current_time <= max_time {
          let value = interpolate_linear(series.points, current_time)
          let point = TimeSeriesPoint::new(current_time, value)
          upsampled_points = upsampled_points.push(point)
          current_time = current_time + target_interval
        }
        
        TimeSeries::{ 
          name: series.name + ".upsampled", 
          points: upsampled_points, 
          aggregation: series.aggregation, 
          interval: Some(target_interval) 
        }
      }
    }
  }
  
  // 下采样（减少数据点）
  fn downsample_avg(series : TimeSeries, target_interval : Int64) -> TimeSeries {
    let (min_time, max_time) = TimeSeries::time_range(series)
    match (min_time, max_time) {
      (None, None) | (_, None) => series
      (Some(min), Some(max)) => {
        let mut downsampled_points = []
        let mut current_start = min
        
        while current_start < max {
          let current_end = current_start + target_interval
          
          // 计算窗口内的平均值
          let mut sum = 0.0
          let mut count = 0
          
          for point in series.points {
            if point.timestamp >= current_start && point.timestamp < current_end {
              sum = sum + point.value
              count = count + 1
            }
          }
          
          let avg = if count > 0 { sum / count.to_double() } else { 0.0 }
          let point = TimeSeriesPoint::new(current_start + target_interval / 2, avg)
          downsampled_points = downsampled_points.push(point)
          current_start = current_end
        }
        
        TimeSeries::{ 
          name: series.name + ".downsampled", 
          points: downsampled_points, 
          aggregation: Some("avg"), 
          interval: Some(target_interval) 
        }
      }
    }
  }
  
  // 创建测试数据（每5分钟一个数据点）
  let base_time = 1640995200000000000L
  let series = TimeSeries::new("test.metric")
  
  let mut test_series = series
  for i in 0..12 {
    let timestamp = base_time + (i * 300000000000L)  // 5分钟间隔
    let value = 100.0 + (i * 5.0)  // 线性增长
    let point = TimeSeriesPoint::new(timestamp, value)
    test_series = TimeSeries::add_point(test_series, point)
  }
  
  assert_eq(test_series.points.length(), 12)
  
  // 测试上采样（从5分钟到1分钟）
  let upsampled_series = upsample_linear(test_series, 60000000000L)  // 1分钟间隔
  assert_eq(upsampled_series.name, "test.metric.upsampled")
  assert_eq(upsampled_series.interval, Some(60000000000L))
  
  // 12个5分钟点应该生成约60个1分钟点
  assert_true(upsampled_series.points.length() >= 55 && upsampled_series.points.length() <= 65)
  
  // 验证第一个插值点
  assert_true(upsampled_series.points[0].value >= 100.0 && upsampled_series.points[0].value <= 105.0)
  
  // 测试下采样（从5分钟到15分钟）
  let downsampled_series = downsample_avg(test_series, 900000000000L)  // 15分钟间隔
  assert_eq(downsampled_series.name, "test.metric.downsampled")
  assert_eq(downsampled_series.aggregation, Some("avg"))
  assert_eq(downsampled_series.interval, Some(900000000000L))
  
  // 12个5分钟点应该生成4个15分钟点
  assert_eq(downsampled_series.points.length(), 4)
  
  // 验证第一个下采样点
  // 第一个15分钟窗口包含3个5分钟点: 100, 105, 110
  // 平均值 = (100 + 105 + 110) / 3 = 105
  assert_true(downsampled_series.points[0].value > 104.0 && downsampled_series.points[0].value < 106.0)
}

// 测试4: 时间序列趋势分析
test "时间序列趋势分析测试" {
  // 复用结构定义
  
  // 移动平均
  fn moving_average(series : TimeSeries, window : Int) -> TimeSeries {
    if series.points.length() < window {
      return series
    }
    
    let mut ma_points = []
    
    for i in window - 1..series.points.length() {
      let mut sum = 0.0
      for j in i - (window - 1)..i + 1 {
        sum = sum + series.points[j].value
      }
      let avg = sum / window.to_double()
      let point = TimeSeriesPoint::new(series.points[i].timestamp, avg)
      ma_points = ma_points.push(point)
    }
    
    TimeSeries::{ 
      name: series.name + ".ma" + window.to_string(), 
      points: ma_points, 
      aggregation: Some("moving_average"), 
      interval: series.interval 
    }
  }
  
  // 趋势检测
  fn detect_trend(series : TimeSeries) -> String {
    if series.points.length() < 2 {
      return "insufficient_data"
    }
    
    // 简单线性回归计算趋势
    let n = series.points.length().to_double()
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    
    for i in 0..series.points.length() {
      let x = i.to_double()
      let y = series.points[i].value
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + (x * y)
      sum_x2 = sum_x2 + (x * x)
    }
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    
    if slope > 0.1 {
      "increasing"
    } else if slope < -0.1 {
      "decreasing"
    } else {
      "stable"
    }
  }
  
  // 季节性检测
  fn detect_seasonality(series : TimeSeries, period : Int) -> Bool {
    if series.points.length() < period * 2 {
      return false
    }
    
    // 简单的自相关检测
    let mut correlation = 0.0
    let count = series.points.length() - period
    
    for i in 0..count {
      let diff = series.points[i].value - series.points[i + period].value
      correlation = correlation + (diff * diff)
    }
    
    // 如果差异很小，可能存在季节性
    correlation / count.to_double() < 10.0
  }
  
  // 异常检测
  fn detect_anomalies(series : TimeSeries, threshold : Double) -> Array[Int] {
    if series.points.length() < 3 {
      return []
    }
    
    // 计算移动平均和标准差
    let ma_series = moving_average(series, 3)
    let mut anomalies = []
    
    for i in 0..ma_series.points.length() {
      let ma_value = ma_series.points[i].value
      let actual_value = series.points[i + 2].value  // 对应的点
      let deviation = (actual_value - ma_value).abs() / ma_value
      
      if deviation > threshold {
        anomalies.push(i + 2)
      }
    }
    
    anomalies
  }
  
  // 创建测试数据
  let base_time = 1640995200000000000L
  let series = TimeSeries::new("test.metric")
  
  // 创建上升趋势数据
  let mut test_series = series
  for i in 0..30 {
    let timestamp = base_time + (i * 86400000000000L)  // 每天一个数据点
    let value = 100.0 + (i * 2.0) + (i % 7).to_double()  // 上升趋势 + 周期性
    let point = TimeSeriesPoint::new(timestamp, value)
    test_series = TimeSeries::add_point(test_series, point)
  }
  
  // 添加异常值
  let anomaly_point = TimeSeriesPoint::new(
    base_time + (15 * 86400000000000L), 
    200.0  // 异常高值
  )
  test_series = TimeSeries::add_point(test_series, anomaly_point)
  
  // 测试移动平均
  let ma_series = moving_average(test_series, 5)
  assert_eq(ma_series.name, "test.metric.ma5")
  assert_eq(ma_series.points.length(), 26)  // 30 - 5 + 1
  
  // 验证移动平均值的平滑性
  for i in 1..ma_series.points.length() {
    let prev_diff = (ma_series.points[i].value - ma_series.points[i-1].value).abs()
    let raw_prev_diff = (test_series.points[i+4].value - test_series.points[i+3].value).abs()
    assert_true(prev_diff <= raw_prev_diff)  // 移动平均应该更平滑
  }
  
  // 测试趋势检测
  let trend = detect_trend(test_series)
  assert_eq(trend, "increasing")
  
  // 测试季节性检测
  let has_seasonality = detect_seasonality(test_series, 7)  // 7天周期
  assert_true(has_seasonality)
  
  // 测试异常检测
  let anomalies = detect_anomalies(test_series, 0.2)  // 20%阈值
  assert_true(anomalies.length() > 0)
  assert_true(anomalies.contains(15))  // 应该检测到第15个点的异常
}

// 测试5: 时间序列预测
test "时间序列预测测试" {
  // 复用结构定义
  
  // 简单线性预测
  fn linear_predict(series : TimeSeries, steps : Int) -> TimeSeries {
    if series.points.length() < 2 {
      return series
    }
    
    // 计算线性回归参数
    let n = series.points.length().to_double()
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    
    for i in 0..series.points.length() {
      let x = i.to_double()
      let y = series.points[i].value
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + (x * y)
      sum_x2 = sum_x2 + (x * x)
    }
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    // 生成预测点
    let mut predicted_points = []
    let last_timestamp = series.points[series.points.length() - 1].timestamp
    let interval = match series.interval {
      Some(interval) => interval
      None => 86400000000000L  // 默认1天
    }
    
    for i in 1..steps + 1 {
      let x = (series.points.length() + i - 1).to_double()
      let predicted_value = slope * x + intercept
      let timestamp = last_timestamp + (i.to_long() * interval)
      let point = TimeSeriesPoint::new(timestamp, predicted_value)
      predicted_points = predicted_points.push(point)
    }
    
    TimeSeries::{ 
      name: series.name + ".predicted", 
      points: predicted_points, 
      aggregation: Some("linear_prediction"), 
      interval: series.interval 
    }
  }
  
  // 指数平滑预测
  fn exponential_smoothing_predict(series : TimeSeries, alpha : Double, steps : Int) -> TimeSeries {
    if series.points.length() < 1 {
      return series
    }
    
    // 计算指数平滑值
    let mut smoothed_values = []
    let mut last_smoothed = series.points[0].value
    smoothed_values = smoothed_values.push(last_smoothed)
    
    for i in 1..series.points.length() {
      let smoothed = alpha * series.points[i].value + (1.0 - alpha) * last_smoothed
      smoothed_values = smoothed_values.push(smoothed)
      last_smoothed = smoothed
    }
    
    // 生成预测点（使用最后的平滑值）
    let mut predicted_points = []
    let last_timestamp = series.points[series.points.length() - 1].timestamp
    let interval = match series.interval {
      Some(interval) => interval
      None => 86400000000000L  // 默认1天
    }
    
    for i in 1..steps + 1 {
      let timestamp = last_timestamp + (i.to_long() * interval)
      let point = TimeSeriesPoint::new(timestamp, last_smoothed)
      predicted_points = predicted_points.push(point)
    }
    
    TimeSeries::{ 
      name: series.name + ".exp_predicted", 
      points: predicted_points, 
      aggregation: Some("exponential_smoothing"), 
      interval: series.interval 
    }
  }
  
  // 预测准确性评估
  fn evaluate_prediction(actual : TimeSeries, predicted : TimeSeries) -> Double {
    if actual.points.length() != predicted.points.length() {
      return -1.0  // 无效比较
    }
    
    let mut total_error = 0.0
    for i in 0..actual.points.length() {
      let error = (actual.points[i].value - predicted.points[i].value).abs()
      total_error = total_error + error
    }
    
    total_error / actual.points.length().to_double()
  }
  
  // 创建测试数据（线性增长）
  let base_time = 1640995200000000000L
  let series = TimeSeries::new("test.metric")
  
  let mut test_series = series
  for i in 0..20 {
    let timestamp = base_time + (i * 86400000000000L)  // 每天一个数据点
    let value = 100.0 + (i * 5.0)  // 每天增长5
    let point = TimeSeriesPoint::new(timestamp, value)
    test_series = TimeSeries::add_point(test_series, point)
  }
  
  // 测试线性预测
  let linear_predicted = linear_predict(test_series, 5)
  assert_eq(linear_predicted.name, "test.metric.predicted")
  assert_eq(linear_predicted.points.length(), 5)
  
  // 验证预测值的合理性
  let last_actual_value = test_series.points[19].value  // 100 + 19*5 = 195
  let first_predicted_value = linear_predicted.points[0].value  // 应该接近200
  
  assert_true(first_predicted_value > 190.0 && first_predicted_value < 210.0)
  
  // 验证预测值的线性增长
  for i in 1..linear_predicted.points.length() {
    let diff = linear_predicted.points[i].value - linear_predicted.points[i-1].value
    assert_true(diff > 4.0 && diff < 6.0)  // 应该接近每天增长5
  }
  
  // 测试指数平滑预测
  let exp_predicted = exponential_smoothing_predict(test_series, 0.3, 5)
  assert_eq(exp_predicted.name, "test.metric.exp_predicted")
  assert_eq(exp_predicted.points.length(), 5)
  
  // 验证指数平滑预测值的稳定性
  for i in 1..exp_predicted.points.length() {
    assert_eq(exp_predicted.points[i].value, exp_predicted.points[0].value)  // 所有预测值应该相同
  }
  
  // 测试预测准确性评估
  // 使用前15个点训练，后5个点验证
  let training_series = TimeSeries::{ 
    name: test_series.name, 
    points: test_series.points.slice(0, 15), 
    aggregation: test_series.aggregation, 
    interval: test_series.interval 
  }
  
  let validation_series = TimeSeries::{ 
    name: test_series.name, 
    points: test_series.points.slice(15, 20), 
    aggregation: test_series.aggregation, 
    interval: test_series.interval 
  }
  
  let training_predicted = linear_predict(training_series, 5)
  let mae = evaluate_prediction(validation_series, training_predicted)
  
  assert_true(mae >= 0.0)  // 平均绝对误差应该非负
  assert_true(mae < 50.0)  // 误差不应该太大
}