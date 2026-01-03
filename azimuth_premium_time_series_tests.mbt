// Azimuth Time Series Data Processing Tests
// This file contains test cases for time series data processing and analysis

// Test 1: Time Series Data Structure
test "时间序列数据结构操作" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  // 定义时间序列
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    unit: String
  }
  
  // 创建测试时间序列
  let cpu_usage = {
    name: "cpu_usage",
    points: [
      { timestamp: 1000, value: 45.2, metadata: [("host", "server1")] },
      { timestamp: 2000, value: 52.8, metadata: [("host", "server1")] },
      { timestamp: 3000, value: 38.9, metadata: [("host", "server1")] },
      { timestamp: 4000, value: 61.3, metadata: [("host", "server1")] }
    ],
    unit: "percent"
  }
  
  // 验证时间序列属性
  assert_eq(cpu_usage.name, "cpu_usage")
  assert_eq(cpu_usage.points.length(), 4)
  assert_eq(cpu_usage.unit, "percent")
  
  // 获取最新值
  let latest_point = cpu_usage.points[cpu_usage.points.length() - 1]
  assert_eq(latest_point.value, 61.3)
  assert_eq(latest_point.timestamp, 4000)
}

// Test 2: Time Series Aggregation
test "时间序列聚合操作" {
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  // 时间窗口聚合函数
  let aggregate_by_window = fn(points: Array[TimeSeriesPoint], window_size_ms: Int) {
    if points.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut window_start = points[0].timestamp
    let mut window_points = []
    
    for point in points {
      if point.timestamp < window_start + window_size_ms {
        window_points = window_points.push(point)
      } else {
        // 聚合当前窗口
        if window_points.length() > 0 {
          let values = window_points.map(fn(p) { p.value })
          let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
          let avg = sum / (values.length() as Float)
          let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
          let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
          
          result = result.push({
            timestamp: window_start + window_size_ms / 2,
            value: avg,
            metadata: [("aggregation", "window"), ("count", values.length().to_string())]
          })
        }
        
        // 开始新窗口
        window_start = point.timestamp
        window_points = [point]
      }
    }
    
    // 处理最后一个窗口
    if window_points.length() > 0 {
      let values = window_points.map(fn(p) { p.value })
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / (values.length() as Float)
      
      result = result.push({
        timestamp: window_start + window_size_ms / 2,
        value: avg,
        metadata: [("aggregation", "window"), ("count", values.length().to_string())]
      })
    }
    
    result
  }
  
  // 创建测试数据
  let test_points = [
    { timestamp: 1000, value: 10.0, metadata: [] },
    { timestamp: 1500, value: 15.0, metadata: [] },
    { timestamp: 2000, value: 20.0, metadata: [] },
    { timestamp: 3000, value: 30.0, metadata: [] },
    { timestamp: 3500, value: 35.0, metadata: [] },
    { timestamp: 4000, value: 40.0, metadata: [] }
  ]
  
  // 按2秒窗口聚合
  let aggregated = aggregate_by_window(test_points, 2000)
  
  // 应该有3个聚合窗口
  assert_eq(aggregated.length(), 3)
  
  // 第一个窗口 (1000-3000): 10.0, 15.0, 20.0 -> 平均值 15.0
  assert_eq(aggregated[0].timestamp, 2000)
  assert_eq(aggregated[0].value, 15.0)
  
  // 第二个窗口 (3000-5000): 30.0, 35.0, 40.0 -> 平均值 35.0
  assert_eq(aggregated[1].timestamp, 4000)
  assert_eq(aggregated[1].value, 35.0)
}

// Test 3: Time Series Resampling
test "时间序列重采样" {
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  // 上采样函数（线性插值）
  let upsample = fn(points: Array[TimeSeriesPoint], target_interval_ms: Int) {
    if points.length() < 2 {
      return points
    }
    
    let mut result = [points[0]]
    
    for i in 0..(points.length() - 1) {
      let current = points[i]
      let next = points[i + 1]
      
      let time_diff = next.timestamp - current.timestamp
      let value_diff = next.value - current.value
      
      // 计算需要插入的点数
      let steps = time_diff / target_interval_ms
      
      if steps > 1 {
        for step in 1..steps {
          let ratio = (step * target_interval_ms) as Float / time_diff as Float
          let interpolated_value = current.value + value_diff * ratio
          let interpolated_timestamp = current.timestamp + step * target_interval_ms
          
          result = result.push({
            timestamp: interpolated_timestamp,
            value: interpolated_value,
            metadata: [("interpolated", "true")]
          })
        }
      }
      
      result = result.push(next)
    }
    
    result
  }
  
  // 下采样函数（取平均值）
  let downsample = fn(points: Array[TimeSeriesPoint], target_interval_ms: Int) {
    if points.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut window_start = points[0].timestamp
    let mut window_values = []
    
    for point in points {
      if point.timestamp < window_start + target_interval_ms {
        window_values = window_values.push(point.value)
      } else {
        // 聚合当前窗口
        if window_values.length() > 0 {
          let sum = window_values.reduce(fn(acc, v) { acc + v }, 0.0)
          let avg = sum / (window_values.length() as Float)
          
          result = result.push({
            timestamp: window_start + target_interval_ms / 2,
            value: avg,
            metadata: [("downsampled", "true")]
          })
        }
        
        // 开始新窗口
        window_start = point.timestamp
        window_values = [point.value]
      }
    }
    
    // 处理最后一个窗口
    if window_values.length() > 0 {
      let sum = window_values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / (window_values.length() as Float)
      
      result = result.push({
        timestamp: window_start + target_interval_ms / 2,
        value: avg,
        metadata: [("downsampled", "true")]
      })
    }
    
    result
  }
  
  // 创建测试数据（1秒间隔）
  let test_points = [
    { timestamp: 1000, value: 10.0, metadata: [] },
    { timestamp: 2000, value: 20.0, metadata: [] },
    { timestamp: 3000, value: 15.0, metadata: [] },
    { timestamp: 4000, value: 25.0, metadata: [] }
  ]
  
  // 上采样到500ms间隔
  let upsampled = upsample(test_points, 500)
  
  // 验证上采样结果
  assert_true(upsampled.length() > test_points.length())
  
  // 检查第一个插值点 (1500ms): 10.0 + (20.0 - 10.0) * 0.5 = 15.0
  let interpolated_point = upsampled.find(fn(p) { p.timestamp == 1500 })
  match interpolated_point {
    Some(point) => assert_eq(point.value, 15.0)
    None => assert_true(false)
  }
  
  // 下采样到2秒间隔
  let downsampled = downsample(test_points, 2000)
  
  // 验证下采样结果
  assert_true(downsampled.length() < test_points.length())
  
  // 第一个窗口 (1000-3000): 10.0, 20.0 -> 平均值 15.0
  assert_eq(downsampled[0].timestamp, 2000)
  assert_eq(downsampled[0].value, 15.0)
}

// Test 4: Time Series Anomaly Detection
test "时间序列异常检测" {
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  type AnomalyResult = {
    timestamp: Int,
    value: Float,
    expected_value: Float,
    deviation: Float,
    is_anomaly: Bool
  }
  
  // 简单的统计异常检测
  let statistical_anomaly_detection = fn(points: Array[TimeSeriesPoint], threshold: Float) {
    if points.length() < 3 {
      return []
    }
    
    let mut results = []
    
    for i in 1..(points.length() - 1) {
      let prev_point = points[i - 1]
      let current_point = points[i]
      let next_point = points[i + 1]
      
      // 计算移动平均（预测值）
      let predicted_value = (prev_point.value + next_point.value) / 2.0
      
      // 计算偏差
      let deviation = (current_point.value - predicted_value).abs()
      let relative_deviation = deviation / predicted_value
      
      // 判断是否为异常
      let is_anomaly = relative_deviation > threshold
      
      results = results.push({
        timestamp: current_point.timestamp,
        value: current_point.value,
        expected_value: predicted_value,
        deviation: deviation,
        is_anomaly: is_anomaly
      })
    }
    
    results
  }
  
  // 创建测试数据（包含异常）
  let test_points = [
    { timestamp: 1000, value: 10.0, metadata: [] },
    { timestamp: 2000, value: 12.0, metadata: [] },
    { timestamp: 3000, value: 11.0, metadata: [] },
    { timestamp: 4000, value: 50.0, metadata: [] },  // 异常值
    { timestamp: 5000, value: 13.0, metadata: [] },
    { timestamp: 6000, value: 12.0, metadata: [] }
  ]
  
  // 检测异常（30%阈值）
  let anomalies = statistical_anomaly_detection(test_points, 0.3)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 4)  // 排除首尾点
  
  // 检查异常点（4000ms的50.0）
  let anomaly_at_4000 = anomalies.find(fn(r) { r.timestamp == 4000 })
  match anomaly_at_4000 {
    Some(result) => {
      assert_eq(result.value, 50.0)
      assert_true(result.is_anomaly)
      // 预测值应该是 (12.0 + 13.0) / 2 = 12.5
      assert_eq(result.expected_value, 12.5)
    }
    None => assert_true(false)
  }
  
  // 检查正常点（3000ms的11.0）
  let normal_at_3000 = anomalies.find(fn(r) { r.timestamp == 3000 })
  match normal_at_3000 {
    Some(result) => {
      assert_eq(result.value, 11.0)
      assert_false(result.is_anomaly)
    }
    None => assert_true(false)
  }
}

// Test 5: Time Series Trend Analysis
test "时间序列趋势分析" {
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  type TrendResult = {
    slope: Float,
    intercept: Float,
    correlation: Float,
    trend: String  // "increasing", "decreasing", "stable"
  }
  
  // 简单线性回归分析
  let linear_regression = fn(points: Array[TimeSeriesPoint]) {
    if points.length() < 2 {
      return {
        slope: 0.0,
        intercept: 0.0,
        correlation: 0.0,
        trend: "stable"
      }
    }
    
    let n = points.length() as Float
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    let mut sum_y2 = 0.0
    
    for point in points {
      let x = point.timestamp as Float
      let y = point.value
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + x * y
      sum_x2 = sum_x2 + x * x
      sum_y2 = sum_y2 + y * y
    }
    
    // 计算斜率和截距
    let denominator = n * sum_x2 - sum_x * sum_x
    let slope = if denominator != 0.0 {
      (n * sum_xy - sum_x * sum_y) / denominator
    } else {
      0.0
    }
    
    let intercept = (sum_y - slope * sum_x) / n
    
    // 计算相关系数
    let correlation_denominator = (n * sum_x2 - sum_x * sum_x) * (n * sum_y2 - sum_y * sum_y)
    let correlation = if correlation_denominator > 0.0 {
      (n * sum_xy - sum_x * sum_y) / correlation_denominator.sqrt()
    } else {
      0.0
    }
    
    // 确定趋势
    let trend = if slope.abs() < 0.001 {
      "stable"
    } else if slope > 0.0 {
      "increasing"
    } else {
      "decreasing"
    }
    
    {
      slope,
      intercept,
      correlation,
      trend
    }
  }
  
  // 创建上升趋势的测试数据
  let increasing_points = [
    { timestamp: 1000, value: 10.0, metadata: [] },
    { timestamp: 2000, value: 15.0, metadata: [] },
    { timestamp: 3000, value: 20.0, metadata: [] },
    { timestamp: 4000, value: 25.0, metadata: [] },
    { timestamp: 5000, value: 30.0, metadata: [] }
  ]
  
  let increasing_trend = linear_regression(increasing_points)
  assert_eq(increasing_trend.trend, "increasing")
  assert_true(increasing_trend.slope > 0.0)
  
  // 创建下降趋势的测试数据
  let decreasing_points = [
    { timestamp: 1000, value: 50.0, metadata: [] },
    { timestamp: 2000, value: 40.0, metadata: [] },
    { timestamp: 3000, value: 30.0, metadata: [] },
    { timestamp: 4000, value: 20.0, metadata: [] },
    { timestamp: 5000, value: 10.0, metadata: [] }
  ]
  
  let decreasing_trend = linear_regression(decreasing_points)
  assert_eq(decreasing_trend.trend, "decreasing")
  assert_true(decreasing_trend.slope < 0.0)
  
  // 创建稳定趋势的测试数据
  let stable_points = [
    { timestamp: 1000, value: 20.0, metadata: [] },
    { timestamp: 2000, value: 20.5, metadata: [] },
    { timestamp: 3000, value: 19.8, metadata: [] },
    { timestamp: 4000, value: 20.2, metadata: [] },
    { timestamp: 5000, value: 20.1, metadata: [] }
  ]
  
  let stable_trend = linear_regression(stable_points)
  assert_eq(stable_trend.trend, "stable")
  assert_true(stable_trend.slope.abs() < 0.001)
}