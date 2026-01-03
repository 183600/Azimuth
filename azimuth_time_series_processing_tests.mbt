// Azimuth Time Series Data Processing Tests
// 时间序列数据处理测试用例，专注于时间序列数据的聚合、分析、预测和存储优化

// Test 1: 时间序列数据聚合测试
test "时间序列数据聚合测试" {
  // 原始时间序列数据（按秒级别）
  let raw_time_series = [
    (1672531200L, 100), // 2023-01-01 00:00:00
    (1672531201L, 120),
    (1672531202L, 95),
    (1672531203L, 110),
    (1672531204L, 105),
    (1672531205L, 130),
    (1672531206L, 115),
    (1672531207L, 125),
    (1672531208L, 108),
    (1672531209L, 118),
    (1672531210L, 122),
    (1672531211L, 98)
  ]
  
  // 按时间窗口聚合函数
  fn aggregate_by_window(time_series : Array[(Int64, Int)], window_size_seconds : Int) -> Array[(Int64, Float, Int, Int, Int)] {
    if time_series.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut current_window_start = time_series[0].0
    let mut current_window_end = current_window_start + window_size_seconds.to_int64()
    let mut window_values = []
    
    for (timestamp, value) in time_series {
      if timestamp >= current_window_start && timestamp < current_window_end {
        // 在当前窗口内
        window_values = window_values + [value]
      } else {
        // 超出当前窗口，处理当前窗口并开始新窗口
        if window_values.length() > 0 {
          let avg = calculate_average(window_values)
          let min = find_min(window_values)
          let max = find_max(window_values)
          let count = window_values.length()
          result = result + [(current_window_start, avg, min, max, count)]
        }
        
        // 开始新窗口
        current_window_start = timestamp
        current_window_end = current_window_start + window_size_seconds.to_int64()
        window_values = [value]
      }
    }
    
    // 处理最后一个窗口
    if window_values.length() > 0 {
      let avg = calculate_average(window_values)
      let min = find_min(window_values)
      let max = find_max(window_values)
      let count = window_values.length()
      result = result + [(current_window_start, avg, min, max, count)]
    }
    
    result
  }
  
  // 计算平均值
  fn calculate_average(values : Array[Int]) -> Float {
    if values.length() == 0 {
      return 0.0
    }
    
    let mut sum = 0
    for value in values {
      sum = sum + value
    }
    
    sum.to_float() / values.length().to_float()
  }
  
  // 查找最小值
  fn find_min(values : Array[Int]) -> Int {
    if values.length() == 0 {
      return 0
    }
    
    let mut min_val = values[0]
    for value in values {
      if value < min_val {
        min_val = value
      }
    }
    
    min_val
  }
  
  // 查找最大值
  fn find_max(values : Array[Int]) -> Int {
    if values.length() == 0 {
      return 0
    }
    
    let mut max_val = values[0]
    for value in values {
      if value > max_val {
        max_val = value
      }
    }
    
    max_val
  }
  
  // 按5秒窗口聚合
  let aggregated_5s = aggregate_by_window(raw_time_series, 5)
  
  // 验证聚合结果
  assert_eq(aggregated_5s.length(), 3) // 12个数据点，5秒窗口，应该有3个窗口
  
  // 验证第一个窗口 (0-5秒)
  assert_eq(aggregated_5s[0].0, 1672531200L) // 窗口开始时间
  assert_true(aggregated_5s[0].1 > 100.0 && aggregated_5s[0].1 < 110.0) // 平均值
  assert_eq(aggregated_5s[0].2, 95) // 最小值
  assert_eq(aggregated_5s[0].3, 130) // 最大值
  assert_eq(aggregated_5s[0].4, 5) // 数据点数量
  
  // 验证第二个窗口 (5-10秒)
  assert_eq(aggregated_5s[1].0, 1672531205L) // 窗口开始时间
  assert_true(aggregated_5s[1].1 > 110.0 && aggregated_5s[1].1 < 125.0) // 平均值
  assert_eq(aggregated_5s[1].2, 105) // 最小值
  assert_eq(aggregated_5s[1].3, 130) // 最大值
  assert_eq(aggregated_5s[1].4, 5) // 数据点数量
  
  // 验证第三个窗口 (10-15秒)
  assert_eq(aggregated_5s[2].0, 1672531210L) // 窗口开始时间
  assert_true(aggregated_5s[2].1 > 105.0 && aggregated_5s[2].1 < 115.0) // 平均值
  assert_eq(aggregated_5s[2].2, 98) // 最小值
  assert_eq(aggregated_5s[2].3, 122) // 最大值
  assert_eq(aggregated_5s[2].4, 2) // 数据点数量
}

// Test 2: 时间序列数据插值测试
test "时间序列数据插值测试" {
  // 不规则间隔的时间序列数据
  let irregular_time_series = [
    (1672531200L, 100), // 0秒
    (1672531205L, 150), // 5秒
    (1672531215L, 120), // 15秒
    (1672531220L, 180), // 20秒
    (1672531230L, 140)  // 30秒
  ]
  
  // 线性插值函数
  fn linear_interpolate(series : Array[(Int64, Int)], target_timestamp : Int64) -> Option[Int] {
    if series.length() < 2 {
      return None
    }
    
    // 找到目标时间戳前后的数据点
    let mut before_point = None
    let mut after_point = None
    
    for (timestamp, value) in series {
      if timestamp == target_timestamp {
        return Some(value) // 精确匹配
      } else if timestamp < target_timestamp {
        before_point = Some((timestamp, value))
      } else if timestamp > target_timestamp && after_point.is_none() {
        after_point = Some((timestamp, value))
      }
    }
    
    match (before_point, after_point) {
      (Some((t1, v1)), Some((t2, v2))) => {
        // 线性插值公式: y = y1 + (y2 - y1) * ((x - x1) / (x2 - x1))
        let ratio = (target_timestamp - t1).to_float() / (t2 - t1).to_float()
        let interpolated_value = v1.to_float() + (v2.to_float() - v1.to_float()) * ratio
        Some(interpolated_value.to_int())
      }
      _ => None // 无法插值
    }
  }
  
  // 生成规则间隔的时间序列
  fn generate_regular_series(irregular_series : Array[(Int64, Int)], start_time : Int64, end_time : Int64, interval_seconds : Int) -> Array[(Int64, Int)] {
    let mut result = []
    let mut current_time = start_time
    
    while current_time <= end_time {
      match linear_interpolate(irregular_series, current_time) {
        Some(value) => result = result + [(current_time, value)]
        None => () // 无法插值的点跳过
      }
      current_time = current_time + interval_seconds.to_int64()
    }
    
    result
  }
  
  // 生成每5秒间隔的规则时间序列
  let regular_series = generate_regular_series(irregular_time_series, 1672531200L, 1672531230L, 5)
  
  // 验证插值结果
  assert_eq(regular_series.length(), 7) // 从0秒到30秒，每5秒一个点，共7个点
  
  // 验证已知点（应该保持原值）
  assert_eq(regular_series[0], (1672531200L, 100)) // 0秒，原始值
  assert_eq(regular_series[1], (1672531205L, 150)) // 5秒，原始值
  assert_eq(regular_series[3], (1672531215L, 120)) // 15秒，原始值
  assert_eq(regular_series[4], (1672531220L, 180)) // 20秒，原始值
  assert_eq(regular_series[6], (1672531230L, 140)) // 30秒，原始值
  
  // 验证插值点
  // 10秒的插值点应该在5秒(150)和15秒(120)之间
  assert_eq(regular_series[2], (1672531210L, 135)) // 线性插值结果
  // 25秒的插值点应该在20秒(180)和30秒(140)之间
  assert_eq(regular_series[5], (1672531225L, 160)) // 线性插值结果
  
  // 测试边界情况
  let before_start = linear_interpolate(irregular_time_series, 1672531195L)
  let after_end = linear_interpolate(irregular_time_series, 1672531235L)
  
  match (before_start, after_end) {
    (None, None) => assert_true(true) // 边界外无法插值
    _ => assert_true(false)
  }
}

// Test 3: 时间序列数据平滑测试
test "时间序列数据平滑测试" {
  // 包含噪声的时间序列数据
  let noisy_time_series = [
    (1672531200L, 100),
    (1672531201L, 120), // 噪声
    (1672531202L, 98),
    (1672531203L, 105),
    (1672531204L, 85),  // 噪声
    (1672531205L, 102),
    (1672531206L, 110),
    (1672531207L, 95),
    (1672531208L, 108),
    (1672531209L, 125), // 噪声
    (1672531210L, 103),
    (1672531211L, 97),
    (1672531212L, 105),
    (1672531213L, 115),
    (1672531214L, 90)   // 噪声
  ]
  
  // 移动平均平滑函数
  fn moving_average_smooth(series : Array[(Int64, Int)], window_size : Int) -> Array[(Int64, Float)] {
    if series.length() < window_size {
      return []
    }
    
    let mut result = []
    
    for i in window_size - 1 ..< series.length() {
      let mut sum = 0
      for j in (i - window_size + 1) ..= i {
        sum = sum + series[j].1
      }
      
      let average = sum.to_float() / window_size.to_float()
      result = result + [(series[i].0, average)]
    }
    
    result
  }
  
  // 指数平滑函数
  fn exponential_smooth(series : Array[(Int64, Int)], alpha : Float) -> Array[(Int64, Float)] {
    if series.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut smoothed_value = series[0].1.to_float() // 初始值为第一个数据点
    
    for (timestamp, value) in series {
      smoothed_value = alpha * value.to_float() + (1.0 - alpha) * smoothed_value
      result = result + [(timestamp, smoothed_value)]
    }
    
    result
  }
  
  // 应用移动平均平滑（窗口大小为3）
  let ma_smoothed = moving_average_smooth(noisy_time_series, 3)
  
  // 验证移动平均结果
  assert_eq(ma_smoothed.length(), 13) // 原始15个点，窗口大小3，结果13个点
  
  // 验证第一个平滑点（前3个点的平均值）
  let first_ma_value = (100 + 120 + 98).to_float() / 3.0
  assert_eq(ma_smoothed[0].1, first_ma_value)
  
  // 验证平滑效果（平滑后的值应该在原始值范围内）
  for smoothed_point in ma_smoothed {
    assert_true(smoothed_point.1 >= 85.0 && smoothed_point.1 <= 125.0)
  }
  
  // 应用指数平滑（alpha = 0.3）
  let exp_smoothed = exponential_smooth(noisy_time_series, 0.3)
  
  // 验证指数平滑结果
  assert_eq(exp_smoothed.length(), 15) // 指数平滑保持原始数据点数量
  
  // 验证第一个平滑点（等于原始值）
  assert_eq(exp_smoothed[0].1, 100.0)
  
  // 验证第二个平滑点（0.3 * 120 + 0.7 * 100 = 106）
  assert_true(exp_smoothed[1].1 > 105.0 && exp_smoothed[1].1 < 107.0)
  
  // 验证平滑效果（指数平滑后的值应该在原始值范围内）
  for smoothed_point in exp_smoothed {
    assert_true(smoothed_point.1 >= 85.0 && smoothed_point.1 <= 125.0)
  }
  
  // 比较平滑效果
  // 移动平均通常比指数平滑更"滞后"，但对突发噪声更不敏感
  // 这里我们只是验证两种方法都能产生合理的平滑结果
}

// Test 4: 时间序列趋势分析测试
test "时间序列趋势分析测试" {
  // 具有明显趋势的时间序列数据
  let trending_time_series = [
    (1672531200L, 100),
    (1672531201L, 102),
    (1672531202L, 105),
    (1672531203L, 108),
    (1672531204L, 110),
    (1672531205L, 113),
    (1672531206L, 115),
    (1672531207L, 118),
    (1672531208L, 120),
    (1672531209L, 123),
    (1672531210L, 125),
    (1672531211L, 128),
    (1672531212L, 130),
    (1672531213L, 133),
    (1672531214L, 135)
  ]
  
  // 简单线性回归计算趋势
  fn calculate_trend(series : Array[(Int64, Int)]) -> (Float, Float) {
    if series.length() < 2 {
      return (0.0, 0.0) // 斜率和截距
    }
    
    let n = series.length().to_float()
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    
    // 将时间戳转换为相对秒数，避免大数值问题
    let base_time = series[0].0
    
    for (timestamp, value) in series {
      let x = (timestamp - base_time).to_float()
      let y = value.to_float()
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + (x * y)
      sum_x2 = sum_x2 + (x * x)
    }
    
    // 计算斜率和截距
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    (slope, intercept)
  }
  
  // 计算趋势强度（R²）
  fn calculate_trend_strength(series : Array[(Int64, Int)], slope : Float, intercept : Float) -> Float {
    if series.length() < 2 {
      return 0.0
    }
    
    let mut sum_y = 0.0
    let mut sum_y_squared = 0.0
    let mut sum_residual_squared = 0.0
    let base_time = series[0].0
    
    // 计算总平方和
    for (timestamp, value) in series {
      let y = value.to_float()
      sum_y = sum_y + y
      sum_y_squared = sum_y_squared + (y * y)
    }
    
    let mean_y = sum_y / series.length().to_float()
    let total_sum_squares = sum_y_squared - series.length().to_float() * mean_y * mean_y
    
    // 计算残差平方和
    for (timestamp, value) in series {
      let x = (timestamp - base_time).to_float()
      let y = value.to_float()
      let predicted_y = slope * x + intercept
      let residual = y - predicted_y
      sum_residual_squared = sum_residual_squared + (residual * residual)
    }
    
    if total_sum_squares == 0.0 {
      return 0.0
    }
    
    1.0 - (sum_residual_squared / total_sum_squares)
  }
  
  // 计算趋势
  let (slope, intercept) = calculate_trend(trending_time_series)
  
  // 验证趋势计算
  assert_true(slope > 2.0 && slope < 3.0) // 斜率应该在2-3之间
  assert_true(intercept > 95.0 && intercept < 105.0) // 截距应该在100附近
  
  // 计算趋势强度
  let trend_strength = calculate_trend_strength(trending_time_series, slope, intercept)
  
  // 验证趋势强度（应该接近1，表示强趋势）
  assert_true(trend_strength > 0.9 && trend_strength <= 1.0)
  
  // 测试无趋势数据
  let flat_time_series = [
    (1672531200L, 100),
    (1672531201L, 98),
    (1672531202L, 102),
    (1672531203L, 99),
    (1672531204L, 101),
    (1672531205L, 97),
    (1672531206L, 103),
    (1672531207L, 98),
    (1672531208L, 100),
    (1672531209L, 102)
  ]
  
  let (flat_slope, flat_intercept) = calculate_trend(flat_time_series)
  let flat_trend_strength = calculate_trend_strength(flat_time_series, flat_slope, flat_intercept)
  
  // 验证无趋势数据
  assert_true(flat_slope > -0.5 && flat_slope < 0.5) // 斜率接近0
  assert_true(flat_trend_strength < 0.5) // 趋势强度较低
}

// Test 5: 时间序列异常检测测试
test "时间序列异常检测测试" {
  // 包含异常值的时间序列数据
  let time_series_with_anomalies = [
    (1672531200L, 100),
    (1672531201L, 102),
    (1672531202L, 98),
    (1672531203L, 105),
    (1672531204L, 101),
    (1672531205L, 99),
    (1672531206L, 103),
    (1672531207L, 250), // 异常值
    (1672531208L, 97),
    (1672531209L, 104),
    (1672531210L, 98),
    (1672531211L, 102),
    (1672531212L, 15),  // 异常值
    (1672531213L, 101),
    (1672531214L, 99)
  ]
  
  // 基于标准差的异常检测
  fn detect_anomalies_stddev(series : Array[(Int64, Int)], threshold : Float) -> Array[(Int64, Int, String)] {
    if series.length() < 3 {
      return []
    }
    
    // 计算均值和标准差
    let mut sum = 0
    for (_, value) in series {
      sum = sum + value
    }
    let mean = sum.to_float() / series.length().to_float()
    
    let mut variance_sum = 0.0
    for (_, value) in series {
      let diff = value.to_float() - mean
      variance_sum = variance_sum + (diff * diff)
    }
    let variance = variance_sum / series.length().to_float()
    let stddev = variance.sqrt()
    
    // 检测异常值
    let mut anomalies = []
    for (timestamp, value) in series {
      let z_score = (value.to_float() - mean).abs() / stddev
      if z_score > threshold {
        if value.to_float() > mean {
          anomalies = anomalies + [(timestamp, value, "high")]
        } else {
          anomalies = anomalies + [(timestamp, value, "low")]
        }
      }
    }
    
    anomalies
  }
  
  // 基于移动窗口的异常检测
  fn detect_anomalies_moving_window(series : Array[(Int64, Int)], window_size : Int, threshold_multiplier : Float) -> Array[(Int64, Int, String)] {
    if series.length() < window_size + 1 {
      return []
    }
    
    let mut anomalies = []
    
    for i in window_size ..< series.length() {
      // 计算窗口内的统计量
      let mut sum = 0
      for j in (i - window_size) ..< i {
        sum = sum + series[j].1
      }
      let window_mean = sum.to_float() / window_size.to_float()
      
      let mut variance_sum = 0.0
      for j in (i - window_size) ..< i {
        let diff = series[j].1.to_float() - window_mean
        variance_sum = variance_sum + (diff * diff)
      }
      let window_variance = variance_sum / window_size.to_float()
      let window_stddev = window_variance.sqrt()
      
      // 检查当前点是否异常
      let current_value = series[i].1.to_float()
      let z_score = (current_value - window_mean).abs() / window_stddev
      
      if z_score > threshold_multiplier {
        if current_value > window_mean {
          anomalies = anomalies + [(series[i].0, series[i].1, "high")]
        } else {
          anomalies = anomalies + [(series[i].0, series[i].1, "low")]
        }
      }
    }
    
    anomalies
  }
  
  // 使用标准差方法检测异常
  let stddev_anomalies = detect_anomalies_stddev(time_series_with_anomalies, 2.5)
  
  // 验证标准差异常检测结果
  assert_eq(stddev_anomalies.length(), 2) // 应该检测到2个异常值
  
  // 验证高异常值
  assert_eq(stddev_anomalies[0].0, 1672531207L) // 异常时间戳
  assert_eq(stddev_anomalies[0].1, 250) // 异常值
  assert_eq(stddev_anomalies[0].2, "high") // 异常类型
  
  // 验证低异常值
  assert_eq(stddev_anomalies[1].0, 1672531212L) // 异常时间戳
  assert_eq(stddev_anomalies[1].1, 15) // 异常值
  assert_eq(stddev_anomalies[1].2, "low") // 异常类型
  
  // 使用移动窗口方法检测异常
  let window_anomalies = detect_anomalies_moving_window(time_series_with_anomalies, 5, 2.5)
  
  // 验证移动窗口异常检测结果
  assert_eq(window_anomalies.length(), 2) // 应该检测到2个异常值
  
  // 验证异常值
  assert_eq(window_anomalies[0].1, 250) // 高异常值
  assert_eq(window_anomalies[0].2, "high") // 异常类型
  assert_eq(window_anomalies[1].1, 15) // 低异常值
  assert_eq(window_anomalies[1].2, "low") // 异常类型
  
  // 测试无异常数据
  let normal_time_series = [
    (1672531200L, 100),
    (1672531201L, 102),
    (1672531202L, 98),
    (1672531203L, 105),
    (1672531204L, 101),
    (1672531205L, 99),
    (1672531206L, 103),
    (1672531207L, 97),
    (1672531208L, 104),
    (1672531209L, 98)
  ]
  
  let normal_anomalies = detect_anomalies_stddev(normal_time_series, 2.5)
  
  // 验证无异常数据
  assert_eq(normal_anomalies.length(), 0) // 不应该检测到异常值
}

// Test 6: 时间序列预测测试
test "时间序列预测测试" {
  // 历史时间序列数据
  let historical_time_series = [
    (1672531200L, 100),
    (1672531201L, 102),
    (1672531202L, 105),
    (1672531203L, 108),
    (1672531204L, 110),
    (1672531205L, 113),
    (1672531206L, 115),
    (1672531207L, 118),
    (1672531208L, 120),
    (1672531209L, 123)
  ]
  
  // 简单线性预测
  fn linear_predict(series : Array[(Int64, Int)], steps_ahead : Int) -> Array[(Int64, Float)] {
    if series.length() < 2 {
      return []
    }
    
    // 计算线性趋势
    let (slope, intercept) = calculate_trend(series)
    
    // 生成预测
    let mut predictions = []
    let base_time = series[0].0
    let last_time = series[series.length() - 1].0
    
    for i in 1 ..= steps_ahead {
      let future_time = last_time + i.to_int64()
      let x = (future_time - base_time).to_float()
      let predicted_value = slope * x + intercept
      predictions = predictions + [(future_time, predicted_value)]
    }
    
    predictions
  }
  
  // 基于最近值的简单预测
  fn last_value_predict(series : Array[(Int64, Int)], steps_ahead : Int) -> Array[(Int64, Float)] {
    if series.length() == 0 {
      return []
    }
    
    let last_value = series[series.length() - 1].1.to_float()
    let last_time = series[series.length() - 1].0
    let interval = if series.length() > 1 {
      series[1].0 - series[0].0
    } else {
      1L
    }
    
    let mut predictions = []
    for i in 1 ..= steps_ahead {
      let future_time = last_time + interval * i.to_int64()
      predictions = predictions + [(future_time, last_value)]
    }
    
    predictions
  }
  
  // 基于移动平均的预测
  fn moving_average_predict(series : Array[(Int64, Int)], window_size : Int, steps_ahead : Int) -> Array[(Int64, Float)] {
    if series.length() < window_size {
      return []
    }
    
    // 计算最近窗口的平均值
    let mut sum = 0
    for i in (series.length() - window_size) ..< series.length() {
      sum = sum + series[i].1
    }
    let average = sum.to_float() / window_size.to_float()
    
    let last_time = series[series.length() - 1].0
    let interval = if series.length() > 1 {
      series[1].0 - series[0].0
    } else {
      1L
    }
    
    let mut predictions = []
    for i in 1 ..= steps_ahead {
      let future_time = last_time + interval * i.to_int64()
      predictions = predictions + [(future_time, average)]
    }
    
    predictions
  }
  
  // 辅助函数：计算趋势
  fn calculate_trend(series : Array[(Int64, Int)]) -> (Float, Float) {
    if series.length() < 2 {
      return (0.0, 0.0)
    }
    
    let n = series.length().to_float()
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    
    let base_time = series[0].0
    
    for (timestamp, value) in series {
      let x = (timestamp - base_time).to_float()
      let y = value.to_float()
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + (x * y)
      sum_x2 = sum_x2 + (x * x)
    }
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    (slope, intercept)
  }
  
  // 使用线性预测
  let linear_predictions = linear_predict(historical_time_series, 3)
  
  // 验证线性预测结果
  assert_eq(linear_predictions.length(), 3) // 预测3个未来点
  
  // 验证预测时间戳
  assert_eq(linear_predictions[0].0, 1672531210L) // 最后一个时间戳 + 1
  assert_eq(linear_predictions[1].0, 1672531211L) // 最后一个时间戳 + 2
  assert_eq(linear_predictions[2].0, 1672531212L) // 最后一个时间戳 + 3
  
  // 验证预测值（应该呈上升趋势）
  assert_true(linear_predictions[0].1 > 120.0 && linear_predictions[0].1 < 130.0)
  assert_true(linear_predictions[1].1 > linear_predictions[0].1)
  assert_true(linear_predictions[2].1 > linear_predictions[1].1)
  
  // 使用最近值预测
  let last_value_predictions = last_value_predict(historical_time_series, 3)
  
  // 验证最近值预测结果
  assert_eq(last_value_predictions.length(), 3)
  assert_eq(last_value_predictions[0].0, 1672531210L)
  assert_eq(last_value_predictions[0].1, 123.0) // 等于最后一个历史值
  
  // 使用移动平均预测
  let ma_predictions = moving_average_predict(historical_time_series, 5, 3)
  
  // 验证移动平均预测结果
  assert_eq(ma_predictions.length(), 3)
  assert_eq(ma_predictions[0].0, 1672531210L)
  
  // 移动平均值应该在历史值范围内
  assert_true(ma_predictions[0].1 > 100.0 && ma_predictions[0].1 < 125.0)
  
  // 比较不同预测方法
  // 线性预测考虑趋势，适合有明显趋势的数据
  // 最近值预测简单，适合稳定数据
  // 移动平均预测考虑近期历史，适合有短期波动的数据
}

// Test 7: 时间序列数据压缩测试
test "时间序列数据压缩测试" {
  // 高精度时间序列数据
  let high_precision_time_series = [
    (1672531200L, 100.123456789),
    (1672531201L, 102.234567890),
    (1672531202L, 98.345678901),
    (1672531203L, 105.456789012),
    (1672531204L, 101.567890123),
    (1672531205L, 99.678901234),
    (1672531206L, 103.789012345),
    (1672531207L, 97.890123456),
    (1672531208L, 104.901234567),
    (1672531209L, 98.012345678)
  ]
  
  // 时间增量编码压缩
  fn delta_encode(series : Array[(Int64, Float)]) -> (Int64, Array[(Int64, Float)]) {
    if series.length() == 0 {
      return (0L, [])
    }
    
    let base_timestamp = series[0].0
    let base_value = series[0].1
    let mut encoded = []
    
    for i in 0 ..< series.length() {
      if i == 0 {
        encoded = encoded + [(0L, 0.0)] // 第一个点的增量为0
      } else {
        let time_delta = series[i].0 - series[i-1].0
        let value_delta = series[i].1 - series[i-1].1
        encoded = encoded + [(time_delta, value_delta)]
      }
    }
    
    (base_timestamp, base_value, encoded)
  }
  
  // 时间增量解码
  fn delta_decode(base_timestamp : Int64, base_value : Float, encoded : Array[(Int64, Float)]) -> Array[(Int64, Float)] {
    if encoded.length() == 0 {
      return []
    }
    
    let mut decoded = []
    let mut current_timestamp = base_timestamp
    let mut current_value = base_value
    
    for (time_delta, value_delta) in encoded {
      current_timestamp = current_timestamp + time_delta
      current_value = current_value + value_delta
      decoded = decoded + [(current_timestamp, current_value)]
    }
    
    decoded
  }
  
  // 简单的浮点数精度压缩
  fn compress_precision(value : Float, precision : Int) -> Float {
    let factor = 10.0.pow(precision.to_float())
    (value * factor).round() / factor
  }
  
  // 应用增量编码
  let (base_timestamp, base_value, delta_encoded) = delta_encode(high_precision_time_series)
  
  // 验证增量编码
  assert_eq(base_timestamp, 1672531200L) // 基准时间戳
  assert_eq(base_value, 100.123456789) // 基准值
  assert_eq(delta_encoded.length(), 10) // 编码后长度相同
  
  // 验证第一个编码点
  assert_eq(delta_encoded[0], (0L, 0.0)) // 第一个点的增量为0
  
  // 验证增量值
  assert_eq(delta_encoded[1], (1L, 2.111111101)) // 第二个点的增量
  assert_eq(delta_encoded[2], (1L, -3.888888989)) // 第三个点的增量
  
  // 解码验证
  let decoded_series = delta_decode(base_timestamp, base_value, delta_encoded)
  
  // 验证解码结果
  assert_eq(decoded_series.length(), high_precision_time_series.length())
  
  for i in 0 ..< decoded_series.length() {
    assert_eq(decoded_series[i].0, high_precision_time_series[i].0) // 时间戳应该完全一致
    assert_true((decoded_series[i].1 - high_precision_time_series[i].1).abs() < 0.000001) // 浮点数可能有微小误差
  }
  
  // 应用精度压缩
  let mut precision_compressed = []
  for (timestamp, value) in high_precision_time_series {
    let compressed_value = compress_precision(value, 2) // 保留2位小数
    precision_compressed = precision_compressed + [(timestamp, compressed_value)]
  }
  
  // 验证精度压缩
  assert_eq(precision_compressed.length(), high_precision_time_series.length())
  assert_eq(precision_compressed[0].1, 100.12) // 四舍五入到2位小数
  assert_eq(precision_compressed[1].1, 102.23) // 四舍五入到2位小数
  
  // 计算压缩率
  let original_size = high_precision_time_series.length() * (8 + 8) // 每个点8字节时间戳 + 8字节浮点数
  let delta_encoded_size = 8 + 8 + delta_encoded.length() * (8 + 8) // 基准时间戳 + 基准值 + 编码数据
  let precision_compressed_size = precision_compressed.length() * (8 + 4) // 假设压缩后浮点数用4字节
  
  // 增量编码本身不会减少大小，但为后续压缩提供更好的基础
  // 精度压缩直接减少数据大小
  assert_true(precision_compressed_size < original_size)
}

// Test 8: 时间序列数据采样测试
test "时间序列数据采样测试" {
  // 高频时间序列数据（每秒一个点）
  let high_frequency_time_series = []
  for i in 0 ..< 60 {
    let timestamp = 1672531200L + i.to_int64()
    let value = 100 + (i % 10) * 2 // 模拟周期性变化
    high_frequency_time_series = high_frequency_time_series + [(timestamp, value)]
  }
  
  // 均匀采样
  fn uniform_sample(series : Array[(Int64, Int)], interval : Int) -> Array[(Int64, Int)] {
    if series.length() == 0 {
      return []
    }
    
    let mut sampled = []
    for i in 0 ..< series.length() {
      if i % interval == 0 {
        sampled = sampled + [series[i]]
      }
    }
    
    sampled
  }
  
  // 聚合采样（取窗口内的平均值）
  fn aggregate_sample(series : Array[(Int64, Int)], window_size : Int) -> Array[(Int64, Float)] {
    if series.length() == 0 {
      return []
    }
    
    let mut sampled = []
    let mut i = 0
    
    while i < series.length() {
      let window_end = (i + window_size).min(series.length())
      
      // 计算窗口内的平均值
      let mut sum = 0
      for j in i ..< window_end {
        sum = sum + series[j].1
      }
      let average = sum.to_float() / (window_end - i).to_float()
      
      // 使用窗口开始的时间戳
      sampled = sampled + [(series[i].0, average)]
      
      i = window_end
    }
    
    sampled
  }
  
  // 首尾采样（保留每个窗口的首尾点）
  fn head_tail_sample(series : Array[(Int64, Int)], window_size : Int) -> Array[(Int64, Int)] {
    if series.length() == 0 {
      return []
    }
    
    let mut sampled = []
    let mut i = 0
    
    while i < series.length() {
      let window_end = (i + window_size).min(series.length())
      
      // 添加窗口开始的点
      sampled = sampled + [series[i]]
      
      // 如果窗口大小大于1，添加窗口结束的点
      if window_end - i > 1 {
        sampled = sampled + [series[window_end - 1]]
      }
      
      i = window_end
    }
    
    sampled
  }
  
  // 应用均匀采样（每5个点取1个）
  let uniform_sampled = uniform_sample(high_frequency_time_series, 5)
  
  // 验证均匀采样
  assert_eq(uniform_sampled.length(), 12) // 60个点，每5个取1个，共12个点
  assert_eq(uniform_sampled[0], high_frequency_time_series[0]) // 第一个点
  assert_eq(uniform_sampled[1], high_frequency_time_series[5]) // 第6个点
  assert_eq(uniform_sampled[11], high_frequency_time_series[55]) // 第56个点
  
  // 应用聚合采样（每10个点聚合为1个）
  let aggregate_sampled = aggregate_sample(high_frequency_time_series, 10)
  
  // 验证聚合采样
  assert_eq(aggregate_sampled.length(), 6) // 60个点，每10个聚合为1个，共6个点
  
  // 验证第一个聚合窗口的平均值
  let first_window_avg = (100 + 102 + 104 + 106 + 108 + 100 + 102 + 104 + 106 + 108).to_float() / 10.0
  assert_eq(aggregate_sampled[0].1, first_window_avg)
  
  // 应用首尾采样（每10个点取首尾）
  let head_tail_sampled = head_tail_sample(high_frequency_time_series, 10)
  
  // 验证首尾采样
  assert_eq(head_tail_sampled.length(), 12) // 60个点，每10个取首尾，共12个点
  
  // 验证第一个窗口的首尾点
  assert_eq(head_tail_sampled[0], high_frequency_time_series[0]) // 窗口开始
  assert_eq(head_tail_sampled[1], high_frequency_time_series[9]) // 窗口结束
  
  // 比较不同采样方法的效果
  // 均匀采样简单快速，但可能丢失重要信息
  // 聚合采样保留统计特性，但损失时间精度
  // 首尾采样保留变化趋势，但增加数据量
  
  // 验证采样率
  let original_rate = high_frequency_time_series.length()
  let uniform_rate = uniform_sampled.length().to_float() / original_rate.to_float()
  let aggregate_rate = aggregate_sampled.length().to_float() / original_rate.to_float()
  let head_tail_rate = head_tail_sampled.length().to_float() / original_rate.to_float()
  
  assert_eq(uniform_rate, 0.2) // 20%采样率
  assert_eq(aggregate_rate, 0.1) // 10%采样率
  assert_eq(head_tail_rate, 0.2) // 20%采样率
}

// Test 9: 时间序列数据关联性分析测试
test "时间序列数据关联性分析测试" {
  // 两个相关的时间序列
  let series_a = [
    (1672531200L, 100),
    (1672531201L, 102),
    (1672531202L, 105),
    (1672531203L, 108),
    (1672531204L, 110),
    (1672531205L, 113),
    (1672531206L, 115),
    (1672531207L, 118),
    (1672531208L, 120),
    (1672531209L, 123)
  ]
  
  let series_b = [
    (1672531200L, 50),
    (1672531201L, 52),
    (1672531202L, 54),
    (1672531203L, 57),
    (1672531204L, 59),
    (1672531205L, 62),
    (1672531206L, 64),
    (1672531207L, 67),
    (1672531208L, 69),
    (1672531209L, 72)
  ]
  
  // 不相关的时间序列
  let series_c = [
    (1672531200L, 200),
    (1672531201L, 180),
    (1672531202L, 220),
    (1672531203L, 190),
    (1672531204L, 210),
    (1672531205L, 170),
    (1672531206L, 230),
    (1672531207L, 185),
    (1672531208L, 215),
    (1672531209L, 175)
  ]
  
  // 计算相关系数
  fn calculate_correlation(series1 : Array[(Int64, Int)], series2 : Array[(Int64, Int)]) -> Float {
    if series1.length() != series2.length() || series1.length() < 2 {
      return 0.0
    }
    
    let n = series1.length().to_float()
    
    // 提取值
    let values1 = []
    let values2 = []
    for i in 0 ..< series1.length() {
      values1 = values1 + [series1[i].1.to_float()]
      values2 = values2 + [series2[i].1.to_float()]
    }
    
    // 计算均值
    let mut sum1 = 0.0
    let mut sum2 = 0.0
    for i in 0 ..< values1.length() {
      sum1 = sum1 + values1[i]
      sum2 = sum2 + values2[i]
    }
    let mean1 = sum1 / n
    let mean2 = sum2 / n
    
    // 计算协方差和方差
    let mut covariance = 0.0
    let mut variance1 = 0.0
    let mut variance2 = 0.0
    
    for i in 0 ..< values1.length() {
      let diff1 = values1[i] - mean1
      let diff2 = values2[i] - mean2
      
      covariance = covariance + (diff1 * diff2)
      variance1 = variance1 + (diff1 * diff1)
      variance2 = variance2 + (diff2 * diff2)
    }
    
    if variance1 == 0.0 || variance2 == 0.0 {
      return 0.0
    }
    
    covariance / (variance1.sqrt() * variance2.sqrt())
  }
  
  // 计算滞后相关性
  fn calculate_lagged_correlation(series1 : Array[(Int64, Int)], series2 : Array[(Int64, Int)], max_lag : Int) -> Array[(Int, Float)] {
    if series1.length() != series2.length() || series1.length() < 2 {
      return []
    }
    
    let mut correlations = []
    
    for lag in -max_lag ..= max_lag {
      let mut aligned_series1 = []
      let mut aligned_series2 = []
      
      for i in 0 ..< series1.length() {
        let j = i + lag
        
        if j >= 0 && j < series2.length() {
          aligned_series1 = aligned_series1 + [series1[i]]
          aligned_series2 = aligned_series2 + [series2[j]]
        }
      }
      
      if aligned_series1.length() >= 2 {
        let correlation = calculate_correlation(aligned_series1, aligned_series2)
        correlations = correlations + [(lag, correlation)]
      }
    }
    
    correlations
  }
  
  // 计算series_a和series_b的相关性
  let correlation_ab = calculate_correlation(series_a, series_b)
  
  // 验证相关性（应该高度相关，接近1）
  assert_true(correlation_ab > 0.9 && correlation_ab <= 1.0)
  
  // 计算series_a和series_c的相关性
  let correlation_ac = calculate_correlation(series_a, series_c)
  
  // 验证相关性（应该低相关，接近0）
  assert_true(correlation_ac > -0.5 && correlation_ac < 0.5)
  
  // 计算滞后相关性
  let lagged_correlations = calculate_lagged_correlation(series_a, series_b, 2)
  
  // 验证滞后相关性
  assert_eq(lagged_correlations.length(), 5) // 从-2到+2，共5个滞后
  
  // 验证零滞后的相关性最高（因为两个序列同步）
  let max_correlation = lagged_correlations[0].1
  let max_lag = lagged_correlations[0].0
  
  for (lag, correlation) in lagged_correlations {
    if correlation > max_correlation {
      max_correlation = correlation
      max_lag = lag
    }
  }
  
  assert_eq(max_lag, 0) // 最大相关性应该在零滞后
  assert_true(max_correlation > 0.9) // 最大相关性应该很高
}

// Test 10: 时间序列数据季节性分析测试
test "时间序列数据季节性分析测试" {
  // 具有明显季节性的时间序列数据（24小时周期）
  let seasonal_time_series = []
  for hour in 0 ..< 48 { // 2天的数据
    let timestamp = 1672531200L + hour.to_int64() * 3600L // 每小时一个点
    // 模拟日周期：白天高，夜晚低
    let value = if hour % 24 >= 6 && hour % 24 <= 18 {
      100 + (hour % 24 - 6) * 5 // 白天递增
    } else {
      50 + ((hour % 24 + 6) % 24) * 2 // 夜晚缓慢变化
    }
    seasonal_time_series = seasonal_time_series + [(timestamp, value)]
  }
  
  // 简单的自相关函数计算季节性
  fn detect_seasonality(series : Array[(Int64, Int)], max_lag : Int) -> (Int, Float) {
    if series.length() < max_lag * 2 {
      return (0, 0.0)
    }
    
    let mut max_correlation = -1.0
    let mut season_period = 0
    
    for lag in 1 ..= max_lag {
      let mut aligned_series1 = []
      let mut aligned_series2 = []
      
      for i in 0 ..< (series.length() - lag) {
        aligned_series1 = aligned_series1 + [series[i]]
        aligned_series2 = aligned_series2 + [series[i + lag]]
      }
      
      if aligned_series1.length() >= 10 {
        let correlation = calculate_correlation(aligned_series1, aligned_series2)
        if correlation > max_correlation {
          max_correlation = correlation
          season_period = lag
        }
      }
    }
    
    (season_period, max_correlation)
  }
  
  // 计算移动平均以平滑季节性
  fn moving_average(series : Array[(Int64, Int)], window_size : Int) -> Array[(Int64, Float)] {
    if series.length() < window_size {
      return []
    }
    
    let mut result = []
    
    for i in window_size - 1 ..< series.length() {
      let mut sum = 0
      for j in (i - window_size + 1) ..= i {
        sum = sum + series[j].1
      }
      
      let average = sum.to_float() / window_size.to_float()
      result = result + [(series[i].0, average)]
    }
    
    result
  }
  
  // 去季节化
  fn deseasonalize(series : Array[(Int64, Int)], season_period : Int) -> Array[(Int64, Float)] {
    if series.length() < season_period * 2 {
      return []
    }
    
    // 计算每个季节点的平均值
    let mut seasonal_averages = []
    for i in 0 ..< season_period {
      let mut sum = 0
      let mut count = 0
      
      for j in i ..< series.length() {
        if j % season_period == i {
          sum = sum + series[j].1
          count = count + 1
        }
      }
      
      if count > 0 {
        seasonal_averages = seasonal_averages + [sum.to_float() / count.to_float()]
      } else {
        seasonal_averages = seasonal_averages + [0.0]
      }
    }
    
    // 去季节化
    let mut deseasonalized = []
    for i in 0 ..< series.length() {
      let seasonal_index = i % season_period
      let seasonal_factor = seasonal_averages[seasonal_index]
      let overall_average = seasonal_averages.fold(0.0, (acc, avg) => acc + avg) / seasonal_averages.length().to_float()
      let deseasonalized_value = series[i].1.to_float() * (overall_average / seasonal_factor)
      deseasonalized = deseasonalized + [(series[i].0, deseasonalized_value)]
    }
    
    deseasonalized
  }
  
  // 辅助函数：计算相关系数
  fn calculate_correlation(series1 : Array[(Int64, Int)], series2 : Array[(Int64, Int)]) -> Float {
    if series1.length() != series2.length() || series1.length() < 2 {
      return 0.0
    }
    
    let n = series1.length().to_float()
    
    let values1 = []
    let values2 = []
    for i in 0 ..< series1.length() {
      values1 = values1 + [series1[i].1.to_float()]
      values2 = values2 + [series2[i].1.to_float()]
    }
    
    let mut sum1 = 0.0
    let mut sum2 = 0.0
    for i in 0 ..< values1.length() {
      sum1 = sum1 + values1[i]
      sum2 = sum2 + values2[i]
    }
    let mean1 = sum1 / n
    let mean2 = sum2 / n
    
    let mut covariance = 0.0
    let mut variance1 = 0.0
    let mut variance2 = 0.0
    
    for i in 0 ..< values1.length() {
      let diff1 = values1[i] - mean1
      let diff2 = values2[i] - mean2
      
      covariance = covariance + (diff1 * diff2)
      variance1 = variance1 + (diff1 * diff1)
      variance2 = variance2 + (diff2 * diff2)
    }
    
    if variance1 == 0.0 || variance2 == 0.0 {
      return 0.0
    }
    
    covariance / (variance1.sqrt() * variance2.sqrt())
  }
  
  // 检测季节性
  let (season_period, season_strength) = detect_seasonality(seasonal_time_series, 24)
  
  // 验证季节性检测结果
  assert_eq(season_period, 24) // 应该检测到24小时的季节周期
  assert_true(season_strength > 0.7) // 季节性强度应该较高
  
  // 计算移动平均以平滑季节性
  let ma_smoothed = moving_average(seasonal_time_series, 6)
  
  // 验证移动平均结果
  assert_eq(ma_smoothed.length(), seasonal_time_series.length() - 5) // 48-6+1=43个点
  
  // 去季节化
  let deseasonalized = deseasonalize(seasonal_time_series, 24)
  
  // 验证去季节化结果
  assert_eq(deseasonalized.length(), seasonal_time_series.length())
  
  // 去季节化后的数据应该更加平稳（季节性被移除）
  let (deseasonalized_period, deseasonalized_strength) = detect_seasonality(deseasonalized, 24)
  
  // 去季节化后的季节性应该显著降低
  assert_true(deseasonalized_strength < season_strength)
  
  // 验证季节性模式
  // 白天的值应该高于夜晚
  let day_values = []
  let night_values = []
  
  for i in 0 ..< seasonal_time_series.length() {
    let hour = i % 24
    if hour >= 6 && hour <= 18 {
      day_values = day_values + [seasonal_time_series[i].1]
    } else {
      night_values = night_values + [seasonal_time_series[i].1]
    }
  }
  
  // 计算白天和夜晚的平均值
  let day_avg = day_values.fold(0, (acc, val) => acc + val).to_float() / day_values.length().to_float()
  let night_avg = night_values.fold(0, (acc, val) => acc + val).to_float() / night_values.length().to_float()
  
  // 验证白天平均值高于夜晚平均值
  assert_true(day_avg > night_avg)
}