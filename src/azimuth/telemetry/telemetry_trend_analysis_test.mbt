// 遥测数据趋势分析测试用例

test "telemetry_linear_trend_analysis" {
  // 测试线性趋势分析
  
  let time_series = [
    (1, 10.5),
    (2, 12.3),
    (3, 14.1),
    (4, 15.8),
    (5, 17.6),
    (6, 19.2),
    (7, 21.0),
    (8, 22.7)
  ]
  
  // 计算线性回归参数
  let n = time_series.length().to_double()
  let mut sum_x = 0.0
  let mut sum_y = 0.0
  let mut sum_xy = 0.0
  let mut sum_x2 = 0.0
  
  let mut i = 0
  while i < time_series.length() {
    let x = time_series[i].0.to_double()
    let y = time_series[i].1
    
    sum_x = sum_x + x
    sum_y = sum_y + y
    sum_xy = sum_xy + x * y
    sum_x2 = sum_x2 + x * x
    i = i + 1
  }
  
  // 计算斜率和截距
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  let intercept = (sum_y - slope * sum_x) / n
  
  // 验证线性趋势
  assert_eq(slope > 1.5 && slope < 2.0, true)  // 斜率应该在1.5-2.0之间
  assert_eq(intercept > 8.0 && intercept < 10.0, true)  // 截距应该在8-10之间
  
  // 预测下一个值
  let next_x = 9.0
  let predicted_y = slope * next_x + intercept
  assert_eq(predicted_y > 23.0 && predicted_y < 26.0, true)  // 预测值应该在合理范围内
}

test "telemetry_growth_rate_calculation" {
  // 测试增长率计算
  
  let monthly_metrics = [
    (1, 1000),
    (2, 1200),
    (3, 1350),
    (4, 1600),
    (5, 1950),
    (6, 2300)
  ]
  
  // 计算月增长率
  let mut growth_rates = []
  let mut i = 1
  while i < monthly_metrics.length() {
    let prev_value = monthly_metrics[i - 1].1.to_double()
    let curr_value = monthly_metrics[i].1.to_double()
    let growth_rate = (curr_value - prev_value) / prev_value * 100.0
    growth_rates = growth_rates.push(growth_rate)
    i = i + 1
  }
  
  // 验证增长率计算
  assert_eq(growth_rates.length(), 5)  // 应该有5个增长率
  
  // 验证第一个增长率 (1000 -> 1200 = 20%)
  assert_eq(growth_rates[0] > 19.0 && growth_rates[0] < 21.0, true)
  
  // 计算平均增长率
  let mut avg_growth_rate = 0.0
  i = 0
  while i < growth_rates.length() {
    avg_growth_rate = avg_growth_rate + growth_rates[i]
    i = i + 1
  }
  avg_growth_rate = avg_growth_rate / growth_rates.length().to_double()
  
  assert_eq(avg_growth_rate > 15.0 && avg_growth_rate < 25.0, true)
}

test "telemetry_moving_average_trend" {
  // 测试移动平均趋势
  
  let daily_values = [45, 52, 48, 58, 62, 55, 68, 72, 65, 78, 82, 75]
  
  let window_size = 3
  
  // 计算移动平均
  let mut moving_averages = []
  let mut i = window_size - 1
  while i < daily_values.length() {
    let mut sum = 0
    let mut j = i - (window_size - 1)
    while j <= i {
      sum = sum + daily_values[j]
      j = j + 1
    }
    let avg = sum.to_double() / window_size.to_double()
    moving_averages = moving_averages.push(avg)
    i = i + 1
  }
  
  // 验证移动平均趋势
  assert_eq(moving_averages.length(), daily_values.length() - window_size + 1)
  
  // 验证上升趋势（移动平均值应该总体上升）
  let mut increasing_count = 0
  let mut i = 1
  while i < moving_averages.length() {
    if moving_averages[i] > moving_averages[i - 1] {
      increasing_count = increasing_count + 1
    }
    i = i + 1
  }
  
  let increasing_ratio = increasing_count.to_double() / (moving_averages.length() - 1).to_double()
  assert_eq(increasing_ratio > 0.6, true)  // 大部分时间应该呈上升趋势
}

test "telemetry_seasonal_trend_decomposition" {
  // 测试季节性趋势分解
  
  let quarterly_data = [
    (1, 100),  // Q1
    (2, 120),  // Q2
    (3, 80),   // Q3
    (4, 110),  // Q4
    (1, 105),  // Q1第二年
    (2, 125),  // Q2第二年
    (3, 85),   // Q3第二年
    (4, 115)   // Q4第二年
  ]
  
  // 计算季节性模式
  let mut seasonal_patterns = @hashmap.new()
  let mut i = 0
  while i < time_series_data.length() {
    let quarter = time_series_data[i].0
    let value = time_series_data[i].1
    
    if seasonal_patterns.contains(quarter) {
      let (sum, count) = seasonal_patterns[quarter]
      seasonal_patterns[quarter] = (sum + value, count + 1)
    } else {
      seasonal_patterns[quarter] = (value, 1)
    }
    i = i + 1
  }
  
  // 计算季节性平均值
  let mut seasonal_averages = @hashmap.new()
  let keys = seasonal_patterns.keys()
  let mut i = 0
  while i < keys.length() {
    let quarter = keys[i]
    let (sum, count) = seasonal_patterns[quarter]
    seasonal_averages[quarter] = sum / count.to_double()
    i = i + 1
  }
  
  // 验证季节性模式
  assert_eq(seasonal_averages[1] > 100.0 && seasonal_averages[1] < 110.0, true)  // Q1平均值
  assert_eq(seasonal_averages[2] > 120.0 && seasonal_averages[2] < 130.0, true)  // Q2平均值
  assert_eq(seasonal_averages[3] > 80.0 && seasonal_averages[3] < 90.0, true)    // Q3平均值
  assert_eq(seasonal_averages[4] > 110.0 && seasonal_averages[4] < 120.0, true)  // Q4平均值
  
  // Q2应该是最高季度，Q3应该是最低季度
  assert_eq(seasonal_averages[2] > seasonal_averages[1], true)
  assert_eq(seasonal_averages[2] > seasonal_averages[4], true)
  assert_eq(seasonal_averages[3] < seasonal_averages[1], true)
  assert_eq(seasonal_averages[3] < seasonal_averages[4], true)
}

test "telemetry_trend_change_detection" {
  // 测试趋势变化检测
  
  let performance_metrics = [
    10.5, 11.2, 10.8, 11.5, 10.9,  // 稳定期
    15.2, 16.8, 17.5, 18.1, 19.3,  // 上升期
    18.8, 19.0, 18.5, 19.2, 18.7   // 新稳定期
  ]
  
  let change_threshold = 2.0  // 变化阈值
  let window_size = 3
  
  // 检测趋势变化点
  let mut change_points = []
  let mut i = window_size
  while i < performance_metrics.length() - window_size {
    // 计算前窗口平均值
    let mut prev_sum = 0.0
    let mut j = i - window_size
    while j < i {
      prev_sum = prev_sum + performance_metrics[j]
      j = j + 1
    }
    let prev_avg = prev_sum / window_size.to_double()
    
    // 计算后窗口平均值
    let mut next_sum = 0.0
    j = i
    while j < i + window_size {
      next_sum = next_sum + performance_metrics[j]
      j = j + 1
    }
    let next_avg = next_sum / window_size.to_double()
    
    // 检测显著变化
    let change_magnitude = @abs(next_avg - prev_avg)
    if change_magnitude > change_threshold {
      change_points = change_points.push((i, prev_avg, next_avg, change_magnitude))
    }
    i = i + 1
  }
  
  // 验证趋势变化检测结果
  assert_eq(change_points.length(), 2)  // 应该检测到2个变化点
  
  // 第一个变化点：稳定期到上升期
  assert_eq(change_points[0].0, 5)  // 在索引5附近
  assert_eq(change_points[0].3 > 3.0, true)  // 变化幅度应该大于3
  
  // 第二个变化点：上升期到新稳定期
  assert_eq(change_points[1].0, 10)  // 在索引10附近
  assert_eq(change_points[1].3 > 1.0, true)  // 变化幅度应该大于1
}