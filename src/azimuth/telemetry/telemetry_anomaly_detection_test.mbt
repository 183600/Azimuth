// 遥测数据异常检测测试用例

test "telemetry_statistical_outlier_detection" {
  // 测试统计学异常值检测
  
  let metric_values = [10.2, 11.5, 9.8, 10.7, 11.2, 10.5, 9.9, 10.8, 45.6, 10.3, 11.1]
  
  // 计算均值
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  let mean = sum / metric_values.length().to_double()
  
  // 计算标准差
  let mut variance_sum = 0.0
  i = 0
  while i < metric_values.length() {
    let diff = metric_values[i] - mean
    variance_sum = variance_sum + diff * diff
    i = i + 1
  }
  let variance = variance_sum / metric_values.length().to_double()
  let std_dev = @sqrt(variance)
  
  // 检测异常值 (3σ规则)
  let threshold = 3.0 * std_dev
  let mut outliers = []
  i = 0
  while i < metric_values.length() {
    let value = metric_values[i]
    let z_score = @abs(value - mean) / std_dev
    if z_score > 3.0 {
      outliers = outliers.push((i, value, z_score))
    }
    i = i + 1
  }
  
  // 验证异常值检测结果
  assert_eq(outliers.length(), 1)  // 应该检测到1个异常值
  assert_eq(outliers[0].0, 8)      // 异常值在索引8
  assert_eq(outliers[0].1 > 45.0, true)  // 异常值应该大于45
  assert_eq(outliers[0].2 > 3.0, true)   // Z分数应该大于3
}

test "telemetry_spike_detection" {
  // 测试峰值检测
  
  let cpu_usage = [25.3, 26.1, 24.8, 27.2, 89.5, 26.7, 25.9, 27.1, 26.3, 25.8]
  
  let spike_threshold = 2.5  // 峰值阈值：平均值的2.5倍
  
  // 计算移动平均值
  let window_size = 3
  let mut moving_averages = []
  let mut i = window_size
  while i <= cpu_usage.length() {
    let mut window_sum = 0.0
    let mut j = i - window_size
    while j < i {
      window_sum = window_sum + cpu_usage[j]
      j = j + 1
    }
    let avg = window_sum / window_size.to_double()
    moving_averages = moving_averages.push(avg)
    i = i + 1
  }
  
  // 检测峰值
  let mut spikes = []
  i = 0
  while i < moving_averages.length() {
    let current_value = cpu_usage[i + window_size - 1]
    let moving_avg = moving_averages[i]
    
    if current_value > moving_avg * spike_threshold {
      spikes = spikes.push((i + window_size - 1, current_value, moving_avg))
    }
    i = i + 1
  }
  
  // 验证峰值检测结果
  assert_eq(spikes.length(), 1)  // 应该检测到1个峰值
  assert_eq(spikes[0].0, 4)      // 峰值在索引4
  assert_eq(spikes[0].1 > 80.0, true)  // 峰值应该大于80
}

test "telemetry_pattern_anomaly_detection" {
  // 测试模式异常检测
  
  let request_pattern = [
    ("GET", 200),
    ("GET", 200),
    ("POST", 201),
    ("GET", 200),
    ("GET", 200),
    ("POST", 201),
    ("GET", 500),  // 异常：GET请求通常返回200
    ("GET", 200),
    ("POST", 201),
    ("GET", 200)
  ]
  
  // 建立正常模式
  let mut normal_patterns = @hashmap.new()
  let mut i = 0
  while i < request_pattern.length() {
    let method = request_pattern[i].0
    let status = request_pattern[i].1
    
    if normal_patterns.contains(method) {
      let count = normal_patterns[method]
      normal_patterns[method] = count + 1
    } else {
      normal_patterns[method] = 1
    }
    i = i + 1
  }
  
  // 检测模式异常
  let mut anomalies = []
  i = 0
  while i < request_pattern.length() {
    let method = request_pattern[i].0
    let status = request_pattern[i].1
    
    // GET请求通常返回200，返回其他状态码可能是异常
    if method == "GET" && status != 200 {
      anomalies = anomalies.push((i, method, status, "unexpected_status"))
    }
    i = i + 1
  }
  
  // 验证模式异常检测结果
  assert_eq(anomalies.length(), 1)  // 应该检测到1个异常
  assert_eq(anomalies[0].0, 6)      // 异常在索引6
  assert_eq(anomalies[0].1, "GET")  // GET请求
  assert_eq(anomalies[0].2, 500)    // 返回500状态码
}

test "telemetry_rate_change_anomaly" {
  // 测试变化率异常检测
  
  let response_times = [100.0, 105.0, 98.0, 102.0, 350.0, 108.0, 95.0, 103.0]
  
  let max_change_rate = 2.0  // 最大允许变化率（倍数）
  
  // 计算变化率
  let mut change_rates = []
  let mut i = 1
  while i < response_times.length() {
    let prev_value = response_times[i - 1]
    let curr_value = response_times[i]
    let change_rate = curr_value / prev_value
    change_rates = change_rates.push((i, curr_value, change_rate))
    i = i + 1
  }
  
  // 检测变化率异常
  let mut rate_anomalies = []
  i = 0
  while i < change_rates.length() {
    let index = change_rates[i].0
    let value = change_rates[i].1
    let rate = change_rates[i].2
    
    if rate > max_change_rate {
      rate_anomalies = rate_anomalies.push((index, value, rate))
    }
    i = i + 1
  }
  
  // 验证变化率异常检测结果
  assert_eq(rate_anomalies.length(), 1)  // 应该检测到1个异常
  assert_eq(rate_anomalies[0].0, 4)      // 异常在索引4
  assert_eq(rate_anomalies[0].1 > 300.0, true)  // 响应时间应该大于300
  assert_eq(rate_anomalies[0].2 > 3.0, true)    // 变化率应该大于3
}

test "telemetry_seasonal_anomaly_detection" {
  // 测试季节性异常检测
  
  let hourly_metrics = [
    (0, 15.2),   // 午夜
    (6, 25.3),   // 早上
    (12, 85.6),  // 中午（高峰）
    (18, 45.7),  // 傍晚
    (0, 14.8),   // 午夜
    (6, 24.9),   // 早上
    (12, 180.3), // 中午（异常高峰）
    (18, 46.1)   // 傍晚
  ]
  
  // 建立季节性模式（按小时）
  let mut seasonal_patterns = @hashmap.new()
  let mut i = 0
  while i < hourly_metrics.length() / 2 {  // 使用前一半数据建立模式
    let hour = hourly_metrics[i].0
    let value = hourly_metrics[i].1
    
    if seasonal_patterns.contains(hour) {
      let (sum, count) = seasonal_patterns[hour]
      seasonal_patterns[hour] = (sum + value, count + 1)
    } else {
      seasonal_patterns[hour] = (value, 1)
    }
    i = i + 1
  }
  
  // 计算季节性平均值
  let mut seasonal_averages = @hashmap.new()
  let keys = seasonal_patterns.keys()
  let mut i = 0
  while i < keys.length() {
    let hour = keys[i]
    let (sum, count) = seasonal_patterns[hour]
    seasonal_averages[hour] = sum / count.to_double()
    i = i + 1
  }
  
  // 检测季节性异常
  let mut seasonal_anomalies = []
  i = hourly_metrics.length() / 2  // 检测后一半数据
  while i < hourly_metrics.length() {
    let hour = hourly_metrics[i].0
    let value = hourly_metrics[i].1
    
    if seasonal_averages.contains(hour) {
      let avg_value = seasonal_averages[hour]
      let deviation_ratio = value / avg_value
      
      if deviation_ratio > 2.0 {  // 超过平均值2倍认为是异常
        seasonal_anomalies = seasonal_anomalies.push((i, hour, value, avg_value, deviation_ratio))
      }
    }
    i = i + 1
  }
  
  // 验证季节性异常检测结果
  assert_eq(seasonal_anomalies.length(), 1)  // 应该检测到1个异常
  assert_eq(seasonal_anomalies[0].0, 6)      // 异常在索引6
  assert_eq(seasonal_anomalies[0].1, 12)     // 中午12点
  assert_eq(seasonal_anomalies[0].2 > 150.0, true)  // 异常值应该大于150
  assert_eq(seasonal_anomalies[0].4 > 2.0, true)    // 偏差比例应该大于2
}