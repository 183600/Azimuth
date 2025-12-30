// 遥测数据相关性分析测试用例
// 测试不同遥测指标之间的相关性关系和模式发现

test "telemetry_correlation_coefficient_calculation" {
  // 测试相关系数计算
  
  let cpu_usage = [45.2, 52.8, 48.1, 61.5, 58.3, 67.2, 72.1, 69.8]
  let response_time = [120.5, 135.2, 125.8, 156.3, 148.7, 172.4, 185.6, 178.9]
  
  // 计算均值
  let n = cpu_usage.length().to_double()
  let mut cpu_sum = 0.0
  let mut response_sum = 0.0
  
  let mut i = 0
  while i < cpu_usage.length() {
    cpu_sum = cpu_sum + cpu_usage[i]
    response_sum = response_sum + response_time[i]
    i = i + 1
  }
  
  let cpu_mean = cpu_sum / n
  let response_mean = response_sum / n
  
  // 计算协方差和方差
  let mut covariance = 0.0
  let mut cpu_variance = 0.0
  let mut response_variance = 0.0
  
  i = 0
  while i < cpu_usage.length() {
    let cpu_diff = cpu_usage[i] - cpu_mean
    let response_diff = response_time[i] - response_mean
    
    covariance = covariance + cpu_diff * response_diff
    cpu_variance = cpu_variance + cpu_diff * cpu_diff
    response_variance = response_variance + response_diff * response_diff
    i = i + 1
  }
  
  // 计算相关系数
  let correlation = covariance / (@sqrt(cpu_variance) * @sqrt(response_variance))
  
  // 验证强正相关
  assert_eq(correlation > 0.8 && correlation < 1.0, true)
  assert_eq(correlation > 0.0, true)  // 正相关
}

test "telemetry_cross_correlation_lag_analysis" {
  // 测试交叉相关滞后分析
  
  let request_rate = [100, 120, 95, 140, 125, 160, 145, 180, 165, 190]
  let cpu_usage = [35, 38, 36, 42, 40, 48, 45, 52, 49, 55]
  
  let max_lag = 3
  
  // 计算不同滞后下的相关性
  let mut lag_correlations = []
  let mut lag = 0
  
  while lag <= max_lag {
    let mut correlation_sum = 0.0
    let mut count = 0
    
    let mut i = 0
    while i + lag < request_rate.length() {
      correlation_sum = correlation_sum + request_rate[i].to_double() * cpu_usage[i + lag].to_double()
      count = count + 1
      i = i + 1
    }
    
    if count > 0 {
      let avg_correlation = correlation_sum / count.to_double()
      lag_correlations.push((lag, avg_correlation))
    }
    
    lag = lag + 1
  }
  
  // 验证滞后相关性结果
  assert_eq(lag_correlations.length(), max_lag + 1)
  
  // 找到最大相关性对应的滞后
  let mut max_correlation = 0.0
  let mut best_lag = 0
  
  let mut i = 0
  while i < lag_correlations.length() {
    let (current_lag, correlation) = lag_correlations[i]
    if correlation > max_correlation {
      max_correlation = correlation
      best_lag = current_lag
    }
    i = i + 1
  }
  
  // 验证最佳滞后
  assert_eq(best_lag >= 0 && best_lag <= max_lag, true)
  assert_eq(max_correlation > 0.0, true)
}

test "telemetry_pattern_correlation_discovery" {
  // 测试模式相关性发现
  
  let error_patterns = ["timeout", "timeout", "connection_failed", "timeout", "rate_limit", "timeout", "timeout"]
  let performance_degradation = [true, true, false, true, false, true, true]
  
  // 计算模式共现频率
  let mut pattern_counts = @hashmap.new()
  let mut co_occurrence_counts = @hashmap.new()
  
  let mut i = 0
  while i < error_patterns.length() {
    let pattern = error_patterns[i]
    let degradation = performance_degradation[i]
    
    // 统计模式频率
    if pattern_counts.contains(pattern) {
      pattern_counts[pattern] = pattern_counts[pattern] + 1
    } else {
      pattern_counts[pattern] = 1
    }
    
    // 统计与性能下降的共现
    if degradation {
      let co_occurrence_key = pattern + "_degradation"
      if co_occurrence_counts.contains(co_occurrence_key) {
        co_occurrence_counts[co_occurrence_key] = co_occurrence_counts[co_occurrence_key] + 1
      } else {
        co_occurrence_counts[co_occurrence_key] = 1
      }
    }
    
    i = i + 1
  }
  
  // 计算条件概率
  let mut conditional_probabilities = []
  let keys = pattern_counts.keys()
  
  let mut i = 0
  while i < keys.length() {
    let pattern = keys[i]
    let pattern_count = pattern_counts[pattern]
    let co_occurrence_key = pattern + "_degradation"
    
    let co_occurrence_count = 
      if co_occurrence_counts.contains(co_occurrence_key) {
        co_occurrence_counts[co_occurrence_key]
      } else {
        0
      }
    
    let conditional_probability = co_occurrence_count.to_double() / pattern_count.to_double()
    conditional_probabilities.push((pattern, conditional_probability))
    
    i = i + 1
  }
  
  // 验证条件概率计算
  assert_eq(conditional_probabilities.length() > 0, true)
  
  // timeout错误应该与性能高度相关
  let mut timeout_probability = 0.0
  let mut i = 0
  while i < conditional_probabilities.length() {
    let (pattern, probability) = conditional_probabilities[i]
    if pattern == "timeout" {
      timeout_probability = probability
    }
    i = i + 1
  }
  
  assert_eq(timeout_probability > 0.7, true)  // timeout应该70%以上导致性能下降
}

test "telemetry_multivariate_correlation_analysis" {
  // 测试多元相关性分析
  
  let metrics = [
    (45.2, 120.5, 1024),   // cpu, response_time, memory
    (52.8, 135.2, 1156),
    (48.1, 125.8, 1089),
    (61.5, 156.3, 1280),
    (58.3, 148.7, 1225),
    (67.2, 172.4, 1408),
    (72.1, 185.6, 1536),
    (69.8, 178.9, 1489)
  ]
  
  // 计算相关系数矩阵
  let correlation_matrix = [
    (1.0, 0.0, 0.0),  // cpu-cpu, cpu-response, cpu-memory
    (0.0, 1.0, 0.0),  // response-cpu, response-response, response-memory  
    (0.0, 0.0, 1.0)   // memory-cpu, memory-response, memory-memory
  ]
  
  // 计算CPU与响应时间的相关性
  let mut cpu_sum = 0.0
  let mut response_sum = 0.0
  let mut memory_sum = 0.0
  
  let mut i = 0
  while i < metrics.length() {
    cpu_sum = cpu_sum + metrics[i].0
    response_sum = response_sum + metrics[i].1
    memory_sum = memory_sum + metrics[i].2.to_double()
    i = i + 1
  }
  
  let cpu_mean = cpu_sum / metrics.length().to_double()
  let response_mean = response_sum / metrics.length().to_double()
  let memory_mean = memory_sum / metrics.length().to_double()
  
  // CPU-响应时间相关性
  let mut cpu_response_covariance = 0.0
  let mut cpu_variance = 0.0
  let mut response_variance = 0.0
  
  i = 0
  while i < metrics.length() {
    let cpu_diff = metrics[i].0 - cpu_mean
    let response_diff = metrics[i].1 - response_mean
    
    cpu_response_covariance = cpu_response_covariance + cpu_diff * response_diff
    cpu_variance = cpu_variance + cpu_diff * cpu_diff
    response_variance = response_variance + response_diff * response_diff
    i = i + 1
  }
  
  let cpu_response_correlation = cpu_response_covariance / (@sqrt(cpu_variance) * @sqrt(response_variance))
  
  // CPU-内存相关性
  let mut cpu_memory_covariance = 0.0
  let mut memory_variance = 0.0
  
  i = 0
  while i < metrics.length() {
    let cpu_diff = metrics[i].0 - cpu_mean
    let memory_diff = metrics[i].2.to_double() - memory_mean
    
    cpu_memory_covariance = cpu_memory_covariance + cpu_diff * memory_diff
    memory_variance = memory_variance + memory_diff * memory_diff
    i = i + 1
  }
  
  let cpu_memory_correlation = cpu_memory_covariance / (@sqrt(cpu_variance) * @sqrt(memory_variance))
  
  // 验证多元相关性
  assert_eq(cpu_response_correlation > 0.8, true)  // CPU与响应时间强相关
  assert_eq(cpu_memory_correlation > 0.7, true)    // CPU与内存相关
  assert_eq(cpu_response_correlation > cpu_memory_correlation, true)  // CPU-响应时间相关性更强
}

test "telemetry_seasonal_correlation_detection" {
  // 测试季节性相关性检测
  
  let hourly_traffic = [120, 150, 180, 220, 280, 350, 320, 290, 250, 200, 160, 130]
  let hourly_errors = [5, 8, 12, 18, 28, 42, 35, 25, 15, 9, 6, 4]
  
  // 计算小时级别的相关性
  let mut hourly_correlations = []
  let mut hour = 0
  
  while hour < 12 {
    let traffic_value = hourly_traffic[hour].to_double()
    let error_value = hourly_errors[hour].to_double()
    
    // 简化的相关性计算：比值关系
    let error_rate = error_value / traffic_value
    hourly_correlations.push((hour, traffic_value, error_value, error_rate))
    
    hour = hour + 1
  }
  
  // 验证季节性相关性
  assert_eq(hourly_correlations.length(), 12)
  
  // 高峰时段（5-7点）应该有更高的错误率
  let mut peak_hours_error_rate = 0.0
  let mut off_peak_hours_error_rate = 0.0
  let mut peak_count = 0
  let mut off_peak_count = 0
  
  let mut i = 0
  while i < hourly_correlations.length() {
    let (hour, _, _, error_rate) = hourly_correlations[i]
    
    if hour >= 5 && hour <= 7 {
      peak_hours_error_rate = peak_hours_error_rate + error_rate
      peak_count = peak_count + 1
    } else {
      off_peak_hours_error_rate = off_peak_hours_error_rate + error_rate
      off_peak_count = off_peak_count + 1
    }
    
    i = i + 1
  }
  
  let peak_avg_error_rate = peak_hours_error_rate / peak_count.to_double()
  let off_peak_avg_error_rate = off_peak_hours_error_rate / off_peak_count.to_double()
  
  assert_eq(peak_avg_error_rate > off_peak_avg_error_rate, true)
}

test "telemetry_correlation_threshold_detection" {
  // 测试相关性阈值检测
  
  let service_metrics = [
    ("service-a", 85.2, 120.5),
    ("service-b", 45.8, 65.3),
    ("service-c", 92.1, 145.8),
    ("service-d", 38.5, 52.1),
    ("service-e", 78.9, 115.6)
  ]
  
  let correlation_threshold = 0.7
  
  // 计算CPU使用率与响应时间的比值作为相关性指标
  let mut correlation_scores = []
  let mut i = 0
  
  while i < service_metrics.length() {
    let (service_name, cpu_usage, response_time) = service_metrics[i]
    
    // 简化的相关性评分：标准化后的乘积
    let normalized_cpu = cpu_usage / 100.0
    let normalized_response = response_time / 200.0
    let correlation_score = normalized_cpu * normalized_response
    
    correlation_scores.push((service_name, correlation_score))
    i = i + 1
  }
  
  // 检测高相关性服务
  let mut high_correlation_services = []
  i = 0
  while i < correlation_scores.length() {
    let (service_name, score) = correlation_scores[i]
    
    if score > correlation_threshold {
      high_correlation_services.push((service_name, score))
    }
    i = i + 1
  }
  
  // 验证高相关性检测结果
  assert_eq(correlation_scores.length(), 5)
  assert_eq(high_correlation_services.length() >= 2, true)  // 至少2个服务高相关
  
  // 验证高相关性服务的评分
  let mut i = 0
  while i < high_correlation_services.length() {
    let (_, score) = high_correlation_services[i]
    assert_eq(score > correlation_threshold, true)
    i = i + 1
  }
}