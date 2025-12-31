// 遥测异常检测测试用例

test "telemetry_anomaly_detection_statistical" {
  // 测试基于统计的异常检测
  
  let metric_values = [
    45.2, 47.8, 46.5, 48.1, 44.9, 47.2, 46.8, 45.5, 47.1, 46.3,
    48.5, 46.7, 45.8, 47.4, 46.1, 89.2, 47.6, 45.9, 47.3, 46.4
  ]
  
  // 验证数据
  assert_eq(metric_values.length(), 20)
  assert_eq(metric_values[15], 89.2)  // 异常值
  
  // 计算均值
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  let mean = sum / metric_values.length().to_double()
  
  // 计算标准差
  let mut variance = 0.0
  i = 0
  while i < metric_values.length() {
    let deviation = metric_values[i] - mean
    variance = variance + deviation * deviation
    i = i + 1
  }
  variance = variance / metric_values.length().to_double()
  let std_dev = variance.sqrt()
  
  // 验证统计量
  assert_eq(mean > 40.0, true)
  assert_eq(mean < 60.0, true)
  assert_eq(std_dev > 0.0, true)
  
  // 3-sigma规则检测异常
  let threshold = 3.0 * std_dev
  let mut anomalies = []
  
  i = 0
  while i < metric_values.length() {
    let value = metric_values[i]
    let z_score = (value - mean).abs() / std_dev
    
    if z_score > 3.0 {
      anomalies.push((i, value, z_score))
    }
    
    i = i + 1
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].0, 15)        // 索引15
  assert_eq(anomalies[0].1, 89.2)      // 异常值
  assert_eq(anomalies[0].2 > 3.0, true) // Z-score大于3
  
  // 验证异常值的显著性
  let anomaly_value = anomalies[0].1
  assert_eq(anomaly_value > mean + threshold, true)
}

test "telemetry_anomaly_detection_moving_average" {
  // 测试基于移动平均的异常检测
  
  let time_series = [
    100, 102, 98, 105, 101, 99, 103, 97, 104, 102,
    98, 101, 150, 99, 103, 96, 105, 101, 98, 102
  ]
  
  // 验证时间序列数据
  assert_eq(time_series.length(), 20)
  assert_eq(time_series[12], 150)  // 异常值
  
  // 计算移动平均
  let window_size = 5
  let mut moving_averages = []
  
  let mut i = window_size - 1
  while i < time_series.length() {
    let mut window_sum = 0.0
    let mut j = 0
    while j < window_size {
      window_sum = window_sum + time_series[i - j].to_double()
      j = j + 1
    }
    let avg = window_sum / window_size.to_double()
    moving_averages.push(avg)
    i = i + 1
  }
  
  // 验证移动平均计算
  assert_eq(moving_averages.length(), 16)  // 20 - 5 + 1
  
  // 检测异常（偏离移动平均超过阈值）
  let threshold_percentage = 30.0  // 30%阈值
  let mut detected_anomalies = []
  
  i = 0
  while i < moving_averages.length() {
    let actual_index = i + window_size - 1
    let actual_value = time_series[actual_index].to_double()
    let expected_value = moving_averages[i]
    
    let deviation_percentage = ((actual_value - expected_value).abs() / expected_value) * 100.0
    
    if deviation_percentage > threshold_percentage {
      detected_anomalies.push((actual_index, actual_value, expected_value, deviation_percentage))
    }
    
    i = i + 1
  }
  
  // 验证异常检测结果
  assert_eq(detected_anomalies.length(), 1)
  assert_eq(detected_anomalies[0].0, 12)        // 索引12
  assert_eq(detected_anomalies[0].1, 150.0)     // 实际值
  assert_eq(detected_anomalies[0].3 > 30.0, true) // 偏差百分比大于30%
  
  // 验证移动平均的平滑性
  let mut i = 1
  while i < moving_averages.length() {
    let prev_avg = moving_averages[i - 1]
    let curr_avg = moving_averages[i]
    
    // 移动平均变化应该相对平滑
    let avg_change = (curr_avg - prev_avg).abs()
    assert_eq(avg_change < 20.0, true)
    
    i = i + 1
  }
}

test "telemetry_anomaly_detection_pattern_based" {
  // 测试基于模式的异常检测
  
  let log_patterns = [
    "INFO: User login successful",
    "INFO: User logout", 
    "INFO: User login successful",
    "WARN: Slow database query detected",
    "INFO: User login successful",
    "INFO: User logout",
    "ERROR: Database connection failed",
    "INFO: User login successful",
    "INFO: User logout",
    "INFO: User login successful",
    "ERROR: Database connection failed",
    "ERROR: Database connection failed",
    "CRITICAL: System out of memory",
    "INFO: User login successful",
    "INFO: User logout"
  ]
  
  // 验证日志模式
  assert_eq(log_patterns.length(), 15)
  
  // 统计不同级别的日志
  let mut log_levels = {
    "INFO": 0,
    "WARN": 0,
    "ERROR": 0,
    "CRITICAL": 0
  }
  
  let mut i = 0
  while i < log_patterns.length() {
    let log_entry = log_patterns[i]
    let level = log_entry.split(":")[0]
    
    if log_levels.contains(level) {
      log_levels[level] = log_levels[level] + 1
    }
    
    i = i + 1
  }
  
  // 验证日志级别统计
  assert_eq(log_levels["INFO"], 9)
  assert_eq(log_levels["WARN"], 1)
  assert_eq(log_levels["ERROR"], 3)
  assert_eq(log_levels["CRITICAL"], 1)
  
  // 检测异常模式
  let mut anomalies = []
  
  // 异常模式1: 连续ERROR
  let mut consecutive_errors = 0
  i = 0
  while i < log_patterns.length() {
    let log_entry = log_patterns[i]
    if log_entry.has_prefix("ERROR") {
      consecutive_errors = consecutive_errors + 1
      if consecutive_errors >= 2 {
        anomalies.push(("consecutive_errors", i, log_entry))
      }
    } else {
      consecutive_errors = 0
    }
    i = i + 1
  }
  
  // 异常模式2: CRITICAL级别日志
  i = 0
  while i < log_patterns.length() {
    let log_entry = log_patterns[i]
    if log_entry.has_prefix("CRITICAL") {
      anomalies.push(("critical_log", i, log_entry))
    }
    i = i + 1
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 3)  // 1个连续ERROR + 1个CRITICAL + 1个连续ERROR
  
  // 验证特定异常
  assert_eq(anomalies[0].0, "consecutive_errors")
  assert_eq(anomalies[1].0, "consecutive_errors")
  assert_eq(anomalies[2].0, "critical_log")
  
  // 计算异常严重程度
  let mut severity_score = 0
  i = 0
  while i < anomalies.length() {
    let anomaly_type = anomalies[i].0
    
    if anomaly_type == "critical_log" {
      severity_score = severity_score + 10
    } else if anomaly_type == "consecutive_errors" {
      severity_score = severity_score + 5
    }
    
    i = i + 1
  }
  
  // 验证异常严重程度
  assert_eq(severity_score, 20)  // 2*5 + 1*10
  assert_eq(severity_score > 15, true)  // 高严重程度
}

test "telemetry_anomaly_detection_seasonal" {
  // 测试季节性异常检测
  
  let hourly_metrics = [
    10, 12, 15, 25, 45, 68, 85, 92, 88, 75,  // 0-9时 (工作时间)
    65, 58, 52, 48, 42, 38, 35, 32, 28, 22,  // 10-19时 (下班时间)
    18, 15, 12, 10                           // 20-23时 (夜间)
  ]
  
  // 验证小时指标数据
  assert_eq(hourly_metrics.length(), 24)
  
  // 计算每个时间段的基准值
  let work_hours_avg = (85 + 92 + 88 + 75).to_double() / 4.0  // 9-12时高峰
  let evening_hours_avg = (35 + 32 + 28 + 22).to_double() / 4.0  // 16-19时
  let night_hours_avg = (18 + 15 + 12 + 10).to_double() / 4.0  // 20-23时
  
  // 验证基准值
  assert_eq(work_hours_avg > evening_hours_avg, true)
  assert_eq(evening_hours_avg > night_hours_avg, true)
  
  // 模拟异常数据（在夜间出现工作时间级别的负载）
  let anomalous_data = hourly_metrics
  anomalous_data[21] = 80  // 21时出现异常高负载
  
  // 季节性异常检测
  let mut seasonal_anomalies = []
  let threshold_factor = 2.0  // 阈值因子
  
  let mut i = 0
  while i < anomalous_data.length() {
    let hour = i
    let value = anomalous_data[i].to_double()
    
    let expected_value = if hour >= 9 and hour <= 12 {
      work_hours_avg
    } else if hour >= 16 and hour <= 19 {
      evening_hours_avg
    } else if hour >= 20 or hour <= 6 {
      night_hours_avg
    } else {
      (work_hours_avg + night_hours_avg) / 2.0  // 过渡时间
    }
    
    if value > expected_value * threshold_factor {
      seasonal_anomalies.push((hour, value, expected_value))
    }
    
    i = i + 1
  }
  
  // 验证季节性异常检测结果
  assert_eq(seasonal_anomalies.length(), 1)
  assert_eq(seasonal_anomalies[0].0, 21)        // 21时
  assert_eq(seasonal_anomalies[0].1, 80.0)      // 异常值
  assert_eq(seasonal_anomalies[0].2, night_hours_avg) // 期望值
  
  // 验证异常的显著性
  let anomaly_ratio = seasonal_anomalies[0].1 / seasonal_anomalies[0].2
  assert_eq(anomaly_ratio > threshold_factor, true)
  
  // 计算季节性模式强度
  let pattern_strength = (work_hours_avg - night_hours_avg) / work_hours_avg
  assert_eq(pattern_strength > 0.5, true)  // 强季节性模式
}

test "telemetry_anomaly_detection_multivariate" {
  // 测试多元异常检测
  
  let system_metrics = [
    (75.2, 68.5, 45.8),   // CPU, Memory, Disk
    (78.1, 72.3, 48.2),
    (76.5, 69.8, 46.1),
    (77.8, 71.2, 47.5),
    (74.9, 67.9, 44.8),
    (95.5, 95.2, 85.3),   // 异常点
    (76.2, 70.1, 46.9),
    (75.8, 68.7, 45.3),
    (77.1, 71.5, 47.8),
    (76.4, 69.4, 46.2)
  ]
  
  // 验证多元数据
  assert_eq(system_metrics.length(), 10)
  assert_eq(system_metrics[4].0, 74.9)   // 正常点
  assert_eq(system_metrics[5].0, 95.5)   // 异常点
  
  // 计算每个维度的均值和标准差
  let dimensions = 3
  let mut means = []
  let mut std_devs = []
  
  let mut dim = 0
  while dim < dimensions {
    let mut sum = 0.0
    let mut i = 0
    while i < system_metrics.length() {
      if dim == 0 {
        sum = sum + system_metrics[i].0.to_double()
      } else if dim == 1 {
        sum = sum + system_metrics[i].1.to_double()
      } else if dim == 2 {
        sum = sum + system_metrics[i].2.to_double()
      }
      i = i + 1
    }
    let mean = sum / system_metrics.length().to_double()
    means.push(mean)
    
    // 计算标准差
    let mut variance = 0.0
    i = 0
    while i < system_metrics.length() {
      let value = if dim == 0 {
        system_metrics[i].0.to_double()
      } else if dim == 1 {
        system_metrics[i].1.to_double()
      } else {
        system_metrics[i].2.to_double()
      }
      let deviation = value - mean
      variance = variance + deviation * deviation
      i = i + 1
    }
    variance = variance / system_metrics.length().to_double()
    std_devs.push(variance.sqrt())
    
    dim = dim + 1
  }
  
  // 验证统计量
  assert_eq(means.length(), 3)
  assert_eq(std_devs.length(), 3)
  
  // 马氏距离异常检测
  let mut mahalanobis_distances = []
  
  let mut i = 0
  while i < system_metrics.length() {
    let cpu = system_metrics[i].0.to_double()
    let memory = system_metrics[i].1.to_double()
    let disk = system_metrics[i].2.to_double()
    
    // 简化的马氏距离计算（假设维度独立）
    let cpu_z = (cpu - means[0]) / std_devs[0]
    let memory_z = (memory - means[1]) / std_devs[1]
    let disk_z = (disk - means[2]) / std_devs[2]
    
    let mahalanobis_dist = (cpu_z * cpu_z + memory_z * memory_z + disk_z * disk_z).sqrt()
    mahalanobis_distances.push(mahalanobis_dist)
    
    i = i + 1
  }
  
  // 验证马氏距离计算
  assert_eq(mahalanobis_distances.length(), system_metrics.length())
  
  // 检测异常（马氏距离超过阈值）
  let threshold = 3.0  // 3-sigma阈值
  let mut multivariate_anomalies = []
  
  i = 0
  while i < mahalanobis_distances.length() {
    if mahalanobis_distances[i] > threshold {
      multivariate_anomalies.push((i, mahalanobis_distances[i]))
    }
    i = i + 1
  }
  
  // 验证多元异常检测结果
  assert_eq(multivariate_anomalies.length(), 1)
  assert_eq(multivariate_anomalies[0].0, 5)  // 索引5（异常点）
  assert_eq(multivariate_anomalies[0].1 > threshold, true)
  
  // 验证异常点的多维度特征
  let anomaly_index = multivariate_anomalies[0].0
  let anomaly_cpu = system_metrics[anomaly_index].0
  let anomaly_memory = system_metrics[anomaly_index].1
  let anomaly_disk = system_metrics[anomaly_index].2
  
  // 异常点在所有维度上都应该显著偏离均值
  assert_eq(anomaly_cpu > means[0] + 2.0 * std_devs[0], true)
  assert_eq(anomaly_memory > means[1] + 2.0 * std_devs[1], true)
  assert_eq(anomaly_disk > means[2] + 2.0 * std_devs[2], true)
}