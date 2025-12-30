// 遥测多维度指标关联分析测试用例
// 测试跨时间、跨服务、跨指标的多元关联分析

test "telemetry_temporal_correlation_analysis" {
  // 测试时间关联分析
  
  let time_series_data = [
    (1640995200L, 45.2, 120.5, 1024),   // timestamp, cpu, response_time, memory
    (1640995260L, 52.8, 135.2, 1156),
    (1640995320L, 48.1, 125.8, 1089),
    (1640995380L, 61.5, 156.3, 1280),
    (1640995440L, 58.3, 148.7, 1225),
    (1640995500L, 67.2, 172.4, 1408),
    (1640995560L, 72.1, 185.6, 1536),
    (1640995620L, 69.8, 178.9, 1489)
  ]
  
  // 计算时间滞后相关性
  let max_lag = 3
  let mut lag_correlations = []
  
  // CPU与响应时间的滞后相关性
  let mut lag = 0
  while lag <= max_lag {
    let mut correlation_sum = 0.0
    let mut count = 0
    
    let mut i = 0
    while i + lag < time_series_data.length() {
      let cpu_current = time_series_data[i].1
      let response_lagged = time_series_data[i + lag].2
      
      correlation_sum = correlation_sum + cpu_current * response_lagged
      count = count + 1
      i = i + 1
    }
    
    if count > 0 {
      let avg_correlation = correlation_sum / count.to_double()
      lag_correlations.push((lag, avg_correlation))
    }
    
    lag = lag + 1
  }
  
  // 验证时间滞后相关性
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
  
  assert_eq(best_lag >= 0 && best_lag <= max_lag, true)
  assert_eq(max_correlation > 0.0, true)
}

test "telemetry_cross_service_impact_analysis" {
  // 测试跨服务影响分析
  
  let service_metrics = [
    ("user-service", [85.2, 12.5, 1024, 98.5]),   // cpu, error_rate, memory, availability
    ("order-service", [68.7, 8.3, 1536, 97.2]),
    ("payment-service", [45.3, 3.2, 896, 99.8]),
    ("notification-service", [35.8, 15.6, 640, 95.1]),
    ("inventory-service", [78.9, 6.8, 1280, 96.4])
  ]
  
  let service_dependencies = [
    ("order-service", ["user-service", "inventory-service", "payment-service"]),
    ("user-service", ["notification-service"]),
    ("payment-service", ["user-service"])
  ]
  
  // 计算服务间影响因子
  let mut impact_matrix = []
  
  let mut i = 0
  while i < service_metrics.length() {
    let (service_name, metrics) = service_metrics[i]
    
    let mut j = 0
    while j < service_dependencies.length() {
      let (dependent_service, dependencies) = service_dependencies[j]
      
      let mut k = 0
      while k < dependencies.length() {
        let dependency = dependencies[k]
        
        if dependency == service_name {
          // 找到依赖关系，计算影响因子
          let mut dependent_metrics = []
          let mut m = 0
          while m < service_metrics.length() {
            let (svc_name, svc_metrics) = service_metrics[m]
            if svc_name == dependent_service {
              dependent_metrics = svc_metrics
              break
            }
            m = m + 1
          }
          
          // 影响因子基于错误率和可用性的组合
          let dependency_health = (100.0 - metrics[1]) / 100.0 * metrics[3] / 100.0
          let dependent_health = (100.0 - dependent_metrics[1]) / 100.0 * dependent_metrics[3] / 100.0
          
          let impact_factor = dependency_health * 0.6 + dependent_health * 0.4
          
          impact_matrix = impact_matrix.push((service_name, dependent_service, impact_factor))
        }
        
        k = k + 1
      }
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证跨服务影响分析结果
  assert_eq(impact_matrix.length() > 0, true)
  
  // 找到影响最大的服务对
  let mut max_impact = 0.0
  let mut critical_pair = ("", "")
  
  let mut i = 0
  while i < impact_matrix.length() {
    let (source, target, impact) = impact_matrix[i]
    if impact > max_impact {
      max_impact = impact
      critical_pair = (source, target)
    }
    i = i + 1
  }
  
  assert_eq(critical_pair.0 != "", true)
  assert_eq(critical_pair.1 != "", true)
  assert_eq(max_impact > 0.0, true)
}

test "telemetry_multidimensional_anomaly_detection" {
  // 测试多维异常检测
  
  let metric_samples = [
    (45.2, 120.5, 1024, 8.5, 98.2),   // cpu, response_time, memory, error_rate, availability
    (52.8, 135.2, 1156, 12.3, 96.8),
    (48.1, 125.8, 1089, 9.1, 97.5),
    (95.3, 280.7, 1536, 45.2, 85.6),  // 异常点
    (58.3, 148.7, 1225, 11.8, 96.1),
    (67.2, 172.4, 1408, 15.4, 94.7),
    (72.1, 185.6, 1536, 18.9, 93.2),
    (69.8, 178.9, 1489, 16.2, 94.3)
  ]
  
  // 计算各维度的均值和标准差
  let dimension_count = 5
  let mut means = [0.0, 0.0, 0.0, 0.0, 0.0]
  let mut stddevs = [0.0, 0.0, 0.0, 0.0, 0.0]
  
  // 计算均值
  let mut i = 0
  while i < metric_samples.length() {
    let sample = metric_samples[i]
    let mut j = 0
    while j < dimension_count {
      means[j] = means[j] + sample[j]
      j = j + 1
    }
    i = i + 1
  }
  
  let mut j = 0
  while j < dimension_count {
    means[j] = means[j] / metric_samples.length().to_double()
    j = j + 1
  }
  
  // 计算标准差
  let mut i = 0
  while i < metric_samples.length() {
    let sample = metric_samples[i]
    let mut j = 0
    while j < dimension_count {
      let diff = sample[j] - means[j]
      stddevs[j] = stddevs[j] + diff * diff
      j = j + 1
    }
    i = i + 1
  }
  
  let mut j = 0
  while j < dimension_count {
    stddevs[j] = @sqrt(stddevs[j] / metric_samples.length().to_double())
    j = j + 1
  }
  
  // 多维异常检测（马氏距离的简化版本）
  let mut anomaly_scores = []
  let mut i = 0
  while i < metric_samples.length() {
    let sample = metric_samples[i]
    
    // 计算标准化距离
    let mut total_distance = 0.0
    let mut j = 0
    while j < dimension_count {
      let standardized_diff = (sample[j] - means[j]) / stddevs[j]
      total_distance = total_distance + standardized_diff * standardized_diff
      j = j + 1
    }
    
    let mahalanobis_distance = @sqrt(total_distance)
    anomaly_scores.push((i, mahalanobis_distance))
    
    i = i + 1
  }
  
  // 验证多维异常检测结果
  assert_eq(anomaly_scores.length(), metric_samples.length())
  
  // 找到异常点
  let anomaly_threshold = 3.0  // 3σ规则
  let mut detected_anomalies = []
  
  let mut i = 0
  while i < anomaly_scores.length() {
    let (index, score) = anomaly_scores[i]
    if score > anomaly_threshold {
      detected_anomalies.push((index, score))
    }
    i = i + 1
  }
  
  assert_eq(detected_anomalies.length(), 1)  // 应该检测到1个异常点
  assert_eq(detected_anomalies[0].0, 3)      // 异常点应该在索引3
}

test "telemetry_causal_relationship_inference" {
  // 测试因果关系推理
  
  let event_sequence = [
    (1640995200L, "database", "slow_query"),
    (1640995210L, "database", "connection_pool_exhaustion"),
    (1640995220L, "user-service", "response_time_increase"),
    (1640995230L, "user-service", "error_rate_spike"),
    (1640995240L, "api-gateway", "timeout_errors"),
    (1640995250L, "api-gateway", "circuit_breaker_open")
  ]
  
  // 时间窗口（秒）
  let causality_window = 30L
  
  // 构建因果关系图
  let mut causal_relationships = []
  
  let mut i = 0
  while i < event_sequence.length() {
    let (timestamp_i, service_i, event_i) = event_sequence[i]
    
    let mut j = i + 1
    while j < event_sequence.length() {
      let (timestamp_j, service_j, event_j) = event_sequence[j]
      
      // 检查时间窗口内的因果关系
      if timestamp_j - timestamp_i <= causality_window {
        // 简化的因果关系：如果事件A在事件B之前发生且时间相近
        let causal_strength = 1.0 - (timestamp_j - timestamp_i).to_double() / causality_window.to_double()
        causal_relationships.push((service_i, event_i, service_j, event_j, causal_strength))
      }
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证因果关系推理结果
  assert_eq(causal_relationships.length() > 0, true)
  
  // 验证因果链的逻辑性
  let mut database_to_user_service = false
  let mut user_service_to_gateway = false
  
  let mut i = 0
  while i < causal_relationships.length() {
    let (source_service, _, target_service, _, _) = causal_relationships[i]
    
    if source_service == "database" && target_service == "user-service" {
      database_to_user_service = true
    } else if source_service == "user-service" && target_service == "api-gateway" {
      user_service_to_gateway = true
    }
    
    i = i + 1
  }
  
  assert_eq(database_to_user_service, true)
  assert_eq(user_service_to_gateway, true)
  
  // 找到最强的因果关系
  let mut strongest_causality = 0.0
  let mut causal_pair = ("", "", "", "")
  
  let mut i = 0
  while i < causal_relationships.length() {
    let (source_service, source_event, target_service, target_event, strength) = causal_relationships[i]
    if strength > strongest_causality {
      strongest_causality = strength
      causal_pair = (source_service, source_event, target_service, target_event)
    }
    i = i + 1
  }
  
  assert_eq(strongest_causality > 0.0, true)
  assert_eq(causal_pair.0 != "", true)
}

test "telemetry_pattern_correlation_discovery" {
  // 测试模式关联发现
  
  let system_patterns = [
    ("morning_peak", [6, 7, 8, 9], "high_traffic"),
    ("business_hours", [9, 10, 11, 14, 15, 16], "moderate_traffic"),
    ("evening_peak", [17, 18, 19, 20], "high_traffic"),
    ("night_low", [21, 22, 23, 0, 1, 2, 3, 4, 5], "low_traffic")
  ]
  
  let performance_patterns = [
    ("high_cpu", [6, 7, 8, 9, 17, 18, 19, 20], "cpu_intensive"),
    ("high_memory", [9, 10, 11, 14, 15, 16], "memory_intensive"),
    ("network_spike", [12, 13, 17, 18], "network_intensive"),
    ("disk_io_peak", [2, 3, 4, 22, 23], "disk_intensive")
  ]
  
  // 计算模式重叠度
  let mut pattern_correlations = []
  
  let mut i = 0
  while i < system_patterns.length() {
    let (system_name, system_hours, system_type) = system_patterns[i]
    
    let mut j = 0
    while j < performance_patterns.length() {
      let (perf_name, perf_hours, perf_type) = performance_patterns[j]
      
      // 计算时间重叠度
      let mut overlap_count = 0
      let mut k = 0
      while k < system_hours.length() {
        if perf_hours.contains(system_hours[k]) {
          overlap_count = overlap_count + 1
        }
        k = k + 1
      }
      
      let total_hours = system_hours.length()
      let overlap_ratio = overlap_count.to_double() / total_hours.to_double()
      
      // 计算关联强度
      let correlation_strength = overlap_ratio * 100.0
      
      if correlation_strength > 20.0 {  // 只记录强关联
        pattern_correlations.push((system_name, perf_name, correlation_strength, overlap_count))
      }
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证模式关联发现结果
  assert_eq(pattern_correlations.length() > 0, true)
  
  // 验证高峰时段的关联
  let mut morning_high_cpu = false
  let mut evening_high_cpu = false
  
  let mut i = 0
  while i < pattern_correlations.length() {
    let (system_pattern, perf_pattern, strength, _) = pattern_correlations[i]
    
    if system_pattern == "morning_peak" && perf_pattern == "high_cpu" {
      morning_high_cpu = true
    } else if system_pattern == "evening_peak" && perf_pattern == "high_cpu" {
      evening_high_cpu = true
    }
    
    i = i + 1
  }
  
  assert_eq(morning_high_cpu, true)
  assert_eq(evening_high_cpu, true)
  
  // 找到最强的模式关联
  let mut strongest_correlation = 0.0
  let mut strongest_pair = ("", "")
  
  let mut i = 0
  while i < pattern_correlations.length() {
    let (system_pattern, perf_pattern, strength, _) = pattern_correlations[i]
    if strength > strongest_correlation {
      strongest_correlation = strength
      strongest_pair = (system_pattern, perf_pattern)
    }
    i = i + 1
  }
  
  assert_eq(strongest_correlation > 50.0, true)  // 最强关联应该超过50%
  assert_eq(strongest_pair.0 != "", true)
  assert_eq(strongest_pair.1 != "", true)
}