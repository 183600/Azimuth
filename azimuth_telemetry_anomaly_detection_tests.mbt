// Azimuth 遥测系统异常检测测试
// 专注于遥测数据中的异常检测和警报机制

// 测试1: 基于阈值的异常检测
test "基于阈值的异常检测" {
  // 模拟遥测数据，包含正常值和异常值
  let telemetry_data = [
    { timestamp: 1640995200, metric: "cpu", value: 45.0, service: "auth" },
    { timestamp: 1640995260, metric: "cpu", value: 50.0, service: "auth" },
    { timestamp: 1640995320, metric: "cpu", value: 48.0, service: "auth" },
    { timestamp: 1640995380, metric: "cpu", value: 95.0, service: "auth" }, // 异常值
    { timestamp: 1640995440, metric: "cpu", value: 52.0, service: "auth" },
    { timestamp: 1640995500, metric: "memory", value: 1024.0, service: "auth" },
    { timestamp: 1640995560, metric: "memory", value: 1050.0, service: "auth" },
    { timestamp: 1640995620, metric: "memory", value: 7800.0, service: "auth" }, // 异常值
    { timestamp: 1640995680, metric: "memory", value: 1080.0, service: "auth" },
    { timestamp: 1640995740, metric: "disk", value: 2048.0, service: "auth" },
    { timestamp: 1640995800, metric: "disk", value: 2100.0, service: "auth" },
    { timestamp: 1640995860, metric: "disk", value: 2150.0, service: "auth" }
  ]
  
  // 定义阈值规则
  let threshold_rules = {
    "cpu": { warning: 70.0, critical: 90.0 },
    "memory": { warning: 6000.0, critical: 7500.0 },
    "disk": { warning: 8000.0, critical: 9500.0 }
  }
  
  // 基于阈值的异常检测函数
  let threshold_anomaly_detection = fn(data, rules) {
    let mut anomalies = []
    
    for point in data {
      let rule = rules[point.metric]
      match rule {
        Some(threshold) => {
          let severity = 
            if point.value >= threshold.critical { "critical" }
            else if point.value >= threshold.warning { "warning" }
            else { "normal" }
          
          if severity != "normal" {
            anomalies = anomalies.push({
              timestamp: point.timestamp,
              metric: point.metric,
              value: point.value,
              service: point.service,
              severity: severity,
              threshold: if severity == "critical" { threshold.critical } else { threshold.warning }
            })
          }
        }
        None => ()
      }
    }
    
    anomalies
  }
  
  // 执行异常检测
  let anomalies = threshold_anomaly_detection(telemetry_data, threshold_rules)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 2)
  
  // 验证CPU异常
  let cpu_anomaly = anomalies[0]
  assert_eq(cpu_anomaly.metric, "cpu")
  assert_eq(cpu_anomaly.value, 95.0)
  assert_eq(cpu_anomaly.service, "auth")
  assert_eq(cpu_anomaly.severity, "critical")
  assert_eq(cpu_anomaly.threshold, 90.0)
  
  // 验证内存异常
  let memory_anomaly = anomalies[1]
  assert_eq(memory_anomaly.metric, "memory")
  assert_eq(memory_anomaly.value, 7800.0)
  assert_eq(memory_anomaly.service, "auth")
  assert_eq(memory_anomaly.severity, "critical")
  assert_eq(memory_anomaly.threshold, 7500.0)
}

// 测试2: 基于统计的异常检测
test "基于统计的异常检测" {
  // 模拟遥测数据，包含正常值和统计异常值
  let telemetry_data = [
    { timestamp: 1640995200, metric: "response_time", value: 100.0 },
    { timestamp: 1640995260, metric: "response_time", value: 120.0 },
    { timestamp: 1640995320, metric: "response_time", value: 110.0 },
    { timestamp: 1640995380, metric: "response_time", value: 105.0 },
    { timestamp: 1640995440, metric: "response_time", value: 115.0 },
    { timestamp: 1640995500, metric: "response_time", value: 500.0 }, // 异常值
    { timestamp: 1640995560, metric: "response_time", value: 108.0 },
    { timestamp: 1640995620, metric: "response_time", value: 95.0 },
    { timestamp: 1640995680, metric: "response_time", value: 125.0 },
    { timestamp: 1640995740, metric: "response_time", value: 112.0 }
  ]
  
  // 计算均值和标准差
  let calculate_statistics = fn(data) {
    let n = data.length().to_float()
    let mut sum = 0.0
    for point in data {
      sum = sum + point.value
    }
    let mean = sum / n
    
    let mut sum_squared_diff = 0.0
    for point in data {
      let diff = point.value - mean
      sum_squared_diff = sum_squared_diff + diff * diff
    }
    let variance = sum_squared_diff / n
    let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
    
    { mean: mean, std_dev: std_dev }
  }
  
  // 基于统计的异常检测函数
  let statistical_anomaly_detection = fn(data, threshold) {
    let stats = calculate_statistics(data)
    let mut anomalies = []
    
    for point in data {
      let z_score = if stats.std_dev > 0.0 { (point.value - stats.mean) / stats.std_dev } else { 0.0 }
      
      if z_score.abs() > threshold {
        anomalies = anomalies.push({
          timestamp: point.timestamp,
          metric: point.metric,
          value: point.value,
          z_score: z_score,
          mean: stats.mean,
          std_dev: stats.std_dev
        })
      }
    }
    
    anomalies
  }
  
  // 执行异常检测（使用2个标准差作为阈值）
  let anomalies = statistical_anomaly_detection(telemetry_data, 2.0)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 1)
  
  // 验证异常点
  let anomaly = anomalies[0]
  assert_eq(anomaly.timestamp, 1640995500)
  assert_eq(anomaly.metric, "response_time")
  assert_eq(anomaly.value, 500.0)
  assert_true(anomaly.z_score > 2.0)
  assert_true(anomaly.mean >= 100.0 && anomaly.mean <= 120.0)
  assert_true(anomaly.std_dev > 0.0)
}

// 测试3: 基于机器学习的异常检测
test "基于机器学习的异常检测" {
  // 模拟训练数据（正常模式）
  let training_data = [
    { cpu: 45.0, memory: 1024.0, disk: 2048.0, network: 100.0 },
    { cpu: 50.0, memory: 1050.0, disk: 2100.0, network: 120.0 },
    { cpu: 48.0, memory: 1080.0, disk: 2150.0, network: 110.0 },
    { cpu: 52.0, memory: 1100.0, disk: 2200.0, network: 115.0 },
    { cpu: 47.0, memory: 1070.0, disk: 2125.0, network: 105.0 }
  ]
  
  // 模拟测试数据（包含异常）
  let test_data = [
    { cpu: 45.0, memory: 1024.0, disk: 2048.0, network: 100.0, timestamp: 1640995200 }, // 正常
    { cpu: 50.0, memory: 1050.0, disk: 2100.0, network: 120.0, timestamp: 1640995260 }, // 正常
    { cpu: 95.0, memory: 1024.0, disk: 2048.0, network: 100.0, timestamp: 1640995320 }, // 异常（CPU高）
    { cpu: 48.0, memory: 7800.0, disk: 2048.0, network: 100.0, timestamp: 1640995380 }, // 异常（内存高）
    { cpu: 47.0, memory: 1070.0, disk: 2125.0, network: 105.0, timestamp: 1640995440 }  // 正常
  ]
  
  // 计算训练数据的统计特征
  let calculate_feature_stats = fn(data) {
    let mut cpu_sum = 0.0, memory_sum = 0.0, disk_sum = 0.0, network_sum = 0.0
    let n = data.length().to_float()
    
    for point in data {
      cpu_sum = cpu_sum + point.cpu
      memory_sum = memory_sum + point.memory
      disk_sum = disk_sum + point.disk
      network_sum = network_sum + point.network
    }
    
    let cpu_mean = cpu_sum / n
    let memory_mean = memory_sum / n
    let disk_mean = disk_sum / n
    let network_mean = network_sum / n
    
    // 计算标准差
    let mut cpu_var_sum = 0.0, memory_var_sum = 0.0, disk_var_sum = 0.0, network_var_sum = 0.0
    
    for point in data {
      cpu_var_sum = cpu_var_sum + (point.cpu - cpu_mean) * (point.cpu - cpu_mean)
      memory_var_sum = memory_var_sum + (point.memory - memory_mean) * (point.memory - memory_mean)
      disk_var_sum = disk_var_sum + (point.disk - disk_mean) * (point.disk - disk_mean)
      network_var_sum = network_var_sum + (point.network - network_mean) * (point.network - network_mean)
    }
    
    let cpu_std = (cpu_var_sum / n).sqrt()
    let memory_std = (memory_var_sum / n).sqrt()
    let disk_std = (disk_var_sum / n).sqrt()
    let network_std = (network_var_sum / n).sqrt()
    
    {
      cpu: { mean: cpu_mean, std: cpu_std },
      memory: { mean: memory_mean, std: memory_std },
      disk: { mean: disk_mean, std: disk_std },
      network: { mean: network_mean, std: network_std }
    }
  }
  
  // 基于机器学习的异常检测函数（简化版）
  let ml_anomaly_detection = fn(test_data, feature_stats, threshold) {
    let mut anomalies = []
    
    for point in test_data {
      // 计算每个特征的Z分数
      let cpu_z = if feature_stats.cpu.std > 0.0 { (point.cpu - feature_stats.cpu.mean) / feature_stats.cpu.std } else { 0.0 }
      let memory_z = if feature_stats.memory.std > 0.0 { (point.memory - feature_stats.memory.mean) / feature_stats.memory.std } else { 0.0 }
      let disk_z = if feature_stats.disk.std > 0.0 { (point.disk - feature_stats.disk.mean) / feature_stats.disk.std } else { 0.0 }
      let network_z = if feature_stats.network.std > 0.0 { (point.network - feature_stats.network.mean) / feature_stats.network.std } else { 0.0 }
      
      // 计算综合异常分数（欧几里得距离）
      let anomaly_score = (cpu_z * cpu_z + memory_z * memory_z + disk_z * disk_z + network_z * network_z).sqrt()
      
      if anomaly_score > threshold {
        anomalies = anomalies.push({
          timestamp: point.timestamp,
          anomaly_score: anomaly_score,
          features: {
            cpu: { value: point.cpu, z_score: cpu_z },
            memory: { value: point.memory, z_score: memory_z },
            disk: { value: point.disk, z_score: disk_z },
            network: { value: point.network, z_score: network_z }
          }
        })
      }
    }
    
    anomalies
  }
  
  // 计算训练数据的特征统计
  let feature_stats = calculate_feature_stats(training_data)
  
  // 执行异常检测
  let anomalies = ml_anomaly_detection(test_data, feature_stats, 2.0)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 2)
  
  // 验证第一个异常（CPU高）
  let anomaly1 = anomalies[0]
  assert_eq(anomaly1.timestamp, 1640995320)
  assert_true(anomaly1.anomaly_score > 2.0)
  assert_true(anomaly1.features.cpu.z_score > 2.0) // CPU Z分数高
  assert_eq(anomaly1.features.cpu.value, 95.0)
  
  // 验证第二个异常（内存高）
  let anomaly2 = anomalies[1]
  assert_eq(anomaly2.timestamp, 1640995380)
  assert_true(anomaly2.anomaly_score > 2.0)
  assert_true(anomaly2.features.memory.z_score > 2.0) // 内存Z分数高
  assert_eq(anomaly2.features.memory.value, 7800.0)
}

// 测试4: 序列异常检测
test "序列异常检测" {
  // 模拟时间序列遥测数据
  let time_series_data = [
    { timestamp: 1640995200, value: 10.0 },
    { timestamp: 1640995260, value: 12.0 },
    { timestamp: 1640995320, value: 11.0 },
    { timestamp: 1640995380, value: 13.0 },
    { timestamp: 1640995440, value: 15.0 },
    { timestamp: 1640995500, value: 50.0 }, // 异常点
    { timestamp: 1640995560, value: 14.0 },
    { timestamp: 1640995620, value: 16.0 },
    { timestamp: 1640995680, value: 18.0 },
    { timestamp: 1640995740, value: 17.0 }
  ]
  
  // 序列异常检测函数（基于局部离群因子）
  let sequence_anomaly_detection = fn(data, window_size, threshold) {
    let mut anomalies = []
    
    let mut i = window_size
    while i < data.length() - window_size {
      // 获取当前窗口
      let current_window = data.slice(i - window_size, i + window_size + 1)
      
      // 计算当前点与周围点的平均距离
      let current_point = data[i]
      let mut sum_distances = 0.0
      let mut count = 0
      
      for j in range(0, current_window.length()) {
        if j != window_size { // 排除当前点本身
          let distance = (current_point.value - current_window[j].value).abs()
          sum_distances = sum_distances + distance
          count = count + 1
        }
      }
      
      let avg_distance = if count > 0 { sum_distances / count.to_float() } else { 0.0 }
      
      // 计算局部密度
      let local_density = 1.0 / (avg_distance + 0.001) // 避免除以零
      
      // 计算周围点的局部密度
      let mut neighbor_densities = []
      for j in range(0, current_window.length()) {
        if j != window_size {
          let neighbor_point = current_window[j]
          let mut sum_neighbor_distances = 0.0
          let mut neighbor_count = 0
          
          for k in range(0, current_window.length()) {
            if k != j {
              let distance = (neighbor_point.value - current_window[k].value).abs()
              sum_neighbor_distances = sum_neighbor_distances + distance
              neighbor_count = neighbor_count + 1
            }
          }
          
          let neighbor_avg_distance = if neighbor_count > 0 { sum_neighbor_distances / neighbor_count.to_float() } else { 0.0 }
          let neighbor_density = 1.0 / (neighbor_avg_distance + 0.001)
          neighbor_densities = neighbor_densities.push(neighbor_density)
        }
      }
      
      // 计算平均邻居密度
      let mut sum_neighbor_densities = 0.0
      for density in neighbor_densities {
        sum_neighbor_densities = sum_neighbor_densities + density
      }
      let avg_neighbor_density = if neighbor_densities.length() > 0 { sum_neighbor_densities / neighbor_densities.length().to_float() } else { 0.0 }
      
      // 计算局部离群因子
      let lof = if avg_neighbor_density > 0.0 { avg_neighbor_density / local_density } else { 1.0 }
      
      // 检测异常
      if lof > threshold {
        anomalies = anomalies.push({
          timestamp: current_point.timestamp,
          value: current_point.value,
          lof: lof,
          local_density: local_density,
          avg_neighbor_density: avg_neighbor_density
        })
      }
      
      i = i + 1
    }
    
    anomalies
  }
  
  // 执行序列异常检测
  let anomalies = sequence_anomaly_detection(time_series_data, 2, 2.0)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 1)
  
  // 验证异常点
  let anomaly = anomalies[0]
  assert_eq(anomaly.timestamp, 1640995500)
  assert_eq(anomaly.value, 50.0)
  assert_true(anomaly.lof > 2.0)
  assert_true(anomaly.local_density < anomaly.avg_neighbor_density)
}

// 测试5: 多维关联异常检测
test "多维关联异常检测" {
  // 模拟多维遥测数据
  let multi_dimension_data = [
    { timestamp: 1640995200, cpu: 45.0, memory: 1024.0, requests: 100 },
    { timestamp: 1640995260, cpu: 50.0, memory: 1050.0, requests: 120 },
    { timestamp: 1640995320, cpu: 48.0, memory: 1080.0, requests: 110 },
    { timestamp: 1640995380, cpu: 52.0, memory: 1100.0, requests: 115 },
    { timestamp: 1640995440, cpu: 95.0, memory: 1024.0, requests: 105 }, // 异常：CPU高但请求和内存正常
    { timestamp: 1640995500, cpu: 47.0, memory: 1070.0, requests: 112 },
    { timestamp: 1640995560, cpu: 48.0, memory: 7800.0, requests: 118 }, // 异常：内存高但CPU和请求正常
    { timestamp: 1640995620, cpu: 50.0, memory: 1090.0, requests: 125 }
  ]
  
  // 计算特征之间的相关性
  let calculate_correlation = fn(data) {
    let n = data.length().to_float()
    
    // 计算均值
    let mut cpu_sum = 0.0, memory_sum = 0.0, requests_sum = 0.0
    for point in data {
      cpu_sum = cpu_sum + point.cpu
      memory_sum = memory_sum + point.memory
      requests_sum = requests_sum + point.requests.to_float()
    }
    
    let cpu_mean = cpu_sum / n
    let memory_mean = memory_sum / n
    let requests_mean = requests_sum / n
    
    // 计算协方差
    let mut cpu_memory_cov = 0.0, cpu_requests_cov = 0.0, memory_requests_cov = 0.0
    let mut cpu_var = 0.0, memory_var = 0.0, requests_var = 0.0
    
    for point in data {
      let cpu_diff = point.cpu - cpu_mean
      let memory_diff = point.memory - memory_mean
      let requests_diff = point.requests.to_float() - requests_mean
      
      cpu_memory_cov = cpu_memory_cov + cpu_diff * memory_diff
      cpu_requests_cov = cpu_requests_cov + cpu_diff * requests_diff
      memory_requests_cov = memory_requests_cov + memory_diff * requests_diff
      
      cpu_var = cpu_var + cpu_diff * cpu_diff
      memory_var = memory_var + memory_diff * memory_diff
      requests_var = requests_var + requests_diff * requests_diff
    }
    
    cpu_memory_cov = cpu_memory_cov / n
    cpu_requests_cov = cpu_requests_cov / n
    memory_requests_cov = memory_requests_cov / n
    
    cpu_var = cpu_var / n
    memory_var = memory_var / n
    requests_var = requests_var / n
    
    // 计算相关系数
    let cpu_memory_corr = if cpu_var > 0.0 && memory_var > 0.0 { cpu_memory_cov / (cpu_var.sqrt() * memory_var.sqrt()) } else { 0.0 }
    let cpu_requests_corr = if cpu_var > 0.0 && requests_var > 0.0 { cpu_requests_cov / (cpu_var.sqrt() * requests_var.sqrt()) } else { 0.0 }
    let memory_requests_corr = if memory_var > 0.0 && requests_var > 0.0 { memory_requests_cov / (memory_var.sqrt() * requests_var.sqrt()) } else { 0.0 }
    
    {
      cpu_memory: cpu_memory_corr,
      cpu_requests: cpu_requests_corr,
      memory_requests: memory_requests_corr,
      means: { cpu: cpu_mean, memory: memory_mean, requests: requests_mean },
      stds: { 
        cpu: if cpu_var > 0.0 { cpu_var.sqrt() } else { 0.0 },
        memory: if memory_var > 0.0 { memory_var.sqrt() } else { 0.0 },
        requests: if requests_var > 0.0 { requests_var.sqrt() } else { 0.0 }
      }
    }
  }
  
  // 多维关联异常检测函数
  let multi_dimension_anomaly_detection = fn(data, threshold) {
    let correlation = calculate_correlation(data)
    let mut anomalies = []
    
    for point in data {
      // 计算每个特征的Z分数
      let cpu_z = if correlation.stds.cpu > 0.0 { (point.cpu - correlation.means.cpu) / correlation.stds.cpu } else { 0.0 }
      let memory_z = if correlation.stds.memory > 0.0 { (point.memory - correlation.means.memory) / correlation.stds.memory } else { 0.0 }
      let requests_z = if correlation.stds.requests > 0.0 { (point.requests.to_float() - correlation.means.requests) / correlation.stds.requests } else { 0.0 }
      
      // 计算预期的关联关系
      let expected_memory = correlation.means.memory + correlation.cpu_memory * correlation.stds.memory * cpu_z
      let expected_requests = correlation.means.requests + correlation.cpu_requests * correlation.stds.requests * cpu_z
      
      // 计算关联偏差
      let memory_deviation = if correlation.stds.memory > 0.0 { (point.memory - expected_memory) / correlation.stds.memory } else { 0.0 }
      let requests_deviation = if correlation.stds.requests > 0.0 { (point.requests.to_float() - expected_requests) / correlation.stds.requests } else { 0.0 }
      
      // 计算综合异常分数
      let anomaly_score = (cpu_z * cpu_z + memory_deviation * memory_deviation + requests_deviation * requests_deviation).sqrt()
      
      if anomaly_score > threshold {
        anomalies = anomalies.push({
          timestamp: point.timestamp,
          anomaly_score: anomaly_score,
          features: {
            cpu: { value: point.cpu, z_score: cpu_z },
            memory: { value: point.memory, z_score: memory_z, deviation: memory_deviation },
            requests: { value: point.requests, z_score: requests_z, deviation: requests_deviation }
          }
        })
      }
    }
    
    anomalies
  }
  
  // 执行多维关联异常检测
  let anomalies = multi_dimension_anomaly_detection(multi_dimension_data, 2.0)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 2)
  
  // 验证第一个异常（CPU高但请求和内存正常）
  let anomaly1 = anomalies[0]
  assert_eq(anomaly1.timestamp, 1640995440)
  assert_true(anomaly1.anomaly_score > 2.0)
  assert_true(anomaly1.features.cpu.z_score > 2.0) // CPU Z分数高
  assert_eq(anomaly1.features.cpu.value, 95.0)
  
  // 验证第二个异常（内存高但CPU和请求正常）
  let anomaly2 = anomalies[1]
  assert_eq(anomaly2.timestamp, 1640995560)
  assert_true(anomaly2.anomaly_score > 2.0)
  assert_true(anomaly2.features.memory.z_score > 2.0) // 内存Z分数高
  assert_eq(anomaly2.features.memory.value, 7800.0)
}

// 测试6: 异常警报生成
test "异常警报生成" {
  // 模拟检测到的异常
  let detected_anomalies = [
    {
      timestamp: 1640995200,
      metric: "cpu",
      value: 95.0,
      service: "auth",
      severity: "critical",
      threshold: 90.0
    },
    {
      timestamp: 1640995260,
      metric: "memory",
      value: 7800.0,
      service: "auth",
      severity: "critical",
      threshold: 7500.0
    },
    {
      timestamp: 1640995320,
      metric: "response_time",
      value: 500.0,
      service: "api",
      severity: "warning",
      threshold: 300.0
    }
  ]
  
  // 警报模板
  let alert_templates = {
    "critical": {
      title: "Critical Alert",
      priority: "high",
      action: "immediate"
    },
    "warning": {
      title: "Warning Alert",
      priority: "medium",
      action: "investigate"
    }
  }
  
  // 生成警报函数
  let generate_alerts = fn(anomalies, templates) {
    let mut alerts = []
    
    for anomaly in anomalies {
      let template = templates[anomaly.severity]
      match template {
        Some(t) => {
          alerts = alerts.push({
            id: "alert-" + anomaly.timestamp.to_string(),
            title: t.title,
            priority: t.priority,
            action: t.action,
            timestamp: anomaly.timestamp,
            service: anomaly.service,
            metric: anomaly.metric,
            value: anomaly.value,
            threshold: anomaly.threshold,
            message: anomaly.service + " service " + anomaly.metric + " is " + anomaly.value.to_string() + ", exceeding threshold of " + anomaly.threshold.to_string()
          })
        }
        None => ()
      }
    }
    
    alerts
  }
  
  // 执行警报生成
  let alerts = generate_alerts(detected_anomalies, alert_templates)
  
  // 验证警报生成结果
  assert_eq(alerts.length(), 3)
  
  // 验证第一个严重警报
  let critical_alert1 = alerts[0]
  assert_eq(critical_alert1.title, "Critical Alert")
  assert_eq(critical_alert1.priority, "high")
  assert_eq(critical_alert1.action, "immediate")
  assert_eq(critical_alert1.service, "auth")
  assert_eq(critical_alert1.metric, "cpu")
  assert_eq(critical_alert1.value, 95.0)
  assert_eq(critical_alert1.threshold, 90.0)
  
  // 验证第二个严重警报
  let critical_alert2 = alerts[1]
  assert_eq(critical_alert2.title, "Critical Alert")
  assert_eq(critical_alert2.priority, "high")
  assert_eq(critical_alert2.action, "immediate")
  assert_eq(critical_alert2.service, "auth")
  assert_eq(critical_alert2.metric, "memory")
  assert_eq(critical_alert2.value, 7800.0)
  assert_eq(critical_alert2.threshold, 7500.0)
  
  // 验证警告警报
  let warning_alert = alerts[2]
  assert_eq(warning_alert.title, "Warning Alert")
  assert_eq(warning_alert.priority, "medium")
  assert_eq(warning_alert.action, "investigate")
  assert_eq(warning_alert.service, "api")
  assert_eq(warning_alert.metric, "response_time")
  assert_eq(warning_alert.value, 500.0)
  assert_eq(warning_alert.threshold, 300.0)
  
  // 按优先级分组警报
  let mut high_priority_alerts = []
  let mut medium_priority_alerts = []
  
  for alert in alerts {
    if alert.priority == "high" {
      high_priority_alerts = high_priority_alerts.push(alert)
    } else if alert.priority == "medium" {
      medium_priority_alerts = medium_priority_alerts.push(alert)
    }
  }
  
  // 验证优先级分组
  assert_eq(high_priority_alerts.length(), 2)
  assert_eq(medium_priority_alerts.length(), 1)
}