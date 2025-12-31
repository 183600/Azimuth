// 性能监控测试用例
// 测试遥测系统的性能指标收集、分析和报告功能

test "performance_metrics_collection" {
  // 测试性能指标收集功能
  
  let performance_metrics = [
    ("cpu_utilization", 65.5, "percent"),
    ("memory_usage", 1024.0, "MB"),
    ("disk_io_rate", 50.2, "MB/s"),
    ("network_latency", 25.3, "ms"),
    ("throughput", 1000.0, "req/s")
  ]
  
  // 验证指标数量
  assert_eq(performance_metrics.length(), 5)
  
  // 验证每个指标的结构和值范围
  let mut i = 0
  while i < performance_metrics.length() {
    let name = performance_metrics[i].0
    let value = performance_metrics[i].1
    let unit = performance_metrics[i].2
    
    // 验证指标名称
    assert_eq(name.length() > 0, true)
    assert_eq(name.contains("_"), true)
    
    // 验证指标值为非负数
    assert_eq(value >= 0.0, true)
    
    // 验证单位
    assert_eq(unit.length() > 0, true)
    
    i = i + 1
  }
  
  // 验证特定指标的合理范围
  assert_eq(performance_metrics[0].1 >= 0.0, true) // CPU利用率
  assert_eq(performance_metrics[0].1 <= 100.0, true)
  
  assert_eq(performance_metrics[3].1 >= 0.0, true) // 网络延迟
  assert_eq(performance_metrics[3].1 <= 1000.0, true)
}

test "performance_threshold_monitoring" {
  // 测试性能阈值监控功能
  
  let thresholds = [
    ("cpu_utilization", 80.0, "percent"),
    ("memory_usage", 2048.0, "MB"),
    ("disk_io_rate", 100.0, "MB/s"),
    ("network_latency", 100.0, "ms")
  ]
  
  let current_values = [
    ("cpu_utilization", 85.0, "percent"),
    ("memory_usage", 1536.0, "MB"),
    ("disk_io_rate", 120.0, "MB/s"),
    ("network_latency", 45.0, "ms")
  ]
  
  // 检查哪些指标超过了阈值
  let mut alerts = []
  let mut i = 0
  while i < thresholds.length() {
    let threshold_name = thresholds[i].0
    let threshold_value = thresholds[i].1
    let threshold_unit = thresholds[i].2
    
    // 查找对应的当前值
    let mut j = 0
    while j < current_values.length() {
      if current_values[j].0 == threshold_name {
        let current_value = current_values[j].1
        
        // 如果当前值超过阈值，生成告警
        if current_value > threshold_value {
          alerts.push((threshold_name, current_value, threshold_value))
        }
        
        break
      }
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证告警结果
  assert_eq(alerts.length(), 2) // CPU和磁盘IO超过阈值
  
  assert_eq(alerts[0].0, "cpu_utilization")
  assert_eq(alerts[0].1, 85.0)
  assert_eq(alerts[0].2, 80.0)
  
  assert_eq(alerts[1].0, "disk_io_rate")
  assert_eq(alerts[1].1, 120.0)
  assert_eq(alerts[1].2, 100.0)
}

test "performance_trend_analysis" {
  // 测试性能趋势分析功能
  
  let time_series_data = [
    (1000L, 45.0), // (timestamp, cpu_usage)
    (2000L, 52.0),
    (3000L, 58.0),
    (4000L, 65.0),
    (5000L, 72.0)
  ]
  
  // 计算趋势（简单线性趋势）
  let first_value = time_series_data[0].1
  let last_value = time_series_data[time_series_data.length() - 1].1
  let trend = last_value - first_value
  
  // 验证趋势计算
  assert_eq(trend > 0.0, true) // 上升趋势
  assert_eq(trend, 27.0) // 从45%增长到72%
  
  // 计算平均增长率
  let growth_rate = trend / first_value * 100.0
  assert_eq(growth_rate > 50.0, true) // 增长超过50%
  assert_eq(growth_rate < 70.0, true)
  
  // 计算移动平均
  let mut moving_averages = []
  let window_size = 3
  
  let mut i = 0
  while i <= time_series_data.length() - window_size {
    let mut sum = 0.0
    let mut j = 0
    while j < window_size {
      sum = sum + time_series_data[i + j].1
      j = j + 1
    }
    let avg = sum / window_size.to_double()
    moving_averages.push(avg)
    i = i + 1
  }
  
  // 验证移动平均计算
  assert_eq(moving_averages.length(), 3) // 5个数据点，窗口大小3，产生3个移动平均
  
  assert_eq(moving_averages[0] > 50.0, true) // (45+52+58)/3 = 51.67
  assert_eq(moving_averages[1] > 55.0, true) // (52+58+65)/3 = 58.33
  assert_eq(moving_averages[2] > 60.0, true) // (58+65+72)/3 = 65.0
}

test "performance_benchmarking" {
  // 测试性能基准测试功能
  
  let benchmark_results = [
    ("operation_a", 100.5, "ms"),
    ("operation_b", 250.3, "ms"),
    ("operation_c", 75.8, "ms"),
    ("operation_d", 150.2, "ms")
  ]
  
  let baseline_results = [
    ("operation_a", 120.0, "ms"),
    ("operation_b", 200.0, "ms"),
    ("operation_c", 80.0, "ms"),
    ("operation_d", 140.0, "ms")
  ]
  
  // 比较基准测试结果与基线
  let mut performance_changes = []
  let mut i = 0
  while i < benchmark_results.length() {
    let operation_name = benchmark_results[i].0
    let current_time = benchmark_results[i].1
    
    // 查找对应的基线时间
    let mut j = 0
    while j < baseline_results.length() {
      if baseline_results[j].0 == operation_name {
        let baseline_time = baseline_results[j].1
        let change_percent = (current_time - baseline_time) / baseline_time * 100.0
        performance_changes.push((operation_name, change_percent))
        break
      }
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证性能变化
  assert_eq(performance_changes.length(), 4)
  
  // operation_a: (100.5-120.0)/120.0 = -16.25% (性能提升)
  assert_eq(performance_changes[0].1 < 0.0, true)
  assert_eq(performance_changes[0].1 > -20.0, true)
  
  // operation_b: (250.3-200.0)/200.0 = 25.15% (性能下降)
  assert_eq(performance_changes[1].1 > 0.0, true)
  assert_eq(performance_changes[1].1 < 30.0, true)
  
  // operation_c: (75.8-80.0)/80.0 = -5.25% (轻微性能提升)
  assert_eq(performance_changes[2].1 < 0.0, true)
  assert_eq(performance_changes[2].1 > -10.0, true)
  
  // operation_d: (150.2-140.0)/140.0 = 7.29% (轻微性能下降)
  assert_eq(performance_changes[3].1 > 0.0, true)
  assert_eq(performance_changes[3].1 < 10.0, true)
}

test "performance_resource_utilization" {
  // 测试性能资源利用率监控
  
  let resource_metrics = [
    ("cpu_cores", 8, "cores"),
    ("cpu_used", 6, "cores"),
    ("memory_total", 16384.0, "MB"),
    ("memory_used", 8192.0, "MB"),
    ("disk_total", 1024000.0, "MB"),
    ("disk_used", 512000.0, "MB")
  ]
  
  // 计算资源利用率
  let mut utilization_rates = []
  
  // CPU利用率
  let cpu_total = resource_metrics[0].1.to_double()
  let cpu_used = resource_metrics[1].1.to_double()
  let cpu_utilization = cpu_used / cpu_total * 100.0
  utilization_rates.push(("cpu", cpu_utilization))
  
  // 内存利用率
  let memory_total = resource_metrics[2].1
  let memory_used = resource_metrics[3].1
  let memory_utilization = memory_used / memory_total * 100.0
  utilization_rates.push(("memory", memory_utilization))
  
  // 磁盘利用率
  let disk_total = resource_metrics[4].1
  let disk_used = resource_metrics[5].1
  let disk_utilization = disk_used / disk_total * 100.0
  utilization_rates.push(("disk", disk_utilization))
  
  // 验证利用率计算
  assert_eq(utilization_rates.length(), 3)
  
  // CPU: 6/8 = 75%
  assert_eq(utilization_rates[0].0, "cpu")
  assert_eq(utilization_rates[0].1, 75.0)
  
  // 内存: 8192/16384 = 50%
  assert_eq(utilization_rates[1].0, "memory")
  assert_eq(utilization_rates[1].1, 50.0)
  
  // 磁盘: 512000/1024000 = 50%
  assert_eq(utilization_rates[2].0, "disk")
  assert_eq(utilization_rates[2].1, 50.0)
  
  // 检查资源警告阈值
  let warning_threshold = 80.0
  let mut warnings = []
  let mut i = 0
  while i < utilization_rates.length() {
    if utilization_rates[i].1 > warning_threshold {
      warnings.push(utilization_rates[i].0)
    }
    i = i + 1
  }
  
  // 验证警告（只有CPU超过80%）
  assert_eq(warnings.length(), 1)
  assert_eq(warnings[0], "cpu")
}