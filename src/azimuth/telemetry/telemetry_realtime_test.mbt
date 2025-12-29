// 遥测数据实时处理测试用例

test "telemetry_realtime_stream_processing" {
  // 测试遥测数据的实时流处理
  
  // 创建实时数据流
  let mut data_stream = []
  let base_timestamp = 1640995200L
  
  // 生成模拟的实时数据流
  let mut i = 0
  while i < 100 {
    let timestamp = base_timestamp + i.to_long()
    let metric_name = "cpu_usage"
    let metric_value = 50.0 + (i.to_double() * 0.5) // 逐渐增加
    let data_point = timestamp.to_string() + "|" + metric_name + "|" + metric_value.to_string()
    data_stream.push(data_point)
    i = i + 1
  }
  
  // 验证数据流创建
  assert_eq(data_stream.length(), 100)
  assert_eq(data_stream[0].contains("cpu_usage"), true)
  assert_eq(data_stream[99].contains("cpu_usage"), true)
  
  // 实时处理窗口（滑动窗口）
  let window_size = 10
  let mut processed_windows = []
  
  i = 0
  while i <= data_stream.length() - window_size {
    // 提取窗口数据
    let mut window_data = []
    let mut j = 0
    while j < window_size {
      window_data.push(data_stream[i + j])
      j = j + 1
    }
    
    // 计算窗口统计
    let mut sum = 0.0
    let mut count = 0
    j = 0
    while j < window_data.length() {
      let data_point = window_data[j]
      let parts = data_point.split("|")
      if parts.length() == 3 {
        sum = sum + parts[2].to_double()
        count = count + 1
      }
      j = j + 1
    }
    
    let average = sum / count.to_double()
    let window_start = i
    let window_end = i + window_size - 1
    
    processed_windows.push((window_start, window_end, average))
    i = i + 1
  }
  
  // 验证窗口处理结果
  assert_eq(processed_windows.length(), 91) // 100 - 10 + 1 = 91个窗口
  
  // 验证第一个窗口
  assert_eq(processed_windows[0].0, 0)
  assert_eq(processed_windows[0].1, 9)
  assert_eq(processed_windows[0].2 > 50.0, true)
  
  // 验证最后一个窗口
  assert_eq(processed_windows[90].0, 90)
  assert_eq(processed_windows[90].1, 99)
  assert_eq(processed_windows[90].2 > processed_windows[0].2, true) // 后面的窗口平均值应该更大
  
  // 验证窗口趋势
  assert_eq(processed_windows[45].2 > processed_windows[44].2, true) // 单调递增
}

test "telemetry_realtime_anomaly_detection" {
  // 测试实时异常检测
  
  // 创建包含异常的数据流
  let mut telemetry_data = []
  let base_timestamp = 1640995200L
  
  // 正常数据（CPU使用率在30-60之间）
  let mut i = 0
  while i < 50 {
    let timestamp = base_timestamp + i.to_long()
    let cpu_usage = 30.0 + (i.to_double() * 0.6) // 30-60范围
    let data_point = timestamp.to_string() + "|cpu|" + cpu_usage.to_string()
    telemetry_data.push(data_point)
    i = i + 1
  }
  
  // 插入异常数据（CPU使用率突然跳到95）
  let anomaly_timestamp = base_timestamp + 50L
  let anomaly_data = anomaly_timestamp.to_string() + "|cpu|95.5"
  telemetry_data.push(anomaly_data)
  
  // 恢复正常数据
  i = 51
  while i < 80 {
    let timestamp = base_timestamp + i.to_long()
    let cpu_usage = 35.0 + ((i - 50).to_double() * 0.5) // 恢复正常范围
    let data_point = timestamp.to_string() + "|cpu|" + cpu_usage.to_string()
    telemetry_data.push(data_point)
    i = i + 1
  }
  
  // 验证数据创建
  assert_eq(telemetry_data.length(), 81)
  
  // 实时异常检测算法
  let detect_anomaly = fn(current_values : Array[Double], threshold : Double) -> Bool {
    if current_values.length() < 3 {
      return false
    }
    
    // 计算移动平均
    let mut sum = 0.0
    let mut i = 0
    while i < current_values.length() - 1 { // 不包括当前值
      sum = sum + current_values[i]
      i = i + 1
    }
    let moving_average = sum / (current_values.length() - 1).to_double()
    
    // 计算当前值与移动平均的偏差
    let current_value = current_values[current_values.length() - 1]
    let deviation = (current_value - moving_average).abs()
    
    // 如果偏差超过阈值，认为是异常
    return deviation > threshold
  }
  
  // 执行实时异常检测
  let mut anomaly_indices = []
  let mut recent_values = []
  let history_size = 5
  
  i = 0
  while i < telemetry_data.length() {
    let data_point = telemetry_data[i]
    let parts = data_point.split("|")
    if parts.length() == 3 {
      let cpu_value = parts[2].to_double()
      
      // 添加到历史值
      recent_values.push(cpu_value)
      
      // 保持历史窗口大小
      if recent_values.length() > history_size {
        let mut new_recent = []
        let mut j = 1
        while j < recent_values.length() {
          new_recent.push(recent_values[j])
          j = j + 1
        }
        recent_values = new_recent
      }
      
      // 检测异常
      if detect_anomaly(recent_values, 20.0) { // 阈值20%
        anomaly_indices.push(i)
      }
    }
    i = i + 1
  }
  
  // 验证异常检测结果
  assert_eq(anomaly_indices.length(), 1) // 应该检测到1个异常
  assert_eq(anomaly_indices[0], 50) // 异常应该在索引50处
  
  // 验证异常数据点
  let anomaly_point = telemetry_data[50]
  assert_eq(anomaly_point.contains("95.5"), true)
}

test "telemetry_realtime_aggregation" {
  // 测试实时数据聚合
  
  // 创建多指标实时数据流
  let mut multi_metric_stream = []
  let base_timestamp = 1640995200L
  
  // 生成多种指标的实时数据
  let metrics = ["cpu", "memory", "disk", "network"]
  let mut i = 0
  while i < 60 { // 1分钟的数据，每秒一个数据点
    let timestamp = base_timestamp + i.to_long()
    let mut j = 0
    while j < metrics.length() {
      let metric_name = metrics[j]
      let base_value = if metric_name == "cpu" {
        50.0
      } else if metric_name == "memory" {
        60.0
      } else if metric_name == "disk" {
        40.0
      } else {
        30.0
      }
      
      let metric_value = base_value + (i.to_double() * 0.1) + (j.to_double() * 5.0)
      let data_point = timestamp.to_string() + "|" + metric_name + "|" + metric_value.to_string()
      multi_metric_stream.push(data_point)
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证数据流创建
  assert_eq(multi_metric_stream.length(), 240) // 60秒 * 4个指标
  
  // 实时聚合（按时间窗口和指标类型）
  let aggregation_window = 10 // 10秒窗口
  let mut aggregated_metrics = []
  
  i = 0
  while i < 60 {
    let window_start = base_timestamp + i.to_long()
    let window_end = window_start + aggregation_window.to_long()
    
    // 按指标类型聚合
    let mut j = 0
    while j < metrics.length() {
      let metric_name = metrics[j]
      let mut metric_values = []
      
      // 收集窗口内的指标值
      let mut k = 0
      while k < multi_metric_stream.length() {
        let data_point = multi_metric_stream[k]
        let parts = data_point.split("|")
        if parts.length() == 3 {
          let timestamp = parts[0].to_long()
          let metric = parts[1]
          let value = parts[2].to_double()
          
          if metric == metric_name and timestamp >= window_start and timestamp < window_end {
            metric_values.push(value)
          }
        }
        k = k + 1
      }
      
      // 计算聚合统计
      if metric_values.length() > 0 {
        let mut sum = 0.0
        let mut min_value = metric_values[0]
        let mut max_value = metric_values[0]
        let mut k = 0
        while k < metric_values.length() {
          sum = sum + metric_values[k]
          if metric_values[k] < min_value {
            min_value = metric_values[k]
          }
          if metric_values[k] > max_value {
            max_value = metric_values[k]
          }
          k = k + 1
        }
        
        let average = sum / metric_values.length().to_double()
        
        aggregated_metrics.push((window_start, metric_name, average, min_value, max_value))
      }
      j = j + 1
    }
    
    i = i + aggregation_window
  }
  
  // 验证聚合结果
  assert_eq(aggregated_metrics.length(), 24) // 6个窗口 * 4个指标
  
  // 验证第一个窗口的CPU聚合
  let first_cpu_agg = aggregated_metrics[0]
  assert_eq(first_cpu_agg.0, base_timestamp)
  assert_eq(first_cpu_agg.1, "cpu")
  assert_eq(first_cpu_agg.2 > 50.0, true) // 平均值应该大于基础值
  assert_eq(first_cpu_agg.3 <= first_cpu_agg.4, true) // min <= max
  
  // 验证最后一个窗口的内存聚合
  let last_memory_agg = aggregated_metrics[23]
  assert_eq(last_memory_agg.1, "memory")
  assert_eq(last_memory_agg.2 > 60.0, true) // 平均值应该大于基础值
}

test "telemetry_realtime_alerting" {
  // 测试实时告警系统
  
  // 创建触发告警的数据流
  let mut alert_stream = []
  let base_timestamp = 1640995200L
  
  // 正常数据
  let mut i = 0
  while i < 20 {
    let timestamp = base_timestamp + i.to_long()
    let error_rate = 0.01 // 1%错误率
    let response_time = 100.0 // 100ms响应时间
    let data_point = timestamp.to_string() + "|error_rate|" + error_rate.to_string() + "|response_time|" + response_time.to_string()
    alert_stream.push(data_point)
    i = i + 1
  }
  
  // 触发错误率告警的数据
  i = 20
  while i < 25 {
    let timestamp = base_timestamp + i.to_long()
    let error_rate = 0.15 // 15%错误率（超过阈值）
    let response_time = 120.0
    let data_point = timestamp.to_string() + "|error_rate|" + error_rate.to_string() + "|response_time|" + response_time.to_string()
    alert_stream.push(data_point)
    i = i + 1
  }
  
  // 触发响应时间告警的数据
  i = 25
  while i < 30 {
    let timestamp = base_timestamp + i.to_long()
    let error_rate = 0.02
    let response_time = 2500.0 // 2.5秒响应时间（超过阈值）
    let data_point = timestamp.to_string() + "|error_rate|" + error_rate.to_string() + "|response_time|" + response_time.to_string()
    alert_stream.push(data_point)
    i = i + 1
  }
  
  // 验证数据流创建
  assert_eq(alert_stream.length(), 30)
  
  // 告警规则定义
  let alert_rules = [
    ("high_error_rate", "error_rate", 0.1, "greater_than"),
    ("high_response_time", "response_time", 1000.0, "greater_than")
  ]
  
  // 实时告警检测
  let mut triggered_alerts = []
  
  i = 0
  while i < alert_stream.length() {
    let data_point = alert_stream[i]
    let parts = data_point.split("|")
    
    if parts.length() >= 5 {
      let timestamp = parts[0].to_long()
      let error_rate = parts[2].to_double()
      let response_time = parts[4].to_double()
      
      // 检查每个告警规则
      let mut j = 0
      while j < alert_rules.length() {
        let rule = alert_rules[j]
        let metric_value = if rule.1 == "error_rate" {
          error_rate
        } else if rule.1 == "response_time" {
          response_time
        } else {
          0.0
        }
        
        let triggered = if rule.3 == "greater_than" {
          metric_value > rule.2
        } else {
          metric_value < rule.2
        }
        
        if triggered {
          let alert_info = timestamp.to_string() + "|" + rule.0 + "|" + metric_value.to_string() + "|" + rule.2.to_string()
          triggered_alerts.push(alert_info)
        }
        j = j + 1
      }
    }
    i = i + 1
  }
  
  // 验证告警触发结果
  assert_eq(triggered_alerts.length(), 10) // 5个高错误率 + 5个高响应时间
  
  // 验证错误率告警
  let mut error_rate_alerts = 0
  let mut response_time_alerts = 0
  let mut i = 0
  while i < triggered_alerts.length() {
    let alert = triggered_alerts[i]
    if alert.contains("high_error_rate") {
      error_rate_alerts = error_rate_alerts + 1
    } else if alert.contains("high_response_time") {
      response_time_alerts = response_time_alerts + 1
    }
    i = i + 1
  }
  
  assert_eq(error_rate_alerts, 5) // 5个错误率告警
  assert_eq(response_time_alerts, 5) // 5个响应时间告警
  
  // 验证告警时间顺序
  i = 0
  while i < triggered_alerts.length() - 1 {
    let current_time = triggered_alerts[i].split("|")[0].to_long()
    let next_time = triggered_alerts[i + 1].split("|")[0].to_long()
    assert_eq(current_time <= next_time, true)
    i = i + 1
  }
}

test "telemetry_realtime_dashboard_metrics" {
  // 测试实时仪表板指标计算
  
  // 创建仪表板所需的实时数据
  let mut dashboard_data = []
  let base_timestamp = 1640995200L
  
  // 生成多种服务的实时指标
  let services = ["api", "database", "cache", "queue"]
  let metrics = ["requests", "errors", "latency", "throughput"]
  
  let mut i = 0
  while i < 30 { // 30秒的数据
    let timestamp = base_timestamp + i.to_long()
    let mut j = 0
    while j < services.length() {
      let service = services[j]
      let mut k = 0
      while k < metrics.length() {
        let metric = metrics[k]
        
        // 模拟不同的指标值
        let metric_value = if metric == "requests" {
          (100 + i * 2).to_string()
        } else if metric == "errors" {
          (5 + i / 3).to_string()
        } else if metric == "latency" {
          (50 + i * 1.5).to_string()
        } else {
          (1000 + i * 10).to_string()
        }
        
        let data_point = timestamp.to_string() + "|" + service + "|" + metric + "|" + metric_value
        dashboard_data.push(data_point)
        k = k + 1
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证数据创建
  assert_eq(dashboard_data.length(), 480) // 30秒 * 4服务 * 4指标
  
  // 计算实时仪表板指标
  let mut dashboard_metrics = []
  let latest_timestamp = base_timestamp + 29L
  
  // 按服务聚合最新指标
  let mut i = 0
  while i < services.length() {
    let service = services[i]
    let mut service_metrics = []
    
    let mut j = 0
    while j < metrics.length() {
      let metric = metrics[j]
      let mut latest_value = ""
      
      // 找到最新的指标值
      let mut k = 0
      while k < dashboard_data.length() {
        let data_point = dashboard_data[k]
        let parts = data_point.split("|")
        if parts.length() == 4 {
          let timestamp = parts[0].to_long()
          let data_service = parts[1]
          let data_metric = parts[2]
          let value = parts[3]
          
          if data_service == service and data_metric == metric and timestamp == latest_timestamp {
            latest_value = value
            break
          }
        }
        k = k + 1
      }
      
      if latest_value.length() > 0 {
        service_metrics.push((metric, latest_value))
      }
      j = j + 1
    }
    
    dashboard_metrics.push((service, service_metrics))
    i = i + 1
  }
  
  // 验证仪表板指标
  assert_eq(dashboard_metrics.length(), 4) // 4个服务
  assert_eq(dashboard_metrics[0].1.length(), 4) // 每个服务4个指标
  
  // 验证API服务的指标
  let api_metrics = dashboard_metrics[0]
  assert_eq(api_metrics.0, "api")
  
  let mut found_requests = false
  let mut found_errors = false
  let mut i = 0
  while i < api_metrics.1.length() {
    if api_metrics.1[i].0 == "requests" {
      assert_eq(api_metrics.1[i].1, "158") // 100 + 29*2 = 158
      found_requests = true
    } else if api_metrics.1[i].0 == "errors" {
      assert_eq(api_metrics.1[i].1, "14") // 5 + 29/3 = 14
      found_errors = true
    }
    i = i + 1
  }
  
  assert_eq(found_requests, true)
  assert_eq(found_errors, true)
  
  // 计算跨服务聚合指标
  let mut total_requests = 0
  let mut total_errors = 0
  let mut avg_latency = 0.0
  let mut service_count = 0
  
  i = 0
  while i < dashboard_metrics.length() {
    let service_metrics = dashboard_metrics[i].1
    let mut j = 0
    while j < service_metrics.length() {
      let metric = service_metrics[j]
      if metric.0 == "requests" {
        total_requests = total_requests + metric.1.to_int()
      } else if metric.0 == "errors" {
        total_errors = total_errors + metric.1.to_int()
      } else if metric.0 == "latency" {
        avg_latency = avg_latency + metric.1.to_double()
        service_count = service_count + 1
      }
      j = j + 1
    }
    i = i + 1
  }
  
  avg_latency = avg_latency / service_count.to_double()
  
  // 验证跨服务聚合
  assert_eq(total_requests, 632) // 158 * 4 services
  assert_eq(total_errors, 56) // 14 * 4 services
  assert_eq(avg_latency > 50.0, true)
  
  // 创建仪表板摘要
  let dashboard_summary = [
    ("total_requests", total_requests.to_string()),
    ("total_errors", total_errors.to_string()),
    ("error_rate", (total_errors.to_double() / total_requests.to_double() * 100.0).to_string()),
    ("avg_latency", avg_latency.to_string())
  ]
  
  // 验证仪表板摘要
  assert_eq(dashboard_summary.length(), 4)
  assert_eq(dashboard_summary[0].1, "632")
  assert_eq(dashboard_summary[1].1, "56")
  assert_eq(dashboard_summary[2].1.contains("8.86"), true) // 56/632 * 100 ≈ 8.86%
}