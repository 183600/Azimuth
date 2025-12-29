// 遥测指标聚合测试用例

test "counter_metrics_aggregation" {
  // 测试计数器指标聚合
  
  let raw_counter_metrics = [
    ("http_requests_total", "service_a", "GET", 150),
    ("http_requests_total", "service_a", "POST", 80),
    ("http_requests_total", "service_b", "GET", 200),
    ("http_requests_total", "service_b", "POST", 120),
    ("http_requests_total", "service_a", "GET", 100),  // 同一服务同一方法的后续计数
    ("error_count", "service_a", "timeout", 5),
    ("error_count", "service_b", "connection", 3),
    ("error_count", "service_a", "timeout", 8)  // 同一服务同一错误类型的后续计数
  ]
  
  // 验证原始计数器指标
  assert_eq(raw_counter_metrics.length(), 8)
  
  // 按指标名、服务、标签分组聚合
  let aggregated_metrics = []
  let processed_keys = []
  
  for metric in raw_counter_metrics {
    let key = metric.0 + ":" + metric.1 + ":" + metric.2
    
    if not processed_keys.contains(key) {
      let mut total_value = 0
      
      // 累加相同键的所有值
      for m in raw_counter_metrics {
        if m.0 + ":" + m.1 + ":" + m.2 == key {
          total_value = total_value + m.3
        }
      }
      
      aggregated_metrics.push((metric.0, metric.1, metric.2, total_value))
      processed_keys.push(key)
    }
  }
  
  // 验证聚合结果
  assert_eq(aggregated_metrics.length(), 5)  // 8个原始指标聚合为5个唯一组合
  
  // 验证具体的聚合结果
  let service_a_get_requests = aggregated_metrics.find(|m| m.0 == "http_requests_total" and m.1 == "service_a" and m.2 == "GET")
  let service_a_post_requests = aggregated_metrics.find(|m| m.0 == "http_requests_total" and m.1 == "service_a" and m.2 == "POST")
  let service_a_timeout_errors = aggregated_metrics.find(|m| m.0 == "error_count" and m.1 == "service_a" and m.2 == "timeout")
  
  assert_eq(service_a_get_requests.3, 250)  // 150 + 100
  assert_eq(service_a_post_requests.3, 80)
  assert_eq(service_a_timeout_errors.3, 13)  // 5 + 8
}

test "gauge_metrics_aggregation" {
  // 测试仪表指标聚合（取最新值）
  
  let raw_gauge_metrics = [
    ("cpu_usage", "service_a", 45.2, "2023-12-01T10:00:00Z"),
    ("memory_usage", "service_a", 1024.0, "2023-12-01T10:00:00Z"),
    ("cpu_usage", "service_a", 48.7, "2023-12-01T10:01:00Z"),
    ("cpu_usage", "service_b", 62.1, "2023-12-01T10:00:30Z"),
    ("memory_usage", "service_a", 1056.0, "2023-12-01T10:01:00Z"),
    ("memory_usage", "service_b", 2048.0, "2023-12-01T10:00:30Z"),
    ("cpu_usage", "service_a", 52.3, "2023-12-01T10:02:00Z")  // service_a的最新CPU使用率
  ]
  
  // 验证原始仪表指标
  assert_eq(raw_gauge_metrics.length(), 7)
  
  // 按指标名和服务分组，取最新时间戳的值
  let latest_gauges = []
  let processed_keys = []
  
  for metric in raw_gauge_metrics {
    let key = metric.0 + ":" + metric.1
    
    if not processed_keys.contains(key) {
      let mut latest_metric = metric
      
      // 查找相同键的最新指标
      for m in raw_gauge_metrics {
        if m.0 + ":" + m.1 == key and m.3 > latest_metric.3 {
          latest_metric = m
        }
      }
      
      latest_gauges.push((metric.0, metric.1, latest_metric.2, latest_metric.3))
      processed_keys.push(key)
    }
  }
  
  // 验证聚合结果
  assert_eq(latest_gauges.length(), 4)  // 7个原始指标聚合为4个唯一服务-指标组合
  
  // 验证具体的聚合结果
  let service_a_cpu = latest_gauges.find(|g| g.0 == "cpu_usage" and g.1 == "service_a")
  let service_a_memory = latest_gauges.find(|g| g.0 == "memory_usage" and g.1 == "service_a")
  let service_b_cpu = latest_gauges.find(|g| g.0 == "cpu_usage" and g.1 == "service_b")
  
  assert_eq(service_a_cpu.2, 52.3)  // 最新值
  assert_eq(service_a_cpu.3, "2023-12-01T10:02:00Z")  // 最新时间戳
  assert_eq(service_a_memory.2, 1056.0)
  assert_eq(service_b_cpu.2, 62.1)
}

test "histogram_metrics_aggregation" {
  // 测试直方图指标聚合
  
  let raw_histogram_metrics = [
    ("request_duration", "service_a", "0.1", 50),    // (指标名, 服务, 桶边界, 计数)
    ("request_duration", "service_a", "0.5", 120),
    ("request_duration", "service_a", "1.0", 180),
    ("request_duration", "service_a", "5.0", 195),
    ("request_duration", "service_a", "inf", 200),
    ("request_duration", "service_b", "0.1", 30),
    ("request_duration", "service_b", "0.5", 80),
    ("request_duration", "service_b", "1.0", 140),
    ("request_duration", "service_b", "5.0", 180),
    ("request_duration", "service_b", "inf", 200)
  ]
  
  // 验证原始直方图指标
  assert_eq(raw_histogram_metrics.length(), 10)
  
  // 按服务和指标分组
  let service_a_histogram = []
  let service_b_histogram = []
  
  for metric in raw_histogram_metrics {
    if metric.1 == "service_a" {
      service_a_histogram.push(metric)
    } else if metric.1 == "service_b" {
      service_b_histogram.push(metric)
    }
  }
  
  // 验证分组结果
  assert_eq(service_a_histogram.length(), 5)
  assert_eq(service_b_histogram.length(), 5)
  
  // 计算分位数（简化计算）
  let service_a_total = service_a_histogram[4].3  // +Inf桶的计数
  let service_b_total = service_b_histogram[4].3
  
  // 验证总计数
  assert_eq(service_a_total, 200)
  assert_eq(service_b_total, 200)
  
  // 计算P95分位数（简化：假设1.0秒桶接近P95）
  let service_a_p95_bucket = service_a_histogram.find(|m| m.2 == "1.0")
  let service_b_p95_bucket = service_b_histogram.find(|m| m.2 == "1.0")
  
  // 验证分位数计算
  assert_eq(service_a_p95_bucket.2, "1.0")
  assert_eq(service_a_p95_bucket.3, 180)
  assert_eq(service_b_p95_bucket.2, "1.0")
  assert_eq(service_b_p95_bucket.3, 140)
  
  // 计算平均请求持续时间（简化估算）
  let service_a_avg_duration = (50.0 * 0.05 + 70.0 * 0.3 + 60.0 * 0.75 + 15.0 * 3.0 + 5.0 * 10.0) / service_a_total.to_float()
  let service_b_avg_duration = (30.0 * 0.05 + 50.0 * 0.3 + 60.0 * 0.75 + 40.0 * 3.0 + 20.0 * 10.0) / service_b_total.to_float()
  
  // 验证平均持续时间在合理范围内
  assert_eq(service_a_avg_duration > 0.0, true)
  assert_eq(service_b_avg_duration > 0.0, true)
}

test "multi_service_metrics_aggregation" {
  // 测试多服务指标聚合
  
  let service_metrics = [
    ("service_a", "cpu_usage", 45.2),
    ("service_a", "memory_usage", 1024.0),
    ("service_a", "request_count", 1500),
    ("service_b", "cpu_usage", 62.1),
    ("service_b", "memory_usage", 2048.0),
    ("service_b", "request_count", 2300),
    ("service_c", "cpu_usage", 38.7),
    ("service_c", "memory_usage", 768.0),
    ("service_c", "request_count", 900)
  ]
  
  // 验证服务指标
  assert_eq(service_metrics.length(), 9)
  
  // 按指标类型聚合所有服务
  let cpu_usages = []
  let memory_usages = []
  let request_counts = []
  
  for metric in service_metrics {
    match metric.1 {
      "cpu_usage" => cpu_usages.push(metric.2)
      "memory_usage" => memory_usages.push(metric.2)
      "request_count" => request_counts.push(metric.2)
      _ => ()
    }
  }
  
  // 验证指标分组
  assert_eq(cpu_usages.length(), 3)
  assert_eq(memory_usages.length(), 3)
  assert_eq(request_counts.length(), 3)
  
  // 计算聚合统计
  let mut total_cpu = 0.0
  let mut total_memory = 0.0
  let mut total_requests = 0
  
  for cpu in cpu_usages {
    total_cpu = total_cpu + cpu
  }
  
  for memory in memory_usages {
    total_memory = total_memory + memory
  }
  
  for requests in request_counts {
    total_requests = total_requests + requests
  }
  
  // 计算平均值
  let avg_cpu = total_cpu / cpu_usages.length().to_float()
  let avg_memory = total_memory / memory_usages.length().to_float()
  let avg_requests = total_requests / request_counts.length()
  
  // 验证聚合统计
  assert_eq(avg_cpu > 40.0, true)
  assert_eq(avg_cpu < 65.0, true)
  assert_eq(avg_memory > 1000.0, true)
  assert_eq(avg_requests > 1000, true)
  
  // 查找最大值和最小值
  let mut max_cpu = cpu_usages[0]
  let mut min_cpu = cpu_usages[0]
  
  for cpu in cpu_usages {
    if cpu > max_cpu {
      max_cpu = cpu
    }
    if cpu < min_cpu {
      min_cpu = cpu
    }
  }
  
  // 验证极值
  assert_eq(max_cpu, 62.1)  // service_b
  assert_eq(min_cpu, 38.7)  // service_c
}