// Azimuth Telemetry System - 指标聚合功能测试
// 测试各种指标的聚合功能，确保数据正确汇总和计算

test "Counter指标聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "counter.aggregation.test")
  
  // 创建多个Counter
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let error_counter = Meter::create_counter(meter, "http.errors.total")
  let success_counter = Meter::create_counter(meter, "http.successes.total")
  
  // 模拟不同属性的指标记录
  let success_attrs = Attributes::new()
  Attributes::set(success_attrs, "status", StringValue("200"))
  Attributes::set(success_attrs, "method", StringValue("GET"))
  Attributes::set(success_attrs, "endpoint", StringValue("/api/users"))
  
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "status", StringValue("500"))
  Attributes::set(error_attrs, "method", StringValue("POST"))
  Attributes::set(error_attrs, "endpoint", StringValue("/api/orders"))
  
  // 记录成功请求
  for i = 0; i < 100; i = i + 1 {
    Counter::add(request_counter, 1.0, Some(success_attrs))
    Counter::add(success_counter, 1.0, Some(success_attrs))
  }
  
  // 记录错误请求
  for i = 0; i < 20; i = i + 1 {
    Counter::add(request_counter, 1.0, Some(error_attrs))
    Counter::add(error_counter, 1.0, Some(error_attrs))
  }
  
  // 记录其他类型的请求
  let other_attrs = Attributes::new()
  Attributes::set(other_attrs, "status", StringValue("404"))
  Attributes::set(other_attrs, "method", StringValue("GET"))
  Attributes::set(other_attrs, "endpoint", StringValue("/api/unknown"))
  
  for i = 0; i < 10; i = i + 1 {
    Counter::add(request_counter, 1.0, Some(other_attrs))
  }
  
  // 模拟聚合计算
  fn aggregate_counter_values(
    counters : Array[(String, Double, Attributes)]
  ) -> (Double, Double, Double) {
    let mut total_requests = 0.0
    let mut total_errors = 0.0
    let mut total_successes = 0.0
    
    for (name, value, attrs) in counters {
      total_requests = total_requests + value
      
      match name {
        "http.errors.total" => total_errors = total_errors + value
        "http.successes.total" => total_successes = total_successes + value
        _ => ()
      }
    }
    
    (total_requests, total_errors, total_successes)
  }
  
  // 模拟收集的指标数据
  let collected_metrics = [
    ("http.requests.total", 100.0, success_attrs),
    ("http.successes.total", 100.0, success_attrs),
    ("http.requests.total", 20.0, error_attrs),
    ("http.errors.total", 20.0, error_attrs),
    ("http.requests.total", 10.0, other_attrs)
  ]
  
  let (total_requests, total_errors, total_successes) = aggregate_counter_values(collected_metrics)
  
  // 验证聚合结果
  assert_eq(total_requests, 130.0)  // 100 + 20 + 10
  assert_eq(total_errors, 20.0)     // 20
  assert_eq(total_successes, 100.0) // 100
  
  // 验证错误率计算
  let error_rate = total_errors / total_requests
  assert_eq(error_rate, 20.0 / 130.0)
  
  // 验证成功率计算
  let success_rate = total_successes / total_requests
  assert_eq(success_rate, 100.0 / 130.0)
}

test "Histogram指标聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "histogram.aggregation.test")
  
  // 创建Histogram
  let response_time_histogram = Meter::create_histogram(meter, "http.response.time")
  
  // 模拟不同响应时间的记录
  let response_times = [
    10.5, 25.3, 50.7, 75.2, 100.8, 150.3, 200.1, 300.5, 500.2, 1000.7,
    15.2, 30.1, 60.5, 80.9, 120.4, 180.6, 250.3, 400.8, 750.1, 1200.5,
    5.1, 12.3, 45.6, 90.2, 140.7, 220.5, 350.9, 600.3, 900.8, 1500.2
  ]
  
  // 记录响应时间
  for time in response_times {
    Histogram::record(response_time_histogram, time)
  }
  
  // 模拟Histogram聚合计算
  fn aggregate_histogram_values(values : Array[Double]) -> (Double, Double, Double, Double, Double, Int) {
    if values.length() == 0 {
      return (0.0, 0.0, 0.0, 0.0, 0.0, 0)
    }
    
    // 计算总和
    let mut sum = 0.0
    for value in values {
      sum = sum + value
    }
    
    // 计算平均值
    let mean = sum / values.length().to_double()
    
    // 计算最小值和最大值
    let mut min = values[0]
    let mut max = values[0]
    
    for value in values {
      if value < min {
        min = value
      }
      if value > max {
        max = value
      }
    }
    
    // 计算中位数
    let sorted_values = values.sort()
    let median = if sorted_values.length() % 2 == 0 {
      let mid = sorted_values.length() / 2
      (sorted_values[mid - 1] + sorted_values[mid]) / 2.0
    } else {
      let mid = sorted_values.length() / 2
      sorted_values[mid]
    }
    
    (sum, mean, min, max, median, values.length())
  }
  
  let (sum, mean, min, max, median, count) = aggregate_histogram_values(response_times)
  
  // 验证聚合结果
  assert_eq(count, 30)
  assert_eq(min, 5.1)
  assert_eq(max, 1500.2)
  
  // 验证总和和平均值
  let expected_sum = 0.0
  for time in response_times {
    expected_sum = expected_sum + time
  }
  assert_eq(sum, expected_sum)
  assert_eq(mean, expected_sum / 30.0)
  
  // 验证百分位数计算（模拟）
  fn calculate_percentile(sorted_values : Array[Double], percentile : Double) -> Double {
    if sorted_values.length() == 0 {
      return 0.0
    }
    
    let index = (percentile / 100.0 * (sorted_values.length() - 1).to_double()).to_int()
    sorted_values[index]
  }
  
  let sorted_times = response_times.sort()
  let p50 = calculate_percentile(sorted_times, 50.0)
  let p95 = calculate_percentile(sorted_times, 95.0)
  let p99 = calculate_percentile(sorted_times, 99.0)
  
  // 验证百分位数
  assert_eq(p50, median)  // 50%百分位数应该等于中位数
  assert_true(p95 > p50)  // 95%百分位数应该大于50%百分位数
  assert_true(p99 > p95)  // 99%百分位数应该大于95%百分位数
}

test "UpDownCounter指标聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "updowncounter.aggregation.test")
  
  // 创建UpDownCounter
  let active_connections = Meter::create_updown_counter(meter, "active.connections")
  let queue_size = Meter::create_updown_counter(meter, "queue.size")
  
  // 模拟连接数变化
  let mut connection_changes = []
  
  // 初始连接
  for i = 0; i < 10; i = i + 1 {
    Counter::add(active_connections, 1.0)
    connection_changes.push((Clock::system(), 1.0))
  }
  
  // 一些连接关闭
  for i = 0; i < 3; i = i + 1 {
    Counter::add(active_connections, -1.0)
    connection_changes.push((Clock::system(), -1.0))
  }
  
  // 更多连接打开
  for i = 0; i < 5; i = i + 1 {
    Counter::add(active_connections, 1.0)
    connection_changes.push((Clock::system(), 1.0))
  }
  
  // 模拟队列大小变化
  let mut queue_changes = []
  
  // 初始队列
  for i = 0; i < 20; i = i + 1 {
    Counter::add(queue_size, 1.0)
    queue_changes.push((Clock::system(), 1.0))
  }
  
  // 处理队列项
  for i = 0; i < 8; i = i + 1 {
    Counter::add(queue_size, -1.0)
    queue_changes.push((Clock::system(), -1.0))
  }
  
  // 更多队列项添加
  for i = 0; i < 15; i = i + 1 {
    Counter::add(queue_size, 1.0)
    queue_changes.push((Clock::system(), 1.0))
  }
  
  // 模拟UpDownCounter聚合计算
  fn aggregate_updowncounter_values(changes : Array[(Clock, Double)]) -> (Double, Double, Double, Double) {
    let mut current_value = 0.0
    let mut max_value = 0.0
    let mut min_value = 0.0
    let mut total_increments = 0.0
    let mut total_decrements = 0.0
    
    for (_, change) in changes {
      current_value = current_value + change
      
      if current_value > max_value {
        max_value = current_value
      }
      
      if current_value < min_value {
        min_value = current_value
      }
      
      if change > 0.0 {
        total_increments = total_increments + change
      } else {
        total_decrements = total_decrements + change
      }
    }
    
    (current_value, max_value, min_value, total_increments + total_decrements)
  }
  
  let (final_connections, max_connections, min_connections, total_conn_change) = 
    aggregate_updowncounter_values(connection_changes)
  
  let (final_queue_size, max_queue_size, min_queue_size, total_queue_change) = 
    aggregate_updowncounter_values(queue_changes)
  
  // 验证连接数聚合结果
  assert_eq(final_connections, 12.0)  // 10 - 3 + 5 = 12
  assert_eq(max_connections, 10.0)   // 最大值是初始的10
  assert_eq(min_connections, 7.0)    // 最小值是10 - 3 = 7
  assert_eq(total_conn_change, 12.0) // 10 - 3 + 5 = 12
  
  // 验证队列大小聚合结果
  assert_eq(final_queue_size, 27.0)  // 20 - 8 + 15 = 27
  assert_eq(max_queue_size, 20.0)    // 最大值是初始的20
  assert_eq(min_queue_size, 12.0)    // 最小值是20 - 8 = 12
  assert_eq(total_queue_change, 27.0) // 20 - 8 + 15 = 27
}

test "Gauge指标聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "gauge.aggregation.test")
  
  // 创建Gauge
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage")
  let memory_usage = Meter::create_gauge(meter, "system.memory.usage")
  
  // 模拟CPU使用率变化
  let mut cpu_readings = []
  let timestamps = []
  
  // 模拟一段时间内的CPU使用率读数
  let cpu_values = [0.15, 0.25, 0.45, 0.65, 0.35, 0.55, 0.75, 0.40, 0.30, 0.20]
  
  for i = 0; i < cpu_values.length(); i = i + 1 {
    // 在实际实现中，Gauge::set会设置当前值
    // 这里我们只是记录值用于聚合
    cpu_readings.push(cpu_values[i])
  }
  
  // 模拟内存使用率变化
  let mut memory_readings = []
  
  let memory_values = [0.40, 0.45, 0.50, 0.55, 0.60, 0.58, 0.62, 0.65, 0.63, 0.60]
  
  for i = 0; i < memory_values.length(); i = i + 1 {
    memory_readings.push(memory_values[i])
  }
  
  // 模拟Gauge聚合计算
  fn aggregate_gauge_values(readings : Array[Double]) -> (Double, Double, Double, Double, Double) {
    if readings.length() == 0 {
      return (0.0, 0.0, 0.0, 0.0, 0.0)
    }
    
    // 计算当前值（最后一个读数）
    let current = readings[readings.length() - 1]
    
    // 计算平均值
    let mut sum = 0.0
    for value in readings {
      sum = sum + value
    }
    let average = sum / readings.length().to_double()
    
    // 计算最小值和最大值
    let mut min = readings[0]
    let mut max = readings[0]
    
    for value in readings {
      if value < min {
        min = value
      }
      if value > max {
        max = value
      }
    }
    
    // 计算变化率（最后一个读数与第一个读数的差）
    let change_rate = if readings.length() > 1 {
      readings[readings.length() - 1] - readings[0]
    } else {
      0.0
    }
    
    (current, average, min, max, change_rate)
  }
  
  let (cpu_current, cpu_average, cpu_min, cpu_max, cpu_change) = 
    aggregate_gauge_values(cpu_readings)
  
  let (memory_current, memory_average, memory_min, memory_max, memory_change) = 
    aggregate_gauge_values(memory_readings)
  
  // 验证CPU使用率聚合结果
  assert_eq(cpu_current, 0.20)      // 最后一个读数
  assert_eq(cpu_min, 0.15)          // 最小值
  assert_eq(cpu_max, 0.75)          // 最大值
  
  // 验证CPU平均值
  let expected_cpu_sum = 0.0
  for value in cpu_values {
    expected_cpu_sum = expected_cpu_sum + value
  }
  assert_eq(cpu_average, expected_cpu_sum / 10.0)
  
  // 验证CPU变化率
  assert_eq(cpu_change, 0.20 - 0.15) // 最后一个读数减去第一个读数
  
  // 验证内存使用率聚合结果
  assert_eq(memory_current, 0.60)    // 最后一个读数
  assert_eq(memory_min, 0.40)        // 最小值
  assert_eq(memory_max, 0.65)        // 最大值
  
  // 验证内存平均值
  let expected_memory_sum = 0.0
  for value in memory_values {
    expected_memory_sum = expected_memory_sum + value
  }
  assert_eq(memory_average, expected_memory_sum / 10.0)
  
  // 验证内存变化率
  assert_eq(memory_change, 0.60 - 0.40) // 最后一个读数减去第一个读数
}

test "多维度指标聚合测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "multidimensional.aggregation.test")
  
  // 创建Counter
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  
  // 定义不同的属性组合
  let endpoints = ["/api/users", "/api/orders", "/api/products"]
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let status_codes = ["200", "400", "404", "500"]
  
  // 模拟多维度指标记录
  let mut metric_records = []
  
  for endpoint in endpoints {
    for method in methods {
      for status_code in status_codes {
        // 创建属性
        let attrs = Attributes::new()
        Attributes::set(attrs, "endpoint", StringValue(endpoint))
        Attributes::set(attrs, "method", StringValue(method))
        Attributes::set(attrs, "status", StringValue(status_code))
        
        // 根据状态码确定请求次数（模拟真实的分布）
        let count = match status_code {
          "200" => 100
          "400" => 20
          "404" => 10
          "500" => 5
          _ => 0
        }
        
        // 记录指标
        for i = 0; i < count; i = i + 1 {
          Counter::add(request_counter, 1.0, Some(attrs))
        }
        
        // 记录聚合数据
        metric_records.push((endpoint, method, status_code, count.to_double()))
      }
    }
  }
  
  // 模拟多维度聚合计算
  fn aggregate_by_dimension(
    records : Array[(String, String, String, Double)],
    dimension : String
  ) -> Array[(String, Double)] {
    let mut aggregation = []
    
    for record in records {
      let key = match dimension {
        "endpoint" => record.0
        "method" => record.1
        "status" => record.2
        _ => "unknown"
      }
      
      let value = record.3
      
      // 查找是否已存在该key的聚合
      let mut found = false
      let mut new_aggregation = []
      
      for (existing_key, existing_value) in aggregation {
        if existing_key == key {
          new_aggregation.push((existing_key, existing_value + value))
          found = true
        } else {
          new_aggregation.push((existing_key, existing_value))
        }
      }
      
      if !found {
        new_aggregation.push((key, value))
      }
      
      aggregation = new_aggregation
    }
    
    aggregation
  }
  
  // 按端点聚合
  let endpoint_aggregation = aggregate_by_dimension(metric_records, "endpoint")
  
  // 按方法聚合
  let method_aggregation = aggregate_by_dimension(metric_records, "method")
  
  // 按状态码聚合
  let status_aggregation = aggregate_by_dimension(metric_records, "status")
  
  // 验证端点聚合结果
  for (endpoint, total) in endpoint_aggregation {
    // 每个端点应该有4个方法 × 4个状态码的组合
    // 总请求数 = 4 × (100 + 20 + 10 + 5) = 4 × 135 = 540
    assert_eq(total, 540.0)
  }
  
  // 验证方法聚合结果
  for (method, total) in method_aggregation {
    // 每个方法应该有3个端点 × 4个状态码的组合
    // 总请求数 = 3 × (100 + 20 + 10 + 5) = 3 × 135 = 405
    assert_eq(total, 405.0)
  }
  
  // 验证状态码聚合结果
  for (status, total) in status_aggregation {
    let expected_total = match status {
      "200" => 3 × 4 × 100  // 3个端点 × 4个方法 × 100次请求
      "400" => 3 × 4 × 20   // 3个端点 × 4个方法 × 20次请求
      "404" => 3 × 4 × 10   // 3个端点 × 4个方法 × 10次请求
      "500" => 3 × 4 × 5    // 3个端点 × 4个方法 × 5次请求
      _ => 0.0
    }
    assert_eq(total, expected_total.to_double())
  }
}