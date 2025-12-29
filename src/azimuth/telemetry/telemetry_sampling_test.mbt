// 遥测数据采样策略测试用例

test "telemetry_sampling_probabilistic" {
  // 测试概率采样策略
  
  // 创建大量遥测数据
  let telemetry_spans = []
  let mut i = 0
  while i < 1000 {
    let span_id = "span_" + i.to_string()
    let trace_id = "trace_" + (i / 10).to_string()
    telemetry_spans.push((span_id, trace_id))
    i = i + 1
  }
  
  // 验证数据创建
  assert_eq(telemetry_spans.length(), 1000)
  
  // 概率采样函数（采样率10%）
  let probabilistic_sample = fn(span_id : String, trace_id : String, sample_rate : Double) -> Bool {
    // 使用span_id的哈希值来决定是否采样
    let mut hash = 0
    let mut j = 0
    while j < span_id.length() {
      hash = hash + span_id[j].to_int()
      j = j + 1
    }
    
    // 归一化到0-1范围
    let normalized = (hash.to_double() % 100.0) / 100.0
    return normalized < sample_rate
  }
  
  // 应用10%概率采样
  let sample_rate = 0.1
  let mut sampled_spans = []
  i = 0
  while i < telemetry_spans.length() {
    let span = telemetry_spans[i]
    if probabilistic_sample(span.0, span.1, sample_rate) {
      sampled_spans.push(span)
    }
    i = i + 1
  }
  
  // 验证采样结果
  assert_eq(sampled_spans.length() > 0, true)
  assert_eq(sampled_spans.length() < telemetry_spans.length(), true)
  
  // 验证采样率大约在10%左右（允许一定误差）
  let actual_sample_rate = sampled_spans.length().to_double() / telemetry_spans.length().to_double()
  assert_eq(actual_sample_rate > 0.05, true) // 至少5%
  assert_eq(actual_sample_rate < 0.15, true) // 不超过15%
  
  // 测试不同采样率
  let high_sample_rate = 0.5
  let mut high_sampled = []
  i = 0
  while i < telemetry_spans.length() {
    let span = telemetry_spans[i]
    if probabilistic_sample(span.0, span.1, high_sample_rate) {
      high_sampled.push(span)
    }
    i = i + 1
  }
  
  let high_actual_rate = high_sampled.length().to_double() / telemetry_spans.length().to_double()
  assert_eq(high_actual_rate > actual_sample_rate, true) // 高采样率应该产生更多样本
  assert_eq(high_actual_rate > 0.4, true) // 至少40%
  assert_eq(high_actual_rate < 0.6, true) // 不超过60%
}

test "telemetry_sampling_trace_based" {
  // 测试基于Trace的采样策略
  
  // 创建属于不同trace的spans
  let trace_spans = []
  let mut i = 0
  while i < 100 {
    let trace_id = "trace_" + (i / 5).to_string() // 每5个spans属于同一个trace
    let span_id = "span_" + i.to_string()
    trace_spans.push((trace_id, span_id))
    i = i + 1
  }
  
  // 验证数据创建
  assert_eq(trace_spans.length(), 100)
  
  // 统计每个trace的span数量
  let mut trace_counts = []
  i = 0
  while i < 20 { // 应该有20个不同的traces
    let trace_id = "trace_" + i.to_string()
    let mut count = 0
    let mut j = 0
    while j < trace_spans.length() {
      if trace_spans[j].0 == trace_id {
        count = count + 1
      }
      j = j + 1
    }
    trace_counts.push((trace_id, count))
    i = i + 1
  }
  
  // 验证trace分布
  assert_eq(trace_counts.length(), 20)
  i = 0
  while i < trace_counts.length() {
    assert_eq(trace_counts[i].1, 5) // 每个trace应该有5个spans
    i = i + 1
  }
  
  // 基于trace的采样：如果trace被采样，则该trace的所有spans都被采样
  let mut sampled_traces = []
  let trace_sample_rate = 0.3 // 30%的traces被采样
  i = 0
  while i < trace_counts.length() {
    let trace_hash = trace_counts[i].0.length() % 10
    if trace_hash.to_double() / 10.0 < trace_sample_rate {
      sampled_traces.push(trace_counts[i].0)
    }
    i = i + 1
  }
  
  // 验证trace采样结果
  assert_eq(sampled_traces.length() > 0, true)
  assert_eq(sampled_traces.length() < trace_counts.length(), true)
  
  // 采样所有属于被选中trace的spans
  let mut sampled_spans = []
  i = 0
  while i < trace_spans.length() {
    let trace_id = trace_spans[i].0
    let mut is_sampled = false
    let mut j = 0
    while j < sampled_traces.length() {
      if sampled_traces[j] == trace_id {
        is_sampled = true
        break
      }
      j = j + 1
    }
    
    if is_sampled {
      sampled_spans.push(trace_spans[i])
    }
    i = i + 1
  }
  
  // 验证span采样结果
  assert_eq(sampled_spans.length() > 0, true)
  
  // 验证同一个trace的所有spans要么都被采样，要么都不被采样
  i = 0
  while i < sampled_traces.length() {
    let trace_id = sampled_traces[i]
    let mut trace_sampled_spans = 0
    let mut j = 0
    while j < sampled_spans.length() {
      if sampled_spans[j].0 == trace_id {
        trace_sampled_spans = trace_sampled_spans + 1
      }
      j = j + 1
    }
    
    // 被采样的trace应该有所有5个spans被采样
    assert_eq(trace_sampled_spans, 5)
    i = i + 1
  }
}

test "telemetry_sampling_adaptive" {
  // 测试自适应采样策略
  
  // 创建不同优先级和类型的遥测数据
  let telemetry_data = []
  
  // 高优先级错误数据（应该总是被采样）
  telemetry_data.push(("error", "database_connection_failed", "high"))
  telemetry_data.push(("error", "authentication_failure", "high"))
  telemetry_data.push(("error", "payment_processing_error", "high"))
  
  // 中等优先级慢查询数据
  telemetry_data.push(("slow_query", "user_lookup_500ms", "medium"))
  telemetry_data.push(("slow_query", "order_processing_800ms", "medium"))
  telemetry_data.push(("slow_query", "inventory_check_600ms", "medium"))
  
  // 低优先级常规请求
  telemetry_data.push(("request", "get_user_profile", "low"))
  telemetry_data.push(("request", "list_products", "low"))
  telemetry_data.push(("request", "search_items", "low"))
  telemetry_data.push(("request", "get_cart_items", "low"))
  telemetry_data.push(("request", "update_settings", "low"))
  
  // 验证数据创建
  assert_eq(telemetry_data.length(), 11)
  
  // 自适应采样函数
  let adaptive_sample = fn(data_type : String, description : String, priority : String) -> Bool {
    if priority == "high" {
      return true // 高优先级总是采样
    } else if priority == "medium" {
      // 中等优先级50%采样
      return description.length() % 2 == 0
    } else if priority == "low" {
      // 低优先级20%采样
      return description.length() % 5 == 0
    }
    return false
  }
  
  // 应用自适应采样
  let mut sampled_data = []
  let mut i = 0
  while i < telemetry_data.length() {
    let data = telemetry_data[i]
    if adaptive_sample(data.0, data.1, data.2) {
      sampled_data.push(data)
    }
    i = i + 1
  }
  
  // 验证采样结果
  assert_eq(sampled_data.length() > 0, true)
  assert_eq(sampled_data.length() < telemetry_data.length(), true)
  
  // 验证高优先级数据全部被采样
  let mut high_priority_sampled = 0
  let mut high_priority_total = 0
  i = 0
  while i < telemetry_data.length() {
    if telemetry_data[i].2 == "high" {
      high_priority_total = high_priority_total + 1
      let mut found = false
      let mut j = 0
      while j < sampled_data.length() {
        if sampled_data[j] == telemetry_data[i] {
          found = true
          break
        }
        j = j + 1
      }
      if found {
        high_priority_sampled = high_priority_sampled + 1
      }
    }
    i = i + 1
  }
  
  assert_eq(high_priority_sampled, high_priority_total) // 所有高优先级都应该被采样
  assert_eq(high_priority_total, 3)
  
  // 验证采样分布
  let mut priority_counts = []
  let priorities = ["high", "medium", "low"]
  i = 0
  while i < priorities.length() {
    let priority = priorities[i]
    let mut sampled_count = 0
    let mut j = 0
    while j < sampled_data.length() {
      if sampled_data[j].2 == priority {
        sampled_count = sampled_count + 1
      }
      j = j + 1
    }
    priority_counts.push((priority, sampled_count))
    i = i + 1
  }
  
  // 验证优先级采样分布
  assert_eq(priority_counts[0].0, "high")
  assert_eq(priority_counts[0].1, 3) // 3个高优先级都被采样
  
  // 中等和低优先级的采样数量应该少于总数
  assert_eq(priority_counts[1].1 <= 3, true) // 中等优先级最多3个
  assert_eq(priority_counts[2].1 <= 5, true) // 低优先级最多5个
}

test "telemetry_sampling_rate_limiting" {
  // 测试基于速率限制的采样策略
  
  // 创建时间序列的遥测数据
  let time_series_data = []
  let mut i = 0
  while i < 100 {
    let timestamp = 1640995200L + i.to_long()
    let metric_value = 50.0 + (i.to_double() * 0.1)
    time_series_data.push((timestamp, metric_value))
    i = i + 1
  }
  
  // 验证数据创建
  assert_eq(time_series_data.length(), 100)
  
  // 速率限制采样：每秒最多采样10个数据点
  let max_samples_per_second = 10
  let mut sampled_data = []
  let mut current_second_samples = 0
  let mut current_second = -1L
  
  i = 0
  while i < time_series_data.length() {
    let data_point = time_series_data[i]
    let data_second = data_point.0 / 1000L // 转换为秒
    
    if data_second != current_second {
      // 新的一秒，重置计数器
      current_second = data_second
      current_second_samples = 0
    }
    
    if current_second_samples < max_samples_per_second {
      sampled_data.push(data_point)
      current_second_samples = current_second_samples + 1
    }
    
    i = i + 1
  }
  
  // 验证采样结果
  assert_eq(sampled_data.length() > 0, true)
  assert_eq(sampled_data.length() <= time_series_data.length(), true)
  
  // 验证采样率不超过限制
  let mut second_counts = []
  let mut start_second = time_series_data[0].0 / 1000L
  let mut end_second = time_series_data[99].0 / 1000L
  
  let mut second = start_second
  while second <= end_second {
    let mut count = 0
    let mut j = 0
    while j < sampled_data.length() {
      if sampled_data[j].0 / 1000L == second {
        count = count + 1
      }
      j = j + 1
    }
    second_counts.push((second, count))
    second = second + 1L
  }
  
  // 验证每秒的采样数都不超过限制
  let mut i = 0
  while i < second_counts.length() {
    assert_eq(second_counts[i].1 <= max_samples_per_second, true)
    i = i + 1
  }
  
  // 验证采样的时间分布
  let first_sampled_time = sampled_data[0].0
  let last_sampled_time = sampled_data[sampled_data.length() - 1].0
  assert_eq(first_sampled_time >= time_series_data[0].0, true)
  assert_eq(last_sampled_time <= time_series_data[99].0, true)
}

test "telemetry_sampling_resource_based" {
  // 测试基于资源的采样策略
  
  // 创建来自不同服务的遥测数据
  let service_metrics = []
  
  // 高流量服务（需要更多采样）
  let mut i = 0
  while i < 50 {
    service_metrics.push(("api_gateway", "request_count", i.to_string()))
    i = i + 1
  }
  
  // 中等流量服务
  i = 0
  while i < 30 {
    service_metrics.push(("user_service", "login_attempt", i.to_string()))
    i = i + 1
  }
  
  // 低流量服务（需要较少采样）
  i = 0
  while i < 20 {
    service_metrics.push(("admin_service", "config_change", i.to_string()))
    i = i + 1
  }
  
  // 验证数据创建
  assert_eq(service_metrics.length(), 100)
  
  // 基于资源的采样策略
  let resource_based_sample = fn(service : String, metric : String, total_metrics : Int) -> Bool {
    // 根据服务流量调整采样率
    if service == "api_gateway" {
      // 高流量服务，采样率5%
      return metric.length() % 20 == 0
    } else if service == "user_service" {
      // 中等流量服务，采样率15%
      return metric.length() % 7 == 0
    } else if service == "admin_service" {
      // 低流量服务，采样率50%
      return metric.length() % 2 == 0
    }
    return false
  }
  
  // 应用基于资源的采样
  let mut sampled_metrics = []
  i = 0
  while i < service_metrics.length() {
    let metric = service_metrics[i]
    if resource_based_sample(metric.0, metric.1, service_metrics.length()) {
      sampled_metrics.push(metric)
    }
    i = i + 1
  }
  
  // 验证采样结果
  assert_eq(sampled_metrics.length() > 0, true)
  assert_eq(sampled_metrics.length() < service_metrics.length(), true)
  
  // 统计每个服务的采样数量
  let mut service_sample_counts = []
  let services = ["api_gateway", "user_service", "admin_service"]
  i = 0
  while i < services.length() {
    let service = services[i]
    let mut sampled_count = 0
    let mut total_count = 0
    let mut j = 0
    while j < service_metrics.length() {
      if service_metrics[j].0 == service {
        total_count = total_count + 1
        let mut found = false
        let mut k = 0
        while k < sampled_metrics.length() {
          if sampled_metrics[k] == service_metrics[j] {
            found = true
            break
          }
          k = k + 1
        }
        if found {
          sampled_count = sampled_count + 1
        }
      }
      j = j + 1
    }
    service_sample_counts.push((service, total_count, sampled_count))
    i = i + 1
  }
  
  // 验证采样分布符合预期
  // api_gateway: 50个总数，应该约5%被采样（2-3个）
  assert_eq(service_sample_counts[0].0, "api_gateway")
  assert_eq(service_sample_counts[0].1, 50)
  assert_eq(service_sample_counts[0].2 <= 5, true)
  
  // user_service: 30个总数，应该约15%被采样（4-5个）
  assert_eq(service_sample_counts[1].0, "user_service")
  assert_eq(service_sample_counts[1].1, 30)
  assert_eq(service_sample_counts[1].2 <= 8, true)
  
  // admin_service: 20个总数，应该约50%被采样（8-12个）
  assert_eq(service_sample_counts[2].0, "admin_service")
  assert_eq(service_sample_counts[2].1, 20)
  assert_eq(service_sample_counts[2].2 <= 12, true)
  
  // 验证低流量服务的采样率高于高流量服务
  let api_gateway_rate = service_sample_counts[0].2.to_double() / service_sample_counts[0].1.to_double()
  let admin_service_rate = service_sample_counts[2].2.to_double() / service_sample_counts[2].1.to_double()
  assert_eq(admin_service_rate > api_gateway_rate, true)
}