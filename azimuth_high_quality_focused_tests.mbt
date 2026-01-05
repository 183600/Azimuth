// Azimuth 高质量聚焦测试用例
// 专注于遥测系统的关键功能和边界情况测试

// 测试1: 遥测数据采样策略测试
test "遥测数据采样策略测试" {
  // 模拟不同优先级的遥测数据
  let telemetry_data = [
    { priority: "critical", service: "payment", latency: 45.2, error_rate: 0.01 },
    { priority: "high", service: "auth", latency: 23.8, error_rate: 0.05 },
    { priority: "medium", service: "user-profile", latency: 67.4, error_rate: 0.12 },
    { priority: "low", service: "analytics", latency: 120.5, error_rate: 0.08 },
    { priority: "critical", service: "payment", latency: 38.9, error_rate: 0.02 }
  ]
  
  // 实现基于优先级的采样策略
  let mut sampled_data = []
  let sampling_rates = {
    "critical": 1.0,   // 100% 采样
    "high": 0.8,       // 80% 采样
    "medium": 0.5,     // 50% 采样
    "low": 0.2         // 20% 采样
  }
  
  for data in telemetry_data {
    let rate = sampling_rates[data.priority]
    let random_factor = (data.latency * 10.0).to_int() % 100 / 100.0
    if random_factor <= rate {
      sampled_data = sampled_data.push(data)
    }
  }
  
  // 验证采样策略效果
  assert_true(sampled_data.length() >= 2) // 至少采样critical级别数据
  assert_true(sampled_data.length() <= telemetry_data.length())
  
  // 验证采样数据质量
  let mut critical_count = 0
  let mut total_latency = 0.0
  
  for data in sampled_data {
    if data.priority == "critical" {
      critical_count = critical_count + 1
    }
    total_latency = total_latency + data.latency
  }
  
  assert_true(critical_count >= 1) // 确保critical数据被采样
  
  if sampled_data.length() > 0 {
    let avg_latency = total_latency / sampled_data.length().to_float()
    assert_true(avg_latency >= 20.0 && avg_latency <= 150.0)
  }
}

// 测试2: 分布式追踪链路完整性测试
test "分布式追踪链路完整性测试" {
  // 模拟分布式追踪链路
  let trace_chain = [
    { service: "gateway", span_id: "span-001", parent_id: "", operation: "request.incoming" },
    { service: "auth", span_id: "span-002", parent_id: "span-001", operation: "authenticate" },
    { service: "user-service", span_id: "span-003", parent_id: "span-002", operation: "get.user" },
    { service: "database", span_id: "span-004", parent_id: "span-003", operation: "query.user" },
    { service: "user-service", span_id: "span-005", parent_id: "span-004", operation: "process.user" },
    { service: "cache", span_id: "span-006", parent_id: "span-005", operation: "cache.result" },
    { service: "user-service", span_id: "span-007", parent_id: "span-006", operation: "response.format" },
    { service: "gateway", span_id: "span-008", parent_id: "span-007", operation: "response.outgoing" }
  ]
  
  // 验证追踪链路完整性
  let mut chain_integrity = true
  let mut root_span_found = false
  
  for i in 0..trace_chain.length() {
    let current_span = trace_chain[i]
    
    // 检查根span
    if current_span.parent_id == "" {
      if root_span_found {
        chain_integrity = false // 多个根span
        break
      }
      root_span_found = true
    }
    
    // 检查父子关系
    if current_span.parent_id != "" {
      let mut parent_found = false
      for potential_parent in trace_chain {
        if potential_parent.span_id == current_span.parent_id {
          parent_found = true
          break
        }
      }
      if not parent_found {
        chain_integrity = false // 找不到父span
        break
      }
    }
  }
  
  assert_true(chain_integrity)
  assert_true(root_span_found)
  
  // 验证追踪链路深度
  let chain_depth = trace_chain.length()
  assert_true(chain_depth >= 3 && chain_depth <= 10)
  
  // 验证关键服务存在
  let mut services_found = []
  for span in trace_chain {
    let already_exists = false
    for service in services_found {
      if service == span.service {
        already_exists = true
        break
      }
    }
    if not already_exists {
      services_found = services_found.push(span.service)
    }
  }
  
  assert_true(services_found.length() >= 3) // 至少涉及3个服务
  assert_true(services_found.includes("gateway"))
  assert_true(services_found.includes("user-service"))
}

// 测试3: 度量数据聚合精度测试
test "度量数据聚合精度测试" {
  // 模拟高精度度量数据
  let raw_metrics = [
    { timestamp: 1640995200, value: 45.123456789, tags: ["service:api", "method:GET"] },
    { timestamp: 1640995260, value: 47.987654321, tags: ["service:api", "method:GET"] },
    { timestamp: 1640995320, value: 44.234567890, tags: ["service:api", "method:POST"] },
    { timestamp: 1640995380, value: 46.789012345, tags: ["service:api", "method:GET"] },
    { timestamp: 1640995440, value: 48.345678901, tags: ["service:api", "method:POST"] }
  ]
  
  // 按标签分组聚合
  let mut get_metrics = []
  let mut post_metrics = []
  
  for metric in raw_metrics {
    if metric.tags.includes("method:GET") {
      get_metrics = get_metrics.push(metric.value)
    } else if metric.tags.includes("method:POST") {
      post_metrics = post_metrics.push(metric.value)
    }
  }
  
  // 计算高精度聚合统计
  let calculate_stats = fn(values: Array[Float]) -> (Float, Float, Float, Float) {
    if values.length() == 0 {
      return (0.0, 0.0, 0.0, 0.0)
    }
    
    let mut sum = 0.0
    let mut min_val = values[0]
    let mut max_val = values[0]
    
    for value in values {
      sum = sum + value
      if value < min_val {
        min_val = value
      }
      if value > max_val {
        max_val = value
      }
    }
    
    let mean = sum / values.length().to_float()
    (mean, min_val, max_val, sum)
  }
  
  let (get_mean, get_min, get_max, get_sum) = calculate_stats(get_metrics)
  let (post_mean, post_min, post_max, post_sum) = calculate_stats(post_metrics)
  
  // 验证聚合精度
  assert_true(get_metrics.length() == 3)
  assert_true(post_metrics.length() == 2)
  
  // 验证平均值精度（保留6位小数）
  let expected_get_mean = (45.123456789 + 47.987654321 + 46.789012345) / 3.0
  assert_true(abs(get_mean - expected_get_mean) < 0.000001)
  
  // 验证最小值和最大值
  assert_eq(get_min, 45.123456789)
  assert_eq(get_max, 47.987654321)
  
  // 验证总和精度
  let expected_get_sum = 45.123456789 + 47.987654321 + 46.789012345
  assert_true(abs(get_sum - expected_get_sum) < 0.000001)
  
  // 验证POST请求统计
  let expected_post_mean = (44.234567890 + 48.345678901) / 2.0
  assert_true(abs(post_mean - expected_post_mean) < 0.000001)
}

// 测试4: 异常情况下遥测系统韧性测试
test "异常情况下遥测系统韧性测试" {
  // 模拟各种异常情况
  let failure_scenarios = [
    { type: "network_timeout", recoverable: true, impact_score: 0.7 },
    { type: "memory_pressure", recoverable: true, impact_score: 0.8 },
    { type: "disk_full", recoverable: false, impact_score: 0.9 },
    { type: "service_unavailable", recoverable: true, impact_score: 0.6 },
    { type: "data_corruption", recoverable: false, impact_score: 0.95 }
  ]
  
  // 模拟遥测系统韧性响应
  let mut system_resilience = 100.0
  let mut active_failures = []
  let mut recovery_actions = []
  
  for scenario in failure_scenarios {
    // 模拟故障发生
    active_failures = active_failures.push(scenario.type)
    system_resilience = system_resilience - (scenario.impact_score * 20.0)
    
    // 模拟韧性响应
    if scenario.recoverable {
      let recovery_action = "auto_recovery_" + scenario.type
      recovery_actions = recovery_actions.push(recovery_action)
      system_resilience = system_resilience + 10.0 // 自动恢复部分韧性
    } else {
      let manual_action = "manual_intervention_" + scenario.type
      recovery_actions = recovery_actions.push(manual_action)
    }
  }
  
  // 验证系统韧性
  assert_true(system_resilience >= 40.0) // 韧性不应低于40%
  assert_true(active_failures.length() == failure_scenarios.length())
  assert_true(recovery_actions.length() == failure_scenarios.length())
  
  // 验证关键功能仍然可用
  let critical_functions_available = system_resilience >= 50.0
  assert_true(critical_functions_available)
  
  // 计算恢复率
  let recoverable_count = 0
  for scenario in failure_scenarios {
    if scenario.recoverable {
      recoverable_count = recoverable_count + 1
    }
  }
  
  let recovery_rate = recoverable_count.to_float() / failure_scenarios.length().to_float()
  assert_true(recovery_rate >= 0.6) // 至少60%的故障可恢复
  
  // 验证遥测数据收集能力
  let telemetry_collection_rate = system_resilience / 100.0
  assert_true(telemetry_collection_rate >= 0.4) // 至少能收集40%的遥测数据
}

// 测试5: 高并发场景下性能隔离测试
test "高并发场景下性能隔离测试" {
  // 模拟多个并发工作负载
  let workloads = [
    { name: "critical", priority: 1, resource_share: 0.5, expected_throughput: 1000 },
    { name: "normal", priority: 2, resource_share: 0.3, expected_throughput: 600 },
    { name: "background", priority: 3, resource_share: 0.2, expected_throughput: 400 }
  ]
  
  // 模拟资源分配和性能隔离
  let total_resources = 1000.0
  let mut allocated_resources = []
  let mut actual_throughput = []
  
  for workload in workloads {
    let allocated = total_resources * workload.resource_share
    allocated_resources = allocated_resources.push(allocated)
    
    // 模拟实际吞吐量（考虑资源竞争）
    let competition_factor = 1.0 - (workloads.length() - 1) * 0.05
    let actual = workload.expected_throughput * workload.resource_share * competition_factor
    actual_throughput = actual_throughput.push(actual.to_int())
  }
  
  // 验证资源分配
  let mut total_allocated = 0.0
  for resource in allocated_resources {
    total_allocated = total_allocated + resource
  }
  
  assert_true(total_allocated <= total_resources) // 分配资源不超过总量
  
  // 验证性能隔离效果
  assert_eq(actual_throughput[0], 475) // critical工作负载
  assert_eq(actual_throughput[1], 285)  // normal工作负载
  assert_eq(actual_throughput[2], 190)  // background工作负载
  
  // 验证优先级保证
  let critical_throughput_ratio = actual_throughput[0].to_float() / workloads[0].expected_throughput.to_float()
  let background_throughput_ratio = actual_throughput[2].to_float() / workloads[2].expected_throughput.to_float()
  
  assert_true(critical_throughput_ratio > background_throughput_ratio) // 高优先级获得更好性能
  
  // 验证资源隔离
  let critical_resource_share = allocated_resources[0] / total_resources
  assert_true(critical_resource_share >= 0.45) // critical工作负载获得至少45%资源
  
  // 模拟突发负载下的隔离效果
  let burst_load_factor = 1.5
  let mut burst_throughput = []
  
  for i in 0..workloads.length() {
    let base_throughput = actual_throughput[i].to_float()
    let isolation_protection = workloads[i].priority == 1 ? 0.9 : 0.6
    let burst = base_throughput * burst_load_factor * isolation_protection
    burst_throughput = burst_throughput.push(burst.to_int())
  }
  
  // 验证突发负载下的性能隔离
  assert_true(burst_throughput[0] > actual_throughput[0]) // critical工作负载仍能提升性能
  assert_true(burst_throughput[0] > burst_throughput[2]) // critical工作负载性能仍然最好
}

// 测试6: 遥测数据生命周期管理测试
test "遥测数据生命周期管理测试" {
  // 模拟遥测数据生命周期阶段
  let data_lifecycle = [
    { stage: "creation", age_hours: 0, retention_policy: "keep", access_frequency: "high" },
    { stage: "active", age_hours: 2, retention_policy: "keep", access_frequency: "high" },
    { stage: "warm", age_hours: 24, retention_policy: "keep", access_frequency: "medium" },
    { stage: "cold", age_hours: 168, retention_policy: "compress", access_frequency: "low" },
    { stage: "archival", age_hours: 720, retention_policy: "archive", access_frequency: "very_low" },
    { stage: "expired", age_hours: 2160, retention_policy: "delete", access_frequency: "none" }
  ]
  
  // 模拟生命周期管理策略
  let mut storage_costs = []
  let mut data_accessibility = []
  
  for stage in data_lifecycle {
    let cost_per_gb = match stage.stage {
      "creation" => 10.0,
      "active" => 8.0,
      "warm" => 4.0,
      "cold" => 1.0,
      "archival" => 0.1,
      "expired" => 0.0,
      _ => 0.0
    }
    
    let accessibility = match stage.access_frequency {
      "high" => 100,
      "medium" => 80,
      "low" => 40,
      "very_low" => 10,
      "none" => 0,
      _ => 0
    }
    
    storage_costs = storage_costs.push(cost_per_gb)
    data_accessibility = data_accessibility.push(accessibility)
  }
  
  // 验证生命周期成本递减
  for i in 1..storage_costs.length() {
    assert_true(storage_costs[i] <= storage_costs[i-1])
  }
  
  // 验证可访问性递减
  for i in 1..data_accessibility.length() {
    assert_true(data_accessibility[i] <= data_accessibility[i-1])
  }
  
  // 验证保留策略执行
  let mut keep_count = 0
  let mut compress_count = 0
  let mut archive_count = 0
  let mut delete_count = 0
  
  for stage in data_lifecycle {
    match stage.retention_policy {
      "keep" => keep_count = keep_count + 1
      "compress" => compress_count = compress_count + 1
      "archive" => archive_count = archive_count + 1
      "delete" => delete_count = delete_count + 1
      _ => ()
    }
  }
  
  assert_eq(keep_count, 3)
  assert_eq(compress_count, 1)
  assert_eq(archive_count, 1)
  assert_eq(delete_count, 1)
  
  // 计算总体存储成本节省
  let initial_cost = storage_costs[0]
  let final_cost = storage_costs[storage_costs.length() - 2] // 排除已删除数据
  let cost_reduction = (initial_cost - final_cost) / initial_cost * 100.0
  
  assert_true(cost_reduction >= 90.0) // 至少90%的成本节省
}

// 测试7: 跨服务遥测数据一致性测试
test "跨服务遥测数据一致性测试" {
  // 模拟跨服务遥测数据
  let service_telemetry = [
    { 
      service: "api-gateway", 
      trace_id: "trace-123", 
      timestamp: 1640995200,
      metrics: [("request_count", 100), ("error_rate", 0.02)],
      spans: [("gateway-in", 10), ("gateway-out", 15)]
    },
    { 
      service: "auth-service", 
      trace_id: "trace-123", 
      timestamp: 1640995205,
      metrics: [("request_count", 95), ("error_rate", 0.01)],
      spans: [("auth-validate", 20), ("auth-respond", 25)]
    },
    { 
      service: "user-service", 
      trace_id: "trace-123", 
      timestamp: 1640995208,
      metrics: [("request_count", 90), ("error_rate", 0.03)],
      spans: [("user-fetch", 30), ("user-process", 35)]
    }
  ]
  
  // 验证跨服务数据一致性
  let mut trace_consistency = true
  let base_trace_id = service_telemetry[0].trace_id
  
  // 检查trace ID一致性
  for telemetry in service_telemetry {
    if telemetry.trace_id != base_trace_id {
      trace_consistency = false
      break
    }
  }
  
  assert_true(trace_consistency)
  
  // 检查时间戳合理性
  let mut timestamps = []
  for telemetry in service_telemetry {
    timestamps = timestamps.push(telemetry.timestamp)
  }
  
  // 验证时间戳递增
  for i in 1..timestamps.length() {
    assert_true(timestamps[i] >= timestamps[i-1])
  }
  
  // 验证请求计数一致性（考虑服务间调用损耗）
  let api_gateway_requests = service_telemetry[0].metrics[0].1
  let auth_service_requests = service_telemetry[1].metrics[0].1
  let user_service_requests = service_telemetry[2].metrics[0].1
  
  assert_true(auth_service_requests <= api_gateway_requests)
  assert_true(user_service_requests <= auth_service_requests)
  
  // 检查错误率一致性
  let error_rates = []
  for telemetry in service_telemetry {
    error_rates = error_rates.push(telemetry.metrics[1].1)
  }
  
  let avg_error_rate = (error_rates[0] + error_rates[1] + error_rates[2]) / 3.0
  assert_true(avg_error_rate <= 0.05) // 平均错误率不应超过5%
  
  // 验证span持续时间合理性
  let mut total_durations = []
  for telemetry in service_telemetry {
    let mut service_total = 0
    for span in telemetry.spans {
      service_total = service_total + span.1
    }
    total_durations = total_durations.push(service_total)
  }
  
  // 验证总持续时间合理性
  let overall_duration = total_durations[0] + total_durations[1] + total_durations[2]
  assert_true(overall_duration >= 50 && overall_duration <= 200) // 合理的总持续时间范围
  
  // 检查服务间调用链完整性
  let mut service_chain = []
  for telemetry in service_telemetry {
    service_chain = service_chain.push(telemetry.service)
  }
  
  assert_true(service_chain.includes("api-gateway"))
  assert_true(service_chain.includes("auth-service"))
  assert_true(service_chain.includes("user-service"))
}

// 测试8: 遥测系统资源限制自适应测试
test "遥测系统资源限制自适应测试" {
  // 模拟不同资源限制场景
  let resource_scenarios = [
    { 
      scenario: "normal", 
      cpu_limit: 80, 
      memory_limit: 70, 
      disk_limit: 60,
      expected_sampling_rate: 1.0,
      expected_batch_size: 100
    },
    { 
      scenario: "moderate_pressure", 
      cpu_limit: 60, 
      memory_limit: 50, 
      disk_limit: 40,
      expected_sampling_rate: 0.8,
      expected_batch_size: 80
    },
    { 
      scenario: "high_pressure", 
      cpu_limit: 40, 
      memory_limit: 30, 
      disk_limit: 20,
      expected_sampling_rate: 0.5,
      expected_batch_size: 50
    },
    { 
      scenario: "extreme_pressure", 
      cpu_limit: 20, 
      memory_limit: 15, 
      disk_limit: 10,
      expected_sampling_rate: 0.2,
      expected_batch_size: 20
    }
  ]
  
  // 模拟自适应调整策略
  let mut adaptation_results = []
  
  for scenario in resource_scenarios {
    // 计算资源压力指数
    let pressure_index = (100.0 - scenario.cpu_limit.to_float()) / 100.0 * 0.4 +
                        (100.0 - scenario.memory_limit.to_float()) / 100.0 * 0.4 +
                        (100.0 - scenario.disk_limit.to_float()) / 100.0 * 0.2
    
    // 自适应调整采样率
    let adaptive_sampling_rate = 1.0 - (pressure_index * 0.8)
    let final_sampling_rate = max(0.1, adaptive_sampling_rate)
    
    // 自适应调整批处理大小
    let adaptive_batch_size = (100.0 * final_sampling_rate).to_int()
    let final_batch_size = max(10, adaptive_batch_size)
    
    adaptation_results = adaptation_results.push({
      scenario: scenario.scenario,
      pressure_index: pressure_index,
      sampling_rate: final_sampling_rate,
      batch_size: final_batch_size
    })
  }
  
  // 验证自适应调整效果
  for i in 0..adaptation_results.length() {
    let result = adaptation_results[i]
    let original = resource_scenarios[i]
    
    // 验证采样率调整
    assert_true(abs(result.sampling_rate - original.expected_sampling_rate) < 0.1)
    
    // 验证批处理大小调整
    assert_true(abs(result.batch_size - original.expected_batch_size) < 20)
    
    // 验证压力指数合理性
    assert_true(result.pressure_index >= 0.0 && result.pressure_index <= 1.0)
  }
  
  // 验证压力递增时采样率递减
  for i in 1..adaptation_results.length() {
    assert_true(adaptation_results[i].sampling_rate <= adaptation_results[i-1].sampling_rate)
    assert_true(adaptation_results[i].batch_size <= adaptation_results[i-1].batch_size)
  }
  
  // 验证极端情况下系统仍能运行
  let extreme_scenario = adaptation_results[adaptation_results.length() - 1]
  assert_true(extreme_scenario.sampling_rate >= 0.1) // 最小10%采样率
  assert_true(extreme_scenario.batch_size >= 10)     // 最小批次大小
  
  // 计算自适应效果
  let normal_throughput = adaptation_results[0].sampling_rate * adaptation_results[0].batch_size.to_float()
  let extreme_throughput = adaptation_results[adaptation_results.length() - 1].sampling_rate * 
                          adaptation_results[adaptation_results.length() - 1].batch_size.to_float()
  
  let throughput_ratio = extreme_throughput / normal_throughput
  assert_true(throughput_ratio >= 0.04) // 极端情况下至少保持4%的吞吐量
}

// 测试9: 遥测数据安全与隐私保护测试
test "遥测数据安全与隐私保护测试" {
  // 模拟敏感遥测数据
  let sensitive_data = [
    { 
      field: "user_id", 
      value: "user-12345", 
      sensitivity: "high", 
      pii_type: "identifier",
      should_mask: true
    },
    { 
      field: "ip_address", 
      value: "192.168.1.100", 
      sensitivity: "medium", 
      pii_type: "network",
      should_mask: true
    },
    { 
      field: "email", 
      value: "user@example.com", 
      sensitivity: "high", 
      pii_type: "contact",
      should_mask: true
    },
    { 
      field: "response_time", 
      value: 245.6, 
      sensitivity: "low", 
      pii_type: "none",
      should_mask: false
    },
    { 
      field: "error_message", 
      value: "Database connection failed for user-12345", 
      sensitivity: "medium", 
      pii_type: "mixed",
      should_mask: true
    }
  ]
  
  // 模拟数据脱敏处理
  let mut masked_data = []
  
  for data in sensitive_data {
    let masked_value = if data.should_mask {
      match data.pii_type {
        "identifier" => "***-*****"
        "network" => "***.***.***.***"
        "contact" => "***@***.***"
        "mixed" => "Database connection failed for ***-*****"
        _ => data.value.to_string()
      }
    } else {
      data.value.to_string()
    }
    
    masked_data = masked_data.push({
      field: data.field,
      original_value: data.value.to_string(),
      masked_value: masked_value,
      sensitivity: data.sensitivity,
      pii_type: data.pii_type
    })
  }
  
  // 验证数据脱敏效果
  assert_eq(masked_data.length(), sensitive_data.length())
  
  // 验证高敏感度数据已脱敏
  for data in masked_data {
    if data.sensitivity == "high" {
      assert_true(data.masked_value.includes("***"))
      assert_not_eq(data.masked_value, data.original_value)
    }
  }
  
  // 验证低敏感度数据未脱敏
  let low_sensitivity_data = masked_data.filter(fn(d) { d.sensitivity == "low" })
  assert_eq(low_sensitivity_data.length(), 1)
  assert_eq(low_sensitivity_data[0].masked_value, low_sensitivity_data[0].original_value)
  
  // 验证PII类型识别准确性
  let pii_types = []
  for data in masked_data {
    if not pii_types.includes(data.pii_type) {
      pii_types = pii_types.push(data.pii_type)
    }
  }
  
  assert_true(pii_types.includes("identifier"))
  assert_true(pii_types.includes("network"))
  assert_true(pii_types.includes("contact"))
  assert_true(pii_types.includes("none"))
  assert_true(pii_types.includes("mixed"))
  
  // 计算脱敏覆盖率
  let masked_count = masked_data.filter(fn(d) { d.masked_value.includes("***") }).length()
  let coverage_rate = masked_count.to_float() / masked_data.length().to_float()
  
  assert_true(coverage_rate >= 0.8) // 至少80%的敏感数据被脱敏
  
  // 验证数据格式保留
  let email_data = masked_data.filter(fn(d) { d.field == "email" })[0]
  assert_true(email_data.masked_value.includes("@"))
  assert_true(email_data.masked_value.includes("."))
  
  let ip_data = masked_data.filter(fn(d) { d.field == "ip_address" })[0]
  assert_true(ip_data.masked_value.split(".").length() == 4)
}

// 测试10: 遥测系统可观测性自监控测试
test "遥测系统可观测性自监控测试" {
  // 模拟遥测系统自身监控指标
  let self_metrics = [
    { metric: "telemetry.collector.uptime", value: 86400, unit: "seconds", health_threshold: 3600 },
    { metric: "telemetry.collector.cpu_usage", value: 45.2, unit: "percent", health_threshold: 80.0 },
    { metric: "telemetry.collector.memory_usage", value: 67.8, unit: "percent", health_threshold: 85.0 },
    { metric: "telemetry.collector.disk_usage", value: 34.5, unit: "percent", health_threshold: 90.0 },
    { metric: "telemetry.collector.throughput", value: 1250, unit: "ops/sec", health_threshold: 500 },
    { metric: "telemetry.collector.error_rate", value: 0.02, unit: "percent", health_threshold: 5.0 },
    { metric: "telemetry.collector.latency_p99", value: 125.5, unit: "ms", health_threshold: 500.0 },
    { metric: "telemetry.collector.queue_depth", value: 45, unit: "count", health_threshold: 1000 }
  ]
  
  // 评估系统健康状态
  let mut health_scores = []
  let mut unhealthy_metrics = []
  
  for metric in self_metrics {
    let is_healthy = match metric.metric {
      "telemetry.collector.uptime" => metric.value >= metric.health_threshold,
      "telemetry.collector.cpu_usage" => metric.value <= metric.health_threshold,
      "telemetry.collector.memory_usage" => metric.value <= metric.health_threshold,
      "telemetry.collector.disk_usage" => metric.value <= metric.health_threshold,
      "telemetry.collector.throughput" => metric.value >= metric.health_threshold,
      "telemetry.collector.error_rate" => metric.value <= metric.health_threshold,
      "telemetry.collector.latency_p99" => metric.value <= metric.health_threshold,
      "telemetry.collector.queue_depth" => metric.value <= metric.health_threshold,
      _ => true
    }
    
    let health_score = if is_healthy { 100.0 } else { 0.0 }
    health_scores = health_scores.push(health_score)
    
    if not is_healthy {
      unhealthy_metrics = unhealthy_metrics.push(metric.metric)
    }
  }
  
  // 计算整体健康评分
  let mut total_health = 0.0
  for score in health_scores {
    total_health = total_health + score
  }
  let overall_health = total_health / health_scores.length().to_float()
  
  // 验证系统健康状态
  assert_true(overall_health >= 80.0) // 整体健康评分应不低于80%
  assert_eq(unhealthy_metrics.length(), 0) // 所有指标都应健康
  
  // 验证关键指标健康
  let uptime_metric = self_metrics.filter(fn(m) { m.metric == "telemetry.collector.uptime" })[0]
  assert_true(uptime_metric.value >= uptime_metric.health_threshold)
  
  let throughput_metric = self_metrics.filter(fn(m) { m.metric == "telemetry.collector.throughput" })[0]
  assert_true(throughput_metric.value >= throughput_metric.health_threshold)
  
  let error_rate_metric = self_metrics.filter(fn(m) { m.metric == "telemetry.collector.error_rate" })[0]
  assert_true(error_rate_metric.value <= error_rate_metric.health_threshold)
  
  // 验证资源使用合理性
  let resource_metrics = self_metrics.filter(fn(m) { 
    m.metric.includes("cpu_usage") || m.metric.includes("memory_usage") || m.metric.includes("disk_usage")
  })
  
  for metric in resource_metrics {
    assert_true(metric.value >= 0.0 && metric.value <= 100.0)
    assert_true(metric.value <= metric.health_threshold) // 资源使用率应在健康阈值内
  }
  
  // 验证性能指标
  let latency_metric = self_metrics.filter(fn(m) { m.metric == "telemetry.collector.latency_p99" })[0]
  assert_true(latency_metric.value >= 0.0)
  assert_true(latency_metric.value <= latency_metric.health_threshold)
  
  // 计算系统效率指标
  let cpu_efficiency = (100.0 - self_metrics[1].value) / 100.0 * throughput_metric.value / 1000.0
  let memory_efficiency = (100.0 - self_metrics[2].value) / 100.0 * throughput_metric.value / 1000.0
  
  assert_true(cpu_efficiency >= 0.5) // CPU效率应不低于50%
  assert_true(memory_efficiency >= 0.3) // 内存效率应不低于30%
  
  // 验证系统稳定性
  let stability_score = (overall_health + cpu_efficiency * 50 + memory_efficiency * 50) / 200.0 * 100.0
  assert_true(stability_score >= 75.0) // 系统稳定性评分应不低于75%
}