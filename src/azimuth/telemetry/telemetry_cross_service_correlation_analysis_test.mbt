// 遥测数据跨服务关联分析测试用例
// 测试跨服务遥测数据的关联分析和依赖关系识别

test "telemetry_cross_service_trace_correlation" {
  // 测试跨服务链路追踪关联
  
  let distributed_traces = [
    ("trace_001", "gateway", "span_001", "entry"),
    ("trace_001", "auth_service", "span_002", "auth"),
    ("trace_001", "user_service", "span_003", "user_lookup"),
    ("trace_001", "order_service", "span_004", "order_process"),
    ("trace_001", "payment_service", "span_005", "payment"),
    ("trace_002", "gateway", "span_006", "entry"),
    ("trace_002", "product_service", "span_007", "product_search")
  ]
  
  // 按trace_id分组
  let mut trace_groups = {}
  let mut i = 0
  while i < distributed_traces.length() {
    let (trace_id, service_name, span_id, operation) = distributed_traces[i]
    
    // 添加到trace组
    if !trace_groups.contains_key(trace_id) {
      trace_groups[trace_id] = []
    }
    trace_groups[trace_id].push((service_name, span_id, operation))
    
    i = i + 1
  }
  
  // 验证trace分组
  assert_eq(trace_groups.size(), 2)  // 应该有2个不同的trace
  assert_eq(trace_groups.contains_key("trace_001"), true)
  assert_eq(trace_groups.contains_key("trace_002"), true)
  assert_eq(trace_groups["trace_001"].length(), 5)  // trace_001有5个span
  assert_eq(trace_groups["trace_002"].length(), 2)  // trace_002有2个span
  
  // 分析服务调用链
  let mut service_chains = []
  let trace_ids = ["trace_001", "trace_002"]
  i = 0
  while i < trace_ids.length() {
    let trace_id = trace_ids[i]
    let spans = trace_groups[trace_id]
    
    // 构建服务调用链
    let mut chain = []
    let mut j = 0
    while j < spans.length() {
      chain.push(spans[j].0)  // 只取服务名
      j = j + 1
    }
    
    service_chains.push(chain)
    i = i + 1
  }
  
  // 验证服务调用链
  assert_eq(service_chains[0], ["gateway", "auth_service", "user_service", "order_service", "payment_service"])
  assert_eq(service_chains[1], ["gateway", "product_service"])
  
  // 分析服务依赖关系
  let mut service_dependencies = {}
  i = 0
  while i < service_chains.length() {
    let chain = service_chains[i]
    
    // 构建依赖关系：前一个服务依赖后一个服务
    let mut j = 0
    while j < chain.length() - 1 {
      let from_service = chain[j]
      let to_service = chain[j + 1]
      
      let dependency_key = from_service + "->" + to_service
      if !service_dependencies.contains_key(dependency_key) {
        service_dependencies[dependency_key] = 0
      }
      service_dependencies[dependency_key] = service_dependencies[dependency_key] + 1
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证服务依赖关系
  assert_eq(service_dependencies.size(), 6)  // 应该有6个依赖关系
  assert_eq(service_dependencies["gateway->auth_service"], 1)
  assert_eq(service_dependencies["gateway->product_service"], 1)
  assert_eq(service_dependencies["auth_service->user_service"], 1)
  assert_eq(service_dependencies["user_service->order_service"], 1)
  assert_eq(service_dependencies["order_service->payment_service"], 1)
}

test "telemetry_cross_service_error_propagation" {
  // 测试跨服务错误传播分析
  
  let error_scenarios = [
    ("error_trace_001", "gateway", "timeout", "auth_service_timeout"),
    ("error_trace_001", "auth_service", "connection_refused", "database_unavailable"),
    ("error_trace_001", "user_service", "dependency_failure", "auth_service_error"),
    ("error_trace_001", "order_service", "business_error", "invalid_user_data"),
    ("error_trace_002", "gateway", "rate_limit", "too_many_requests"),
    ("error_trace_002", "cache_service", "cache_miss", "key_not_found"),
    ("error_trace_002", "product_service", "dependency_failure", "cache_unavailable")
  ]
  
  // 分析错误传播路径
  let mut error_propagation_paths = {}
  let mut i = 0
  while i < error_scenarios.length() {
    let (trace_id, service_name, error_type, error_message) = error_scenarios[i]
    
    // 按trace_id分组错误
    if !error_propagation_paths.contains_key(trace_id) {
      error_propagation_paths[trace_id] = []
    }
    error_propagation_paths[trace_id].push((service_name, error_type, error_message))
    
    i = i + 1
  }
  
  // 验证错误传播分组
  assert_eq(error_propagation_paths.size(), 2)
  assert_eq(error_propagation_paths["error_trace_001"].length(), 4)
  assert_eq(error_propagation_paths["error_trace_002"].length(), 3)
  
  // 分析错误根因
  let mut root_causes = {}
  let trace_ids = ["error_trace_001", "error_trace_002"]
  i = 0
  while i < trace_ids.length() {
    let trace_id = trace_ids[i]
    let errors = error_propagation_paths[trace_id]
    
    // 简化根因分析：寻找最底层的错误
    let mut root_cause = ""
    let mut j = 0
    while j < errors.length() {
      let (_, _, error_message) = errors[j]
      
      // 识别可能的根因
      if error_message.contains("database") || 
         error_message.contains("unavailable") ||
         error_message.contains("too_many_requests") {
        root_cause = error_message
      }
      
      j = j + 1
    }
    
    root_causes[trace_id] = root_cause
    i = i + 1
  }
  
  // 验证根因分析
  assert_eq(root_causes["error_trace_001"], "database_unavailable")
  assert_eq(root_causes["error_trace_002"], "too_many_requests")
  
  // 计算错误影响范围
  let mut error_impact = {}
  i = 0
  while i < error_scenarios.length() {
    let (_, service_name, error_type, _) = error_scenarios[i]
    
    let impact_key = service_name + ":" + error_type
    if !error_impact.contains_key(impact_key) {
      error_impact[impact_key] = 0
    }
    error_impact[impact_key] = error_impact[impact_key] + 1
    
    i = i + 1
  }
  
  // 验证错误影响统计
  assert_eq(error_impact.size(), 7)  // 7种不同的错误类型
  assert_eq(error_impact["gateway:timeout"], 1)
  assert_eq(error_impact["gateway:rate_limit"], 1)
  assert_eq(error_impact["auth_service:connection_refused"], 1)
  assert_eq(error_impact["user_service:dependency_failure"], 1)
  assert_eq(error_impact["order_service:business_error"], 1)
  assert_eq(error_impact["cache_service:cache_miss"], 1)
  assert_eq(error_impact["product_service:dependency_failure"], 1)
}

test "telemetry_cross_service_performance_impact" {
  // 测试跨服务性能影响分析
  
  let performance_metrics = [
    ("perf_trace_001", "gateway", 50, 100),
    ("perf_trace_001", "auth_service", 200, 300),
    ("perf_trace_001", "user_service", 150, 250),
    ("perf_trace_001", "order_service", 300, 500),
    ("perf_trace_001", "payment_service", 500, 800),
    ("perf_trace_002", "gateway", 30, 60),
    ("perf_trace_002", "product_service", 80, 120),
    ("perf_trace_002", "inventory_service", 100, 180)
  ]
  
  // 按trace分组性能数据
  let mut performance_by_trace = {}
  let mut i = 0
  while i < performance_metrics.length() {
    let (trace_id, service_name, duration, cpu_usage) = performance_metrics[i]
    
    if !performance_by_trace.contains_key(trace_id) {
      performance_by_trace[trace_id] = []
    }
    performance_by_trace[trace_id].push((service_name, duration, cpu_usage))
    
    i = i + 1
  }
  
  // 分析每个trace的性能瓶颈
  let mut bottlenecks = {}
  let trace_ids = ["perf_trace_001", "perf_trace_002"]
  i = 0
  while i < trace_ids.length() {
    let trace_id = trace_ids[i]
    let metrics = performance_by_trace[trace_id]
    
    // 找出耗时最长的服务
    let mut max_duration = 0
    let mut bottleneck_service = ""
    let mut total_duration = 0
    let mut total_cpu = 0
    
    let mut j = 0
    while j < metrics.length() {
      let (service_name, duration, cpu_usage) = metrics[j]
      
      total_duration = total_duration + duration
      total_cpu = total_cpu + cpu_usage
      
      if duration > max_duration {
        max_duration = duration
        bottleneck_service = service_name
      }
      
      j = j + 1
    }
    
    bottlenecks[trace_id] = (bottleneck_service, max_duration, total_duration, total_cpu)
    
    i = i + 1
  }
  
  // 验证性能瓶颈分析
  assert_eq(bottlenecks["perf_trace_001"].0, "payment_service")  // 最耗时500ms
  assert_eq(bottlenecks["perf_trace_001"].1, 500)
  assert_eq(bottlenecks["perf_trace_001"].2, 1200)  // 总耗时1200ms
  assert_eq(bottlenecks["perf_trace_001"].3, 1950)  // 总CPU 1950
  
  assert_eq(bottlenecks["perf_trace_002"].0, "inventory_service")  // 最耗时100ms
  assert_eq(bottlenecks["perf_trace_002"].1, 100)
  assert_eq(bottlenecks["perf_trace_002"].2, 210)   // 总耗时210ms
  assert_eq(bottlenecks["perf_trace_002"].3, 360)   // 总CPU 360
  
  // 计算服务性能影响系数
  let mut service_impact_scores = {}
  i = 0
  while i < performance_metrics.length() {
    let (_, service_name, duration, cpu_usage) = performance_metrics[i]
    
    // 简单的性能影响分数：持续时间 + CPU使用率/10
    let impact_score = duration.to_double() + (cpu_usage.to_double() / 10.0)
    
    if !service_impact_scores.contains_key(service_name) {
      service_impact_scores[service_name] = 0.0
    }
    service_impact_scores[service_name] = service_impact_scores[service_name] + impact_score
    
    i = i + 1
  }
  
  // 验证性能影响分数
  assert_eq(service_impact_scores["payment_service"], 580.0)    // 500 + 800/10
  assert_eq(service_impact_scores["order_service"], 350.0)      // 300 + 500/10
  assert_eq(service_impact_scores["auth_service"], 230.0)       // 200 + 300/10
  assert_eq(service_impact_scores["user_service"], 175.0)       // 150 + 250/10
  assert_eq(service_impact_scores["inventory_service"], 118.0)  // 100 + 180/10
}

test "telemetry_cross_service_resource_sharing" {
  // 测试跨服务资源共享分析
  
  let shared_resources = [
    ("shared_db", "user_service", 100, 0.3),
    ("shared_db", "order_service", 150, 0.4),
    ("shared_db", "product_service", 80, 0.25),
    ("shared_cache", "user_service", 50, 0.2),
    ("shared_cache", "product_service", 70, 0.3),
    ("shared_queue", "order_service", 200, 0.6),
    ("shared_queue", "notification_service", 100, 0.3),
    ("shared_storage", "user_service", 500, 0.1),
    ("shared_storage", "order_service", 800, 0.15)
  ]
  
  // 按资源分组使用情况
  let mut resource_usage = {}
  let mut i = 0
  while i < shared_resources.length() {
    let (resource_name, service_name, usage_amount, usage_ratio) = shared_resources[i]
    
    if !resource_usage.contains_key(resource_name) {
      resource_usage[resource_name] = []
    }
    resource_usage[resource_name].push((service_name, usage_amount, usage_ratio))
    
    i = i + 1
  }
  
  // 分析资源竞争情况
  let mut resource_competition = {}
  let resource_names = ["shared_db", "shared_cache", "shared_queue", "shared_storage"]
  i = 0
  while i < resource_names.length() {
    let resource_name = resource_names[i]
    let usages = resource_usage[resource_name]
    
    // 计算总使用率和竞争程度
    let mut total_usage_ratio = 0.0
    let mut max_usage_ratio = 0.0
    let mut competing_services = 0
    
    let mut j = 0
    while j < usages.length() {
      let (_, _, usage_ratio) = usages[j]
      
      total_usage_ratio = total_usage_ratio + usage_ratio
      if usage_ratio > max_usage_ratio {
        max_usage_ratio = usage_ratio
      }
      competing_services = competing_services + 1
      
      j = j + 1
    }
    
    // 计算竞争指数
    let competition_index = if total_usage_ratio > 1.0 {
      (total_usage_ratio - 1.0) * competing_services.to_double()
    } else {
      0.0
    }
    
    resource_competition[resource_name] = (competing_services, total_usage_ratio, max_usage_ratio, competition_index)
    
    i = i + 1
  }
  
  // 验证资源竞争分析
  assert_eq(resource_competition["shared_db"].0, 3)      // 3个服务使用
  assert_eq(resource_competition["shared_db"].1, 0.95)   // 总使用率95%
  assert_eq(resource_competition["shared_db"].2, 0.4)    // 最大使用率40%
  assert_eq(resource_competition["shared_db"].3, 0.0)    // 无竞争
  
  assert_eq(resource_competition["shared_queue"].0, 2)   // 2个服务使用
  assert_eq(resource_competition["shared_queue"].1, 0.9) // 总使用率90%
  assert_eq(resource_competition["shared_queue"].2, 0.6) // 最大使用率60%
  assert_eq(resource_competition["shared_queue"].3, 0.0) // 无竞争
  
  // 识别资源依赖关系
  let mut service_dependencies = {}
  i = 0
  while i < shared_resources.length() {
    let (resource_name, service_name, _, _) = shared_resources[i]
    
    if !service_dependencies.contains_key(service_name) {
      service_dependencies[service_name] = []
    }
    service_dependencies[service_name].push(resource_name)
    
    i = i + 1
  }
  
  // 验证服务依赖关系
  assert_eq(service_dependencies["user_service"].length(), 3)  // 依赖3个共享资源
  assert_eq(service_dependencies["order_service"].length(), 3)  // 依赖3个共享资源
  assert_eq(service_dependencies["product_service"].length(), 2) // 依赖2个共享资源
  
  // 验证具体依赖
  assert_eq(service_dependencies["user_service"].contains("shared_db"), true)
  assert_eq(service_dependencies["user_service"].contains("shared_cache"), true)
  assert_eq(service_dependencies["user_service"].contains("shared_storage"), true)
}

test "telemetry_cross_service_cascading_failure" {
  // 测试跨服务级联故障分析
  
  let failure_chain = [
    ("failure_001", "database", "connection_failure", 0),
    ("failure_001", "auth_service", "dependency_failure", 1),
    ("failure_001", "user_service", "auth_unavailable", 2),
    ("failure_001", "order_service", "user_service_unavailable", 3),
    ("failure_001", "payment_service", "order_service_unavailable", 4),
    ("failure_002", "cache_cluster", "memory_exhaustion", 0),
    ("failure_002", "product_service", "cache_unavailable", 1),
    ("failure_002", "search_service", "product_service_unavailable", 2)
  ]
  
  // 构建故障传播链
  let mut failure_chains = {}
  let mut i = 0
  while i < failure_chain.length() {
    let (failure_id, service_name, failure_type, chain_level) = failure_chain[i]
    
    if !failure_chains.contains_key(failure_id) {
      failure_chains[failure_id] = []
    }
    failure_chains[failure_id].push((chain_level, service_name, failure_type))
    
    i = i + 1
  }
  
  // 分析故障传播深度
  let mut failure_depth = {}
  let failure_ids = ["failure_001", "failure_002"]
  i = 0
  while i < failure_ids.length() {
    let failure_id = failure_ids[i]
    let chain = failure_chains[failure_id]
    
    // 找出最大链路层级
    let mut max_level = 0
    let mut root_cause = ""
    
    let mut j = 0
    while j < chain.length() {
      let (level, service_name, failure_type) = chain[j]
      
      if level > max_level {
        max_level = level
      }
      
      if level == 0 {
        root_cause = service_name + ":" + failure_type
      }
      
      j = j + 1
    }
    
    failure_depth[failure_id] = (max_level, root_cause)
    
    i = i + 1
  }
  
  // 验证故障深度分析
  assert_eq(failure_depth["failure_001"].0, 4)  // 4级传播
  assert_eq(failure_depth["failure_001"].1, "database:connection_failure")
  assert_eq(failure_depth["failure_002"].0, 2)  // 2级传播
  assert_eq(failure_depth["failure_002"].1, "cache_cluster:memory_exhaustion")
  
  // 计算故障影响范围
  let mut failure_impact = {}
  i = 0
  while i < failure_chain.length() {
    let (failure_id, service_name, _, _) = failure_chain[i]
    
    if !failure_impact.contains_key(failure_id) {
      failure_impact[failure_id] = []
    }
    failure_impact[failure_id].push(service_name)
    
    i = i + 1
  }
  
  // 验证故障影响范围
  assert_eq(failure_impact["failure_001"].length(), 5)  // 影响到5个服务
  assert_eq(failure_impact["failure_002"].length(), 3)  // 影响到3个服务
  
  // 识别关键节点（单点故障）
  let mut critical_nodes = {}
  i = 0
  while i < failure_chain.length() {
    let (_, service_name, _, chain_level) = failure_chain[i]
    
    // 中间节点（非根节点也非叶子节点）是关键节点
    if chain_level > 0 && chain_level < 4 {
      if !critical_nodes.contains_key(service_name) {
        critical_nodes[service_name] = 0
      }
      critical_nodes[service_name] = critical_nodes[service_name] + 1
    }
    
    i = i + 1
  }
  
  // 验证关键节点识别
  assert_eq(critical_nodes["auth_service"], 1)
  assert_eq(critical_nodes["user_service"], 1)
  assert_eq(critical_nodes["order_service"], 1)
  assert_eq(critical_nodes["product_service"], 1)
}

test "telemetry_cross_service_latency_accumulation" {
  // 测试跨服务延迟累积分析
  
  let latency_chains = [
    ("chain_001", "gateway", 10),
    ("chain_001", "auth_service", 50),
    ("chain_001", "user_service", 30),
    ("chain_001", "order_service", 100),
    ("chain_001", "payment_service", 200),
    ("chain_002", "gateway", 8),
    ("chain_002", "product_service", 40),
    ("chain_002", "inventory_service", 60),
    ("chain_002", "recommendation_service", 120)
  ]
  
  // 按链路分组延迟数据
  let mut latency_by_chain = {}
  let mut i = 0
  while i < latency_chains.length() {
    let (chain_id, service_name, latency) = latency_chains[i]
    
    if !latency_by_chain.contains_key(chain_id) {
      latency_by_chain[chain_id] = []
    }
    latency_by_chain[chain_id].push((service_name, latency))
    
    i = i + 1
  }
  
  // 分析延迟累积
  let mut latency_analysis = {}
  let chain_ids = ["chain_001", "chain_002"]
  i = 0
  while i < chain_ids.length() {
    let chain_id = chain_ids[i]
    let chain_data = latency_by_chain[chain_id]
    
    // 计算累积延迟和各服务占比
    let mut total_latency = 0
    let mut max_latency = 0
    let mut slowest_service = ""
    let mut service_percentages = {}
    
    let mut j = 0
    while j < chain_data.length() {
      let (service_name, latency) = chain_data[j]
      
      total_latency = total_latency + latency
      
      if latency > max_latency {
        max_latency = latency
        slowest_service = service_name
      }
      
      j = j + 1
    }
    
    // 计算各服务延迟占比
    j = 0
    while j < chain_data.length() {
      let (service_name, latency) = chain_data[j]
      let percentage = (latency.to_double() / total_latency.to_double()) * 100.0
      service_percentages[service_name] = percentage
      j = j + 1
    }
    
    latency_analysis[chain_id] = (total_latency, max_latency, slowest_service, service_percentages)
    
    i = i + 1
  }
  
  // 验证延迟分析
  assert_eq(latency_analysis["chain_001"].0, 390)  // 总延迟390ms
  assert_eq(latency_analysis["chain_001"].1, 200)  // 最大延迟200ms
  assert_eq(latency_analysis["chain_001"].2, "payment_service")
  
  assert_eq(latency_analysis["chain_002"].0, 228)  // 总延迟228ms
  assert_eq(latency_analysis["chain_002"].1, 120)  // 最大延迟120ms
  assert_eq(latency_analysis["chain_002"].2, "recommendation_service")
  
  // 验证延迟占比
  assert_eq(latency_analysis["chain_001"].3["payment_service"] > 50.0, true)  // payment_service占比超过50%
  assert_eq(latency_analysis["chain_002"].3["recommendation_service"] > 50.0, true)  // recommendation_service占比超过50%
  
  // 识别延迟热点
  let mut latency_hotspots = {}
  i = 0
  while i < latency_chains.length() {
    let (_, service_name, latency) = latency_chains[i]
    
    if !latency_hotspots.contains_key(service_name) {
      latency_hotspots[service_name] = []
    }
    latency_hotspots[service_name].push(latency)
    
    i = i + 1
  }
  
  // 计算平均延迟
  let mut avg_latencies = {}
  let service_names = latency_hotspots.keys()
  i = 0
  while i < service_names.length() {
    let service_name = service_names[i]
    let latencies = latency_hotspots[service_name]
    
    let mut total = 0
    let mut j = 0
    while j < latencies.length() {
      total = total + latencies[j]
      j = j + 1
    }
    let avg = total / latencies.length()
    avg_latencies[service_name] = avg
    
    i = i + 1
  }
  
  // 验证延迟热点
  assert_eq(avg_latencies["payment_service"], 200)  // 最高平均延迟
  assert_eq(avg_latencies["recommendation_service"], 120)
  assert_eq(avg_latencies["order_service"], 100)
  assert_eq(avg_latencies["gateway"], 9)  // 最低平均延迟(10+8)/2
}

test "telemetry_cross_service_throughput_correlation" {
  // 测试跨服务吞吐量关联分析
  
  let throughput_data = [
    ("throughput_001", "gateway", 1000),
    ("throughput_001", "auth_service", 800),
    ("throughput_001", "user_service", 600),
    ("throughput_001", "order_service", 400),
    ("throughput_001", "payment_service", 200),
    ("throughput_002", "gateway", 1500),
    ("throughput_002", "product_service", 1200),
    ("throughput_002", "inventory_service", 900),
    ("throughput_002", "search_service", 700)
  ]
  
  // 按场景分组吞吐量数据
  let mut throughput_by_scenario = {}
  let mut i = 0
  while i < throughput_data.length() {
    let (scenario_id, service_name, throughput) = throughput_data[i]
    
    if !throughput_by_scenario.contains_key(scenario_id) {
      throughput_by_scenario[scenario_id] = []
    }
    throughput_by_scenario[scenario_id].push((service_name, throughput))
    
    i = i + 1
  }
  
  // 分析吞吐量衰减模式
  let mut throughput_patterns = {}
  let scenario_ids = ["throughput_001", "throughput_002"]
  i = 0
  while i < scenario_ids.length() {
    let scenario_id = scenario_ids[i]
    let scenario_data = throughput_by_scenario[scenario_id]
    
    // 找出入口服务和出口服务的吞吐量
    let mut entry_throughput = 0
    let mut exit_throughput = 0
    
    let mut j = 0
    while j < scenario_data.length() {
      let (service_name, throughput) = scenario_data[j]
      
      if service_name == "gateway" {
        entry_throughput = throughput
      } else if (scenario_id == "throughput_001" && service_name == "payment_service") ||
                (scenario_id == "throughput_002" && service_name == "search_service") {
        exit_throughput = throughput
      }
      
      j = j + 1
    }
    
    // 计算吞吐量衰减率
    let decay_rate = if entry_throughput > 0 {
      (entry_throughput - exit_throughput).to_double() / entry_throughput.to_double()
    } else {
      0.0
    }
    
    throughput_patterns[scenario_id] = (entry_throughput, exit_throughput, decay_rate)
    
    i = i + 1
  }
  
  // 验证吞吐量模式
  assert_eq(throughput_patterns["throughput_001"].0, 1000)  // 入口1000
  assert_eq(throughput_patterns["throughput_001"].1, 200)   // 出口200
  assert_eq(throughput_patterns["throughput_001"].2, 0.8)   // 80%衰减率
  
  assert_eq(throughput_patterns["throughput_002"].0, 1500)  // 入口1500
  assert_eq(throughput_patterns["throughput_002"].1, 700)   // 出口700
  assert_eq(throughput_patterns["throughput_002"].2, 0.5333333333333333)  // 53.33%衰减率
  
  // 分析服务吞吐量相关性
  let mut service_correlations = {}
  i = 0
  while i < throughput_data.length() {
    let (_, service_name, throughput) = throughput_data[i]
    
    if !service_correlations.contains_key(service_name) {
      service_correlations[service_name] = []
    }
    service_correlations[service_name].push(throughput)
    
    i = i + 1
  }
  
  // 计算各服务平均吞吐量
  let mut avg_throughputs = {}
  let service_names = service_correlations.keys()
  i = 0
  while i < service_names.length() {
    let service_name = service_names[i]
    let throughputs = service_correlations[service_name]
    
    let mut total = 0
    let mut j = 0
    while j < throughputs.length() {
      total = total + throughputs[j]
      j = j + 1
    }
    let avg = total / throughputs.length()
    avg_throughputs[service_name] = avg
    
    i = i + 1
  }
  
  // 验证吞吐量相关性
  assert_eq(avg_throughputs["gateway"], 1250)     // (1000+1500)/2
  assert_eq(avg_throughputs["auth_service"], 800)
  assert_eq(avg_throughputs["user_service"], 600)
  assert_eq(avg_throughputs["order_service"], 400)
  assert_eq(avg_throughputs["payment_service"], 200)
  
  // 识别吞吐量瓶颈
  let mut bottlenecks = []
  i = 0
  while i < scenario_ids.length() {
    let scenario_id = scenario_ids[i]
    let scenario_data = throughput_by_scenario[scenario_id]
    
    // 找出吞吐量最小的服务
    let mut min_throughput = 999999
    let mut bottleneck_service = ""
    
    let mut j = 0
    while j < scenario_data.length() {
      let (service_name, throughput) = scenario_data[j]
      
      if throughput < min_throughput && service_name != "gateway" {
        min_throughput = throughput
        bottleneck_service = service_name
      }
      
      j = j + 1
    }
    
    bottlenecks.push((scenario_id, bottleneck_service, min_throughput))
    
    i = i + 1
  }
  
  // 验证吞吐量瓶颈
  assert_eq(bottlenecks[0].1, "payment_service")   // 最低吞吐量200
  assert_eq(bottlenecks[0].2, 200)
  assert_eq(bottlenecks[1].1, "search_service")    // 最低吞吐量700
  assert_eq(bottlenecks[1].2, 700)
}