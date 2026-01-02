// Azimuth 遥测系统自适应性能调优测试
// 专注于测试遥测系统的自适应性能调优功能

// 测试1: 性能指标收集
test "性能指标收集测试" {
  // 模拟系统性能指标
  let performance_metrics = {
    cpu_usage: 45.2,
    memory_usage: 67.8,
    disk_io: 120.5, // MB/s
    network_io: 85.3, // MB/s
    response_time: 125, // ms
    throughput: 1500, // requests/sec
    error_rate: 0.02,
    cache_hit_rate: 0.85
  }
  
  // 性能指标收集函数
  let collect_performance_metrics = fn() -> {
    timestamp: Int,
    metrics: { String: Double },
    system_health: String
  } {
    let current_time = 1634567890
    
    // 简单的健康评估
    let health = if performance_metrics.cpu_usage > 80.0 || 
                    performance_metrics.memory_usage > 90.0 ||
                    performance_metrics.error_rate > 0.05 {
      "degraded"
    } else if performance_metrics.cpu_usage > 60.0 || 
              performance_metrics.memory_usage > 70.0 {
      "warning"
    } else {
      "healthy"
    }
    
    {
      timestamp: current_time,
      metrics: performance_metrics,
      system_health: health
    }
  }
  
  // 收集指标
  let metrics_result = collect_performance_metrics()
  
  // 验证结果
  assert_eq(metrics_result.timestamp, 1634567890)
  assert_eq(metrics_result.metrics.cpu_usage, 45.2)
  assert_eq(metrics_result.metrics.memory_usage, 67.8)
  assert_eq(metrics_result.system_health, "warning")
}

// 测试2: 性能瓶颈识别
test "性能瓶颈识别测试" {
  // 性能基准
  let performance_benchmarks = {
    cpu_usage: { optimal: 50.0, warning: 70.0, critical: 90.0 },
    memory_usage: { optimal: 60.0, warning: 80.0, critical: 95.0 },
    response_time: { optimal: 100, warning: 200, critical: 500 },
    throughput: { optimal: 2000, warning: 1000, critical: 500 },
    error_rate: { optimal: 0.01, warning: 0.03, critical: 0.05 }
  }
  
  // 当前性能数据
  let current_performance = {
    cpu_usage: 85.0,
    memory_usage: 65.0,
    response_time: 350,
    throughput: 800,
    error_rate: 0.04
  }
  
  // 瓶颈识别函数
  let identify_bottlenecks = fn(current: { String: Double }, 
                              benchmarks: { String: { optimal: Double, warning: Double, critical: Double } }) -> {
    bottlenecks: Array[String],
    severity: String,
    recommendations: Array[String]
  } {
    let bottlenecks = []
    let recommendations = []
    
    // CPU使用率检查
    if current.cpu_usage > benchmarks.cpu_usage.critical {
      bottlenecks.push("cpu_usage")
      recommendations.push("增加CPU资源或优化CPU密集型操作")
    } else if current.cpu_usage > benchmarks.cpu_usage.warning {
      bottlenecks.push("cpu_usage")
      recommendations.push("监控CPU使用率，准备扩容")
    }
    
    // 响应时间检查
    if current.response_time > benchmarks.response_time.critical {
      bottlenecks.push("response_time")
      recommendations.push("优化数据库查询或增加缓存")
    } else if current.response_time > benchmarks.response_time.warning {
      bottlenecks.push("response_time")
      recommendations.push("分析慢查询，优化代码路径")
    }
    
    // 吞吐量检查
    if current.throughput < benchmarks.throughput.critical {
      bottlenecks.push("throughput")
      recommendations.push("增加并发处理能力或优化算法")
    }
    
    // 错误率检查
    if current.error_rate > benchmarks.error_rate.warning {
      bottlenecks.push("error_rate")
      recommendations.push("检查错误日志，修复异常处理")
    }
    
    let severity = if bottlenecks.length() >= 3 { "critical" }
                  else if bottlenecks.length() >= 2 { "warning" }
                  else if bottlenecks.length() >= 1 { "minor" }
                  else { "none" }
    
    {
      bottlenecks: bottlenecks,
      severity: severity,
      recommendations: recommendations
    }
  }
  
  // 识别瓶颈
  let bottleneck_analysis = identify_bottlenecks(current_performance, performance_benchmarks)
  
  // 验证结果
  assert_eq(bottleneck_analysis.bottlenecks.length(), 4)
  assert_true(bottleneck_analysis.bottlenecks.contains("cpu_usage"))
  assert_true(bottleneck_analysis.bottlenecks.contains("response_time"))
  assert_true(bottleneck_analysis.bottlenecks.contains("throughput"))
  assert_true(bottleneck_analysis.bottlenecks.contains("error_rate"))
  assert_eq(bottleneck_analysis.severity, "critical")
  assert_true(bottleneck_analysis.recommendations.length() > 0)
}

// 测试3: 自适应配置调整
test "自适应配置调整测试" {
  // 当前配置
  let current_config = {
    collection_interval: 5000, // 5秒
    buffer_size: 1000,
    batch_size: 100,
    connection_pool_size: 10,
    cache_size: 500,
    compression_enabled: true
  }
  
  // 性能问题
  let performance_issues = ["high_memory_usage", "low_throughput", "high_response_time"]
  
  // 配置调整函数
  let adapt_configuration = fn(config: {
    collection_interval: Int,
    buffer_size: Int,
    batch_size: Int,
    connection_pool_size: Int,
    cache_size: Int,
    compression_enabled: Bool
  }, issues: Array[String]) -> {
    new_config: {
      collection_interval: Int,
      buffer_size: Int,
      batch_size: Int,
      connection_pool_size: Int,
      cache_size: Int,
      compression_enabled: Bool
    },
    changes: Array[String]
  } {
    let mut new_config = config
    let changes = []
    
    issues.each_fn(issue => {
      match issue {
        "high_memory_usage" => {
          if new_config.buffer_size > 500 {
            new_config.buffer_size = new_config.buffer_size / 2
            changes.push("Reduced buffer size to lower memory usage")
          }
          if new_config.cache_size > 200 {
            new_config.cache_size = new_config.cache_size / 2
            changes.push("Reduced cache size to lower memory usage")
          }
        }
        "low_throughput" => {
          if new_config.batch_size < 200 {
            new_config.batch_size = new_config.batch_size * 2
            changes.push("Increased batch size to improve throughput")
          }
          if new_config.collection_interval > 2000 {
            new_config.collection_interval = new_config.collection_interval / 2
            changes.push("Reduced collection interval to improve throughput")
          }
        }
        "high_response_time" => {
          if new_config.connection_pool_size < 20 {
            new_config.connection_pool_size = new_config.connection_pool_size + 5
            changes.push("Increased connection pool size to reduce response time")
          }
        }
        _ => ()
      }
    })
    
    { new_config: new_config, changes: changes }
  }
  
  // 执行配置调整
  let adaptation_result = adapt_configuration(current_config, performance_issues)
  
  // 验证调整结果
  assert_eq(adaptation_result.changes.length(), 5)
  assert_eq(adaptation_result.new_config.buffer_size, 500) // 1000 / 2
  assert_eq(adaptation_result.new_config.cache_size, 250) // 500 / 2
  assert_eq(adaptation_result.new_config.batch_size, 200) // 100 * 2
  assert_eq(adaptation_result.new_config.collection_interval, 2500) // 5000 / 2
  assert_eq(adaptation_result.new_config.connection_pool_size, 15) // 10 + 5
}

// 测试4: 资源分配优化
test "资源分配优化测试" {
  // 当前资源使用情况
  let resource_usage = {
    cpu_cores: { total: 8, used: 6, available: 2 },
    memory_gb: { total: 16, used: 12, available: 4 },
    disk_gb: { total: 500, used: 200, available: 300 },
    network_bandwidth_mbps: { total: 1000, used: 400, available: 600 }
  }
  
  // 组件资源需求
  let component_requirements = {
    metrics_collector: { cpu_cores: 2, memory_gb: 2, priority: "high" },
    data_processor: { cpu_cores: 4, memory_gb: 4, priority: "high" },
    storage_manager: { cpu_cores: 1, memory_gb: 1, priority: "medium" },
    api_server: { cpu_cores: 2, memory_gb: 3, priority: "high" },
    dashboard: { cpu_cores: 1, memory_gb: 1, priority: "low" }
  }
  
  // 资源分配优化函数
  let optimize_resource_allocation = fn(usage: {
    cpu_cores: { total: Int, used: Int, available: Int },
    memory_gb: { total: Int, used: Int, available: Int },
    disk_gb: { total: Int, used: Int, available: Int },
    network_bandwidth_mbps: { total: Int, used: Int, available: Int }
  }, requirements: { String: { cpu_cores: Int, memory_gb: Int, priority: String } }) -> {
    allocation_plan: Array[String],
    can_meet_requirements: Bool,
    resource_utilization: Double
  } {
    let allocation_plan = []
    let mut total_cpu_needed = 0
    let mut total_memory_needed = 0
    
    // 按优先级排序组件
    let sorted_components = requirements.to_array().sort_by(fn(a, b) {
      let priority_order = ["high", "medium", "low"]
      let a_priority = priority_order.index_of(a.value.priority).unwrap_or(999)
      let b_priority = priority_order.index_of(b.value.priority).unwrap_or(999)
      a_priority - b_priority
    })
    
    // 分配资源
    sorted_components.each_fn(component => {
      let name = component.key
      let req = component.value
      
      if usage.cpu_cores.available >= req.cpu_cores && 
         usage.memory_gb.available >= req.memory_gb {
        allocation_plan.push("Allocate " + req.cpu_cores.to_string() + 
                            " CPU cores and " + req.memory_gb.to_string() + 
                            "GB memory to " + name)
        total_cpu_needed = total_cpu_needed + req.cpu_cores
        total_memory_needed = total_memory_needed + req.memory_gb
      } else {
        allocation_plan.push("Insufficient resources for " + name + 
                            " (needs " + req.cpu_cores.to_string() + 
                            " CPU cores, " + req.memory_gb.to_string() + "GB memory)")
      }
    })
    
    let can_meet = total_cpu_needed <= usage.cpu_cores.available && 
                   total_memory_needed <= usage.memory_gb.available
    
    let utilization = (total_cpu_needed.to_double() / usage.cpu_cores.total.to_double() + 
                      total_memory_needed.to_double() / usage.memory_gb.total.to_double()) / 2.0 * 100.0
    
    {
      allocation_plan: allocation_plan,
      can_meet_requirements: can_meet,
      resource_utilization: utilization
    }
  }
  
  // 执行资源分配优化
  let allocation_result = optimize_resource_allocation(resource_usage, component_requirements)
  
  // 验证结果
  assert_eq(allocation_result.allocation_plan.length(), 5)
  assert_true(allocation_result.can_meet_requirements)
  assert_true(allocation_result.resource_utilization > 0.0)
  assert_true(allocation_result.resource_utilization <= 100.0)
}

// 测试5: 性能趋势预测
test "性能趋势预测测试" {
  // 历史性能数据
  let historical_performance = [
    { timestamp: 1634567600, cpu_usage: 40.0, memory_usage: 60.0, response_time: 100 },
    { timestamp: 1634567660, cpu_usage: 42.0, memory_usage: 62.0, response_time: 105 },
    { timestamp: 1634567720, cpu_usage: 45.0, memory_usage: 65.0, response_time: 110 },
    { timestamp: 1634567780, cpu_usage: 48.0, memory_usage: 68.0, response_time: 120 },
    { timestamp: 1634567840, cpu_usage: 52.0, memory_usage: 72.0, response_time: 130 }
  ]
  
  // 趋势预测函数
  let predict_performance_trend = fn(data: Array[{
    timestamp: Int,
    cpu_usage: Double,
    memory_usage: Double,
    response_time: Int
  }]) -> {
    cpu_trend: String,
    memory_trend: String,
    response_time_trend: String,
    predicted_next_values: { cpu_usage: Double, memory_usage: Double, response_time: Int }
  } {
    if data.length() < 3 {
      return {
        cpu_trend: "insufficient_data",
        memory_trend: "insufficient_data",
        response_time_trend: "insufficient_data",
        predicted_next_values: { cpu_usage: 0.0, memory_usage: 0.0, response_time: 0 }
      }
    }
    
    // 简单线性趋势计算
    let calculate_trend = fn(values: Array[Double]) -> String {
      if values.length() < 2 { "stable" }
      else {
        let first = values[0]
        let last = values[values.length() - 1]
        let change_percent = (last - first) / first * 100.0
        
        if change_percent > 10.0 { "increasing" }
        else if change_percent < -10.0 { "decreasing" }
        else { "stable" }
      }
    }
    
    // 简单线性预测
    let predict_next = fn(values: Array[Double]) -> Double {
      if values.length() < 2 { values[0] }
      else {
        let last = values[values.length() - 1]
        let second_last = values[values.length() - 2]
        last + (last - second_last) // 简单的外推
      }
    }
    
    let cpu_values = data.map_fn(d) { d.cpu_usage }
    let memory_values = data.map_fn(d) { d.memory_usage }
    let response_values = data.map_fn(d) { d.response_time.to_double() }
    
    {
      cpu_trend: calculate_trend(cpu_values),
      memory_trend: calculate_trend(memory_values),
      response_time_trend: calculate_trend(response_values),
      predicted_next_values: {
        cpu_usage: predict_next(cpu_values),
        memory_usage: predict_next(memory_values),
        response_time: predict_next(response_values).to_int()
      }
    }
  }
  
  // 执行趋势预测
  let trend_prediction = predict_performance_trend(historical_performance)
  
  // 验证结果
  assert_eq(trend_prediction.cpu_trend, "increasing")
  assert_eq(trend_prediction.memory_trend, "increasing")
  assert_eq(trend_prediction.response_time_trend, "increasing")
  assert_true(trend_prediction.predicted_next_values.cpu_usage > 52.0)
  assert_true(trend_prediction.predicted_next_values.memory_usage > 72.0)
  assert_true(trend_prediction.predicted_next_values.response_time > 130)
}

// 测试6: 自动扩缩容决策
test "自动扩缩容决策测试" {
  // 当前系统状态
  let system_state = {
    current_instances: 3,
    avg_cpu_usage: 75.0,
    avg_memory_usage: 80.0,
    request_queue_length: 150,
    response_time_p95: 450, // ms
    error_rate: 0.04
  }
  
  // 扩缩容策略
  let scaling_policy = {
    scale_up_cpu_threshold: 70.0,
    scale_up_memory_threshold: 75.0,
    scale_up_queue_threshold: 100,
    scale_up_response_time_threshold: 300,
    scale_down_cpu_threshold: 30.0,
    scale_down_memory_threshold: 40.0,
    scale_down_queue_threshold: 20,
    scale_down_response_time_threshold: 100,
    min_instances: 2,
    max_instances: 10
  }
  
  // 扩缩容决策函数
  let make_scaling_decision = fn(state: {
    current_instances: Int,
    avg_cpu_usage: Double,
    avg_memory_usage: Double,
    request_queue_length: Int,
    response_time_p95: Int,
    error_rate: Double
  }, policy: {
    scale_up_cpu_threshold: Double,
    scale_up_memory_threshold: Double,
    scale_up_queue_threshold: Int,
    scale_up_response_time_threshold: Int,
    scale_down_cpu_threshold: Double,
    scale_down_memory_threshold: Double,
    scale_down_queue_threshold: Int,
    scale_down_response_time_threshold: Int,
    min_instances: Int,
    max_instances: Int
  }) -> {
    should_scale: Bool,
    direction: String,
    target_instances: Int,
    reasons: Array[String]
  } {
    let scale_up_reasons = []
    let scale_down_reasons = []
    
    // 检查扩容条件
    if state.avg_cpu_usage > policy.scale_up_cpu_threshold {
      scale_up_reasons.push("CPU usage " + state.avg_cpu_usage.to_string() + 
                           "% exceeds threshold " + policy.scale_up_cpu_threshold.to_string() + "%")
    }
    
    if state.avg_memory_usage > policy.scale_up_memory_threshold {
      scale_up_reasons.push("Memory usage " + state.avg_memory_usage.to_string() + 
                           "% exceeds threshold " + policy.scale_up_memory_threshold.to_string() + "%")
    }
    
    if state.request_queue_length > policy.scale_up_queue_threshold {
      scale_up_reasons.push("Request queue length " + state.request_queue_length.to_string() + 
                           " exceeds threshold " + policy.scale_up_queue_threshold.to_string())
    }
    
    if state.response_time_p95 > policy.scale_up_response_time_threshold {
      scale_up_reasons.push("Response time P95 " + state.response_time_p95.to_string() + 
                           "ms exceeds threshold " + policy.scale_up_response_time_threshold.to_string() + "ms")
    }
    
    // 检查缩容条件
    if state.avg_cpu_usage < policy.scale_down_cpu_threshold {
      scale_down_reasons.push("CPU usage " + state.avg_cpu_usage.to_string() + 
                             "% below threshold " + policy.scale_down_cpu_threshold.to_string() + "%")
    }
    
    if state.avg_memory_usage < policy.scale_down_memory_threshold {
      scale_down_reasons.push("Memory usage " + state.avg_memory_usage.to_string() + 
                             "% below threshold " + policy.scale_down_memory_threshold.to_string() + "%")
    }
    
    if state.request_queue_length < policy.scale_down_queue_threshold {
      scale_down_reasons.push("Request queue length " + state.request_queue_length.to_string() + 
                             " below threshold " + policy.scale_down_queue_threshold.to_string())
    }
    
    if state.response_time_p95 < policy.scale_down_response_time_threshold {
      scale_down_reasons.push("Response time P95 " + state.response_time_p95.to_string() + 
                             "ms below threshold " + policy.scale_down_response_time_threshold.to_string() + "ms")
    }
    
    // 决策逻辑
    let should_scale_up = scale_up_reasons.length() >= 2
    let should_scale_down = scale_down_reasons.length() >= 3 && state.current_instances > policy.min_instances
    
    if should_scale_up {
      let target = min(state.current_instances + 1, policy.max_instances)
      {
        should_scale: true,
        direction: "scale_up",
        target_instances: target,
        reasons: scale_up_reasons
      }
    } else if should_scale_down {
      let target = max(state.current_instances - 1, policy.min_instances)
      {
        should_scale: true,
        direction: "scale_down",
        target_instances: target,
        reasons: scale_down_reasons
      }
    } else {
      {
        should_scale: false,
        direction: "none",
        target_instances: state.current_instances,
        reasons: []
      }
    }
  }
  
  // 执行扩缩容决策
  let scaling_decision = make_scaling_decision(system_state, scaling_policy)
  
  // 验证结果
  assert_true(scaling_decision.should_scale)
  assert_eq(scaling_decision.direction, "scale_up")
  assert_eq(scaling_decision.target_instances, 4) // 3 + 1
  assert_true(scaling_decision.reasons.length() >= 2)
}

// 测试7: 性能优化效果评估
test "性能优化效果评估测试" {
  // 优化前性能数据
  let before_optimization = {
    avg_response_time: 250, // ms
    throughput: 1200, // requests/sec
    cpu_usage: 80.0, // %
    memory_usage: 85.0, // %
    error_rate: 0.03, // %
    cost_per_hour: 10.5 // $
  }
  
  // 优化后性能数据
  let after_optimization = {
    avg_response_time: 180, // ms
    throughput: 1800, // requests/sec
    cpu_usage: 65.0, // %
    memory_usage: 70.0, // %
    error_rate: 0.01, // %
    cost_per_hour: 12.0 // $
  }
  
  // 优化效果评估函数
  let evaluate_optimization_impact = fn(before: {
    avg_response_time: Int,
    throughput: Int,
    cpu_usage: Double,
    memory_usage: Double,
    error_rate: Double,
    cost_per_hour: Double
  }, after: {
    avg_response_time: Int,
    throughput: Int,
    cpu_usage: Double,
    memory_usage: Double,
    error_rate: Double,
    cost_per_hour: Double
  }) -> {
    improvements: Array[String],
    regressions: Array[String],
    overall_score: Double,
    cost_benefit_ratio: Double
  } {
    let improvements = []
    let regressions = []
    
    // 响应时间改进
    let response_time_improvement = (before.avg_response_time - after.avg_response_time).to_double() / before.avg_response_time.to_double() * 100.0
    if response_time_improvement > 0 {
      improvements.push("Response time improved by " + response_time_improvement.to_string() + "%")
    } else {
      regressions.push("Response time degraded by " + abs(response_time_improvement).to_string() + "%")
    }
    
    // 吞吐量改进
    let throughput_improvement = (after.throughput - before.throughput).to_double() / before.throughput.to_double() * 100.0
    if throughput_improvement > 0 {
      improvements.push("Throughput improved by " + throughput_improvement.to_string() + "%")
    } else {
      regressions.push("Throughput degraded by " + abs(throughput_improvement).to_string() + "%")
    }
    
    // CPU使用率改进
    let cpu_improvement = (before.cpu_usage - after.cpu_usage) / before.cpu_usage * 100.0
    if cpu_improvement > 0 {
      improvements.push("CPU usage reduced by " + cpu_improvement.to_string() + "%")
    } else {
      regressions.push("CPU usage increased by " + abs(cpu_improvement).to_string() + "%")
    }
    
    // 内存使用率改进
    let memory_improvement = (before.memory_usage - after.memory_usage) / before.memory_usage * 100.0
    if memory_improvement > 0 {
      improvements.push("Memory usage reduced by " + memory_improvement.to_string() + "%")
    } else {
      regressions.push("Memory usage increased by " + abs(memory_improvement).to_string() + "%")
    }
    
    // 错误率改进
    let error_improvement = (before.error_rate - after.error_rate) / before.error_rate * 100.0
    if error_improvement > 0 {
      improvements.push("Error rate reduced by " + error_improvement.to_string() + "%")
    } else {
      regressions.push("Error rate increased by " + abs(error_improvement).to_string() + "%")
    }
    
    // 成本变化
    let cost_change = (after.cost_per_hour - before.cost_per_hour) / before.cost_per_hour * 100.0
    if cost_change > 0 {
      regressions.push("Cost increased by " + cost_change.to_string() + "%")
    } else {
      improvements.push("Cost reduced by " + abs(cost_change).to_string() + "%")
    }
    
    // 总体评分 (简单加权)
    let overall_score = (response_time_improvement + throughput_improvement + cpu_improvement + 
                        memory_improvement + error_improvement - cost_change) / 6.0
    
    // 成本效益比
    let performance_gain = (throughput_improvement + response_time_improvement) / 2.0
    let cost_benefit_ratio = if cost_change >= 0 { performance_gain / cost_change } else { performance_gain }
    
    {
      improvements: improvements,
      regressions: regressions,
      overall_score: overall_score,
      cost_benefit_ratio: cost_benefit_ratio
    }
  }
  
  // 评估优化效果
  let optimization_evaluation = evaluate_optimization_impact(before_optimization, after_optimization)
  
  // 验证结果
  assert_eq(optimization_evaluation.improvements.length(), 5)
  assert_eq(optimization_evaluation.regressions.length(), 1)
  assert_true(optimization_evaluation.overall_score > 0.0)
  assert_true(optimization_evaluation.cost_benefit_ratio > 0.0)
}

// 测试8: 自适应学习机制
test "自适应学习机制测试" {
  // 历史优化记录
  let optimization_history = [
    {
      timestamp: 1634567000,
      action: "increase_buffer_size",
      context: { cpu_usage: 85.0, memory_usage: 70.0, throughput: 800 },
      outcome: { success: true, improvement: 15.0, side_effects: ["memory_usage+5%"] }
    },
    {
      timestamp: 1634567200,
      action: "increase_connection_pool",
      context: { cpu_usage: 60.0, memory_usage: 65.0, throughput: 1200 },
      outcome: { success: true, improvement: 8.0, side_effects: ["cpu_usage+3%"] }
    },
    {
      timestamp: 1634567400,
      action: "enable_compression",
      context: { cpu_usage: 75.0, memory_usage: 80.0, throughput: 1000 },
      outcome: { success: false, improvement: -2.0, side_effects: ["cpu_usage+10%", "response_time+20ms"] }
    },
    {
      timestamp: 1634567600,
      action: "increase_batch_size",
      context: { cpu_usage: 70.0, memory_usage: 75.0, throughput: 900 },
      outcome: { success: true, improvement: 12.0, side_effects: ["memory_usage+8%"] }
    }
  ]
  
  // 当前系统状态
  let current_context = { cpu_usage: 72.0, memory_usage: 78.0, throughput: 950 }
  
  // 自适应学习函数
  let adaptive_learning_recommendation = fn(history: Array[{
    timestamp: Int,
    action: String,
    context: { cpu_usage: Double, memory_usage: Double, throughput: Int },
    outcome: { success: Bool, improvement: Double, side_effects: Array[String] }
  }], current: { cpu_usage: Double, memory_usage: Double, throughput: Int }) -> {
    recommended_action: String,
    confidence: Double,
    expected_improvement: Double,
    reasoning: String
  } {
    // 找到相似上下文的历史记录
    let similar_contexts = history.filter_fn(record => {
      let cpu_diff = abs(record.context.cpu_usage - current.cpu_usage)
      let memory_diff = abs(record.context.memory_usage - current.memory_usage)
      let throughput_diff = abs(record.context.throughput - current.throughput)
      
      cpu_diff < 10.0 && memory_diff < 10.0 && throughput_diff < 200
    })
    
    if similar_contexts.length() == 0 {
      return {
        recommended_action: "no_historical_data",
        confidence: 0.0,
        expected_improvement: 0.0,
        reasoning: "No similar historical contexts found"
      }
    }
    
    // 分析成功操作
    let successful_actions = similar_contexts.filter_fn(record => record.outcome.success)
    
    if successful_actions.length() == 0 {
      return {
        recommended_action: "no_successful_actions",
        confidence: 0.0,
        expected_improvement: 0.0,
        reasoning: "No successful actions in similar contexts"
      }
    }
    
    // 选择最成功的操作
    let best_action = successful_actions.reduce_fn(best, record => {
      if record.outcome.improvement > best.outcome.improvement { record } else { best }
    }, successful_actions[0])
    
    // 计算置信度 (基于相似度和成功率)
    let similarity_score = similar_contexts.length().to_double() / history.length().to_double()
    let success_rate = successful_actions.length().to_double() / similar_contexts.length().to_double()
    let confidence = similarity_score * success_rate
    
    {
      recommended_action: best_action.action,
      confidence: confidence,
      expected_improvement: best_action.outcome.improvement,
      reasoning: "Based on " + successful_actions.length().to_string() + 
                " successful actions in similar contexts, with expected improvement of " + 
                best_action.outcome.improvement.to_string() + "%"
    }
  }
  
  // 获取自适应学习建议
  let learning_recommendation = adaptive_learning_recommendation(optimization_history, current_context)
  
  // 验证结果
  assert_true(learning_recommendation.confidence > 0.0)
  assert_true(learning_recommendation.expected_improvement > 0.0)
  assert_true(learning_recommendation.reasoning.contains("successful actions"))
  assert_true(learning_recommendation.recommended_action != "no_historical_data")
  assert_true(learning_recommendation.recommended_action != "no_successful_actions")
}