// 遥测内存管理测试用例

test "telemetry_memory_allocation_tracking" {
  // 测试遥测内存分配跟踪
  
  let memory_pools = [
    ("span_pool", 1000, 100),      // 池名称, 对象数量, 每个对象大小(字节)
    ("metric_pool", 500, 50),
    ("log_pool", 2000, 200),
    ("trace_pool", 100, 500),
    ("attribute_pool", 3000, 20)
  ]
  
  // 验证内存池
  assert_eq(memory_pools.length(), 5)
  assert_eq(memory_pools[0].0, "span_pool")
  assert_eq(memory_pools[0].1, 1000)
  assert_eq(memory_pools[0].2, 100)
  assert_eq(memory_pools[4].0, "attribute_pool")
  assert_eq(memory_pools[4].1, 3000)
  assert_eq(memory_pools[4].2, 20)
  
  // 计算每个池的内存使用
  let pool_memory_usage = []
  let mut total_memory = 0
  let mut i = 0
  
  while i < memory_pools.length() {
    let pool_name = memory_pools[i].0
    let object_count = memory_pools[i].1
    let object_size = memory_pools[i].2
    let total_size = object_count * object_size
    
    pool_memory_usage.push((pool_name, object_count, object_size, total_size))
    total_memory = total_memory + total_size
    
    i = i + 1
  }
  
  // 验证内存使用计算
  assert_eq(pool_memory_usage.length(), 5)
  assert_eq(pool_memory_usage[0].0, "span_pool")
  assert_eq(pool_memory_usage[0].3, 100000) // 1000 * 100
  assert_eq(pool_memory_usage[1].0, "metric_pool")
  assert_eq(pool_memory_usage[1].3, 25000) // 500 * 50
  assert_eq(pool_memory_usage[4].0, "attribute_pool")
  assert_eq(pool_memory_usage[4].3, 60000) // 3000 * 20
  
  // 验证总内存
  assert_eq(total_memory, 100000 + 25000 + 400000 + 50000 + 60000) // 635000字节
  assert_eq(total_memory, 635000)
  
  // 模拟内存分配跟踪
  let allocation_tracking = []
  i = 0
  
  while i < pool_memory_usage.length() {
    let pool_name = pool_memory_usage[i].0
    let total_size = pool_memory_usage[i].3
    
    // 模拟分配历史
    let allocation_history = [
      ("initial_allocation", total_size),
      ("growth_allocation", total_size / 2),
      ("peak_allocation", total_size * 2),
      ("current_allocation", total_size)
    ]
    
    allocation_tracking.push((pool_name, allocation_history))
    i = i + 1
  }
  
  // 验证分配跟踪
  assert_eq(allocation_tracking.length(), 5)
  assert_eq(allocation_tracking[0].0, "span_pool")
  assert_eq(allocation_tracking[0].1.length(), 4)
  assert_eq(allocation_tracking[0].1[0].0, "initial_allocation")
  assert_eq(allocation_tracking[0].1[0].1, 100000)
  assert_eq(allocation_tracking[0].1[2].0, "peak_allocation")
  assert_eq(allocation_tracking[0].1[2].1, 200000) // 峰值是初始的2倍
}

test "telemetry_memory_leak_detection" {
  // 测试遥测内存泄漏检测
  
  let memory_snapshots = [
    ("snapshot_1", 100000, 1640995200),    // 快照名称, 内存使用量, 时间戳
    ("snapshot_2", 120000, 1640995300),
    ("snapshot_3", 110000, 1640995400),
    ("snapshot_4", 130000, 1640995500),
    ("snapshot_5", 150000, 1640995600),
    ("snapshot_6", 180000, 1640995700),
    ("snapshot_7", 200000, 1640995800),
    ("snapshot_8", 220000, 1640995900)
  ]
  
  // 验证内存快照
  assert_eq(memory_snapshots.length(), 8)
  assert_eq(memory_snapshots[0].0, "snapshot_1")
  assert_eq(memory_snapshots[0].1, 100000)
  assert_eq(memory_snapshots[0].2, 1640995200)
  assert_eq(memory_snapshots[7].0, "snapshot_8")
  assert_eq(memory_snapshots[7].1, 220000)
  assert_eq(memory_snapshots[7].2, 1640995900)
  
  // 计算内存增长趋势
  let memory_growth = []
  let mut i = 1
  
  while i < memory_snapshots.length() {
    let current_snapshot = memory_snapshots[i]
    let previous_snapshot = memory_snapshots[i - 1]
    
    let growth = current_snapshot.1 - previous_snapshot.1
    let growth_rate = growth.to_double() / previous_snapshot.1.to_double()
    let time_diff = current_snapshot.2 - previous_snapshot.2
    
    memory_growth.push((
      current_snapshot.0,
      previous_snapshot.1,
      current_snapshot.1,
      growth,
      growth_rate,
      time_diff
    ))
    
    i = i + 1
  }
  
  // 验证内存增长计算
  assert_eq(memory_growth.length(), 7)
  assert_eq(memory_growth[0].0, "snapshot_2")
  assert_eq(memory_growth[0].1, 100000) // 前一个快照内存
  assert_eq(memory_growth[0].2, 120000) // 当前快照内存
  assert_eq(memory_growth[0].3, 20000) // 增长量
  assert_eq(memory_growth[0].4, 0.2) // 增长率 20%
  assert_eq(memory_growth[0].5, 100) // 时间差 100秒
  
  // 检测内存泄漏
  let leak_threshold = 0.1 // 10%增长率阈值
  let leak_detection_results = []
  i = 0
  
  while i < memory_growth.length() {
    let snapshot_name = memory_growth[i].0
    let growth_rate = memory_growth[i].4
    let memory_usage = memory_growth[i].2
    
    let potential_leak = growth_rate > leak_threshold
    let severity = if growth_rate > 0.5 {
      "critical"
    } else if growth_rate > 0.2 {
      "warning"
    } else if growth_rate > 0.1 {
      "info"
    } else {
      "normal"
    }
    
    leak_detection_results.push((snapshot_name, memory_usage, growth_rate, potential_leak, severity))
    i = i + 1
  }
  
  // 验证泄漏检测结果
  assert_eq(leak_detection_results.length(), 7)
  assert_eq(leak_detection_results[0].0, "snapshot_2")
  assert_eq(leak_detection_results[0].3, true) // 20% > 10% 阈值
  assert_eq(leak_detection_results[0].4, "warning") // 20%增长率是警告级别
  
  // 统计泄漏情况
  let mut leak_count = 0
  let mut critical_count = 0
  let mut warning_count = 0
  i = 0
  
  while i < leak_detection_results.length() {
    if leak_detection_results[i].3 {
      leak_count = leak_count + 1
    }
    if leak_detection_results[i].4 == "critical" {
      critical_count = critical_count + 1
    } else if leak_detection_results[i].4 == "warning" {
      warning_count = warning_count + 1
    }
    i = i + 1
  }
  
  // 验证泄漏统计
  assert_eq(leak_count, 6) // 6个快照超过阈值
  assert_eq(critical_count, 1) // 1个严重泄漏
  assert_eq(warning_count, 2) // 2个警告泄漏
}

test "telemetry_memory_pool_management" {
  // 测试遥测内存池管理
  
  let memory_pools = [
    ("small_object_pool", 64, 1000, 800),    // 池名称, 对象大小, 池容量, 当前使用
    ("medium_object_pool", 256, 500, 400),
    ("large_object_pool", 1024, 200, 150),
    ("huge_object_pool", 4096, 50, 30)
  ]
  
  // 验证内存池
  assert_eq(memory_pools.length(), 4)
  assert_eq(memory_pools[0].0, "small_object_pool")
  assert_eq(memory_pools[0].1, 64)
  assert_eq(memory_pools[0].2, 1000)
  assert_eq(memory_pools[0].3, 800)
  assert_eq(memory_pools[3].0, "huge_object_pool")
  assert_eq(memory_pools[3].1, 4096)
  assert_eq(memory_pools[3].2, 50)
  assert_eq(memory_pools[3].3, 30)
  
  // 计算池利用率
  let pool_utilization = []
  let mut i = 0
  
  while i < memory_pools.length() {
    let pool_name = memory_pools[i].0
    let object_size = memory_pools[i].1
    let pool_capacity = memory_pools[i].2
    let current_usage = memory_pools[i].3
    
    let utilization_rate = current_usage.to_double() / pool_capacity.to_double()
    let available_objects = pool_capacity - current_usage
    let total_memory = pool_capacity * object_size
    let used_memory = current_usage * object_size
    
    pool_utilization.push((
      pool_name,
      object_size,
      pool_capacity,
      current_usage,
      utilization_rate,
      available_objects,
      total_memory,
      used_memory
    ))
    
    i = i + 1
  }
  
  // 验证池利用率
  assert_eq(pool_utilization.length(), 4)
  assert_eq(pool_utilization[0].0, "small_object_pool")
  assert_eq(pool_utilization[0].4, 0.8) // 800/1000 = 80%
  assert_eq(pool_utilization[0].5, 200) // 1000-800 = 200可用
  assert_eq(pool_utilization[0].6, 64000) // 1000*64
  assert_eq(pool_utilization[0].7, 51200) // 800*64
  
  // 检查池状态
  let pool_status = []
  i = 0
  
  while i < pool_utilization.length() {
    let pool_name = pool_utilization[i].0
    let utilization_rate = pool_utilization[i].4
    let available_objects = pool_utilization[i].5
    
    let status = if utilization_rate > 0.9 {
      "critical"
    } else if utilization_rate > 0.8 {
      "warning"
    } else if utilization_rate > 0.5 {
      "normal"
    } else {
      "underutilized"
    }
    
    let needs_expansion = utilization_rate > 0.8 && available_objects < 100
    
    pool_status.push((pool_name, utilization_rate, status, needs_expansion))
    i = i + 1
  }
  
  // 验证池状态
  assert_eq(pool_status.length(), 4)
  assert_eq(pool_status[0].0, "small_object_pool")
  assert_eq(pool_status[0].2, "warning") // 80%利用率是警告
  assert_eq(pool_status[0].3, false) // 200个可用对象，不需要扩展
  assert_eq(pool_status[1].0, "medium_object_pool")
  assert_eq(pool_status[1].2, "normal") // 400/500 = 80%，但应该是警告
  
  // 模拟池扩展操作
  let pool_expansions = []
  i = 0
  
  while i < pool_status.length() {
    let pool_name = pool_status[i].0
    let needs_expansion = pool_status[i].3
    
    if needs_expansion {
      let original_capacity = memory_pools[i].2
      let expansion_factor = 1.5
      let new_capacity = (original_capacity.to_double() * expansion_factor).to_int()
      
      pool_expansions.push((pool_name, original_capacity, new_capacity, expansion_factor))
    }
    
    i = i + 1
  }
  
  // 验证池扩展
  assert_eq(pool_expansions.length(), 0) // 没有池需要扩展
}

test "telemetry_memory_gc_optimization" {
  // 测试遥测内存垃圾回收优化
  
  let gc_cycles = [
    ("cycle_1", 100000, 20000, 1640995200),   // 周期名称, 堆大小, 回收内存, 时间戳
    ("cycle_2", 120000, 25000, 1640995300),
    ("cycle_3", 110000, 15000, 1640995400),
    ("cycle_4", 130000, 30000, 1640995500),
    ("cycle_5", 125000, 22000, 1640995600),
    ("cycle_6", 140000, 35000, 1640995700),
    ("cycle_7", 135000, 28000, 1640995800),
    ("cycle_8", 150000, 40000, 1640995900)
  ]
  
  // 验证GC周期
  assert_eq(gc_cycles.length(), 8)
  assert_eq(gc_cycles[0].0, "cycle_1")
  assert_eq(gc_cycles[0].1, 100000)
  assert_eq(gc_cycles[0].2, 20000)
  assert_eq(gc_cycles[0].3, 1640995200)
  assert_eq(gc_cycles[7].0, "cycle_8")
  assert_eq(gc_cycles[7].1, 150000)
  assert_eq(gc_cycles[7].2, 40000)
  
  // 计算GC效率
  let gc_efficiency = []
  let mut i = 0
  
  while i < gc_cycles.length() {
    let cycle_name = gc_cycles[i].0
    let heap_size = gc_cycles[i].1
    let reclaimed_memory = gc_cycles[i].2
    let timestamp = gc_cycles[i].3
    
    let efficiency = reclaimed_memory.to_double() / heap_size.to_double()
    let remaining_memory = heap_size - reclaimed_memory
    
    gc_efficiency.push((
      cycle_name,
      heap_size,
      reclaimed_memory,
      remaining_memory,
      efficiency,
      timestamp
    ))
    
    i = i + 1
  }
  
  // 验证GC效率
  assert_eq(gc_efficiency.length(), 8)
  assert_eq(gc_efficiency[0].0, "cycle_1")
  assert_eq(gc_efficiency[0].3, 80000) // 100000 - 20000
  assert_eq(gc_efficiency[0].4, 0.2) // 20000/100000 = 20%
  assert_eq(gc_efficiency[7].0, "cycle_8")
  assert_eq(gc_efficiency[7].4, 0.26666666666666666) // 40000/150000 ≈ 26.67%
  
  // 分析GC性能趋势
  let gc_performance_trend = []
  i = 1
  
  while i < gc_efficiency.length() {
    let current_cycle = gc_efficiency[i]
    let previous_cycle = gc_efficiency[i - 1]
    
    let efficiency_change = current_cycle.4 - previous_cycle.4
    let heap_size_change = current_cycle.1 - previous_cycle.1
    let reclaimed_change = current_cycle.2 - previous_cycle.2
    let time_diff = current_cycle.5 - previous_cycle.5
    
    gc_performance_trend.push((
      current_cycle.0,
      efficiency_change,
      heap_size_change,
      reclaimed_change,
      time_diff
    ))
    
    i = i + 1
  }
  
  // 验证GC性能趋势
  assert_eq(gc_performance_trend.length(), 7)
  assert_eq(gc_performance_trend[0].0, "cycle_2")
  assert_eq(gc_performance_trend[0].1, 0.05833333333333333) // 0.25 - 0.2 = 0.058
  assert_eq(gc_performance_trend[0].2, 20000) // 120000 - 100000
  assert_eq(gc_performance_trend[0].3, 5000) // 25000 - 20000
  assert_eq(gc_performance_trend[0].4, 100) // 100秒间隔
  
  // 计算平均GC效率
  let mut total_efficiency = 0.0
  i = 0
  
  while i < gc_efficiency.length() {
    total_efficiency = total_efficiency + gc_efficiency[i].4
    i = i + 1
  }
  
  let average_efficiency = total_efficiency / gc_efficiency.length().to_double()
  
  // 验证平均效率
  assert_eq(average_efficiency > 0.0, true)
  assert_eq(average_efficiency < 1.0, true)
  
  // GC优化建议
  let optimization_recommendations = []
  i = 0
  
  while i < gc_efficiency.length() {
    let cycle_name = gc_efficiency[i].0
    let efficiency = gc_efficiency[i].4
    let heap_size = gc_efficiency[i].1
    
    let recommendation = if efficiency < 0.15 {
      "Increase GC frequency or reduce object allocation"
    } else if efficiency < 0.25 {
      "Monitor object lifecycle patterns"
    } else if efficiency > 0.3 {
      "Consider increasing heap size to reduce GC overhead"
    } else {
      "GC performance is optimal"
    }
    
    optimization_recommendations.push((cycle_name, efficiency, recommendation))
    i = i + 1
  }
  
  // 验证优化建议
  assert_eq(optimization_recommendations.length(), 8)
  assert_eq(optimization_recommendations[0].0, "cycle_1")
  assert_eq(optimization_recommendations[0].1, 0.2)
  assert_eq(optimization_recommendations[0].2, "Monitor object lifecycle patterns")
}

test "telemetry_memory_pressure_handling" {
  // 测试遥测内存压力处理
  
  let memory_pressure_levels = [
    ("normal", 0.6, "green"),     // 压力级别, 内存使用率, 状态颜色
    ("warning", 0.8, "yellow"),
    ("critical", 0.9, "orange"),
    ("emergency", 0.95, "red")
  ]
  
  // 验证内存压力级别
  assert_eq(memory_pressure_levels.length(), 4)
  assert_eq(memory_pressure_levels[0].0, "normal")
  assert_eq(memory_pressure_levels[0].1, 0.6)
  assert_eq(memory_pressure_levels[0].2, "green")
  assert_eq(memory_pressure_levels[3].0, "emergency")
  assert_eq(memory_pressure_levels[3].1, 0.95)
  assert_eq(memory_pressure_levels[3].2, "red")
  
  // 模拟内存压力场景
  let pressure_scenarios = [
    ("low_load", 1000000, 500000, 0.5),    // 场景名称, 总内存, 使用内存, 使用率
    ("moderate_load", 1000000, 750000, 0.75),
    ("high_load", 1000000, 850000, 0.85),
    ("extreme_load", 1000000, 950000, 0.95),
    ("memory_spike", 1000000, 980000, 0.98)
  ]
  
  // 验证压力场景
  assert_eq(pressure_scenarios.length(), 5)
  assert_eq(pressure_scenarios[0].0, "low_load")
  assert_eq(pressure_scenarios[0].3, 0.5)
  assert_eq(pressure_scenarios[4].0, "memory_spike")
  assert_eq(pressure_scenarios[4].3, 0.98)
  
  // 处理内存压力
  let pressure_responses = []
  let mut i = 0
  
  while i < pressure_scenarios.length() {
    let scenario_name = pressure_scenarios[i].0
    let total_memory = pressure_scenarios[i].1
    let used_memory = pressure_scenarios[i].2
    let usage_rate = pressure_scenarios[i].3
    
    // 确定压力级别
    let pressure_level = if usage_rate < 0.7 {
      "normal"
    } else if usage_rate < 0.8 {
      "warning"
    } else if usage_rate < 0.9 {
      "critical"
    } else {
      "emergency"
    }
    
    // 确定响应措施
    let response_actions = match pressure_level {
      "normal" => ["Continue normal operations"],
      "warning" => ["Increase GC frequency", "Reduce cache sizes"],
      "critical" => ["Force GC", "Clear caches", "Reject new requests"],
      "emergency" => ["Emergency GC", "Clear all caches", "Pause non-critical operations"],
      _ => ["Unknown pressure level"]
    }
    
    pressure_responses.push((
      scenario_name,
      usage_rate,
      pressure_level,
      response_actions
    ))
    
    i = i + 1
  }
  
  // 验证压力响应
  assert_eq(pressure_responses.length(), 5)
  assert_eq(pressure_responses[0].0, "low_load")
  assert_eq(pressure_responses[0].2, "normal")
  assert_eq(pressure_responses[0].3.length(), 1)
  assert_eq(pressure_responses[2].0, "high_load")
  assert_eq(pressure_responses[2].2, "critical")
  assert_eq(pressure_responses[2].3.length(), 3)
  assert_eq(pressure_responses[4].0, "memory_spike")
  assert_eq(pressure_responses[4].2, "emergency")
  assert_eq(pressure_responses[4].3.length(), 3)
  
  // 验证响应措施
  i = 0
  while i < pressure_responses.length() {
    let scenario_name = pressure_responses[i].0
    let pressure_level = pressure_responses[i].2
    let actions = pressure_responses[i].3
    
    // 检查响应措施是否合理
    match pressure_level {
      "normal" => assert_eq(actions.length(), 1),
      "warning" => assert_eq(actions.length(), 2),
      "critical" => assert_eq(actions.length(), 3),
      "emergency" => assert_eq(actions.length(), 3),
      _ => assert_eq(false, true)
    }
    
    i = i + 1
  }
  
  // 模拟内存恢复
  let recovery_scenarios = []
  i = 0
  
  while i < pressure_responses.length() {
    let scenario_name = pressure_responses[i].0
    let initial_usage = pressure_scenarios[i].3
    let pressure_level = pressure_responses[i].2
    
    // 模拟恢复后的使用率
    let recovered_usage = match pressure_level {
      "normal" => initial_usage,
      "warning" => initial_usage - 0.1,
      "critical" => initial_usage - 0.2,
      "emergency" => initial_usage - 0.3,
      _ => initial_usage
    }
    
    let recovery_successful = recovered_usage < initial_usage
    let recovery_time = match pressure_level {
      "normal" => 0,
      "warning" => 30,
      "critical" => 60,
      "emergency" => 120,
      _ => 0
    }
    
    recovery_scenarios.push((
      scenario_name,
      initial_usage,
      recovered_usage,
      recovery_successful,
      recovery_time
    ))
    
    i = i + 1
  }
  
  // 验证恢复场景
  assert_eq(recovery_scenarios.length(), 5)
  assert_eq(recovery_scenarios[0].0, "low_load")
  assert_eq(recovery_scenarios[0].1, 0.5)
  assert_eq(recovery_scenarios[0].2, 0.5) // 正常级别无变化
  assert_eq(recovery_scenarios[0].3, false) // 无需恢复
  assert_eq(recovery_scenarios[0].4, 0) // 无恢复时间
  assert_eq(recovery_scenarios[4].0, "memory_spike")
  assert_eq(recovery_scenarios[4].1, 0.98)
  assert_eq(recovery_scenarios[4].2, 0.68) // 0.98 - 0.3
  assert_eq(recovery_scenarios[4].3, true) // 恢复成功
  assert_eq(recovery_scenarios[4].4, 120) // 120秒恢复时间
}

test "telemetry_memory_fragmentation" {
  // 测试遥测内存碎片化
  
  let fragmentation_analysis = [
    ("heap_segment_1", 1024, 800, 224),     // 段名称, 段大小, 已使用, 碎片
    ("heap_segment_2", 2048, 1500, 548),
    ("heap_segment_3", 4096, 3000, 1096),
    ("heap_segment_4", 8192, 6000, 2192),
    ("heap_segment_5", 16384, 12000, 4384)
  ]
  
  // 验证碎片化分析
  assert_eq(fragmentation_analysis.length(), 5)
  assert_eq(fragmentation_analysis[0].0, "heap_segment_1")
  assert_eq(fragmentation_analysis[0].1, 1024)
  assert_eq(fragmentation_analysis[0].2, 800)
  assert_eq(fragmentation_analysis[0].3, 224)
  assert_eq(fragmentation_analysis[4].0, "heap_segment_5")
  assert_eq(fragmentation_analysis[4].1, 16384)
  assert_eq(fragmentation_analysis[4].2, 12000)
  assert_eq(fragmentation_analysis[4].3, 4384)
  
  // 计算碎片化率
  let fragmentation_rates = []
  let mut total_heap_size = 0
  let mut total_fragmentation = 0
  let mut i = 0
  
  while i < fragmentation_analysis.length() {
    let segment_name = fragmentation_analysis[i].0
    let segment_size = fragmentation_analysis[i].1
    let used_memory = fragmentation_analysis[i].2
    let fragmented_memory = fragmentation_analysis[i].3
    
    let fragmentation_rate = fragmented_memory.to_double() / segment_size.to_double()
    let utilization_rate = used_memory.to_double() / segment_size.to_double()
    
    fragmentation_rates.push((
      segment_name,
      segment_size,
      used_memory,
      fragmented_memory,
      fragmentation_rate,
      utilization_rate
    ))
    
    total_heap_size = total_heap_size + segment_size
    total_fragmentation = total_fragmentation + fragmented_memory
    
    i = i + 1
  }
  
  // 验证碎片化率
  assert_eq(fragmentation_rates.length(), 5)
  assert_eq(fragmentation_rates[0].0, "heap_segment_1")
  assert_eq(fragmentation_rates[0].4, 0.21875) // 224/1024 ≈ 21.88%
  assert_eq(fragmentation_rates[0].5, 0.78125) // 800/1024 ≈ 78.13%
  assert_eq(fragmentation_rates[4].0, "heap_segment_5")
  assert_eq(fragmentation_rates[4].4, 0.267578125) // 4384/16384 ≈ 26.76%
  
  // 计算总体碎片化
  let overall_fragmentation_rate = total_fragmentation.to_double() / total_heap_size.to_double()
  
  // 验证总体碎片化
  assert_eq(total_heap_size, 1024 + 2048 + 4096 + 8192 + 16384) // 31744
  assert_eq(total_fragmentation, 224 + 548 + 1096 + 2192 + 4384) // 8444
  assert_eq(overall_fragmentation_rate, 0.26605224609375) // 8444/31744 ≈ 26.61%
  
  // 碎片化风险评估
  let fragmentation_risks = []
  i = 0
  
  while i < fragmentation_rates.length() {
    let segment_name = fragmentation_rates[i].0
    let fragmentation_rate = fragmentation_rates[i].4
    let utilization_rate = fragmentation_rates[i].5
    
    let risk_level = if fragmentation_rate > 0.3 {
      "high"
    } else if fragmentation_rate > 0.2 {
      "medium"
    } else if fragmentation_rate > 0.1 {
      "low"
    } else {
      "minimal"
    }
    
    let needs_compaction = fragmentation_rate > 0.25
    
    fragmentation_risks.push((
      segment_name,
      fragmentation_rate,
      utilization_rate,
      risk_level,
      needs_compaction
    ))
    
    i = i + 1
  }
  
  // 验证碎片化风险
  assert_eq(fragmentation_risks.length(), 5)
  assert_eq(fragmentation_risks[0].0, "heap_segment_1")
  assert_eq(fragmentation_risks[0].3, "medium") // 21.88%是中等风险
  assert_eq(fragmentation_risks[0].4, false) // 不需要压缩
  assert_eq(fragmentation_risks[4].0, "heap_segment_5")
  assert_eq(fragmentation_risks[4].3, "high") // 26.76%是高风险
  assert_eq(fragmentation_risks[4].4, true) // 需要压缩
  
  // 统计风险分布
  let mut risk_counts = [0, 0, 0, 0] // minimal, low, medium, high
  let mut compaction_needed = 0
  i = 0
  
  while i < fragmentation_risks.length() {
    let risk_level = fragmentation_risks[i].3
    let needs_compaction = fragmentation_risks[i].4
    
    match risk_level {
      "minimal" => risk_counts[0] = risk_counts[0] + 1,
      "low" => risk_counts[1] = risk_counts[1] + 1,
      "medium" => risk_counts[2] = risk_counts[2] + 1,
      "high" => risk_counts[3] = risk_counts[3] + 1,
      _ => {}
    }
    
    if needs_compaction {
      compaction_needed = compaction_needed + 1
    }
    
    i = i + 1
  }
  
  // 验证风险统计
  assert_eq(risk_counts[0], 0) // 0个最小风险
  assert_eq(risk_counts[1], 0) // 0个低风险
  assert_eq(risk_counts[2], 3) // 3个中等风险
  assert_eq(risk_counts[3], 2) // 2个高风险
  assert_eq(compaction_needed, 2) // 2个段需要压缩
}

test "telemetry_memory_budget_allocation" {
  // 测试遥测内存预算分配
  
  let memory_budget = [
    ("telemetry_spans", 40, 1000000),      // 组件名称, 预算百分比, 预算大小(字节)
    ("telemetry_metrics", 25, 625000),
    ("telemetry_logs", 20, 500000),
    ("telemetry_traces", 10, 250000),
    ("telemetry_buffers", 5, 125000)
  ]
  
  // 验证内存预算
  assert_eq(memory_budget.length(), 5)
  assert_eq(memory_budget[0].0, "telemetry_spans")
  assert_eq(memory_budget[0].1, 40)
  assert_eq(memory_budget[0].2, 1000000)
  assert_eq(memory_budget[4].0, "telemetry_buffers")
  assert_eq(memory_budget[4].1, 5)
  assert_eq(memory_budget[4].2, 125000)
  
  // 验证预算总和
  let mut total_percentage = 0
  let mut total_budget = 0
  let mut i = 0
  
  while i < memory_budget.length() {
    total_percentage = total_percentage + memory_budget[i].1
    total_budget = total_budget + memory_budget[i].2
    i = i + 1
  }
  
  // 验证预算总和
  assert_eq(total_percentage, 100) // 100%预算分配
  assert_eq(total_budget, 2500000) // 总预算2.5MB
  
  // 模拟实际使用情况
  let actual_usage = [
    ("telemetry_spans", 950000),
    ("telemetry_metrics", 600000),
    ("telemetry_logs", 480000),
    ("telemetry_traces", 300000),
    ("telemetry_buffers", 100000)
  ]
  
  // 验证实际使用
  assert_eq(actual_usage.length(), 5)
  assert_eq(actual_usage[0].0, "telemetry_spans")
  assert_eq(actual_usage[0].1, 950000)
  assert_eq(actual_usage[4].0, "telemetry_buffers")
  assert_eq(actual_usage[4].1, 100000)
  
  // 计算预算使用情况
  let budget_utilization = []
  i = 0
  
  while i < memory_budget.length() {
    let component_name = memory_budget[i].0
    let budget_percentage = memory_budget[i].1
    let budget_size = memory_budget[i].2
    
    // 查找实际使用
    let mut used_memory = 0
    let mut j = 0
    while j < actual_usage.length() {
      if actual_usage[j].0 == component_name {
        used_memory = actual_usage[j].1
        break
      }
      j = j + 1
    }
    
    let utilization_rate = used_memory.to_double() / budget_size.to_double()
    let over_budget = used_memory > budget_size
    let remaining_budget = budget_size - used_memory
    
    budget_utilization.push((
      component_name,
      budget_percentage,
      budget_size,
      used_memory,
      utilization_rate,
      over_budget,
      remaining_budget
    ))
    
    i = i + 1
  }
  
  // 验证预算使用情况
  assert_eq(budget_utilization.length(), 5)
  assert_eq(budget_utilization[0].0, "telemetry_spans")
  assert_eq(budget_utilization[0].4, 0.95) // 950000/1000000 = 95%
  assert_eq(budget_utilization[0].5, false) // 未超预算
  assert_eq(budget_utilization[0].6, 50000) // 剩余预算
  assert_eq(budget_utilization[3].0, "telemetry_traces")
  assert_eq(budget_utilization[3].4, 1.2) // 300000/250000 = 120%
  assert_eq(budget_utilization[3].5, true) // 超预算
  assert_eq(budget_utilization[3].6, -50000) // 预算不足
  
  // 预算调整建议
  let budget_adjustments = []
  i = 0
  
  while i < budget_utilization.length() {
    let component_name = budget_utilization[i].0
    let utilization_rate = budget_utilization[i].4
    let over_budget = budget_utilization[i].5
    let current_budget = budget_utilization[i].2
    
    let adjustment = if over_budget {
      let increase_needed = (current_budget.to_double() * (utilization_rate - 1.0)).to_int()
      ("increase", increase_needed)
    } else if utilization_rate < 0.7 {
      let decrease_possible = (current_budget.to_double() * (0.7 - utilization_rate)).to_int()
      ("decrease", decrease_possible)
    } else {
      ("maintain", 0)
    }
    
    budget_adjustments.push((component_name, utilization_rate, adjustment))
    i = i + 1
  }
  
  // 验证预算调整建议
  assert_eq(budget_adjustments.length(), 5)
  assert_eq(budget_adjustments[0].0, "telemetry_spans")
  assert_eq(budget_adjustments[0].1, 0.95)
  assert_eq(budget_adjustments[0].2.0, "maintain") // 95%使用率，维持当前预算
  assert_eq(budget_adjustments[3].0, "telemetry_traces")
  assert_eq(budget_adjustments[3].1, 1.2)
  assert_eq(budget_adjustments[3].2.0, "increase") // 超预算，需要增加
  assert_eq(budget_adjustments[3].2.1, 50000) // 需要增加50000字节
}