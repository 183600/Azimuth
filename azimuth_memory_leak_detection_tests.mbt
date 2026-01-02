// Azimuth 遥测系统内存泄漏检测测试
// 专注于测试遥测系统的内存泄漏检测和防护机制

// 测试1: 内存使用基线建立
test "内存使用基线建立测试" {
  // 模拟系统内存使用基线数据
  let memory_baseline = {
    total_heap_size: 1024 * 1024 * 100, // 100MB
    used_heap_size: 1024 * 1024 * 30,   // 30MB
    heap_growth_rate: 1024 * 10,        // 10KB/s
    gc_frequency: 5,                     // 5次/分钟
    gc_pause_time: 15,                   // 15ms平均暂停时间
    memory_fragmentation: 0.15           // 15%碎片率
  }
  
  // 基线建立函数
  let establish_memory_baseline = fn(measurements: Array<{
    timestamp: Int,
    heap_used: Int,
    heap_total: Int,
    gc_count: Int,
    gc_pause_total: Int
  }>) -> {
    baseline: {
      total_heap_size: Int,
      used_heap_size: Int,
      heap_growth_rate: Int,
      gc_frequency: Double,
      gc_pause_time: Double,
      memory_fragmentation: Double
    },
    is_stable: Bool,
    confidence_score: Double
  } {
    if measurements.length() < 10 {
      return {
        baseline: {
          total_heap_size: 0, used_heap_size: 0, heap_growth_rate: 0,
          gc_frequency: 0.0, gc_pause_time: 0.0, memory_fragmentation: 0.0
        },
        is_stable: false,
        confidence_score: 0.0
      }
    }
    
    // 计算平均堆使用量
    let avg_heap_used = measurements.map_fn(m) { m.heap_used }.fold(0, fn(acc, x) { acc + x }) / measurements.length()
    let avg_heap_total = measurements.map_fn(m) { m.heap_total }.fold(0, fn(acc, x) { acc + x }) / measurements.length()
    
    // 计算堆增长率 (基于最后5个测量点)
    let recent_measurements = measurements.slice(measurements.length() - 5)
    let first_recent = recent_measurements[0]
    let last_recent = recent_measurements[recent_measurements.length() - 1]
    let time_diff = last_recent.timestamp - first_recent.timestamp
    let heap_diff = last_recent.heap_used - first_recent.heap_used
    let growth_rate = if time_diff > 0 { heap_diff / time_diff } else { 0 }
    
    // 计算GC频率
    let total_gc_events = measurements[measurements.length() - 1].gc_count - measurements[0].gc_count
    let time_span = (measurements[measurements.length() - 1].timestamp - measurements[0].timestamp) / 60 // 分钟
    let gc_frequency = if time_span > 0 { total_gc_events.to_double() / time_span.to_double() } else { 0.0 }
    
    // 计算平均GC暂停时间
    let total_gc_pause = measurements[measurements.length() - 1].gc_pause_total - measurements[0].gc_pause_total
    let avg_gc_pause = if total_gc_events > 0 { total_gc_pause / total_gc_events } else { 0 }
    
    // 估算内存碎片率 (简化计算)
    let fragmentation = (avg_heap_total - avg_heap_used).to_double() / avg_heap_total.to_double()
    
    // 稳定性检查 (基于变异系数)
    let heap_values = measurements.map_fn(m) { m.heap_used.to_double() }
    let mean = heap_values.fold(0.0, fn(acc, x) { acc + x }) / heap_values.length().to_double()
    let variance = heap_values.fold(0.0, fn(acc, x) {
      let diff = x - mean
      acc + diff * diff
    }) / heap_values.length().to_double()
    let std_dev = sqrt(variance)
    let coefficient_of_variation = if mean > 0 { std_dev / mean } else { 1.0 }
    
    let is_stable = coefficient_of_variation < 0.1 // 变异系数小于10%认为稳定
    let confidence_score = if is_stable { 0.9 } else { 0.5 }
    
    let baseline = {
      total_heap_size: avg_heap_total,
      used_heap_size: avg_heap_used,
      heap_growth_rate: growth_rate,
      gc_frequency: gc_frequency,
      gc_pause_time: avg_gc_pause.to_double(),
      memory_fragmentation: fragmentation
    }
    
    {
      baseline: baseline,
      is_stable: is_stable,
      confidence_score: confidence_score
    }
  }
  
  // 模拟测量数据
  let measurements = [
    { timestamp: 1634567600, heap_used: 30 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 100, gc_pause_total: 1500 },
    { timestamp: 1634567660, heap_used: 31 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 105, gc_pause_total: 1575 },
    { timestamp: 1634567720, heap_used: 30 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 110, gc_pause_total: 1650 },
    { timestamp: 1634567780, heap_used: 32 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 115, gc_pause_total: 1725 },
    { timestamp: 1634567840, heap_used: 31 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 120, gc_pause_total: 1800 },
    { timestamp: 1634567900, heap_used: 33 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 125, gc_pause_total: 1875 },
    { timestamp: 1634567960, heap_used: 32 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 130, gc_pause_total: 1950 },
    { timestamp: 1634568020, heap_used: 34 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 135, gc_pause_total: 2025 },
    { timestamp: 1634568080, heap_used: 33 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 140, gc_pause_total: 2100 },
    { timestamp: 1634568140, heap_used: 35 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 145, gc_pause_total: 2175 }
  ]
  
  // 建立基线
  let baseline_result = establish_memory_baseline(measurements)
  
  // 验证基线结果
  assert_true(baseline_result.is_stable)
  assert_true(baseline_result.confidence_score > 0.5)
  assert_eq(baseline_result.baseline.total_heap_size, 100 * 1024 * 1024)
  assert_true(baseline_result.baseline.used_heap_size > 30 * 1024 * 1024)
  assert_true(baseline_result.baseline.heap_growth_rate > 0)
  assert_true(baseline_result.baseline.gc_frequency > 0.0)
  assert_true(baseline_result.baseline.gc_pause_time > 0.0)
  assert_true(baseline_result.baseline.memory_fragmentation > 0.0)
}

// 测试2: 内存泄漏检测算法
test "内存泄漏检测算法测试" {
  // 内存泄漏检测配置
  let leak_detection_config = {
    growth_threshold_percent: 20.0,    // 增长超过20%视为潜在泄漏
    persistent_growth_minutes: 10,     // 持续增长10分钟
    gc_resistance_threshold: 3,        // GC后内存不减少次数阈值
    memory_spike_threshold_mb: 50,     // 内存突增阈值(MB)
    fragmentation_threshold: 0.3       // 碎片率阈值
  }
  
  // 当前内存状态
  let current_memory_state = {
    heap_used: 80 * 1024 * 1024,      // 80MB
    heap_total: 100 * 1024 * 1024,    // 100MB
    gc_count: 150,
    gc_pause_total: 2250,
    timestamp: 1634568200
  }
  
  // 历史内存状态
  let historical_memory = [
    { heap_used: 30 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 100, gc_pause_total: 1500, timestamp: 1634567600 },
    { heap_used: 32 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 110, gc_pause_total: 1650, timestamp: 1634567720 },
    { heap_used: 35 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 120, gc_pause_total: 1800, timestamp: 1634567840 },
    { heap_used: 40 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 130, gc_pause_total: 1950, timestamp: 1634567960 },
    { heap_used: 50 * 1024 * 1024, heap_total: 100 * 1024 * 1024, gc_count: 140, gc_pause_total: 2100, timestamp: 1634568080 }
  ]
  
  // 内存泄漏检测函数
  let detect_memory_leak = fn(current: {
    heap_used: Int,
    heap_total: Int,
    gc_count: Int,
    gc_pause_total: Int,
    timestamp: Int
  }, historical: Array[{
    heap_used: Int,
    heap_total: Int,
    gc_count: Int,
    gc_pause_total: Int,
    timestamp: Int
  }], config: {
    growth_threshold_percent: Double,
    persistent_growth_minutes: Int,
    gc_resistance_threshold: Int,
    memory_spike_threshold_mb: Int,
    fragmentation_threshold: Double
  }) -> {
    has_leak: Bool,
    leak_indicators: Array[String],
    severity: String,
    confidence: Double,
    recommended_actions: Array[String]
  } {
    if historical.length() < 3 {
      return {
        has_leak: false,
        leak_indicators: [],
        severity: "none",
        confidence: 0.0,
        recommended_actions: []
      }
    }
    
    let leak_indicators = []
    
    // 1. 检查持续增长
    let initial_memory = historical[0].heap_used
    let growth_percent = (current.heap_used - initial_memory).to_double() / initial_memory.to_double() * 100.0
    
    if growth_percent > config.growth_threshold_percent {
      leak_indicators.push("Memory growth of " + growth_percent.to_string() + 
                          "% exceeds threshold of " + config.growth_threshold_percent.to_string() + "%")
    }
    
    // 2. 检查增长持续时间
    let time_span = (current.timestamp - historical[0].timestamp) / 60 // 分钟
    if time_span >= config.persistent_growth_minutes && growth_percent > 10.0 {
      leak_indicators.push("Persistent memory growth over " + time_span.to_string() + " minutes")
    }
    
    // 3. 检查GC抵抗性 (GC后内存不减少)
    let mut gc_resistance_count = 0
    for i = 1; i < historical.length(); i = i + 1 {
      if historical[i].heap_used > historical[i-1].heap_used {
        gc_resistance_count = gc_resistance_count + 1
      }
    }
    
    if gc_resistance_count >= config.gc_resistance_threshold {
      leak_indicators.push("Memory shows resistance to garbage collection (" + 
                          gc_resistance_count.to_string() + " instances)")
    }
    
    // 4. 检查内存突增
    let recent_growth = current.heap_used - historical[historical.length() - 1].heap_used
    let recent_growth_mb = recent_growth / (1024 * 1024)
    
    if recent_growth_mb > config.memory_spike_threshold_mb {
      leak_indicators.push("Recent memory spike of " + recent_growth_mb.to_string() + 
                          "MB exceeds threshold")
    }
    
    // 5. 检查内存碎片率
    let fragmentation = (current.heap_total - current.heap_used).to_double() / current.heap_total.to_double()
    if fragmentation > config.fragmentation_threshold {
      leak_indicators.push("Memory fragmentation of " + (fragmentation * 100.0).to_string() + 
                          "% exceeds threshold")
    }
    
    // 评估严重程度
    let severity = if leak_indicators.length() >= 4 { "critical" }
                  else if leak_indicators.length() >= 3 { "high" }
                  else if leak_indicators.length() >= 2 { "medium" }
                  else if leak_indicators.length() >= 1 { "low" }
                  else { "none" }
    
    // 计算置信度
    let confidence = if leak_indicators.length() > 0 { 
      0.5 + (leak_indicators.length().to_double() / 5.0) * 0.5 
    } else { 0.0 }
    
    // 推荐行动
    let recommended_actions = match severity {
      "critical" => [
        "Immediate memory dump analysis",
        "Restart service if possible",
        "Enable detailed memory profiling",
        "Check for circular references"
      ]
      "high" => [
        "Schedule maintenance window",
        "Increase monitoring frequency",
        "Review recent code changes",
        "Check for unclosed resources"
      ]
      "medium" => [
        "Monitor closely for next hour",
        "Review memory allocation patterns",
        "Consider proactive restart"
      ]
      "low" => [
        "Continue monitoring",
        "Document observation"
      ]
      _ => []
    }
    
    {
      has_leak: leak_indicators.length() > 0,
      leak_indicators: leak_indicators,
      severity: severity,
      confidence: confidence,
      recommended_actions: recommended_actions
    }
  }
  
  // 执行泄漏检测
  let leak_detection_result = detect_memory_leak(current_memory_state, historical_memory, leak_detection_config)
  
  // 验证检测结果
  assert_true(leak_detection_result.has_leak)
  assert_true(leak_detection_result.leak_indicators.length() > 0)
  assert_eq(leak_detection_result.severity, "high") // 增长了166%，应该触发高级别警报
  assert_true(leak_detection_result.confidence > 0.5)
  assert_true(leak_detection_result.recommended_actions.length() > 0)
}

// 测试3: 内存热点分析
test "内存热点分析测试" {
  // 模拟内存分配热点数据
  let memory_hotspots = [
    {
      component: "metrics_buffer",
      allocation_count: 15000,
      total_size_bytes: 25 * 1024 * 1024, // 25MB
      average_lifetime_ms: 3000,
      allocation_rate: 500 // 分配/秒
    },
    {
      component: "trace_collector",
      allocation_count: 8000,
      total_size_bytes: 15 * 1024 * 1024, // 15MB
      average_lifetime_ms: 5000,
      allocation_rate: 200 // 分配/秒
    },
    {
      component: "cache_manager",
      allocation_count: 5000,
      total_size_bytes: 30 * 1024 * 1024, // 30MB
      average_lifetime_ms: 60000, // 1分钟
      allocation_rate: 100 // 分配/秒
    },
    {
      component: "serialization",
      allocation_count: 20000,
      total_size_bytes: 10 * 1024 * 1024, // 10MB
      average_lifetime_ms: 500, // 短生命周期
      allocation_rate: 1000 // 分配/秒
    }
  ]
  
  // 内存热点分析函数
  let analyze_memory_hotspots = fn(hotspots: Array[{
    component: String,
    allocation_count: Int,
    total_size_bytes: Int,
    average_lifetime_ms: Int,
    allocation_rate: Int
  }]) -> {
    top_consumers: Array[String],
    high_frequency_allocators: Array[String],
    long_lived_objects: Array[String],
    optimization_opportunities: Array[String],
    overall_pressure_score: Double
  } {
    // 按总内存消耗排序
    let top_consumers = hotspots.sort_by(fn(a, b) {
      if a.total_size_bytes > b.total_size_bytes { -1 }
      else if a.total_size_bytes < b.total_size_bytes { 1 }
      else { 0 }
    }).slice(0, 3).map_fn(h) { h.component }
    
    // 高频分配器 (分配率 > 300/秒)
    let high_frequency_allocators = hotspots
      .filter_fn(h) { h.allocation_rate > 300 }
      .map_fn(h) { h.component }
    
    // 长生命周期对象 (平均生命周期 > 10秒)
    let long_lived_objects = hotspots
      .filter_fn(h) { h.average_lifetime_ms > 10000 }
      .map_fn(h) { h.component }
    
    // 优化机会识别
    let optimization_opportunities = []
    
    // 识别高分配率但短生命周期的对象
    hotspots.each_fn(hotspot => {
      if hotspot.allocation_rate > 500 && hotspot.average_lifetime_ms < 1000 {
        optimization_opportunities.push(
          hotspot.component + ": Consider object pooling for high-frequency, short-lived allocations"
        )
      }
      
      // 识别大内存消耗但低分配率的对象
      if hotspot.total_size_bytes > 20 * 1024 * 1024 && hotspot.allocation_rate < 200 {
        optimization_opportunities.push(
          hotspot.component + ": Consider lazy loading or streaming for large objects"
        )
      }
      
      // 识别中等大小但长生命周期的对象
      if hotspot.total_size_bytes > 10 * 1024 * 1024 && 
         hotspot.average_lifetime_ms > 30000 &&
         hotspot.allocation_rate > 50 {
        optimization_opportunities.push(
          hotspot.component + ": Consider cache eviction policy optimization"
        )
      }
    })
    
    // 计算总体内存压力分数
    let total_allocations = hotspots.fold(0, fn(acc, h) { acc + h.allocation_count })
    let total_memory = hotspots.fold(0, fn(acc, h) { acc + h.total_size_bytes })
    let avg_allocation_rate = hotspots.fold(0, fn(acc, h) { acc + h.allocation_rate }) / hotspots.length()
    
    let pressure_score = (total_allocations.to_double() / 1000000.0 + // 标准化分配次数
                         total_memory.to_double() / (1024 * 1024 * 100).to_double() + // 标准化内存使用
                         avg_allocation_rate.to_double() / 1000.0) / 3.0 // 标准化分配率
    
    {
      top_consumers: top_consumers,
      high_frequency_allocators: high_frequency_allocators,
      long_lived_objects: long_lived_objects,
      optimization_opportunities: optimization_opportunities,
      overall_pressure_score: pressure_score
    }
  }
  
  // 执行热点分析
  let hotspot_analysis = analyze_memory_hotspots(memory_hotspots)
  
  // 验证分析结果
  assert_eq(hotspot_analysis.top_consumers.length(), 3)
  assert_eq(hotspot_analysis.top_consumers[0], "cache_manager") // 30MB最大
  assert_eq(hotspot_analysis.top_consumers[1], "metrics_buffer") // 25MB第二
  assert_true(hotspot_analysis.high_frequency_allocators.contains("serialization")) // 1000/秒
  assert_true(hotspot_analysis.high_frequency_allocators.contains("metrics_buffer"))  // 500/秒
  assert_eq(hotspot_analysis.long_lived_objects.length(), 1)
  assert_eq(hotspot_analysis.long_lived_objects[0], "cache_manager") // 60秒生命周期
  assert_true(hotspot_analysis.optimization_opportunities.length() > 0)
  assert_true(hotspot_analysis.overall_pressure_score > 0.0)
}

// 测试4: GC优化分析
test "GC优化分析测试" {
  // GC性能数据
  let gc_performance_data = [
    { timestamp: 1634567600, gc_type: "minor", pause_time_ms: 10, reclaimed_bytes: 5 * 1024 * 1024, before_heap: 30 * 1024 * 1024, after_heap: 25 * 1024 * 1024 },
    { timestamp: 1634567660, gc_type: "minor", pause_time_ms: 12, reclaimed_bytes: 4 * 1024 * 1024, before_heap: 32 * 1024 * 1024, after_heap: 28 * 1024 * 1024 },
    { timestamp: 1634567720, gc_type: "minor", pause_time_ms: 8, reclaimed_bytes: 6 * 1024 * 1024, before_heap: 35 * 1024 * 1024, after_heap: 29 * 1024 * 1024 },
    { timestamp: 1634567780, gc_type: "major", pause_time_ms: 150, reclaimed_bytes: 15 * 1024 * 1024, before_heap: 40 * 1024 * 1024, after_heap: 25 * 1024 * 1024 },
    { timestamp: 1634567840, gc_type: "minor", pause_time_ms: 15, reclaimed_bytes: 5 * 1024 * 1024, before_heap: 30 * 1024 * 1024, after_heap: 25 * 1024 * 1024 },
    { timestamp: 1634567900, gc_type: "minor", pause_time_ms: 11, reclaimed_bytes: 4 * 1024 * 1024, before_heap: 32 * 1024 * 1024, after_heap: 28 * 1024 * 1024 },
    { timestamp: 1634567960, gc_type: "minor", pause_time_ms: 9, reclaimed_bytes: 5 * 1024 * 1024, before_heap: 33 * 1024 * 1024, after_heap: 28 * 1024 * 1024 },
    { timestamp: 1634568020, gc_type: "major", pause_time_ms: 180, reclaimed_bytes: 18 * 1024 * 1024, before_heap: 45 * 1024 * 1024, after_heap: 27 * 1024 * 1024 }
  ]
  
  // GC优化分析函数
  let analyze_gc_optimization = fn(gc_data: Array[{
    timestamp: Int,
    gc_type: String,
    pause_time_ms: Int,
    reclaimed_bytes: Int,
    before_heap: Int,
    after_heap: Int
  }]) -> {
    gc_efficiency: Double,
    pause_time_distribution: { minor: { avg: Double, max: Int }, major: { avg: Double, max: Int } },
    gc_frequency: { minor_per_hour: Double, major_per_hour: Double },
    optimization_recommendations: Array[String],
    gc_pressure_score: Double
  } {
    if gc_data.length() == 0 {
      return {
        gc_efficiency: 0.0,
        pause_time_distribution: { minor: { avg: 0.0, max: 0 }, major: { avg: 0.0, max: 0 } },
        gc_frequency: { minor_per_hour: 0.0, major_per_hour: 0.0 },
        optimization_recommendations: [],
        gc_pressure_score: 0.0
      }
    }
    
    // 分别处理minor和major GC
    let minor_gc = gc_data.filter_fn(d) { d.gc_type == "minor" }
    let major_gc = gc_data.filter_fn(d) { d.gc_type == "major" }
    
    // 计算GC效率 (回收字节数/暂停时间)
    let calculate_efficiency = fn(data: Array[{
      timestamp: Int,
      gc_type: String,
      pause_time_ms: Int,
      reclaimed_bytes: Int,
      before_heap: Int,
      after_heap: Int
    }]) -> Double {
      if data.length() == 0 { return 0.0 }
      
      let total_reclaimed = data.fold(0, fn(acc, d) { acc + d.reclaimed_bytes })
      let total_pause = data.fold(0, fn(acc, d) { acc + d.pause_time_ms })
      
      if total_pause == 0 { return 0.0 }
      total_reclaimed.to_double() / total_pause.to_double()
    }
    
    let minor_efficiency = calculate_efficiency(minor_gc)
    let major_efficiency = calculate_efficiency(major_gc)
    
    // 加权平均效率
    let total_count = minor_gc.length() + major_gc.length()
    let gc_efficiency = if total_count > 0 {
      (minor_efficiency * minor_gc.length().to_double() + 
       major_efficiency * major_gc.length().to_double()) / total_count.to_double()
    } else { 0.0 }
    
    // 计算暂停时间分布
    let minor_avg_pause = if minor_gc.length() > 0 {
      minor_gc.fold(0, fn(acc, d) { acc + d.pause_time_ms }).to_double() / minor_gc.length().to_double()
    } else { 0.0 }
    
    let minor_max_pause = if minor_gc.length() > 0 {
      minor_gc.fold(0, fn(acc, d) { if d.pause_time_ms > acc { d.pause_time_ms } else { acc } })
    } else { 0 }
    
    let major_avg_pause = if major_gc.length() > 0 {
      major_gc.fold(0, fn(acc, d) { acc + d.pause_time_ms }).to_double() / major_gc.length().to_double()
    } else { 0.0 }
    
    let major_max_pause = if major_gc.length() > 0 {
      major_gc.fold(0, fn(acc, d) { if d.pause_time_ms > acc { d.pause_time_ms } else { acc } })
    } else { 0 }
    
    // 计算GC频率
    let time_span_hours = if gc_data.length() > 1 {
      (gc_data[gc_data.length() - 1].timestamp - gc_data[0].timestamp).to_double() / 3600.0
    } else { 1.0 }
    
    let minor_per_hour = minor_gc.length().to_double() / time_span_hours
    let major_per_hour = major_gc.length().to_double() / time_span_hours
    
    // 优化建议
    let optimization_recommendations = []
    
    // 检查major GC频率
    if major_per_hour > 2.0 {
      optimization_recommendations.push("Major GC frequency too high (" + major_per_hour.to_string() + 
                                       "/hr). Consider increasing heap size or optimizing object allocation")
    }
    
    // 检查暂停时间
    if major_avg_pause > 100.0 {
      optimization_recommendations.push("Major GC pause time too high (" + major_avg_pause.to_string() + 
                                       "ms). Consider using concurrent GC algorithms")
    }
    
    // 检查GC效率
    if gc_efficiency < 10000.0 { // 低于10KB/ms
      optimization_recommendations.push("GC efficiency low (" + gc_efficiency.to_string() + 
                                       " bytes/ms). Review object allocation patterns")
    }
    
    // 检查minor/major比例
    let minor_major_ratio = if major_gc.length() > 0 { 
      minor_gc.length().to_double() / major_gc.length().to_double() 
    } else { 999.0 }
    
    if minor_major_ratio < 5.0 {
      optimization_recommendations.push("Low minor/major GC ratio (" + minor_major_ratio.to_string() + 
                                       "). May indicate premature promotion to old generation")
    }
    
    // 计算GC压力分数
    let gc_pressure_score = (major_avg_pause / 10.0 + // 标准化暂停时间
                            major_per_hour * 10.0 +   // 标准化频率
                            (10000.0 / max(gc_efficiency, 1.0))) / 3.0 // 标准化效率
    
    {
      gc_efficiency: gc_efficiency,
      pause_time_distribution: {
        minor: { avg: minor_avg_pause, max: minor_max_pause },
        major: { avg: major_avg_pause, max: major_max_pause }
      },
      gc_frequency: { minor_per_hour: minor_per_hour, major_per_hour: major_per_hour },
      optimization_recommendations: optimization_recommendations,
      gc_pressure_score: gc_pressure_score
    }
  }
  
  // 执行GC优化分析
  let gc_optimization_result = analyze_gc_optimization(gc_performance_data)
  
  // 验证分析结果
  assert_true(gc_optimization_result.gc_efficiency > 0.0)
  assert_eq(gc_optimization_result.pause_time_distribution.minor.avg, 11.0) // (10+12+8+15+11+9)/6
  assert_eq(gc_optimization_result.pause_time_distribution.minor.max, 15)
  assert_eq(gc_optimization_result.pause_time_distribution.major.avg, 165.0) // (150+180)/2
  assert_eq(gc_optimization_result.pause_time_distribution.major.max, 180)
  assert_true(gc_optimization_result.gc_frequency.minor_per_hour > 0.0)
  assert_true(gc_optimization_result.gc_frequency.major_per_hour > 0.0)
  assert_true(gc_optimization_result.optimization_recommendations.length() > 0)
  assert_true(gc_optimization_result.gc_pressure_score > 0.0)
}

// 测试5: 内存池效果评估
test "内存池效果评估测试" {
  // 内存池使用前后对比数据
  let before_pool = {
    allocation_count: 50000,
    total_allocated_bytes: 100 * 1024 * 1024, // 100MB
    gc_count: 25,
    gc_pause_total_ms: 500,
    allocation_time_avg_ns: 1500,
    memory_fragmentation: 0.25
  }
  
  let after_pool = {
    allocation_count: 50000,
    total_allocated_bytes: 80 * 1024 * 1024, // 80MB (减少了20MB)
    gc_count: 15, // 减少了10次GC
    gc_pause_total_ms: 200, // 减少了300ms暂停时间
    allocation_time_avg_ns: 500, // 减少了1000ns分配时间
    memory_fragmentation: 0.15 // 减少了10%碎片率
  }
  
  // 内存池效果评估函数
  let evaluate_memory_pool_effectiveness = fn(before: {
    allocation_count: Int,
    total_allocated_bytes: Int,
    gc_count: Int,
    gc_pause_total_ms: Int,
    allocation_time_avg_ns: Int,
    memory_fragmentation: Double
  }, after: {
    allocation_count: Int,
    total_allocated_bytes: Int,
    gc_count: Int,
    gc_pause_total_ms: Int,
    allocation_time_avg_ns: Int,
    memory_fragmentation: Double
  }) -> {
    memory_reduction_percent: Double,
    gc_reduction_percent: Double,
    pause_time_reduction_percent: Double,
    allocation_speed_improvement: Double,
    fragmentation_reduction: Double,
    overall_effectiveness_score: Double,
    recommendations: Array[String]
  } {
    // 内存使用减少百分比
    let memory_reduction = (before.total_allocated_bytes - after.total_allocated_bytes).to_double() / 
                          before.total_allocated_bytes.to_double() * 100.0
    
    // GC次数减少百分比
    let gc_reduction = (before.gc_count - after.gc_count).to_double() / 
                      before.gc_count.to_double() * 100.0
    
    // GC暂停时间减少百分比
    let pause_time_reduction = (before.gc_pause_total_ms - after.gc_pause_total_ms).to_double() / 
                              before.gc_pause_total_ms.to_double() * 100.0
    
    // 分配速度提升百分比
    let allocation_speed_improvement = (before.allocation_time_avg_ns - after.allocation_time_avg_ns).to_double() / 
                                      before.allocation_time_avg_ns.to_double() * 100.0
    
    // 碎片率减少
    let fragmentation_reduction = (before.memory_fragmentation - after.memory_fragmentation) * 100.0
    
    // 总体效果评分 (加权平均)
    let overall_score = (memory_reduction * 0.3 + 
                        gc_reduction * 0.2 + 
                        pause_time_reduction * 0.2 + 
                        allocation_speed_improvement * 0.2 + 
                        fragmentation_reduction * 0.1)
    
    // 生成建议
    let recommendations = []
    
    if memory_reduction < 10.0 {
      recommendations.push("Consider increasing pool size or optimizing pool partitioning")
    }
    
    if gc_reduction < 20.0 {
      recommendations.push("Review pool usage patterns to reduce GC pressure")
    }
    
    if allocation_speed_improvement < 50.0 {
      recommendations.push("Consider implementing lock-free pool or thread-local pools")
    }
    
    if fragmentation_reduction < 5.0 {
      recommendations.push("Consider implementing variable-size pools or buddy allocation")
    }
    
    if overall_score > 50.0 {
      recommendations.push("Memory pool implementation is highly effective")
    } else if overall_score > 25.0 {
      recommendations.push("Memory pool shows moderate effectiveness")
    } else {
      recommendations.push("Memory pool needs optimization or may not be suitable for workload")
    }
    
    {
      memory_reduction_percent: memory_reduction,
      gc_reduction_percent: gc_reduction,
      pause_time_reduction_percent: pause_time_reduction,
      allocation_speed_improvement: allocation_speed_improvement,
      fragmentation_reduction: fragmentation_reduction,
      overall_effectiveness_score: overall_score,
      recommendations: recommendations
    }
  }
  
  // 评估内存池效果
  let pool_evaluation = evaluate_memory_pool_effectiveness(before_pool, after_pool)
  
  // 验证评估结果
  assert_eq(pool_evaluation.memory_reduction_percent, 20.0) // (100-80)/100*100
  assert_eq(pool_evaluation.gc_reduction_percent, 40.0) // (25-15)/25*100
  assert_eq(pool_evaluation.pause_time_reduction_percent, 60.0) // (500-200)/500*100
  assert_eq(pool_evaluation.allocation_speed_improvement, 66.67) // (1500-500)/1500*100
  assert_eq(pool_evaluation.fragmentation_reduction, 10.0) // (0.25-0.15)*100
  assert_true(pool_evaluation.overall_effectiveness_score > 30.0)
  assert_true(pool_evaluation.recommendations.length() > 0)
}

// 测试6: 内存泄漏修复验证
test "内存泄漏修复验证测试" {
  // 修复前的内存使用模式
  let memory_before_fix = [
    { timestamp: 1634567600, heap_used: 30 * 1024 * 1024, gc_count: 100 },
    { timestamp: 1634567660, heap_used: 35 * 1024 * 1024, gc_count: 105 },
    { timestamp: 1634567720, heap_used: 40 * 1024 * 1024, gc_count: 110 },
    { timestamp: 1634567780, heap_used: 45 * 1024 * 1024, gc_count: 115 },
    { timestamp: 1634567840, heap_used: 50 * 1024 * 1024, gc_count: 120 },
    { timestamp: 1634567900, heap_used: 55 * 1024 * 1024, gc_count: 125 },
    { timestamp: 1634567960, heap_used: 60 * 1024 * 1024, gc_count: 130 },
    { timestamp: 1634568020, heap_used: 65 * 1024 * 1024, gc_count: 135 }
  ]
  
  // 修复后的内存使用模式
  let memory_after_fix = [
    { timestamp: 1634568200, heap_used: 30 * 1024 * 1024, gc_count: 200 },
    { timestamp: 1634568260, heap_used: 32 * 1024 * 1024, gc_count: 205 },
    { timestamp: 1634568320, heap_used: 31 * 1024 * 1024, gc_count: 210 },
    { timestamp: 1634568380, heap_used: 33 * 1024 * 1024, gc_count: 215 },
    { timestamp: 1634568440, heap_used: 30 * 1024 * 1024, gc_count: 220 },
    { timestamp: 1634568500, heap_used: 32 * 1024 * 1024, gc_count: 225 },
    { timestamp: 1634568560, heap_used: 31 * 1024 * 1024, gc_count: 230 },
    { timestamp: 1634568620, heap_used: 33 * 1024 * 1024, gc_count: 235 }
  ]
  
  // 修复验证函数
  let verify_memory_leak_fix = fn(before: Array[{
    timestamp: Int,
    heap_used: Int,
    gc_count: Int
  }>, after: Array[{
    timestamp: Int,
    heap_used: Int,
    gc_count: Int
  }>) -> {
    fix_successful: Bool,
    growth_rate_before: Double,
    growth_rate_after: Double,
    stability_improvement: Double,
    gc_efficiency_improvement: Double,
    verification_confidence: Double,
    remaining_concerns: Array[String]
  } {
    // 计算修复前的增长率
    let before_growth = if before.length() >= 2 {
      let first = before[0]
      let last = before[before.length() - 1]
      let time_diff = last.timestamp - first.timestamp
      let heap_diff = last.heap_used - first.heap_used
      if time_diff > 0 { heap_diff.to_double() / time_diff.to_double() } else { 0.0 }
    } else { 0.0 }
    
    // 计算修复后的增长率
    let after_growth = if after.length() >= 2 {
      let first = after[0]
      let last = after[after.length() - 1]
      let time_diff = last.timestamp - first.timestamp
      let heap_diff = last.heap_used - first.heap_used
      if time_diff > 0 { heap_diff.to_double() / time_diff.to_double() } else { 0.0 }
    } else { 0.0 }
    
    // 计算稳定性改进 (基于变异系数)
    let calculate_stability = fn(data: Array<{ timestamp: Int, heap_used: Int, gc_count: Int }>) -> Double {
      if data.length() < 2 { return 1.0 }
      
      let heap_values = data.map_fn(d) { d.heap_used.to_double() }
      let mean = heap_values.fold(0.0, fn(acc, x) { acc + x }) / heap_values.length().to_double()
      let variance = heap_values.fold(0.0, fn(acc, x) {
        let diff = x - mean
        acc + diff * diff
      }) / heap_values.length().to_double()
      let std_dev = sqrt(variance)
      
      if mean > 0 { 1.0 - (std_dev / mean) } else { 0.0 } // 稳定性分数，越高越稳定
    }
    
    let before_stability = calculate_stability(before)
    let after_stability = calculate_stability(after)
    let stability_improvement = after_stability - before_stability
    
    // 计算GC效率改进
    let calculate_gc_efficiency = fn(data: Array<{ timestamp: Int, heap_used: Int, gc_count: Int }>) -> Double {
      if data.length() < 2 { return 0.0 }
      
      let first = data[0]
      let last = data[data.length() - 1]
      let heap_diff = last.heap_used - first.heap_used
      let gc_diff = last.gc_count - first.gc_count
      
      if gc_diff > 0 { heap_diff.to_double() / gc_diff.to_double() } else { 0.0 }
    }
    
    let before_gc_efficiency = calculate_gc_efficiency(before)
    let after_gc_efficiency = calculate_gc_efficiency(after)
    let gc_efficiency_improvement = after_gc_efficiency - before_gc_efficiency
    
    // 验证修复是否成功
    let fix_successful = before_growth > 0 && after_growth <= before_growth * 0.2 && // 增长率减少80%以上
                         stability_improvement > 0.1 && // 稳定性提升10%以上
                         gc_efficiency_improvement < 0 // GC效率改进(负值表示每次GC回收更多内存)
    
    // 计算验证置信度
    let growth_improvement = if before_growth > 0 { 1.0 - (after_growth / before_growth) } else { 0.0 }
    let verification_confidence = (growth_improvement * 0.5 + 
                                  min(stability_improvement * 5.0, 1.0) * 0.3 + 
                                  min(abs(gc_efficiency_improvement) * 10.0, 1.0) * 0.2)
    
    // 识别剩余问题
    let remaining_concerns = []
    
    if after_growth > 100.0 { // 仍有显著增长
      remaining_concerns.push("Memory still growing at " + after_growth.to_string() + " bytes/sec")
    }
    
    if after_stability < 0.8 { // 稳定性仍不足
      remaining_concerns.push("Memory usage still highly variable")
    }
    
    if after_gc_efficiency > 1000.0 { // GC效率仍低
      remaining_concerns.push("GC efficiency still suboptimal")
    }
    
    {
      fix_successful: fix_successful,
      growth_rate_before: before_growth,
      growth_rate_after: after_growth,
      stability_improvement: stability_improvement,
      gc_efficiency_improvement: gc_efficiency_improvement,
      verification_confidence: verification_confidence,
      remaining_concerns: remaining_concerns
    }
  }
  
  // 验证修复效果
  let fix_verification = verify_memory_leak_fix(memory_before_fix, memory_after_fix)
  
  // 验证结果
  assert_true(fix_verification.fix_successful)
  assert_true(fix_verification.growth_rate_before > 0.0)
  assert_true(fix_verification.growth_rate_after < fix_verification.growth_rate_before)
  assert_true(fix_verification.stability_improvement > 0.0)
  assert_true(fix_verification.verification_confidence > 0.5)
}

// 测试7: 内存监控自动化
test "内存监控自动化测试" {
  // 监控配置
  let monitoring_config = {
    sampling_interval_sec: 60,       // 1分钟采样一次
    alert_threshold_mb: 100,         // 100MB警报阈值
    growth_rate_threshold_mb_per_hr: 50, // 每小时增长50MB警报阈值
    gc_frequency_threshold_per_hr: 10,  // 每小时10次GC警报阈值
    fragmentation_threshold: 0.4,    // 40%碎片率警报阈值
    auto_recovery_enabled: true,     // 启用自动恢复
    auto_recovery_actions: ["restart_service", "increase_heap", "force_gc"]
  }
  
  // 当前系统状态
  let current_system_state = {
    heap_used: 120 * 1024 * 1024,    // 120MB
    heap_total: 200 * 1024 * 1024,   // 200MB
    gc_count: 150,
    timestamp: 1634568600,
    service_uptime: 7200,            // 2小时
    last_restart: 1634561400         // 2小时前重启
  }
  
  // 历史监控数据
  let monitoring_history = [
    { timestamp: 1634565000, heap_used: 40 * 1024 * 1024, gc_count: 50 },
    { timestamp: 1634565060, heap_used: 42 * 1024 * 1024, gc_count: 52 },
    { timestamp: 1634565120, heap_used: 45 * 1024 * 1024, gc_count: 54 },
    { timestamp: 1634565180, heap_used: 48 * 1024 * 1024, gc_count: 56 },
    { timestamp: 1634565240, heap_used: 52 * 1024 * 1024, gc_count: 58 },
    { timestamp: 1634565300, heap_used: 56 * 1024 * 1024, gc_count: 60 },
    { timestamp: 1634565360, heap_used: 60 * 1024 * 1024, gc_count: 62 },
    { timestamp: 1634565420, heap_used: 65 * 1024 * 1024, gc_count: 64 },
    { timestamp: 1634565480, heap_used: 70 * 1024 * 1024, gc_count: 66 },
    { timestamp: 1634565540, heap_used: 75 * 1024 * 1024, gc_count: 68 },
    { timestamp: 1634565600, heap_used: 80 * 1024 * 1024, gc_count: 70 },
    { timestamp: 1634565660, heap_used: 85 * 1024 * 1024, gc_count: 72 },
    { timestamp: 1634565720, heap_used: 90 * 1024 * 1024, gc_count: 74 },
    { timestamp: 1634565780, heap_used: 95 * 1024 * 1024, gc_count: 76 },
    { timestamp: 1634565840, heap_used: 100 * 1024 * 1024, gc_count: 78 },
    { timestamp: 1634565900, heap_used: 105 * 1024 * 1024, gc_count: 80 },
    { timestamp: 1634565960, heap_used: 110 * 1024 * 1024, gc_count: 82 },
    { timestamp: 1634566020, heap_used: 115 * 1024 * 1024, gc_count: 84 },
    { timestamp: 1634566080, heap_used: 120 * 1024 * 1024, gc_count: 86 }
  ]
  
  // 自动化监控函数
  let automated_memory_monitoring = fn(config: {
    sampling_interval_sec: Int,
    alert_threshold_mb: Int,
    growth_rate_threshold_mb_per_hr: Int,
    gc_frequency_threshold_per_hr: Int,
    fragmentation_threshold: Double,
    auto_recovery_enabled: Bool,
    auto_recovery_actions: Array[String]
  }, current: {
    heap_used: Int,
    heap_total: Int,
    gc_count: Int,
    timestamp: Int,
    service_uptime: Int,
    last_restart: Int
  }, history: Array[{
    timestamp: Int,
    heap_used: Int,
    gc_count: Int
  }>) -> {
    alerts: Array[String],
    auto_recovery_triggered: Bool,
    recommended_action: String,
    monitoring_health: String,
    next_check_time: Int
  } {
    let alerts = []
    
    // 检查内存使用量
    let current_heap_mb = current.heap_used / (1024 * 1024)
    if current_heap_mb > config.alert_threshold_mb {
      alerts.push("Memory usage (" + current_heap_mb.to_string() + 
                  "MB) exceeds threshold (" + config.alert_threshold_mb.to_string() + "MB)")
    }
    
    // 检查增长率
    if history.length() >= 2 {
      let oldest = history[0]
      let growth_mb = (current.heap_used - oldest.heap_used) / (1024 * 1024)
      let time_hours = (current.timestamp - oldest.timestamp).to_double() / 3600.0
      let growth_rate_mb_per_hr = if time_hours > 0 { growth_mb.to_double() / time_hours } else { 0.0 }
      
      if growth_rate_mb_per_hr > config.growth_rate_threshold_mb_per_hr.to_double() {
        alerts.push("Memory growth rate (" + growth_rate_mb_per_hr.to_string() + 
                    "MB/hr) exceeds threshold (" + config.growth_rate_threshold_mb_per_hr.to_string() + "MB/hr)")
      }
    }
    
    // 检查GC频率
    if history.length() >= 2 {
      let oldest = history[0]
      let gc_diff = current.gc_count - oldest.gc_count
      let time_hours = (current.timestamp - oldest.timestamp).to_double() / 3600.0
      let gc_per_hr = if time_hours > 0 { gc_diff.to_double() / time_hours } else { 0.0 }
      
      if gc_per_hr > config.gc_frequency_threshold_per_hr.to_double() {
        alerts.push("GC frequency (" + gc_per_hr.to_string() + 
                    "/hr) exceeds threshold (" + config.gc_frequency_threshold_per_hr.to_string() + "/hr)")
      }
    }
    
    // 检查内存碎片率
    let fragmentation = (current.heap_total - current.heap_used).to_double() / current.heap_total.to_double()
    if fragmentation > config.fragmentation_threshold {
      alerts.push("Memory fragmentation (" + (fragmentation * 100.0).to_string() + 
                  "%) exceeds threshold (" + (config.fragmentation_threshold * 100.0).to_string() + "%)")
    }
    
    // 确定是否需要自动恢复
    let auto_recovery_triggered = config.auto_recovery_enabled && alerts.length() >= 2
    
    // 推荐行动
    let recommended_action = if auto_recovery_triggered {
      // 基于警报类型选择最合适的恢复行动
      if alerts.any_fn(alert => alert.contains("Memory usage") && current_heap_mb > 150) {
        "restart_service"
      } else if alerts.any_fn(alert => alert.contains("fragmentation")) {
        "increase_heap"
      } else {
        "force_gc"
      }
    } else if alerts.length() > 0 {
      "increase_monitoring_frequency"
    } else {
      "continue_monitoring"
    }
    
    // 监控系统健康状态
    let monitoring_health = if alerts.length() == 0 { "healthy" }
                           else if alerts.length() <= 2 { "warning" }
                           else if alerts.length() <= 4 { "degraded" }
                           else { "critical" }
    
    // 下次检查时间
    let next_check_time = current.timestamp + config.sampling_interval_sec
    
    {
      alerts: alerts,
      auto_recovery_triggered: auto_recovery_triggered,
      recommended_action: recommended_action,
      monitoring_health: monitoring_health,
      next_check_time: next_check_time
    }
  }
  
  // 执行自动化监控
  let monitoring_result = automated_memory_monitoring(monitoring_config, current_system_state, monitoring_history)
  
  // 验证监控结果
  assert_true(monitoring_result.alerts.length() > 0)
  assert_eq(monitoring_result.monitoring_health, "warning") // 120MB超过100MB阈值
  assert_true(monitoring_result.alerts.any_fn(alert => alert.contains("Memory usage")))
  assert_eq(monitoring_result.next_check_time, 1634568600 + 60) // 当前时间+60秒
}

// 测试8: 内存泄漏预测模型
test "内存泄漏预测模型测试" {
  // 历史内存使用数据 (用于训练预测模型)
  let training_data = [
    { day: 1, memory_usage_mb: 30, gc_efficiency: 0.8, allocation_rate: 100, object_lifetime_avg: 5000 },
    { day: 2, memory_usage_mb: 32, gc_efficiency: 0.78, allocation_rate: 110, object_lifetime_avg: 5200 },
    { day: 3, memory_usage_mb: 35, gc_efficiency: 0.75, allocation_rate: 120, object_lifetime_avg: 5500 },
    { day: 4, memory_usage_mb: 38, gc_efficiency: 0.72, allocation_rate: 130, object_lifetime_avg: 5800 },
    { day: 5, memory_usage_mb: 42, gc_efficiency: 0.70, allocation_rate: 140, object_lifetime_avg: 6200 },
    { day: 6, memory_usage_mb: 46, gc_efficiency: 0.68, allocation_rate: 150, object_lifetime_avg: 6600 },
    { day: 7, memory_usage_mb: 51, gc_efficiency: 0.65, allocation_rate: 160, object_lifetime_avg: 7100 },
    { day: 8, memory_usage_mb: 56, gc_efficiency: 0.62, allocation_rate: 170, object_lifetime_avg: 7700 },
    { day: 9, memory_usage_mb: 62, gc_efficiency: 0.60, allocation_rate: 180, object_lifetime_avg: 8400 },
    { day: 10, memory_usage_mb: 68, gc_efficiency: 0.58, allocation_rate: 190, object_lifetime_avg: 9200 }
  ]
  
  // 当前系统状态
  let current_metrics = {
    memory_usage_mb: 75,
    gc_efficiency: 0.55,
    allocation_rate: 200,
    object_lifetime_avg: 10000
  }
  
  // 内存泄漏预测函数 (简化线性回归模型)
  let predict_memory_leak = fn(training_data: Array[{
    day: Int,
    memory_usage_mb: Int,
    gc_efficiency: Double,
    allocation_rate: Int,
    object_lifetime_avg: Int
  }], current: {
    memory_usage_mb: Int,
    gc_efficiency: Double,
    allocation_rate: Int,
    object_lifetime_avg: Int
  }) -> {
    leak_probability: Double,
    predicted_usage_7days: Int,
    predicted_usage_30days: Int,
    time_to_threshold_days: Int,
    key_indicators: Array[String],
    confidence_level: Double
  } {
    if training_data.length() < 5 {
      return {
        leak_probability: 0.0,
        predicted_usage_7days: 0,
        predicted_usage_30days: 0,
        time_to_threshold_days: -1,
        key_indicators: [],
        confidence_level: 0.0
      }
    }
    
    // 简化的线性回归计算每日增长率
    let days = training_data.map_fn(d) { d.day.to_double() }
    let memory_usage = training_data.map_fn(d) { d.memory_usage_mb.to_double() }
    
    // 计算斜率 (增长率)
    let n = days.length().to_double()
    let sum_x = days.fold(0.0, fn(acc, x) { acc + x })
    let sum_y = memory_usage.fold(0.0, fn(acc, y) { acc + y })
    let sum_xy = days.zip(memory_usage).fold(0.0, fn(acc, (x, y)) { acc + x * y })
    let sum_x2 = days.fold(0.0, fn(acc, x) { acc + x * x })
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    // 预测未来使用量
    let predict_usage = fn(days_ahead: Int) -> Int {
      let future_day = (training_data[training_data.length() - 1].day + days_ahead).to_double()
      (slope * future_day + intercept).to_int()
    }
    
    let predicted_7days = predict_usage(7)
    let predicted_30days = predict_usage(30)
    
    // 计算达到阈值的时间 (假设阈值为200MB)
    let threshold = 200
    let time_to_threshold = if slope > 0 {
      ((threshold - intercept) / slope - training_data[training_data.length() - 1].day).to_int()
    } else { -1 }
    
    // 计算泄漏概率 (基于多个指标)
    let growth_factor = if slope > 0 { min(slope * 10, 1.0) } else { 0.0 } // 标准化增长率
    
    let gc_efficiency_factor = if current.gc_efficiency < 0.6 {
      (0.6 - current.gc_efficiency) * 2.0 // GC效率低于60%时增加风险
    } else { 0.0 }
    
    let allocation_rate_factor = if current.allocation_rate > 150 {
      min((current.allocation_rate - 150).to_double() / 100.0, 1.0)
    } else { 0.0 }
    
    let lifetime_factor = if current.object_lifetime_avg > 8000 {
      min((current.object_lifetime_avg - 8000).to_double() / 10000.0, 1.0)
    } else { 0.0 }
    
    let leak_probability = (growth_factor * 0.4 + 
                           gc_efficiency_factor * 0.3 + 
                           allocation_rate_factor * 0.2 + 
                           lifetime_factor * 0.1)
    
    // 识别关键指标
    let key_indicators = []
    
    if slope > 3.0 {
      key_indicators.push("High memory growth rate: " + slope.to_string() + "MB/day")
    }
    
    if current.gc_efficiency < 0.6 {
      key_indicators.push("Low GC efficiency: " + (current.gc_efficiency * 100.0).to_string() + "%")
    }
    
    if current.allocation_rate > 150 {
      key_indicators.push("High allocation rate: " + current.allocation_rate.to_string() + "/sec")
    }
    
    if current.object_lifetime_avg > 8000 {
      key_indicators.push("Long object lifetime: " + current.object_lifetime_avg.to_string() + "ms")
    }
    
    // 计算置信度 (基于训练数据量和拟合度)
    let confidence_level = if training_data.length() >= 10 { 0.8 }
                          else if training_data.length() >= 7 { 0.6 }
                          else { 0.4 }
    
    {
      leak_probability: leak_probability,
      predicted_usage_7days: predicted_7days,
      predicted_usage_30days: predicted_30days,
      time_to_threshold_days: time_to_threshold,
      key_indicators: key_indicators,
      confidence_level: confidence_level
    }
  }
  
  // 执行预测
  let prediction_result = predict_memory_leak(training_data, current_metrics)
  
  // 验证预测结果
  assert_true(prediction_result.leak_probability > 0.0)
  assert_true(prediction_result.predicted_usage_7days > current_metrics.memory_usage_mb)
  assert_true(prediction_result.predicted_usage_30days > prediction_result.predicted_usage_7days)
  assert_true(prediction_result.key_indicators.length() > 0)
  assert_true(prediction_result.confidence_level > 0.0)
}