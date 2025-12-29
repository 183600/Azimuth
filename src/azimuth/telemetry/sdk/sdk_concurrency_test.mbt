// SDK并发和资源管理测试用例
test "sdk_concurrent_operations" {
  // 测试SDK并发操作
  
  struct ConcurrentOperation {
    operation_id: String
    operation_type: String
    start_time: Int64
    end_time: Int64?
    status: String
    thread_id: Int
    data_size: Int
  }
  
  struct ConcurrencyMetrics {
    total_operations: Int
    successful_operations: Int
    failed_operations: Int
    avg_duration_ms: Double
    max_duration_ms: Double
    min_duration_ms: Double
    concurrent_threads: Int
  }
  
  // 模拟并发操作
  let simulate_concurrent_operations = fn(
    operation_count: Int,
    thread_count: Int
  ) -> (Array[ConcurrentOperation], ConcurrencyMetrics) {
    let operations = []
    let base_time = 1640995200000000L
    
    let mut i = 0
    while i < operation_count {
      let thread_id = i % thread_count
      let start_time = base_time + i.to_int64() * 1000L
      let duration = 50 + (i % 200)  // 50-250ms duration
      let end_time = start_time + duration.to_int64() * 1000000L  // Convert to nanoseconds
      
      let status = if i % 20 != 0 { "success" } else { "failed" }
      let data_size = 1024 * (1 + i % 10)  // 1KB to 10KB
      
      let operation = ConcurrentOperation{
        operation_id: "op_" + i.to_string(),
        operation_type: if i % 3 == 0 { "span" } else if i % 3 == 1 { "metric" } else { "log" },
        start_time: start_time,
        end_time: Some(end_time),
        status: status,
        thread_id: thread_id,
        data_size: data_size
      }
      
      operations.push(operation)
      i = i + 1
    }
    
    // 计算并发指标
    let successful_ops = operations.filter(fn(op) { op.status == "success" })
    let failed_ops = operations.filter(fn(op) { op.status == "failed" })
    
    let durations = successful_ops.map(fn(op) {
      match op.end_time {
        Some(end) => (end - op.start_time).to_double() / 1000000.0,  // Convert to ms
        None => 0.0
      }
    })
    
    let avg_duration = if durations.length() > 0 {
      durations.reduce(fn(acc, d) { acc + d }) / durations.length().to_double()
    } else {
      0.0
    }
    
    let max_duration = if durations.length() > 0 {
      durations.reduce(fn(acc, d) { if d > acc { d } else { acc } })
    } else {
      0.0
    }
    
    let min_duration = if durations.length() > 0 {
      durations.reduce(fn(acc, d) { if d < acc { d } else { acc } })
    } else {
      0.0
    }
    
    let metrics = ConcurrencyMetrics{
      total_operations: operation_count,
      successful_operations: successful_ops.length(),
      failed_operations: failed_ops.length(),
      avg_duration_ms: avg_duration,
      max_duration_ms: max_duration,
      min_duration_ms: min_duration,
      concurrent_threads: thread_count
    }
    
    (operations, metrics)
  }
  
  // 分析并发性能
  let analyze_concurrency_performance = fn(
    operations: Array[ConcurrentOperation],
    metrics: ConcurrencyMetrics
  ) -> Array[String] {
    let insights = []
    
    // 成功率分析
    let success_rate = metrics.successful_operations.to_double() / metrics.total_operations.to_double() * 100.0
    insights.push("Success rate: " + success_rate.to_string() + "%")
    
    if success_rate < 95.0 {
      insights.push("WARNING: Low success rate detected")
    }
    
    // 性能分析
    if metrics.avg_duration_ms > 150.0 {
      insights.push("WARNING: High average latency: " + metrics.avg_duration_ms.to_string() + "ms")
    }
    
    if metrics.max_duration_ms > metrics.avg_duration_ms * 3.0 {
      insights.push("WARNING: High latency variance detected")
    }
    
    // 并发度分析
    let operations_per_thread = metrics.total_operations / metrics.concurrent_threads
    insights.push("Operations per thread: " + operations_per_thread.to_string())
    
    // 操作类型分析
    let span_ops = operations.filter(fn(op) { op.operation_type == "span" })
    let metric_ops = operations.filter(fn(op) { op.operation_type == "metric" })
    let log_ops = operations.filter(fn(op) { op.operation_type == "log" })
    
    insights.push("Span operations: " + span_ops.length().to_string())
    insights.push("Metric operations: " + metric_ops.length().to_string())
    insights.push("Log operations: " + log_ops.length().to_string())
    
    // 吞吐量分析
    let total_time_ms = if operations.length() > 0 {
      let last_op = operations[operations.length() - 1]
      let first_op = operations[0]
      ((last_op.start_time - first_op.start_time) / 1000000L).to_double()
    } else {
      0.0
    }
    
    let throughput = if total_time_ms > 0.0 {
      metrics.total_operations.to_double() / (total_time_ms / 1000.0)
    } else {
      0.0
    }
    
    insights.push("Throughput: " + throughput.to_string() + " ops/sec")
    
    insights
  }
  
  // 测试低并发度
  let (low_concurrency_ops, low_concurrency_metrics) = simulate_concurrent_operations(100, 2)
  
  assert_eq(low_concurrency_metrics.total_operations, 100)
  assert_eq(low_concurrency_metrics.concurrent_threads, 2)
  assert_eq(low_concurrency_metrics.successful_operations + low_concurrency_metrics.failed_operations, 100)
  
  let low_insights = analyze_concurrency_performance(low_concurrency_ops, low_concurrency_metrics)
  assert_eq(low_insights.length(), 8)
  
  // 测试中等并发度
  let (medium_concurrency_ops, medium_concurrency_metrics) = simulate_concurrent_operations(500, 8)
  
  assert_eq(medium_concurrency_metrics.total_operations, 500)
  assert_eq(medium_concurrency_metrics.concurrent_threads, 8)
  assert_eq(medium_concurrency_metrics.successful_operations + medium_concurrency_metrics.failed_operations, 500)
  
  let medium_insights = analyze_concurrency_performance(medium_concurrency_ops, medium_concurrency_metrics)
  assert_eq(medium_insights.length(), 8)
  
  // 测试高并发度
  let (high_concurrency_ops, high_concurrency_metrics) = simulate_concurrent_operations(1000, 16)
  
  assert_eq(high_concurrency_metrics.total_operations, 1000)
  assert_eq(high_concurrency_metrics.concurrent_threads, 16)
  assert_eq(high_concurrency_metrics.successful_operations + high_concurrency_metrics.failed_operations, 1000)
  
  let high_insights = analyze_concurrency_performance(high_concurrency_ops, high_concurrency_metrics)
  assert_eq(high_insights.length(), 8)
  
  // 验证性能指标合理性
  assert_eq(low_concurrency_metrics.avg_duration_ms >= 50.0, true)
  assert_eq(high_concurrency_metrics.avg_duration_ms >= 50.0, true)
  assert_eq(low_concurrency_metrics.max_duration_ms >= low_concurrency_metrics.min_duration_ms, true)
  assert_eq(high_concurrency_metrics.max_duration_ms >= high_concurrency_metrics.min_duration_ms, true)
}

test "sdk_resource_management" {
  // 测试SDK资源管理
  
  enum ResourceType {
    Memory
    Connection
    Buffer
    Thread
    FileHandle
  }
  
  struct ResourceAllocation {
    resource_type: ResourceType
    allocation_id: String
    size_bytes: Int
    allocated_at: Int64
    released_at: Int64?
    is_active: Bool
    thread_id: Int
  }
  
  struct ResourceMetrics {
    total_allocated: Int
    total_released: Int
    currently_active: Int
    peak_usage: Int
    total_memory_bytes: Int64
    memory_efficiency: Double
  }
  
  // 模拟资源分配
  let allocate_resource = fn(
    resource_type: ResourceType,
    size_bytes: Int,
    thread_id: Int
  ) -> ResourceAllocation {
    let timestamp = 1640995200000000L  // 模拟时间戳
    
    ResourceAllocation{
      resource_type: resource_type,
      allocation_id: resource_type.to_string() + "_" + timestamp.to_string(),
      size_bytes: size_bytes,
      allocated_at: timestamp,
      released_at: None,
      is_active: true,
      thread_id: thread_id
    }
  }
  
  // 释放资源
  let release_resource = fn(allocation: ResourceAllocation) -> ResourceAllocation {
    let release_time = allocation.allocated_at + 5000000000L  // 5秒后释放
    { allocation |
      released_at: Some(release_time),
      is_active: false
    }
  }
  
  // 模拟资源管理场景
  let simulate_resource_management = fn(
    operation_count: Int,
    max_concurrent_resources: Int
  ) -> (Array[ResourceAllocation], ResourceMetrics) {
    let allocations = []
    let mut current_active = 0
    let mut peak_usage = 0
    let mut total_memory = 0L
    
    let mut i = 0
    while i < operation_count {
      let resource_type = match i % 5 {
        0 => Memory,
        1 => Connection,
        2 => Buffer,
        3 => Thread,
        _ => FileHandle
      }
      
      let size = match resource_type {
        Memory => 1024 * 1024 * (1 + i % 10),  // 1MB to 10MB
        Connection => 64 * 1024,  // 64KB
        Buffer => 256 * 1024 * (1 + i % 4),  // 256KB to 1MB
        Thread => 8 * 1024 * 1024,  // 8MB
        FileHandle => 32 * 1024  // 32KB
      }
      
      let thread_id = i % 8
      
      // 分配资源
      let allocation = allocate_resource(resource_type, size, thread_id)
      
      // 检查并发限制
      if current_active < max_concurrent_resources {
        allocations.push(allocation)
        current_active = current_active + 1
        total_memory = total_memory + size.to_int64()
        
        if current_active > peak_usage {
          peak_usage = current_active
        }
        
        // 随机释放一些资源
        if i % 3 == 0 && allocations.length() > 0 {
          let release_index = i % allocations.length()
          let released = release_resource(allocations[release_index])
          allocations[release_index] = released
          current_active = current_active - 1
        }
      }
      
      i = i + 1
    }
    
    // 释放所有剩余资源
    let mut j = 0
    while j < allocations.length() {
      if allocations[j].is_active {
        allocations[j] = release_resource(allocations[j])
        current_active = current_active - 1
      }
      j = j + 1
    }
    
    // 计算资源指标
    let total_allocated = allocations.length()
    let total_released = allocations.filter(fn(a) { !a.is_active }).length()
    let currently_active = allocations.filter(fn(a) { a.is_active }).length()
    
    let memory_efficiency = if total_memory > 0L {
      let used_memory = allocations.filter(fn(a) { a.is_active }).map(fn(a) { a.size_bytes.to_int64() }).reduce(fn(acc, m) { acc + m })
      (used_memory.to_double() / total_memory.to_double()) * 100.0
    } else {
      0.0
    }
    
    let metrics = ResourceMetrics{
      total_allocated: total_allocated,
      total_released: total_released,
      currently_active: currently_active,
      peak_usage: peak_usage,
      total_memory_bytes: total_memory,
      memory_efficiency: memory_efficiency
    }
    
    (allocations, metrics)
  }
  
  // 分析资源使用模式
  let analyze_resource_usage = fn(
    allocations: Array[ResourceAllocation],
    metrics: ResourceMetrics
  ) -> Array[String] {
    let insights = []
    
    // 总体使用情况
    insights.push("Total allocations: " + metrics.total_allocated.to_string())
    insights.push("Total released: " + metrics.total_released.to_string())
    insights.push("Currently active: " + metrics.currently_active.to_string())
    insights.push("Peak usage: " + metrics.peak_usage.to_string())
    
    // 内存使用分析
    insights.push("Total memory allocated: " + (metrics.total_memory_bytes / 1024 / 1024).to_string() + "MB")
    insights.push("Memory efficiency: " + metrics.memory_efficiency.to_string() + "%")
    
    if metrics.memory_efficiency < 70.0 {
      insights.push("WARNING: Low memory efficiency detected")
    }
    
    // 按资源类型分析
    let memory_allocations = allocations.filter(fn(a) { a.resource_type == Memory })
    let connection_allocations = allocations.filter(fn(a) { a.resource_type == Connection })
    let buffer_allocations = allocations.filter(fn(a) { a.resource_type == Buffer })
    let thread_allocations = allocations.filter(fn(a) { a.resource_type == Thread })
    let file_handle_allocations = allocations.filter(fn(a) { a.resource_type == FileHandle })
    
    insights.push("Memory allocations: " + memory_allocations.length().to_string())
    insights.push("Connection allocations: " + connection_allocations.length().to_string())
    insights.push("Buffer allocations: " + buffer_allocations.length().to_string())
    insights.push("Thread allocations: " + thread_allocations.length().to_string())
    insights.push("File handle allocations: " + file_handle_allocations.length().to_string())
    
    // 资源生命周期分析
    let active_allocations = allocations.filter(fn(a) { a.is_active })
    let released_allocations = allocations.filter(fn(a) { !a.is_active })
    
    if active_allocations.length() > 0 {
      insights.push("WARNING: " + active_allocations.length().to_string() + " resources still active")
    }
    
    // 计算平均资源生命周期
    let lifetimes = released_allocations.map(fn(a) {
      match a.released_at {
        Some(released) => (released - a.allocated_at) / 1000000L,  // Convert to ms
        None => 0L
      }
    })
    
    if lifetimes.length() > 0 {
      let avg_lifetime = lifetimes.reduce(fn(acc, l) { acc + l }) / lifetimes.length().to_int64()
      insights.push("Average resource lifetime: " + avg_lifetime.to_string() + "ms")
    }
    
    insights
  }
  
  // 测试低负载资源管理
  let (low_load_allocations, low_load_metrics) = simulate_resource_management(50, 10)
  
  assert_eq(low_load_metrics.total_allocated, 50)
  assert_eq(low_load_metrics.currently_active, 0)  // 所有资源都应该被释放
  assert_eq(low_load_metrics.peak_usage <= 10, true)
  
  let low_insights = analyze_resource_usage(low_load_allocations, low_load_metrics)
  assert_eq(low_insights.length() >= 10, true)
  
  // 测试中等负载资源管理
  let (medium_load_allocations, medium_load_metrics) = simulate_resource_management(200, 25)
  
  assert_eq(medium_load_metrics.total_allocated, 200)
  assert_eq(medium_load_metrics.currently_active, 0)
  assert_eq(medium_load_metrics.peak_usage <= 25, true)
  assert_eq(medium_load_metrics.total_memory_bytes > low_load_metrics.total_memory_bytes, true)
  
  let medium_insights = analyze_resource_usage(medium_load_allocations, medium_load_metrics)
  assert_eq(medium_insights.length() >= 10, true)
  
  // 测试高负载资源管理
  let (high_load_allocations, high_load_metrics) = simulate_resource_management(500, 50)
  
  assert_eq(high_load_metrics.total_allocated, 500)
  assert_eq(high_load_metrics.currently_active, 0)
  assert_eq(high_load_metrics.peak_usage <= 50, true)
  assert_eq(high_load_metrics.total_memory_bytes > medium_load_metrics.total_memory_bytes, true)
  
  let high_insights = analyze_resource_usage(high_load_allocations, high_load_metrics)
  assert_eq(high_insights.length() >= 10, true)
  
  // 验证资源分配的合理性
  assert_eq(low_load_metrics.memory_efficiency >= 0.0 && low_load_metrics.memory_efficiency <= 100.0, true)
  assert_eq(high_load_metrics.memory_efficiency >= 0.0 && high_load_metrics.memory_efficiency <= 100.0, true)
  
  // 验证内存使用量随负载增加
  assert_eq(high_load_metrics.total_memory_bytes > medium_load_metrics.total_memory_bytes, true)
  assert_eq(medium_load_metrics.total_memory_bytes > low_load_metrics.total_memory_bytes, true)
}

test "sdk_memory_leak_detection" {
  // 测试SDK内存泄漏检测
  
  struct MemorySnapshot {
    timestamp: Int64
    heap_used_bytes: Int64
    heap_total_bytes: Int64
    stack_used_bytes: Int64
    buffer_count: Int
    active_objects: Int
    gc_count: Int
  }
  
  struct MemoryLeakReport {
    snapshots: Array[MemorySnapshot]
    leak_detected: Bool
    leak_rate_bytes_per_sec: Double
    peak_memory_usage: Int64
    avg_memory_usage: Double
    gc_efficiency: Double
  }
  
  // 模拟内存快照
  let take_memory_snapshot = fn(
    operation_count: Int,
    base_memory: Int64,
    leak_factor: Double
  ) -> MemorySnapshot {
    let timestamp = 1640995200000000L + operation_count.to_int64() * 1000000L
    
    // 模拟内存使用模式
    let normal_growth = base_memory + (operation_count * 1024).to_int64()  // 正常增长
    let leak_growth = (operation_count.to_double() * leak_factor * 1024.0).to_int64()  // 泄漏增长
    let random_variation = (operation_count % 100 * 1024).to_int64()  // 随机变化
    
    let heap_used = normal_growth + leak_growth + random_variation
    let heap_total = heap_used + (1024 * 1024 * 100).to_int64()  // 假设总堆空间
    let stack_used = (1024 * 1024 * 10).to_int64()  // 固定栈使用
    let buffer_count = operation_count / 10
    let active_objects = operation_count * 2
    let gc_count = operation_count / 50
    
    MemorySnapshot{
      timestamp: timestamp,
      heap_used_bytes: heap_used,
      heap_total_bytes: heap_total,
      stack_used_bytes: stack_used,
      buffer_count: buffer_count,
      active_objects: active_objects,
      gc_count: gc_count
    }
  }
  
  // 检测内存泄漏
  let detect_memory_leak = fn(snapshots: Array[MemorySnapshot]) -> MemoryLeakReport {
    if snapshots.length() < 2 {
      return MemoryLeakReport{
        snapshots: snapshots,
        leak_detected: false,
        leak_rate_bytes_per_sec: 0.0,
        peak_memory_usage: 0L,
        avg_memory_usage: 0.0,
        gc_efficiency: 100.0
      }
    }
    
    let first_snapshot = snapshots[0]
    let last_snapshot = snapshots[snapshots.length() - 1]
    
    // 计算泄漏率
    let time_diff_ms = (last_snapshot.timestamp - first_snapshot.timestamp) / 1000000L
    let memory_diff = last_snapshot.heap_used_bytes - first_snapshot.heap_used_bytes
    
    let leak_rate = if time_diff_ms > 0L {
      memory_diff.to_double() / (time_diff_ms.to_double() / 1000.0)  // bytes per second
    } else {
      0.0
    }
    
    // 计算峰值和平均内存使用
    let memory_usages = snapshots.map(fn(s) { s.heap_used_bytes.to_double() })
    let peak_memory = memory_usages.reduce(fn(acc, m) { if m > acc { m } else { acc } }).to_int64()
    let avg_memory = memory_usages.reduce(fn(acc, m) { acc + m }) / memory_usages.length().to_double()
    
    // 计算GC效率
    let total_gc = snapshots[snapshots.length() - 1].gc_count
    let gc_efficiency = if total_gc > 0 {
      let total_memory_freed = snapshots.map(fn(s) { s.heap_used_bytes }).reduce(fn(acc, m) { if m > acc { m } else { acc } }) - snapshots[0].heap_used_bytes
      (total_memory_freed.to_double() / (total_gc.to_double() * 1024.0 * 1024.0)) * 100.0
    } else {
      100.0
    }
    
    // 检测泄漏（如果泄漏率超过阈值）
    let leak_detected = leak_rate > 1024.0  // 超过1KB/s认为是泄漏
    
    MemoryLeakReport{
      snapshots: snapshots,
      leak_detected: leak_detected,
      leak_rate_bytes_per_sec: leak_rate,
      peak_memory_usage: peak_memory,
      avg_memory_usage: avg_memory,
      gc_efficiency: gc_efficiency
    }
  }
  
  // 生成内存使用报告
  let generate_memory_report = fn(report: MemoryLeakReport) -> Array[String] {
    let insights = []
    
    insights.push("Memory snapshots analyzed: " + report.snapshots.length().to_string())
    insights.push("Leak detected: " + (if report.leak_detected { "YES" } else { "NO" }))
    insights.push("Leak rate: " + (report.leak_rate_bytes_per_sec / 1024.0).to_string() + " KB/sec")
    insights.push("Peak memory usage: " + (report.peak_memory_usage / 1024 / 1024).to_string() + " MB")
    insights.push("Average memory usage: " + (report.avg_memory_usage / 1024 / 1024).to_string() + " MB")
    insights.push("GC efficiency: " + report.gc_efficiency.to_string() + "%")
    
    if report.leak_detected {
      insights.push("WARNING: Memory leak detected!")
      insights.push("RECOMMENDATION: Investigate object lifecycle management")
    }
    
    if report.gc_efficiency < 50.0 {
      insights.push("WARNING: Low GC efficiency detected")
      insights.push("RECOMMENDATION: Optimize object allocation patterns")
    }
    
    // 内存使用趋势分析
    if report.snapshots.length() >= 3 {
      let recent_snapshots = report.snapshots.slice(report.snapshots.length() - 3, report.snapshots.length())
      let recent_growth = recent_snapshots[2].heap_used_bytes - recent_snapshots[0].heap_used_bytes
      let recent_time = (recent_snapshots[2].timestamp - recent_snapshots[0].timestamp) / 1000000L
      
      if recent_time > 0L {
        let recent_rate = recent_growth.to_double() / (recent_time.to_double() / 1000.0)
        insights.push("Recent growth rate: " + (recent_rate / 1024.0).to_string() + " KB/sec")
        
        if recent_rate > report.leak_rate_bytes_per_sec * 1.5 {
          insights.push("WARNING: Memory growth rate is accelerating")
        }
      }
    }
    
    insights
  }
  
  // 测试正常内存使用（无泄漏）
  let normal_snapshots = []
  let base_memory = (50 * 1024 * 1024).to_int64()  // 50MB base
  let mut i = 0
  while i < 20 {
    let snapshot = take_memory_snapshot(i, base_memory, 0.0)  // 无泄漏
    normal_snapshots.push(snapshot)
    i = i + 1
  }
  
  let normal_report = detect_memory_leak(normal_snapshots)
  let normal_insights = generate_memory_report(normal_report)
  
  assert_eq(normal_report.leak_detected, false)
  assert_eq(normal_report.leak_rate_bytes_per_sec <= 1024.0, true)
  assert_eq(normal_insights.length() >= 6, true)
  
  // 测试内存泄漏场景
  let leak_snapshots = []
  let mut j = 0
  while j < 20 {
    let snapshot = take_memory_snapshot(j, base_memory, 10.0)  // 10x泄漏因子
    leak_snapshots.push(snapshot)
    j = j + 1
  }
  
  let leak_report = detect_memory_leak(leak_snapshots)
  let leak_insights = generate_memory_report(leak_report)
  
  assert_eq(leak_report.leak_detected, true)
  assert_eq(leak_report.leak_rate_bytes_per_sec > 1024.0, true)
  assert_eq(leak_insights.length() >= 7, true)
  assert_eq(leak_insights.any(fn(insight) { insight.contains("Memory leak detected") }), true)
  
  // 测试边界情况
  let single_snapshot = [take_memory_snapshot(0, base_memory, 0.0)]
  let single_report = detect_memory_leak(single_snapshot)
  
  assert_eq(single_report.leak_detected, false)
  assert_eq(single_report.leak_rate_bytes_per_sec, 0.0)
  
  // 验证内存使用量的合理性
  assert_eq(normal_report.peak_memory_usage > 0L, true)
  assert_eq(leak_report.peak_memory_usage > normal_report.peak_memory_usage, true)
  assert_eq(normal_report.avg_memory_usage > 0.0, true)
  assert_eq(leak_report.avg_memory_usage > normal_report.avg_memory_usage, true)
}