// 内存优化测试
// 测试遥测系统的内存使用优化和垃圾回收策略

test "telemetry_buffer_pool_optimization" {
  // 测试遥测缓冲区池的内存优化
  
  // 创建缓冲区池
  let buffer_pool = BufferPool::new(
    initial_size: 10,
    max_size: 100,
    buffer_capacity: 1024
  )
  
  // 验证初始状态
  assert_eq(buffer_pool.available_buffers(), 10)
  assert_eq(buffer_pool.allocated_buffers(), 0)
  
  // 模拟高频缓冲区分配和释放
  let allocated_buffers = []
  for i = 0; i < 50; i = i + 1 {
    let buffer = buffer_pool.acquire()
    allocated_buffers.push(buffer)
  }
  
  // 验证分配状态
  assert_eq(buffer_pool.available_buffers(), 0)  // 初始缓冲区已用完
  assert_eq(buffer_pool.allocated_buffers(), 50) // 新分配了40个缓冲区
  
  // 释放一半缓冲区
  for i = 0; i < 25; i = i + 1 {
    buffer_pool.release(allocated_buffers[i])
  }
  
  // 验证释放后的状态
  assert_eq(buffer_pool.available_buffers(), 25)  // 缓冲区回到池中
  assert_eq(buffer_pool.allocated_buffers(), 25)  // 还有25个在使用中
  
  // 测试缓冲区池的内存效率
  let memory_usage = buffer_pool.get_memory_usage()
  assert_eq(memory_usage.total_allocated_mb <= 10.0, true) // 不应超过10MB
  assert_eq(memory_usage.pool_efficiency >= 0.8, true)    // 池效率应高于80%
}

test "metric_data_compression_in_memory" {
  // 测试指标数据的内存压缩
  
  // 创建大量指标数据
  let raw_metrics = []
  for i = 0; i < 10000; i = i + 1 {
    raw_metrics.push(MetricPoint{
      name: "cpu_usage",
      value: 50.0 + @random.float(50.0),
      timestamp: 1609459200L + i.to_int64(),
      labels: [
        ("host", "server-" + (i % 10).to_string()),
        ("region", if i % 2 == 0 { "us-east" } else { "us-west" })
      ]
    })
  }
  
  // 计算原始内存占用
  let original_memory = calculate_metrics_memory_usage(raw_metrics)
  assert_eq(original_memory > 1.0, true) // 至少1MB
  
  // 应用内存压缩
  let compressor = MetricCompressor::new()
  let compressed_metrics = compressor.compress_in_memory(raw_metrics)
  
  // 验证压缩效果
  let compressed_memory = calculate_compressed_memory_usage(compressed_metrics)
  let compression_ratio = compressed_memory / original_memory
  
  assert_eq(compression_ratio < 0.6, true) // 至少40%的压缩率
  assert_eq(compressed_metrics.count, raw_metrics.length()) // 数据点数量不变
  
  // 验证压缩数据的访问性能
  let access_start_time = get_current_timestamp()
  for i = 0; i < 1000; i = i + 1 {
    let random_index = @random.int(raw_metrics.length())
    let _ = compressed_metrics.get_metric(random_index)
  }
  let access_end_time = get_current_timestamp()
  let access_duration = access_end_time - access_start_time
  
  assert_eq(access_duration < 100, true) // 访问时间应小于100ms
}

test "garbage_collection_optimization" {
  // 测试垃圾回收优化策略
  
  // 创建GC优化器
  let gc_optimizer = GCOptimizer::new(GCConfig{
    threshold_memory_mb: 100,
    trigger_interval_ms: 5000L,
    max_gc_pause_ms: 50,
    strategy: "generational"
  })
  
  // 模拟内存压力
  let memory_pressure_data = []
  for i = 0; i < 1000; i = i + 1 {
    // 创建临时对象增加内存压力
    let temp_data = create_large_temporary_data(10000) // 10KB数据
    memory_pressure_data.push(temp_data)
    
    // 每100个对象检查一次GC状态
    if i % 100 == 0 {
      let gc_stats = gc_optimizer.check_gc_status()
      if gc_stats.memory_usage_mb > 50 {
        gc_optimizer.trigger_optimized_gc()
      }
    }
  }
  
  // 验证GC优化效果
  let final_gc_stats = gc_optimizer.get_gc_statistics()
  assert_eq(final_gc_stats.total_gc_runs > 0, true)
  assert_eq(final_gc_stats.average_gc_pause_ms <= 50, true) // 平均暂停时间不超过50ms
  assert_eq(final_gc_stats.memory_recovered_mb > 0, true)    // 回收了内存
  
  // 测试分代GC策略
  let generational_stats = gc_optimizer.get_generational_stats()
  assert_eq(generational_stats.young_gc_runs > generational_stats.old_gc_runs, true)
  assert_eq(generational_stats.young_gc_efficiency >= 0.8, true)
}

test "memory_leak_detection" {
  // 测试内存泄漏检测
  
  // 创建内存泄漏检测器
  let leak_detector = MemoryLeakDetector::new()
  
  // 记录初始内存状态
  let initial_memory = leak_detector.get_memory_snapshot()
  
  // 模拟可能导致内存泄漏的操作
  let potential_leak_sources = []
  
  for i = 0; i < 100; i = i + 1 {
    // 创建可能泄漏的对象
    let telemetry_session = TelemetrySession::new("session-" + i.to_string())
    potential_leak_sources.push(telemetry_session)
    
    // 模拟未正确清理的资源
    if i % 10 == 0 {
      let _ = create_uncleaned_resource("resource-" + i.to_string())
    }
  }
  
  // 强制GC并检查内存状态
  force_garbage_collection()
  let after_gc_memory = leak_detector.get_memory_snapshot()
  
  // 分析内存增长
  let memory_growth = after_gc_memory.heap_size_mb - initial_memory.heap_size_mb
  let leak_analysis = leak_detector.analyze_memory_growth(memory_growth, potential_leak_sources.length())
  
  // 验证泄漏检测结果
  if leak_analysis.potential_leak_detected {
    assert_eq(leak_analysis.leak_sources.length() > 0, true)
    assert_eq(leak_analysis.memory_leak_rate_mb_per_object > 0, true)
    
    // 生成泄漏报告
    let leak_report = leak_detector.generate_leak_report()
    assert_eq(leak_report.contains("Potential memory leak detected"), true)
    assert_eq(leak_report.contains("TelemetrySession"), true)
  }
  
  // 清理资源并再次检查
  cleanup_all_resources(potential_leak_sources)
  force_garbage_collection()
  let final_memory = leak_detector.get_memory_snapshot()
  
  // 验证内存已释放
  let final_growth = final_memory.heap_size_mb - initial_memory.heap_size_mb
  assert_eq(final_growth < memory_growth, true) // 内存使用应该下降
}

test "cache_memory_management" {
  // 测试缓存内存管理
  
  // 创建内存感知缓存
  let cache = MemoryAwareCache::new(CacheConfig{
    max_memory_mb: 50,
    eviction_policy: "lru",
    cleanup_threshold: 0.8
  })
  
  // 填充缓存直到达到内存限制
  let cache_keys = []
  for i = 0; i < 1000; i = i + 1 {
    let key = "metric-" + i.to_string()
    let value = create_large_metric_data(1000) // 1KB数据
    
    let put_result = cache.put(key, value)
    cache_keys.push(key)
    
    // 检查缓存状态
    if i % 100 == 0 {
      let cache_stats = cache.get_memory_stats()
      if cache_stats.memory_usage_mb > 40 { // 接近限制
        break
      }
    }
  }
  
  // 验证缓存内存管理
  let final_cache_stats = cache.get_memory_stats()
  assert_eq(final_cache_stats.memory_usage_mb <= 50, true) // 不应超过限制
  assert_eq(final_cache_stats.eviction_count > 0, true)     // 应该有驱逐操作
  
  // 测试LRU驱逐策略
  let first_key = cache_keys[0]
  let first_value_before = cache.get(first_key)
  
  // 访问一些键，然后添加新键触发驱逐
  for i = 0; i < 10; i = i + 1 {
    let _ = cache.get(cache_keys[i]) // 访问前10个键
  }
  
  // 添加更多数据触发驱逐
  for i = 1000; i < 1200; i = i + 1 {
    let _ = cache.put("new-metric-" + i.to_string(), create_large_metric_data(1000))
  }
  
  // 验证LRU策略效果
  let first_value_after = cache.get(first_key)
  match (first_value_before, first_value_after) {
    (Some(_), None) => assert_eq(true, true) // 旧值应该被驱逐
    (Some(before), Some(after)) => assert_eq(before, after) // 如果还在，应该相同
    _ => @test.fail("Unexpected cache behavior")
  }
  
  // 测试缓存内存清理
  let memory_before_cleanup = final_cache_stats.memory_usage_mb
  cache.cleanup_expired_entries()
  let memory_after_cleanup = cache.get_memory_stats().memory_usage_mb
  
  assert_eq(memory_after_cleanup <= memory_before_cleanup, true) // 清理后内存应该减少
}

test "object_pooling_optimization" {
  // 测试对象池化优化
  
  // 创建不同类型的对象池
  let span_pool = ObjectPool::new(create_span, reset_span, 100)
  let metric_pool = ObjectPool::new(create_metric, reset_metric, 200)
  let log_pool = ObjectPool::new(create_log_record, reset_log_record, 150)
  
  // 模拟高频对象创建和销毁
  let operation_count = 10000
  
  // 使用对象池的版本
  let pool_start_time = get_current_timestamp()
  for i = 0; i < operation_count; i = i + 1 {
    let span = span_pool.acquire()
    let metric = metric_pool.acquire()
    let log_record = log_pool.acquire()
    
    // 模拟使用对象
    span.name = "operation-" + i.to_string()
    metric.value = @random.float(100.0)
    log_record.message = "Log message " + i.to_string()
    
    // 归还对象到池中
    span_pool.release(span)
    metric_pool.release(metric)
    log_pool.release(log_record)
  }
  let pool_end_time = get_current_timestamp()
  let pool_duration = pool_end_time - pool_start_time
  
  // 直接创建对象的版本（不使用池）
  let direct_start_time = get_current_timestamp()
  for i = 0; i < operation_count; i = i + 1 {
    let span = create_span()
    let metric = create_metric()
    let log_record = create_log_record()
    
    span.name = "operation-" + i.to_string()
    metric.value = @random.float(100.0)
    log_record.message = "Log message " + i.to_string()
    
    // 对象会被GC回收
  }
  let direct_end_time = get_current_timestamp()
  let direct_duration = direct_end_time - direct_start_time
  
  // 验证对象池的性能优势
  let performance_improvement = (direct_duration - pool_duration).to_float() / direct_duration.to_float()
  assert_eq(performance_improvement > 0.2, true) // 至少20%的性能提升
  
  // 验证对象池的内存效率
  let span_pool_stats = span_pool.get_statistics()
  let metric_pool_stats = metric_pool.get_statistics()
  let log_pool_stats = log_pool.get_statistics()
  
  assert_eq(span_pool_stats.hit_rate >= 0.9, true)    // 命中率应高于90%
  assert_eq(metric_pool_stats.hit_rate >= 0.9, true)
  assert_eq(log_pool_stats.hit_rate >= 0.9, true)
  
  assert_eq(span_pool_stats.memory_saved_mb > 0, true) // 应该节省内存
  assert_eq(metric_pool_stats.memory_saved_mb > 0, true)
  assert_eq(log_pool_stats.memory_saved_mb > 0, true)
}