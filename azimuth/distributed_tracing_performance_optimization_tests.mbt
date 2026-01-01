// 分布式追踪性能优化测试
// 测试追踪性能调优、批量处理、异步操作和资源优化

pub test "追踪性能基准测试" {
  let performance_manager = azimuth::PerformanceManager::new()
  
  // 创建性能基准测试管理器
  let benchmark_manager = azimuth::PerformanceManager::create_benchmark_manager(performance_manager)
  
  // 定义基准测试场景
  let benchmark_scenarios = [
    {
      name: "lightweight_tracing",
      span_count: 10,
      attribute_count: 5,
      event_count: 2,
      expected_latency_ms: 10,
      expected_memory_kb: 100
    },
    {
      name: "medium_tracing",
      span_count: 100,
      attribute_count: 10,
      event_count: 5,
      expected_latency_ms: 50,
      expected_memory_kb: 500
    },
    {
      name: "heavy_tracing",
      span_count: 1000,
      attribute_count: 20,
      event_count: 10,
      expected_latency_ms: 200,
      expected_memory_kb: 2000
    }
  ]
  
  // 执行基准测试
  let benchmark_results = []
  
  for scenario in benchmark_scenarios {
    let result = azimuth::BenchmarkManager::run_scenario(benchmark_manager, scenario)
    benchmark_results.push(result)
    
    // 验证性能指标在预期范围内
    assert_true(result.actual_latency_ms <= scenario.expected_latency_ms * 1.5)  # 允许50%误差
    assert_true(result.actual_memory_kb <= scenario.expected_memory_kb * 1.5)
    assert_true(result.success_rate >= 0.99)  # 99%成功率
  }
  
  // 验证性能扩展性
  let light_result = benchmark_results[0]
  let medium_result = benchmark_results[1]
  let heavy_result = benchmark_results[2]
  
  # 延迟应该随负载线性增长（而不是指数增长）
  let light_to_medium_ratio = medium_result.actual_latency_ms.to_double() / light_result.actual_latency_ms.to_double()
  let medium_to_heavy_ratio = heavy_result.actual_latency_ms.to_double() / medium_result.actual_latency_ms.to_double()
  
  assert_true(light_to_medium_ratio <= 10)  # 不应该超过10倍
  assert_true(medium_to_heavy_ratio <= 10)  # 不应该超过10倍
  
  // 内存使用也应该合理扩展
  let light_to_medium_memory_ratio = medium_result.actual_memory_kb.to_double() / light_result.actual_memory_kb.to_double()
  let medium_to_heavy_memory_ratio = heavy_result.actual_memory_kb.to_double() / medium_result.actual_memory_kb.to_double()
  
  assert_true(light_to_medium_memory_ratio <= 10)
  assert_true(medium_to_heavy_memory_ratio <= 10)
  
  // 测试并发性能
  let concurrency_levels = [1, 5, 10, 20, 50]
  let concurrency_results = []
  
  for level in concurrency_levels {
    let concurrency_result = azimuth::BenchmarkManager::run_concurrency_test(benchmark_manager, {
      scenario: "medium_tracing",
      concurrent_operations: level,
      duration_seconds: 10
    })
    
    concurrency_results.push(concurrency_result)
    
    # 验证并发性能
    assert_true(concurrency_result.throughput_ops_per_sec > 0)
    assert_true(concurrency_result.average_latency_ms < 1000)  # 平均延迟小于1秒
    assert_true(concurrency_result.error_rate < 0.01)  # 错误率小于1%
  }
  
  // 验证并发扩展性
  let single_thread_throughput = concurrency_results[0].throughput_ops_per_sec
  let max_thread_throughput = concurrency_results[concurrency_results.length() - 1].throughput_ops_per_sec
  
  # 吞吐量应该随线程数增加而增加（虽然不是线性）
  assert_true(max_thread_throughput > single_thread_throughput)
  
  # 测试资源使用优化
  let resource_optimization = azimuth::BenchmarkManager::analyze_resource_usage(benchmark_manager)
  
  assert_true(resource_optimization.cpu_usage_percentage >= 0 && resource_optimization.cpu_usage_percentage <= 100)
  assert_true(resource_optimization.memory_usage_mb > 0)
  assert_true(resource_optimization.gc_frequency_per_sec >= 0)
  assert_true(resource_optimization.thread_count > 0)
  
  // 验证资源使用在合理范围内
  assert_true(resource_optimization.memory_usage_mb < 1000)  # 小于1GB
  assert_true(resource_optimization.gc_frequency_per_sec < 10)  # GC频率不应过高
}

pub test "批量追踪处理优化测试" {
  let performance_manager = azimuth::PerformanceManager::new()
  
  // 创建批量处理管理器
  let batch_manager = azimuth::PerformanceManager::create_batch_manager(performance_manager)
  
  // 配置批量处理参数
  let batch_configs = [
    {
      name: "small_batch",
      batch_size: 10,
      flush_interval_ms: 100,
      max_wait_ms: 500,
      compression_enabled: false
    },
    {
      name: "medium_batch",
      batch_size: 100,
      flush_interval_ms: 500,
      max_wait_ms: 2000,
      compression_enabled: true
    },
    {
      name: "large_batch",
      batch_size: 1000,
      flush_interval_ms: 1000,
      max_wait_ms: 5000,
      compression_enabled: true
    }
  ]
  
  // 测试不同批量配置的性能
  let batch_results = []
  
  for config in batch_configs {
    azimuth::BatchManager::configure(batch_manager, config)
    
    # 生成测试数据
    let test_spans = []
    for i in 0..config.batch_size * 5 {  # 5个批次的数据
      let span = create_test_span(i)
      test_spans.push(span)
    }
    
    # 执行批量处理
    let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    for span in test_spans {
      azimuth::BatchManager::add_span(batch_manager, span)
    }
    
    # 等待所有批次处理完成
    azimuth::BatchManager::flush_all(batch_manager)
    
    let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let total_time_ms = (end_time - start_time) / 1000000L
    
    # 获取处理统计
    let stats = azimuth::BatchManager::get_statistics(batch_manager)
    
    batch_results.push({
      config_name: config.name,
      batch_size: config.batch_size,
      total_spans: test_spans.length(),
      total_time_ms,
      batches_processed: stats.batches_processed,
      average_batch_time_ms: stats.average_batch_time_ms,
      throughput_spans_per_sec: (test_spans.length().to_double() / total_time_ms.to_double()) * 1000.0,
      compression_ratio: stats.compression_ratio
    })
  }
  
  // 验证批量处理性能优势
  let small_batch = batch_results[0]
  let medium_batch = batch_results[1]
  let large_batch = batch_results[2]
  
  # 大批次应该有更高的吞吐量
  assert_true(large_batch.throughput_spans_per_sec > medium_batch.throughput_spans_per_sec)
  assert_true(medium_batch.throughput_spans_per_sec > small_batch.throughput_spans_per_sec)
  
  # 验证压缩效果
  assert_true(medium_batch.compression_ratio > 1.0)
  assert_true(large_batch.compression_ratio > 1.0)
  assert_true(large_batch.compression_ratio >= medium_batch.compression_ratio)  # 大批次压缩效果更好
  
  // 测试自适应批量大小
  let adaptive_batch_manager = azimuth::PerformanceManager::create_adaptive_batch_manager(performance_manager)
  
  # 配置自适应参数
  azimuth::AdaptiveBatchManager::configure(adaptive_batch_manager, {
    min_batch_size: 10,
    max_batch_size: 1000,
    target_latency_ms: 50,
    adjustment_interval_ms: 5000,
    load_threshold_high: 100,
    load_threshold_low: 20
  })
  
  # 模拟不同负载情况
  let load_scenarios = [
    { name: "low_load", spans_per_second: 10, duration_seconds: 10 },
    { name: "medium_load", spans_per_second: 100, duration_seconds: 10 },
    { name: "high_load", spans_per_second: 500, duration_seconds: 10 }
  ]
  
  let adaptive_results = []
  
  for scenario in load_scenarios {
    azimuth::AdaptiveBatchManager::reset_metrics(adaptive_batch_manager)
    
    # 生成负载
    let total_spans = scenario.spans_per_second * scenario.duration_seconds
    let span_interval_ms = 1000 / scenario.spans_per_second
    
    for i in 0..total_spans {
      let span = create_test_span(i)
      azimuth::AdaptiveBatchManager::add_span(adaptive_batch_manager, span)
      
      # 模拟时间间隔
      azimuth::Clock::sleep(span_interval_ms)
    }
    
    # 等待处理完成
    azimuth::AdaptiveBatchManager::flush_all(adaptive_batch_manager)
    
    # 获取自适应结果
    let adaptive_stats = azimuth::AdaptiveBatchManager::get_statistics(adaptive_batch_manager)
    
    adaptive_results.push({
      scenario_name: scenario.name,
      load_spans_per_sec: scenario.spans_per_second,
      adaptive_batch_size: adaptive_stats.current_batch_size,
      average_latency_ms: adaptive_stats.average_latency_ms,
      throughput_spans_per_sec: adaptive_stats.throughput_spans_per_sec,
      adjustment_count: adaptive_stats.adjustment_count
    })
  }
  
  # 验证自适应调整效果
  let low_load_result = adaptive_results[0]
  let high_load_result = adaptive_results[2]
  
  # 高负载时应该使用更大的批次大小
  assert_true(high_load_result.adaptive_batch_size > low_load_result.adaptive_batch_size)
  
  # 延迟应该保持在目标范围内
  assert_true(high_load_result.average_latency_ms <= 100)  # 允许一定超出
  
  // 测试批量处理错误处理
  let error_handling_manager = azimuth::PerformanceManager::create_error_aware_batch_manager(performance_manager)
  
  # 配置错误处理策略
  azimuth::ErrorAwareBatchManager::configure_error_handling(error_handling_manager, {
    max_retry_attempts: 3,
    retry_backoff_ms: 100,
    dead_letter_queue_enabled: true,
    error_threshold_percentage: 10.0
  })
  
  # 模拟部分失败的场景
  let test_spans_with_errors = []
  for i in 0..100 {
    let span = if i % 10 == 0 { 
      create_failing_test_span(i) 
    } else { 
      create_test_span(i) 
    }
    test_spans_with_errors.push(span)
  }
  
  # 处理包含错误的批次
  for span in test_spans_with_errors {
    azimuth::ErrorAwareBatchManager::add_span(error_handling_manager, span)
  }
  
  azimuth::ErrorAwareBatchManager::flush_all(error_handling_manager)
  
  # 获取错误处理统计
  let error_stats = azimuth::ErrorAwareBatchManager::get_error_statistics(error_handling_manager)
  
  assert_true(error_stats.total_spans >= 100)
  assert_true(error_stats.successful_spans >= 90)  # 至少90%成功
  assert_true(error_stats.failed_spans >= 5)      # 至少5%失败
  assert_true(error_stats.retry_attempts > 0)
  assert_true(error_stats.dead_letter_queue_size >= 5)
  
  # 测试批量处理的内存优化
  let memory_optimized_manager = azimuth::PerformanceManager::create_memory_optimized_batch_manager(performance_manager)
  
  # 配置内存优化参数
  azimuth::MemoryOptimizedBatchManager::configure(memory_optimized_manager, {
    max_memory_mb: 100,
    memory_check_interval_ms: 1000,
    gc_trigger_threshold: 0.8,
    object_pool_enabled: true,
    span_recycling_enabled: true
  })
  
  # 生成大量数据测试内存使用
  let memory_test_spans = []
  for i in 0..10000 {
    let span = create_large_test_span(i)  # 创建较大的span
    memory_test_spans.push(span)
  }
  
  let memory_start = azimuth::MemoryOptimizedBatchManager::get_memory_usage(memory_optimized_manager)
  
  for span in memory_test_spans {
    azimuth::MemoryOptimizedBatchManager::add_span(memory_optimized_manager, span)
  }
  
  azimuth::MemoryOptimizedBatchManager::flush_all(memory_optimized_manager)
  
  let memory_end = azimuth::MemoryOptimizedBatchManager::get_memory_usage(memory_optimized_manager)
  
  # 验证内存使用在限制范围内
  assert_true(memory_end.peak_memory_mb <= 100)  # 不应超过100MB
  
  # 验证内存优化效果
  let memory_efficiency = memory_test_spans.length().to_double() / memory_end.peak_memory_mb.to_double()
  assert_true(memory_efficiency > 50)  # 每MB至少处理50个span
}

pub test "异步追踪操作优化测试" {
  let performance_manager = azimuth::PerformanceManager::new()
  
  // 创建异步操作管理器
  let async_manager = azimuth::PerformanceManager::create_async_manager(performance_manager)
  
  // 配置异步处理参数
  azimuth::AsyncManager::configure(async_manager, {
    worker_threads: 8,
    queue_capacity: 10000,
    batch_size: 50,
    timeout_ms: 5000,
    backpressure_threshold: 0.8
  })
  
  // 测试异步span创建
  let async_span_results = []
  let span_counts = [100, 500, 1000, 5000]
  
  for count in span_counts {
    let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    let async_tasks = []
    for i in 0..count {
      let task = azimuth::AsyncManager::create_span_async(async_manager, {
        name: "async-span-" + i.to_string(),
        operation_name: "async-operation",
        attributes: [("test.id", i.to_string())]
      })
      async_tasks.push(task)
    }
    
    # 等待所有任务完成
    let completed_spans = []
    for task in async_tasks {
      let span = azimuth::AsyncManager::wait_for_completion(async_manager, task)
      completed_spans.push(span)
    }
    
    let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let total_time_ms = (end_time - start_time) / 1000000L
    
    async_span_results.push({
      span_count: count,
      total_time_ms,
      throughput_spans_per_sec: (count.to_double() / total_time_ms.to_double()) * 1000.0,
      average_latency_ms: total_time_ms / count.to_long()
    })
  }
  
  # 验证异步处理的扩展性
  let first_result = async_span_results[0]
  let last_result = async_span_results[async_span_results.length() - 1]
  
  # 吞吐量应该随负载增加而保持相对稳定
  let throughput_ratio = last_result.throughput_spans_per_sec / first_result.throughput_spans_per_sec
  assert_true(throughput_ratio > 0.5)  # 不应该下降超过50%
  
  # 测试异步事件处理
  let async_event_results = []
  let event_counts = [50, 200, 1000]
  
  for count in event_counts {
    let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    let event_tasks = []
    for i in 0..count {
      let task = azimuth::AsyncManager::add_event_async(async_manager, {
        span_id: "span-" + (i % 10).to_string(),
        event_name: "async-event-" + i.to_string(),
        attributes: [("event.id", i.to_string())]
      })
      event_tasks.push(task)
    }
    
    # 等待所有事件处理完成
    let completed_events = azimuth::AsyncManager::wait_for_all_events(async_manager, event_tasks)
    
    let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let total_time_ms = (end_time - start_time) / 1000000L
    
    async_event_results.push({
      event_count: count,
      total_time_ms,
      throughput_events_per_sec: (count.to_double() / total_time_ms.to_double()) * 1000.0
    })
  }
  
  # 验证异步事件处理性能
  for result in async_event_results {
    assert_true(result.throughput_events_per_sec > 100)  # 每秒至少处理100个事件
  }
  
  // 测试异步导出操作
  let export_manager = azimuth::AsyncManager::create_export_manager(async_manager)
  
  # 配置导出参数
  azimuth::ExportManager::configure(export_manager, {
    export_interval_ms: 1000,
    max_export_batch_size: 500,
    retry_attempts: 3,
    compression_enabled: true
  })
  
  # 生成测试数据用于导出
  let export_spans = []
  for i in 0..1000 {
    let span = create_test_span(i)
    export_spans.push(span)
  }
  
  # 异步导出
  let export_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let export_tasks = []
  for batch_start in range(0, export_spans.length(), 100) {
    let batch_end = min(batch_start + 100, export_spans.length())
    let batch = export_spans.slice(batch_start, batch_end)
    
    let task = azimuth::ExportManager::export_batch_async(export_manager, batch)
    export_tasks.push(task)
  }
  
  # 等待所有导出完成
  let export_results = []
  for task in export_tasks {
    let result = azimuth::AsyncManager::wait_for_completion(async_manager, task)
    export_results.push(result)
  }
  
  let export_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let export_time_ms = (export_end - export_start) / 1000000L
  
  # 验证导出结果
  let successful_exports = export_results.filter(fn(r) { r.success }).length
  assert_true(successful_exports >= export_tasks.length() * 0.95)  # 至少95%成功
  
  let export_throughput = export_spans.length().to_double() / (export_time_ms.to_double() / 1000.0)
  assert_true(export_throughput > 100)  # 每秒至少导出100个span
  
  // 测试异步操作的背压处理
  let backpressure_test_spans = []
  for i in 0..20000 {  # 生成大量span触发背压
    let span = create_test_span(i)
    backpressure_test_spans.push(span)
  }
  
  let backpressure_stats_before = azimuth::AsyncManager::get_backpressure_stats(async_manager)
  
  # 快速提交大量任务
  let backpressure_tasks = []
  for span in backpressure_test_spans {
    let task = azimuth::AsyncManager::create_span_async(async_manager, {
      name: span.name,
      operation_name: span.operation_name
    })
    backpressure_tasks.push(task)
  }
  
  # 检查背压是否触发
  let backpressure_stats_after = azimuth::AsyncManager::get_backpressure_stats(async_manager)
  
  # 验证背压机制
  assert_true(backpressure_stats_after.queue_utilization > backpressure_stats_before.queue_utilization)
  assert_true(backpressure_stats_after.dropped_tasks >= 0)
  
  # 测试异步操作的优先级处理
  let priority_manager = azimuth::AsyncManager::create_priority_manager(async_manager)
  
  # 配置优先级
  azimuth::PriorityManager::configure_priorities(priority_manager, [
    { level: "critical", weight: 10, max_queue_size: 100 },
    { level: "high", weight: 5, max_queue_size: 500 },
    { level: "normal", weight: 1, max_queue_size: 2000 },
    { level: "low", weight: 0.5, max_queue_size: 5000 }
  ])
  
  # 创建不同优先级的任务
  let priority_tasks = []
  let priority_counts = { "critical": 10, "high": 50, "normal": 200, "low": 100 }
  
  for (priority, count) in priority_counts {
    for i in 0..count {
      let task = azimuth::PriorityManager::create_priority_task(priority_manager, {
        priority: priority,
        operation: fn() { create_test_span(i) },
        metadata: [("priority", priority), ("id", i.to_string())]
      })
      priority_tasks.push(task)
    }
  }
  
  # 等待所有任务完成
  let priority_results = []
  for task in priority_tasks {
    let result = azimuth::PriorityManager::wait_for_completion(priority_manager, task)
    priority_results.push(result)
  }
  
  # 获取优先级处理统计
  let priority_stats = azimuth::PriorityManager::get_priority_statistics(priority_manager)
  
  # 验证优先级处理效果
  assert_true(priority_stats.critical.average_wait_time_ms < priority_stats.normal.average_wait_time_ms)
  assert_true(priority_stats.high.average_wait_time_ms < priority_stats.normal.average_wait_time_ms)
  assert_true(priority_stats.normal.average_wait_time_ms < priority_stats.low.average_wait_time_ms)
  
  # 测试异步操作的生命周期管理
  let lifecycle_manager = azimuth::AsyncManager::create_lifecycle_manager(async_manager)
  
  # 创建长期运行的任务
  let long_running_tasks = []
  for i in 0..10 {
    let task = azimuth::LifecycleManager::create_long_running_task(lifecycle_manager, {
      task_id: "long-task-" + i.to_string(),
      operation: fn() { simulate_long_running_operation() },
      timeout_seconds: 30,
      health_check_interval_seconds: 5
    })
    long_running_tasks.push(task)
  }
  
  # 检查任务健康状态
  let health_status = azimuth::LifecycleManager::check_all_tasks_health(lifecycle_manager)
  assert_true(health_status.healthy_tasks >= 8)  # 至少80%的任务健康
  
  # 测试任务取消
  let cancel_task = long_running_tasks[0]
  azimuth::LifecycleManager::cancel_task(lifecycle_manager, cancel_task.task_id)
  
  let cancel_status = azimuth::LifecycleManager::get_task_status(lifecycle_manager, cancel_task.task_id)
  assert_eq(cancel_status.state, "cancelled")
  
  # 测试优雅关闭
  let shutdown_result = azimuth::LifecycleManager::graceful_shutdown(lifecycle_manager, {
    timeout_seconds: 10,
    wait_for_completion: true
  })
  
  assert_true(shutdown_result.success)
  assert_true(shutdown_result.completed_tasks >= 8)  # 大部分任务应该完成
}

pub test "追踪资源优化测试" {
  let performance_manager = azimuth::PerformanceManager::new()
  
  // 创建资源优化管理器
  let resource_manager = azimuth::PerformanceManager::create_resource_manager(performance_manager)
  
  // 测试对象池优化
  let object_pool_manager = azimuth::ResourceManager::create_object_pool_manager(resource_manager)
  
  # 配置对象池
  azimuth::ObjectPoolManager::configure_pool(object_pool_manager, {
    object_type: "span",
    initial_size: 100,
    max_size: 1000,
    growth_factor: 1.5,
    shrink_threshold: 0.25,
    shrink_interval_ms: 30000
  })
  
  # 测试对象池性能
  let pool_test_iterations = 10000
  
  # 不使用对象池的基准测试
  let no_pool_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..pool_test_iterations {
    let span = create_test_span(i)
    # 模拟使用span
    use_span(span)
  }
  
  let no_pool_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let no_pool_time = (no_pool_end - no_pool_start) / 1000000L
  
  # 使用对象池的测试
  let with_pool_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..pool_test_iterations {
    let span = azimuth::ObjectPoolManager::acquire_span(object_pool_manager)
    configure_span(span, i)
    use_span(span)
    azimuth::ObjectPoolManager::release_span(object_pool_manager, span)
  }
  
  let with_pool_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let with_pool_time = (with_pool_end - with_pool_start) / 1000000L
  
  # 验证对象池性能提升
  assert_true(with_pool_time < no_pool_time)
  
  let performance_improvement = (no_pool_time.to_double() - with_pool_time.to_double()) / no_pool_time.to_double()
  assert_true(performance_improvement > 0.1)  # 至少10%的性能提升
  
  # 测试对象池统计
  let pool_stats = azimuth::ObjectPoolManager::get_statistics(object_pool_manager)
  
  assert_true(pool_stats.acquired_count >= pool_test_iterations)
  assert_true(pool_stats.released_count >= pool_test_iterations)
  assert_true(pool_stats.hit_rate > 0.9)  # 至少90%的命中率
  assert_true(pool_stats.current_size <= pool_stats.max_size)
  
  # 测试内存池优化
  let memory_pool_manager = azimuth::ResourceManager::create_memory_pool_manager(resource_manager)
  
  # 配置内存池
  azimuth::MemoryPoolManager::configure_pools(memory_pool_manager, [
    {
      name: "small_buffer",
      block_size: 1024,      # 1KB
      initial_blocks: 50,
      max_blocks: 500
    },
    {
      name: "medium_buffer",
      block_size: 8192,      # 8KB
      initial_blocks: 20,
      max_blocks: 200
    },
    {
      name: "large_buffer",
      block_size: 65536,     # 64KB
      initial_blocks: 5,
      max_blocks: 50
    }
  ])
  
  # 测试内存池分配
  let memory_allocations = []
  let allocation_sizes = [512, 1024, 4096, 8192, 16384, 32768, 65536]
  
  for size in allocation_sizes {
    let allocations = []
    
    # 分配多个块
    for i in 0..100 {
      let buffer = azimuth::MemoryPoolManager::allocate(memory_pool_manager, size)
      allocations.push(buffer)
    }
    
    # 使用缓冲区
    for buffer in allocations {
      use_buffer(buffer, size)
    }
    
    # 释放缓冲区
    for buffer in allocations {
      azimuth::MemoryPoolManager::deallocate(memory_pool_manager, buffer)
    }
    
    memory_allocations.push({
      block_size: size,
      allocation_count: allocations.length()
    })
  }
  
  # 获取内存池统计
  let memory_stats = azimuth::MemoryPoolManager::get_statistics(memory_pool_manager)
  
  # 验证内存池效率
  assert_true(memory_stats.total_allocations > 0)
  assert_true(memory_stats.total_deallocations > 0)
  assert_true(memory_stats.fragmentation_ratio < 0.2)  # 碎片率小于20%
  
  # 测试CPU资源优化
  let cpu_optimizer = azimuth::ResourceManager::create_cpu_optimizer(resource_manager)
  
  # 配置CPU优化
  azimuth::CPUOptimizer::configure(cpu_optimizer, {
    max_cpu_usage_percentage: 80.0,
    thread_pool_sizing: "adaptive",
    work_stealing_enabled: true,
    cpu_affinity_enabled: true
  })
  
  # 测试CPU密集型操作
  let cpu_test_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let cpu_tasks = []
  for i in 0..1000 {
    let task = azimuth::CPUOptimizer::submit_cpu_intensive_task(cpu_optimizer, {
      task_id: "cpu-task-" + i.to_string(),
      operation: fn() { cpu_intensive_operation(i) },
      priority: if i % 10 == 0 { "high" } else { "normal" }
    })
    cpu_tasks.push(task)
  }
  
  # 等待所有CPU任务完成
  for task in cpu_tasks {
    azimuth::CPUOptimizer::wait_for_completion(cpu_optimizer, task)
  }
  
  let cpu_test_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let cpu_test_time = (cpu_test_end - cpu_test_start) / 1000000L
  
  # 获取CPU使用统计
  let cpu_stats = azimuth::CPUOptimizer::get_cpu_statistics(cpu_optimizer)
  
  # 验证CPU优化效果
  assert_true(cpu_stats.average_cpu_usage <= 85.0)  # CPU使用率不应超过85%
  assert_true(cpu_stats.thread_utilization > 0.7)   # 线程利用率应该较高
  assert_true(cpu_stats.context_switches_per_sec < 10000)  # 上下文切换不应过多
  
  # 测试网络I/O优化
  let network_optimizer = azimuth::ResourceManager::create_network_optimizer(resource_manager)
  
  # 配置网络优化
  azimuth::NetworkOptimizer::configure(network_optimizer, {
    connection_pool_size: 50,
    keep_alive_enabled: true,
    tcp_nodelay_enabled: true,
    compression_enabled: true,
    batch_size: 10
  })
  
  # 测试网络操作
  let network_test_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let network_tasks = []
  for i in 0..100 {
    let task = azimuth::NetworkOptimizer::submit_network_task(network_optimizer, {
      task_id: "network-task-" + i.to_string(),
      operation: fn() { simulate_network_operation(i) },
      endpoint: "http://telemetry-collector:4317",
      timeout_ms: 5000
    })
    network_tasks.push(task)
  }
  
  # 等待所有网络任务完成
  let network_results = []
  for task in network_tasks {
    let result = azimuth::NetworkOptimizer::wait_for_completion(network_optimizer, task)
    network_results.push(result)
  }
  
  let network_test_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let network_test_time = (network_test_end - network_test_start) / 1000000L
  
  # 获取网络统计
  let network_stats = azimuth::NetworkOptimizer::get_network_statistics(network_optimizer)
  
  # 验证网络优化效果
  assert_true(network_stats.connection_reuse_rate > 0.8)  # 连接复用率应该较高
  assert_true(network_stats.compression_ratio > 1.5)     # 压缩比应该有效
  assert_true(network_stats.average_latency_ms < 100)    # 平均延迟应该较低
  
  # 测试磁盘I/O优化
  let disk_optimizer = azimuth::ResourceManager::create_disk_optimizer(resource_manager)
  
  # 配置磁盘优化
  azimuth::DiskOptimizer::configure(disk_optimizer, {
    buffer_size: 8192,
    write_ahead_enabled: true,
    read_ahead_enabled: true,
    file_cache_size_mb: 100,
    sync_mode: "periodic"
  })
  
  # 测试磁盘操作
  let disk_test_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let disk_tasks = []
  for i in 0..50 {
    let task = azimuth::DiskOptimizer::submit_disk_task(disk_optimizer, {
      task_id: "disk-task-" + i.to_string(),
      operation: "write",
      data: generate_test_data_for_disk(i * 1024),  # KB级别的数据
      file_path: "/tmp/telemetry-test-" + i.to_string() + ".dat"
    })
    disk_tasks.push(task)
  }
  
  # 等待所有磁盘任务完成
  let disk_results = []
  for task in disk_tasks {
    let result = azimuth::DiskOptimizer::wait_for_completion(disk_optimizer, task)
    disk_results.push(result)
  }
  
  let disk_test_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let disk_test_time = (disk_test_end - disk_test_start) / 1000000L
  
  # 获取磁盘统计
  let disk_stats = azimuth::DiskOptimizer::get_disk_statistics(disk_optimizer)
  
  # 验证磁盘优化效果
  assert_true(disk_stats.write_throughput_mbps > 10)    # 写入吞吐量应该合理
  assert_true(disk_stats.cache_hit_rate > 0.5)         # 缓存命中率应该较高
  assert_true(disk_stats.average_io_latency_ms < 20)   # I/O延迟应该较低
  
  // 测试综合资源优化
  let comprehensive_optimizer = azimuth::ResourceManager::create_comprehensive_optimizer(resource_manager)
  
  # 配置综合优化
  azimuth::ComprehensiveOptimizer::configure(comprehensive_optimizer, {
    memory_limit_mb: 512,
    cpu_limit_percentage: 75.0,
    network_bandwidth_limit_mbps: 100,
    disk_io_limit_iops: 1000,
    optimization_interval_seconds: 10
  })
  
  # 运行综合优化测试
  let optimization_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  # 提交混合负载
  let mixed_tasks = []
  for i in 0..200 {
    let task_type = i % 4
    let task = match task_type {
      0 => azimuth::ComprehensiveOptimizer::submit_cpu_task(comprehensive_optimizer, fn() { cpu_intensive_operation(i) }),
      1 => azimuth::ComprehensiveOptimizer::submit_memory_task(comprehensive_optimizer, fn() { memory_intensive_operation(i) }),
      2 => azimuth::ComprehensiveOptimizer::submit_network_task(comprehensive_optimizer, fn() { simulate_network_operation(i) }),
      _ => azimuth::ComprehensiveOptimizer::submit_disk_task(comprehensive_optimizer, fn() { disk_intensive_operation(i) })
    }
    mixed_tasks.push(task)
  }
  
  # 等待所有任务完成
  for task in mixed_tasks {
    azimuth::ComprehensiveOptimizer::wait_for_completion(comprehensive_optimizer, task)
  }
  
  let optimization_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let optimization_time = (optimization_end - optimization_start) / 1000000L
  
  # 获取综合优化统计
  let comprehensive_stats = azimuth::ComprehensiveOptimizer::get_comprehensive_statistics(comprehensive_optimizer)
  
  # 验证综合优化效果
  assert_true(comprehensive_stats.memory_usage_mb <= 512)      # 内存使用不超过限制
  assert_true(comprehensive_stats.cpu_usage_percentage <= 80) # CPU使用不超过限制
  assert_true(comprehensive_stats.resource_utilization_score > 0.7) # 资源利用率应该较高
  
  # 验证资源平衡
  let resource_balance = comprehensive_stats.resource_balance_score
  assert_true(resource_balance > 0.6)  # 资源使用应该相对平衡
}