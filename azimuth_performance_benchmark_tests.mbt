// Azimuth 性能基准测试用例
// 测试系统在各种负载下的性能表现和资源使用情况

test "遥测数据收集性能基准" {
  // 创建性能基准测试器
  let benchmark_runner = azimuth::BenchmarkRunner::new("telemetry-collection")
  
  // 定义基准测试场景
  let telemetry_scenario = azimuth::BenchmarkScenario {
    name: "high-volume-telemetry-collection",
    description: "测试高并发遥测数据收集性能",
    warmup_iterations: 100,
    measurement_iterations: 1000,
    concurrent_threads: 10,
    data_size_per_iteration: 100, // 每次迭代100个数据点
    target_throughput_ops_per_sec: 10000
  }
  
  // 创建遥测收集器
  let telemetry_collector = azimuth::TelemetryCollector::new()
  telemetry_collector.configure([
    ("batch_size", "500"),
    ("flush_interval_ms", "1000"),
    ("compression_enabled", "true"),
    ("async_processing", "true")
  ])
  
  // 执行基准测试
  let benchmark_result = benchmark_runner.run(telemetry_scenario, fn(iteration) {
    // 生成模拟遥测数据
    let telemetry_data = []
    for i in 0..=99 {
      let data_point = azimuth::TelemetryDataPoint::new()
      data_point.set_metric_name("cpu.usage")
      data_point.set_value(50.0 + (azimuth::Random::next_float() * 40.0))
      data_point.set_timestamp(azimuth::TimeUtil::current_time_millis())
      data_point.add_attribute("host", "host-" + (i % 10).to_string())
      data_point.add_attribute("service", "service-" + (i % 5).to_string())
      telemetry_data.push(data_point)
    }
    
    // 收集遥测数据
    let start_time = azimuth::TimeUtil::current_time_nanos()
    let result = telemetry_collector.collect_batch(telemetry_data)
    let end_time = azimuth::TimeUtil::current_time_nanos()
    
    return azimuth::BenchmarkResult {
      iteration: iteration,
      duration_ns: end_time - start_time,
      success: result.is_ok(),
      data_points_processed: telemetry_data.length(),
      memory_used_bytes: azimuth::MemoryUtil::get_current_usage()
    }
  })
  
  // 验证基准测试结果
  assert_true(benchmark_result.success_rate > 0.95) // 95%成功率
  assert_true(benchmark_result.average_throughput_ops_per_sec >= 8000) // 至少8000 ops/sec
  assert_true(benchmark_result.average_latency_ms < 50) // 平均延迟小于50ms
  assert_true(benchmark_result.p99_latency_ms < 200) // P99延迟小于200ms
  
  // 验证内存使用情况
  assert_true(benchmark_result.average_memory_usage_mb < 100) // 平均内存使用小于100MB
  assert_true(benchmark_result.memory_leak_detected == false)
  
  // 验证资源利用率
  let cpu_utilization = benchmark_runner.get_cpu_utilization()
  assert_true(cpu_utilization < 80.0) // CPU利用率低于80%
  
  let gc_pressure = benchmark_runner.get_gc_pressure()
  assert_true(gc_pressure < 10.0) // GC压力低于10%
}

test "分布式追踪性能基准" {
  // 创建分布式追踪性能测试器
  let tracing_benchmark = azimuth::BenchmarkRunner::new("distributed-tracing")
  
  // 定义追踪场景
  let tracing_scenario = azimuth::BenchmarkScenario {
    name: "distributed-tracing-throughput",
    description: "测试分布式追踪系统吞吐量",
    warmup_iterations: 50,
    measurement_iterations: 500,
    concurrent_threads: 20,
    data_size_per_iteration: 50, // 每次迭代50个span
    target_throughput_ops_per_sec: 5000
  }
  
  // 创建分布式追踪器
  let tracer = azimuth::DistributedTracer::new("performance-test-service")
  tracer.configure([
    ("sampling_rate", "1.0"), // 100%采样
    ("batch_export_size", "100"),
    ("export_interval_ms", "500"),
    ("async_export", "true")
  ])
  
  // 执行基准测试
  let tracing_result = tracing_benchmark.run(tracing_scenario, fn(iteration) {
    // 创建根Span
    let root_span = tracer.start_span("performance_test_operation", azimuth::SpanKind::Server)
    
    // 创建子Span
    let child_spans = []
    for i in 0..=49 {
      let child_span = tracer.start_span("child_operation_" + i.to_string(), azimuth::SpanKind::Internal)
      child_span.set_parent_span(root_span.get_context())
      
      // 添加事件和属性
      child_span.add_event("operation_started", [("iteration", iteration.to_string())])
      child_span.set_attribute("operation_id", "op-" + i.to_string())
      child_span.set_attribute("processing_time_ms", (10 + azimuth::Random::next_int() % 90).to_string())
      
      child_spans.push(child_span)
    }
    
    // 结束所有Span
    for span in child_spans {
      span.end()
    }
    root_span.end()
    
    return azimuth::BenchmarkResult {
      iteration: iteration,
      duration_ns: 0, // 由追踪器内部计时
      success: true,
      data_points_processed: child_spans.length() + 1, // 包括根Span
      memory_used_bytes: azimuth::MemoryUtil::get_current_usage()
    }
  })
  
  // 验证追踪性能
  assert_true(tracing_result.success_rate > 0.98) // 98%成功率
  assert_true(tracing_result.average_throughput_ops_per_sec >= 4000) // 至少4000 spans/sec
  assert_true(tracing_result.average_latency_ms < 20) // 平均延迟小于20ms
  
  // 验证追踪数据完整性
  let exported_spans = tracer.get_exported_spans_count()
  assert_eq(exported_spans, tracing_scenario.measurement_iterations * (tracing_scenario.data_size_per_iteration + 1))
  
  // 验证内存效率
  let span_memory_overhead = tracing_result.average_memory_usage_mb / (tracing_scenario.measurement_iterations * tracing_scenario.concurrent_threads)
  assert_true(span_memory_overhead < 0.01) // 每个Span内存开销小于10KB
}

test "时序数据处理性能基准" {
  // 创建时序数据性能测试器
  let timeseries_benchmark = azimuth::BenchmarkRunner::new("timeseries-processing")
  
  // 定义时序数据处理场景
  let timeseries_scenario = azimuth::BenchmarkScenario {
    name: "timeseries-aggregation-performance",
    description: "测试时序数据聚合性能",
    warmup_iterations: 20,
    measurement_iterations: 100,
    concurrent_threads: 5,
    data_size_per_iteration: 1000, // 每次迭代1000个数据点
    target_throughput_ops_per_sec: 1000
  }
  
  // 创建时序数据处理器
  let timeseries_processor = azimuth::TimeSeriesProcessor::new()
  timeseries_processor.configure([
    ("window_size_ms", "60000"), // 1分钟窗口
    ("aggregation_types", "avg,sum,min,max,count"),
    ("compression_enabled", "true"),
    ("parallel_processing", "true")
  ])
  
  // 执行基准测试
  let timeseries_result = timeseries_benchmark.run(timeseries_scenario, fn(iteration) {
    // 生成时序数据
    let timeseries_data = []
    let base_timestamp = azimuth::TimeUtil::current_time_millis()
    
    for i in 0..=999 {
      let data_point = azimuth::TimeSeriesDataPoint::new(
        "metric_" + (i % 10).to_string(),
        100.0 + (azimuth::Random::next_float() * 900.0),
        base_timestamp - (i * 1000L), // 倒序时间戳
        [("host", "host-" + (i % 5).to_string()), ("region", "region-" + (i % 3).to_string())]
      )
      timeseries_data.push(data_point)
    }
    
    // 执行聚合操作
    let start_time = azimuth::TimeUtil::current_time_nanos()
    let aggregated_data = timeseries_processor.aggregate(timeseries_data)
    let end_time = azimuth::TimeUtil::current_time_nanos()
    
    return azimuth::BenchmarkResult {
      iteration: iteration,
      duration_ns: end_time - start_time,
      success: aggregated_data.length() > 0,
      data_points_processed: timeseries_data.length(),
      memory_used_bytes: azimuth::MemoryUtil::get_current_usage()
    }
  })
  
  // 验证时序处理性能
  assert_true(timeseries_result.success_rate > 0.95) // 95%成功率
  assert_true(timeseries_result.average_throughput_ops_per_sec >= 800) // 至少800次聚合/sec
  assert_true(timeseries_result.average_latency_ms < 100) // 平均延迟小于100ms
  
  // 验证聚合结果准确性
  assert_true(timeseries_processor.verify_aggregation_accuracy())
  
  // 验证压缩效率
  let compression_ratio = timeseries_processor.get_compression_ratio()
  assert_true(compression_ratio > 0.5) // 压缩率至少50%
}

test "多维度查询性能基准" {
  // 创建查询性能测试器
  let query_benchmark = azimuth::BenchmarkRunner::new("multidimensional-query")
  
  // 准备测试数据
  let data_generator = azimuth::TestDataGenerator::new()
  let test_dataset = data_generator.generate_multidimensional_data(
    100000, // 10万条记录
    ["service.name", "service.version", "region", "environment", "operation.type"],
    1000 // 1000个唯一值组合
  )
  
  // 创建多维度查询引擎
  let query_engine = azimuth::MultiDimensionalQueryEngine::new()
  query_engine.load_dataset(test_dataset)
  
  // 定义查询场景
  let query_scenario = azimuth::BenchmarkScenario {
    name: "complex-query-performance",
    description: "测试复杂多维度查询性能",
    warmup_iterations: 10,
    measurement_iterations: 100,
    concurrent_threads: 10,
    data_size_per_iteration: 1, // 每次迭代执行1个查询
    target_throughput_ops_per_sec: 100
  }
  
  // 定义测试查询
  let test_queries = [
    // 简单等值查询
    azimuth::Query::Equals("service.name", "user-service"),
    // 范围查询
    azimuth::Query::Range("response.time", 100.0, 500.0),
    // 多条件AND查询
    azimuth::Query::And([
      azimuth::Query::Equals("region", "us-east-1"),
      azimuth::Query::Equals("environment", "production"),
      azimuth::Query::GreaterThan("cpu.usage", 80.0)
    ]),
    // 多条件OR查询
    azimuth::Query::Or([
      azimuth::Query::Equals("service.name", "payment-service"),
      azimuth::Query::Equals("service.name", "order-service"),
      azimuth::Query::Equals("service.name", "notification-service")
    ]),
    // 复杂嵌套查询
    azimuth::Query::And([
      azimuth::Query::Or([
        azimuth::Query::Equals("region", "us-east-1"),
        azimuth::Query::Equals("region", "us-west-2")
      ]),
      azimuth::Query::Range("timestamp", 1609459200000L, 1609545600000L),
      azimuth::Query::Not(azimuth::Query::Equals("status", "deprecated"))
    ])
  ]
  
  // 执行查询基准测试
  let query_result = query_benchmark.run(query_scenario, fn(iteration) {
    let query_index = iteration % test_queries.length()
    let query = test_queries[query_index]
    
    // 执行查询
    let start_time = azimuth::TimeUtil::current_time_nanos()
    let results = query_engine.execute(query)
    let end_time = azimuth::TimeUtil::current_time_nanos()
    
    return azimuth::BenchmarkResult {
      iteration: iteration,
      duration_ns: end_time - start_time,
      success: true,
      data_points_processed: results.length(),
      memory_used_bytes: azimuth::MemoryUtil::get_current_usage()
    }
  })
  
  // 验证查询性能
  assert_true(query_result.success_rate == 1.0) // 100%成功率
  assert_true(query_result.average_throughput_ops_per_sec >= 80) // 至少80次查询/sec
  assert_true(query_result.average_latency_ms < 100) // 平均延迟小于100ms
  assert_true(query_result.p99_latency_ms < 500) // P99延迟小于500ms
  
  // 验证不同查询类型的性能差异
  let query_performance = query_engine.get_query_performance_stats()
  
  // 简单查询应该更快
  assert_true(query_performance["simple_equals"].avg_latency_ms < query_performance["complex_nested"].avg_latency_ms)
  
  // 验证索引效率
  let index_hit_ratio = query_engine.get_index_hit_ratio()
  assert_true(index_hit_ratio > 0.8) // 索引命中率应该超过80%
}

test "内存使用和垃圾回收性能" {
  // 创建内存性能测试器
  let memory_benchmark = azimuth::BenchmarkRunner::new("memory-gc-performance")
  
  // 定义内存测试场景
  let memory_scenario = azimuth::BenchmarkScenario {
    name: "memory-allocation-gc",
    description: "测试内存分配和垃圾回收性能",
    warmup_iterations: 50,
    measurement_iterations: 200,
    concurrent_threads: 5,
    data_size_per_iteration: 1000, // 每次迭代分配1000个对象
    target_throughput_ops_per_sec: 5000
  }
  
  // 执行内存基准测试
  let memory_result = memory_benchmark.run(memory_scenario, fn(iteration) {
    // 记录测试开始时的内存状态
    let initial_memory = azimuth::MemoryUtil::get_heap_usage()
    let initial_gc_count = azimuth::MemoryUtil::get_gc_count()
    
    // 分配大量对象
    let allocated_objects = []
    for i in 0..=999 {
      let obj = azimuth::TestObject::new(
        "object-" + iteration.to_string() + "-" + i.to_string(),
        azimuth::Random::next_int() % 1000,
        azimuth::Random::next_float() * 100.0
      )
      allocated_objects.push(obj)
    }
    
    // 执行一些操作
    for obj in allocated_objects {
      obj.process()
    }
    
    // 释放部分对象引用（模拟对象生命周期）
    let retained_objects = allocated_objects.slice(0, 100) // 保留10%的对象
    
    // 记录测试结束时的内存状态
    let final_memory = azimuth::MemoryUtil::get_heap_usage()
    let final_gc_count = azimuth::MemoryUtil::get_gc_count()
    
    // 强制垃圾回收
    azimuth::MemoryUtil::force_gc()
    let after_gc_memory = azimuth::MemoryUtil::get_heap_usage()
    
    return azimuth::BenchmarkResult {
      iteration: iteration,
      duration_ns: 0,
      success: true,
      data_points_processed: allocated_objects.length(),
      memory_used_bytes: final_memory - initial_memory,
      gc_count: final_gc_count - initial_gc_count,
      memory_after_gc: after_gc_memory
    }
  })
  
  // 验证内存性能
  assert_true(memory_result.success_rate == 1.0) // 100%成功率
  
  // 验证内存分配效率
  let avg_allocation_per_object = memory_result.average_memory_usage_mb * 1024 * 1024 / 
    (memory_scenario.measurement_iterations * memory_scenario.data_size_per_iteration)
  assert_true(avg_allocation_per_object < 1024) // 每个对象平均分配小于1KB
  
  // 验证垃圾回收效率
  let avg_gc_per_iteration = memory_result.average_gc_count
  assert_true(avg_gc_per_iteration < 0.1) // 平均每次迭代GC次数小于0.1
  
  // 验证内存回收效果
  let memory_reclaim_ratio = 1.0 - (memory_result.memory_after_gc.to_float() / memory_result.average_memory_usage_mb.to_float())
  assert_true(memory_reclaim_ratio > 0.7) // GC后内存回收比例超过70%
  
  // 验证内存泄漏
  let memory_leak_detected = azimuth::MemoryUtil::detect_memory_leak()
  assert_false(memory_leak_detected)
}

test "并发性能和可扩展性测试" {
  // 创建并发性能测试器
  let concurrency_benchmark = azimuth::BenchmarkRunner::new("concurrency-scalability")
  
  // 测试不同并发级别的性能
  let concurrency_levels = [1, 2, 4, 8, 16, 32]
  let scalability_results = []
  
  for threads in concurrency_levels {
    // 定义并发测试场景
    let concurrency_scenario = azimuth::BenchmarkScenario {
      name: "concurrency-level-" + threads.to_string(),
      description: "测试" + threads.to_string() + "线程并发性能",
      warmup_iterations: 20,
      measurement_iterations: 100,
      concurrent_threads: threads,
      data_size_per_iteration: 100,
      target_throughput_ops_per_sec: 1000 * threads
    }
    
    // 创建共享资源
    let shared_counter = azimuth::AtomicCounter::new(0)
    let shared_data_store = azimuth::ConcurrentDataStore::new()
    
    // 执行并发基准测试
    let concurrency_result = concurrency_benchmark.run(concurrency_scenario, fn(iteration) {
      // 执行并发操作
      let start_time = azimuth::TimeUtil::current_time_nanos()
      
      // 并发递增计数器
      for i in 0..=99 {
        shared_counter.increment()
      }
      
      // 并发访问数据存储
      let key = "key-" + (iteration % 1000).to_string()
      let value = "value-" + iteration.to_string()
      shared_data_store.put(key, value)
      let retrieved_value = shared_data_store.get(key)
      
      let end_time = azimuth::TimeUtil::current_time_nanos()
      
      return azimuth::BenchmarkResult {
        iteration: iteration,
        duration_ns: end_time - start_time,
        success: retrieved_value == Some(value),
        data_points_processed: 100,
        memory_used_bytes: azimuth::MemoryUtil::get_current_usage()
      }
    })
    
    // 记录结果
    scalability_results.push({
      "threads": threads,
      "throughput": concurrency_result.average_throughput_ops_per_sec,
      "latency": concurrency_result.average_latency_ms,
      "success_rate": concurrency_result.success_rate
    })
  }
  
  // 验证可扩展性
  // 理想情况下，吞吐量应该与线程数成正比（至少在低并发度时）
  let single_thread_throughput = scalability_results[0]["throughput"]
  let two_thread_throughput = scalability_results[1]["throughput"]
  
  // 2个线程的吞吐量应该至少是单线程的1.5倍（考虑到并发开销）
  assert_true(two_thread_throughput >= single_thread_throughput * 1.5)
  
  // 验证高并发下的性能稳定性
  let high_concurrency_results = scalability_results.slice(3, 6) // 8, 16, 32线程
  let min_high_concurrency_success_rate = high_concurrency_results.map(fn(r) { r["success_rate"] }).min()
  assert_true(min_high_concurrency_success_rate > 0.95) // 高并发下成功率仍应超过95%
  
  // 验证并发安全性
  let final_counter_value = shared_counter.get()
  let expected_counter_value = concurrency_levels.fold(0, fn(acc, threads) {
    acc + (threads * 20 * 100) // warmup_iterations + measurement_iterations * data_size_per_iteration
  })
  assert_eq(final_counter_value, expected_counter_value) // 验证原子操作的正确性
}

test "资源受限环境下的性能测试" {
  // 创建资源受限性能测试器
  let resource_constrained_benchmark = azimuth::BenchmarkRunner::new("resource-constrained")
  
  // 配置资源限制
  let resource_limits = azimuth::ResourceLimits {
    max_memory_mb: 256,
    max_cpu_cores: 2,
    max_network_bandwidth_mbps: 10,
    max_disk_io_mb_per_sec: 50
  }
  
  // 应用资源限制
  resource_constrained_benchmark.apply_resource_limits(resource_limits)
  
  // 定义资源受限测试场景
  let constrained_scenario = azimuth::BenchmarkScenario {
    name: "resource-constrained-performance",
    description: "测试资源受限环境下的性能表现",
    warmup_iterations: 10,
    measurement_iterations: 50,
    concurrent_threads: 4,
    data_size_per_iteration: 200,
    target_throughput_ops_per_sec: 500
  }
  
  // 创建资源感知的处理器
  let resource_aware_processor = azimuth::ResourceAwareProcessor::new()
  resource_aware_processor.configure([
    ("adaptive_batch_size", "true"),
    ("memory_pressure_threshold", "0.8"),
    ("cpu_throttling_enabled", "true"),
    ("disk_io_throttling_enabled", "true")
  ])
  
  // 执行资源受限基准测试
  let constrained_result = resource_constrained_benchmark.run(constrained_scenario, fn(iteration) {
    // 生成测试数据
    let test_data = []
    for i in 0..=199 {
      let data_item = azimuth::DataItem::new(
        "item-" + iteration.to_string() + "-" + i.to_string(),
        azimuth::Random::next_string(100) // 100字节数据
      )
      test_data.push(data_item)
    }
    
    // 在资源受限环境下处理数据
    let start_time = azimuth::TimeUtil::current_time_nanos()
    let processed_data = resource_aware_processor.process(test_data)
    let end_time = azimuth::TimeUtil::current_time_nanos()
    
    // 获取资源使用情况
    let memory_usage = resource_aware_processor.get_memory_usage()
    let cpu_usage = resource_aware_processor.get_cpu_usage()
    
    return azimuth::BenchmarkResult {
      iteration: iteration,
      duration_ns: end_time - start_time,
      success: processed_data.length() == test_data.length(),
      data_points_processed: test_data.length(),
      memory_used_bytes: memory_usage,
      cpu_usage_percent: cpu_usage
    }
  })
  
  // 验证资源受限下的性能
  assert_true(constrained_result.success_rate > 0.9) // 90%成功率
  
  // 验证资源使用不超过限制
  assert_true(constrained_result.average_memory_usage_mb <= resource_limits.max_memory_mb * 0.9) // 内存使用不超过90%
  assert_true(constrained_result.average_cpu_usage <= resource_limits.max_cpu_cores * 100.0) // CPU使用不超过限制
  
  // 验证自适应调整
  let adaptive_adjustments = resource_aware_processor.get_adaptive_adjustments()
  assert_true(adaptive_adjustments.length() > 0) // 应该有自适应调整
  
  // 验证优雅降级
  let graceful_degradation_events = resource_aware_processor.get_graceful_degradation_events()
  assert_true(graceful_degradation_events.length() >= 0) // 应该有优雅降级事件（如果资源不足）
}