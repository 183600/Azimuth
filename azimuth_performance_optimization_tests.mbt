// Azimuth Performance Optimization Tests
// 性能优化测试用例

test "telemetry data compression efficiency" {
  // 测试遥测数据压缩效率
  let compression_strategies = [
    @azimuth.CompressionStrategy {
      name : "gzip",
      algorithm : "gzip",
      level : 6,
      window_size : 32768
    },
    @azimuth.CompressionStrategy {
      name : "lz4",
      algorithm : "lz4",
      level : 1,
      window_size : 65536
    },
    @azimuth.CompressionStrategy {
      name : "zstd",
      algorithm : "zstd",
      level : 3,
      window_size : 131072
    }
  ]
  
  // 生成大量测试数据
  let test_data = @azimuth.generate_test_telemetry_data(
    1000, // 数据点数量
    50,   // 每个数据点的属性数量
    10    // 每个数据点的事件数量
  )
  
  // 测试不同压缩策略
  let compression_results = []
  for strategy in compression_strategies {
    let result = @azimuth.compress_telemetry_data(test_data, strategy)
    compression_results = compression_results.push(result)
  }
  
  // 验证压缩结果
  assert_eq(compression_results.length(), 3)
  
  let original_size = @azimuth.calculate_data_size(test_data)
  
  // 验证所有压缩策略都有效
  for result in compression_results {
    assert_true(result.compressed_size > 0)
    assert_true(result.compressed_size < original_size)
    assert_true(result.compression_ratio > 1.0)
    assert_true(result.compression_time_ms > 0)
    assert_true(result.decompression_time_ms > 0)
  }
  
  // 找出最佳压缩策略
  let best_compression = compression_results
    .sort_by(fn(a, b) { a.compression_ratio - b.compression_ratio })
    .reverse()[0]
  
  // 验证最佳压缩策略
  assert_true(best_compression.compression_ratio >= 2.0) // 至少2倍压缩率
  
  // 验证压缩后数据的完整性
  for result in compression_results {
    let decompressed_data = @azimuth.decompress_telemetry_data(
      result.compressed_data,
      result.strategy
    )
    
    // 验证解压后数据与原始数据一致
    assert_eq(decompressed_data.data_points.length(), test_data.data_points.length())
    
    for i in 0..test_data.data_points.length() {
      let original = test_data.data_points[i]
      let decompressed = decompressed_data.data_points[i]
      
      assert_eq(original.timestamp, decompressed.timestamp)
      assert_eq(original.trace_id, decompressed.trace_id)
      assert_eq(original.span_id, decompressed.span_id)
      assert_eq(original.operation_name, decompressed.operation_name)
      assert_eq(original.duration_ms, decompressed.duration_ms)
    }
  }
  
  // 测试增量压缩
  let incremental_data = @azimuth.generate_incremental_telemetry_data(
    test_data,
    100, // 增量数据点数量
    10   // 时间间隔(ms)
  )
  
  let incremental_compression = @azimuth.compress_incremental_data(
    test_data,
    incremental_data,
    compression_strategies[0] // 使用gzip
  )
  
  // 验证增量压缩效率
  assert_true(incremental_compression.compressed_size > 0)
  assert_true(incremental_compression.compression_ratio > 1.0)
  
  // 增量压缩应该比全量压缩更高效
  let full_compression = compression_results[0]
  assert_true(incremental_compression.compressed_size < full_compression.compressed_size)
}

test "batch processing optimization" {
  // 测试批处理优化
  let batch_configs = [
    @azimuth.BatchConfig {
      batch_size : 10,
      max_wait_time_ms : 100,
      compression_enabled : true,
      parallel_processing : false
    },
    @azimuth.BatchConfig {
      batch_size : 50,
      max_wait_time_ms : 500,
      compression_enabled : true,
      parallel_processing : true
    },
    @azimuth.BatchConfig {
      batch_size : 100,
      max_wait_time_ms : 1000,
      compression_enabled : false,
      parallel_processing : true
    },
    @azimuth.BatchConfig {
      batch_size : 200,
      max_wait_time_ms : 2000,
      compression_enabled : true,
      parallel_processing : true
    }
  ]
  
  // 生成测试数据
  let test_data = @azimuth.generate_test_telemetry_data(
    1000, // 数据点数量
    20,   // 每个数据点的属性数量
    5     // 每个数据点的事件数量
  )
  
  // 测试不同批处理配置
  let batch_results = []
  for config in batch_configs {
    let result = @azimuth.process_batch_telemetry_data(test_data, config)
    batch_results = batch_results.push(result)
  }
  
  // 验证批处理结果
  assert_eq(batch_results.length(), 4)
  
  // 验证所有批处理配置都有效
  for result in batch_results {
    assert_true(result.total_batches > 0)
    assert_true(result.processed_items == test_data.data_points.length())
    assert_true(result.total_processing_time_ms > 0)
    assert_true(result.avg_batch_processing_time_ms > 0)
    assert_true(result.throughput_items_per_sec > 0)
  }
  
  // 找出最高吞吐量的配置
  let best_throughput = batch_results
    .sort_by(fn(a, b) { a.throughput_items_per_sec - b.throughput_items_per_sec })
    .reverse()[0]
  
  // 验证最佳吞吐量
  assert_true(best_throughput.throughput_items_per_sec >= 1000) // 至少1000项/秒
  
  // 找出最低延迟的配置
  let lowest_latency = batch_results
    .sort_by(fn(a, b) { a.avg_batch_processing_time_ms - b.avg_batch_processing_time_ms })[0]
  
  // 验证最低延迟
  assert_true(lowest_latency.avg_batch_processing_time_ms <= 100) // 平均批处理时间<=100ms
  
  // 测试自适应批处理
  let adaptive_config = @azimuth.AdaptiveBatchConfig {
    initial_batch_size : 50,
    min_batch_size : 10,
    max_batch_size : 500,
    target_latency_ms : 100,
    adjustment_factor : 0.1,
    evaluation_interval_ms : 5000
  }
  
  let adaptive_result = @azimuth.process_adaptive_batch_telemetry_data(
    test_data,
    adaptive_config,
    30000 // 测试持续时间30秒
  )
  
  // 验证自适应批处理结果
  assert_true(adaptive_result.total_batches > 0)
  assert_true(adaptive_result.processed_items == test_data.data_points.length())
  assert_true(adaptive_result.batch_size_adjustments > 0)
  assert_true(adaptive_result.final_batch_size >= adaptive_config.min_batch_size)
  assert_true(adaptive_result.final_batch_size <= adaptive_config.max_batch_size)
  
  // 验证自适应效果
  assert_true(abs(adaptive_result.final_avg_latency_ms - adaptive_config.target_latency_ms) <= 
              adaptive_config.target_latency_ms * 0.2) // 最终延迟与目标延迟差异<=20%
}

test "memory usage optimization" {
  // 测试内存使用优化
  let memory_optimization_strategies = [
    @azimuth.MemoryOptimizationStrategy {
      name : "object_pooling",
      pooling_enabled : true,
      pool_size : 1000,
      preallocation_enabled : true
    },
    @azimuth.MemoryOptimizationStrategy {
      name : "lazy_loading",
      pooling_enabled : false,
      pool_size : 0,
      preallocation_enabled : false
    },
    @azimuth.MemoryOptimizationStrategy {
      name : "flyweight_pattern",
      pooling_enabled : true,
      pool_size : 500,
      preallocation_enabled : true
    },
    @azimuth.MemoryOptimizationStrategy {
      name : "compression",
      pooling_enabled : false,
      pool_size : 0,
      preallocation_enabled : false
    }
  ]
  
  // 生成大量测试数据
  let large_test_data = @azimuth.generate_test_telemetry_data(
    10000, // 数据点数量
    100,   // 每个数据点的属性数量
    20     // 每个数据点的事件数量
  )
  
  // 测试不同内存优化策略
  let memory_results = []
  for strategy in memory_optimization_strategies {
    let result = @azimuth.process_telemetry_with_memory_optimization(
      large_test_data,
      strategy
    )
    memory_results = memory_results.push(result)
  }
  
  // 验证内存优化结果
  assert_eq(memory_results.length(), 4)
  
  // 验证所有策略都有效
  for result in memory_results {
    assert_true(result.peak_memory_usage_mb > 0)
    assert_true(result.avg_memory_usage_mb > 0)
    assert_true(result.processing_time_ms > 0)
    assert_true(result.memory_efficiency_score > 0.0)
    assert_true(result.memory_efficiency_score <= 1.0)
  }
  
  // 找出内存效率最高的策略
  let most_efficient = memory_results
    .sort_by(fn(a, b) { a.memory_efficiency_score - b.memory_efficiency_score })
    .reverse()[0]
  
  // 验证最高内存效率
  assert_true(most_efficient.memory_efficiency_score >= 0.7) // 至少70%效率
  
  // 找出内存使用最少的策略
  let lowest_memory = memory_results
    .sort_by(fn(a, b) { a.peak_memory_usage_mb - b.peak_memory_usage_mb })[0]
  
  // 验证最低内存使用
  assert_true(lowest_memory.peak_memory_usage_mb <= memory_results[0].peak_memory_usage_mb * 0.8)
  
  // 测试内存泄漏检测
  let memory_leak_test = @azimuth.test_memory_leaks(
    memory_optimization_strategies[0], // 使用对象池策略
    1000, // 迭代次数
    100   // 每次迭代的数据量
  )
  
  // 验证内存泄漏检测结果
  assert_eq(memory_leak_test.total_iterations, 1000)
  assert_true(memory_leak_test.initial_memory_mb > 0)
  assert_true(memory_leak_test.final_memory_mb > 0)
  
  // 验证没有显著的内存泄漏
  let memory_growth = memory_leak_test.final_memory_mb - memory_leak_test.initial_memory_mb
  let memory_growth_rate = memory_growth / memory_leak_test.initial_memory_mb
  assert_true(memory_growth_rate <= 0.1) // 内存增长率<=10%
  
  // 测试内存压力下的性能
  let memory_pressure_test = @azimuth.test_memory_pressure_performance(
    memory_optimization_strategies,
    5000, // 数据点数量
    80    // 目标内存使用率(%)
  )
  
  // 验证内存压力测试结果
  assert_eq(memory_pressure_test.total_strategies_tested, 4)
  assert_true(memory_pressure_test.pressure_detected)
  assert_true(memory_pressure_test.garbage_collection_triggered)
  assert_true(memory_pressure_test.memory_reclaimed > 0)
  
  // 验证压力下的稳定性
  assert_true(memory_pressure_test.system_stability_maintained)
  assert_true(memory_pressure_test.data_integrity_preserved)
}

test "cpu utilization optimization" {
  // 测试CPU利用率优化
  let cpu_optimization_strategies = [
    @azimuth.CPUOptimizationStrategy {
      name : "thread_pool_optimization",
      max_threads : 4,
      work_stealing_enabled : true,
      affinity_enabled : false
    },
    @azimuth.CPUOptimizationStrategy {
      name : "parallel_processing",
      max_threads : 8,
      work_stealing_enabled : false,
      affinity_enabled : true
    },
    @azimuth.CPUOptimizationStrategy {
      name : "async_processing",
      max_threads : 2,
      work_stealing_enabled : true,
      affinity_enabled : false
    },
    @azimuth.CPUOptimizationStrategy {
      name : "batch_processing",
      max_threads : 16,
      work_stealing_enabled : true,
      affinity_enabled : true
    }
  ]
  
  // 生成CPU密集型测试数据
  let cpu_intensive_data = @azimuth.generate_cpu_intensive_telemetry_data(
    5000,  // 数据点数量
    50,    // 每个数据点的属性数量
    10,    // 每个数据点的事件数量
    100    // 每个属性的复杂度
  )
  
  // 测试不同CPU优化策略
  let cpu_results = []
  for strategy in cpu_optimization_strategies {
    let result = @azimuth.process_telemetry_with_cpu_optimization(
      cpu_intensive_data,
      strategy
    )
    cpu_results = cpu_results.push(result)
  }
  
  // 验证CPU优化结果
  assert_eq(cpu_results.length(), 4)
  
  // 验证所有策略都有效
  for result in cpu_results {
    assert_true(result.processing_time_ms > 0)
    assert_true(result.cpu_utilization_percent >= 0.0)
    assert_true(result.cpu_utilization_percent <= 100.0)
    assert_true(result.context_switches > 0)
    assert_true(result.cpu_efficiency_score > 0.0)
    assert_true(result.cpu_efficiency_score <= 1.0)
  }
  
  // 找出CPU效率最高的策略
  let most_cpu_efficient = cpu_results
    .sort_by(fn(a, b) { a.cpu_efficiency_score - b.cpu_efficiency_score })
    .reverse()[0]
  
  // 验证最高CPU效率
  assert_true(most_cpu_efficient.cpu_efficiency_score >= 0.7) // 至少70%效率
  
  // 找出处理时间最短的策略
  let fastest_processing = cpu_results
    .sort_by(fn(a, b) { a.processing_time_ms - b.processing_time_ms })[0]
  
  // 验证最短处理时间
  assert_true(fastest_processing.processing_time_ms <= cpu_results[0].processing_time_ms * 0.8)
  
  // 测试CPU亲和性优化
  let affinity_test = @azimuth.test_cpu_affinity_optimization(
    cpu_intensive_data,
    8, // 可用CPU核心数
    4  // 使用的核心数
  )
  
  // 验证CPU亲和性优化结果
  assert_true(affinity_test.affinity_configured)
  assert_true(affinity_test.cache_misses_reduced)
  assert_true(affinity_test.context_switches_reduced)
  assert_true(affinity_test.performance_improvement_percent > 0)
  
  // 测试CPU负载均衡
  let load_balancing_test = @azimuth.test_cpu_load_balancing(
    cpu_optimization_strategies,
    10000, // 数据点数量
    100    // 并发任务数
  )
  
  // 验证CPU负载均衡结果
  assert_eq(load_balancing_test.total_tasks, 100)
  assert_true(load_balancing_test.load_balanced)
  assert_true(load_balancing_test.cpu_utilization_evenness >= 0.8) // CPU利用率均匀性>=80%
  assert_true(load_balancing_test.task_distribution_variance <= 0.1) // 任务分布方差<=10%
  
  // 验证负载均衡效果
  assert_true(load_balancing_test.overall_throughput_improved)
  assert_true(load_balancing_test.bottleneck_eliminated)
}

test "io optimization techniques" {
  // 测试IO优化技术
  let io_optimization_strategies = [
    @azimuth.IOOptimizationStrategy {
      name : "buffered_io",
      buffer_size : 8192,
      async_enabled : false,
      mmap_enabled : false
    },
    @azimuth.IOOptimizationStrategy {
      name : "async_io",
      buffer_size : 4096,
      async_enabled : true,
      mmap_enabled : false
    },
    @azimuth.IOOptimizationStrategy {
      name : "memory_mapped_io",
      buffer_size : 16384,
      async_enabled : false,
      mmap_enabled : true
    },
    @azimuth.IOOptimizationStrategy {
      name : "direct_io",
      buffer_size : 32768,
      async_enabled : true,
      mmap_enabled : false
    }
  ]
  
  // 生成IO密集型测试数据
  let io_intensive_data = @azimuth.generate_io_intensive_telemetry_data(
    10000, // 数据点数量
    1000,  // 每个数据点的大小(bytes)
    50     // 每个数据点的IO操作数
  )
  
  // 测试不同IO优化策略
  let io_results = []
  for strategy in io_optimization_strategies {
    let result = @azimuth.process_telemetry_with_io_optimization(
      io_intensive_data,
      strategy
    )
    io_results = io_results.push(result)
  }
  
  // 验证IO优化结果
  assert_eq(io_results.length(), 4)
  
  // 验证所有策略都有效
  for result in io_results {
    assert_true(result.total_io_time_ms > 0)
    assert_true.result.read_throughput_mbps > 0.0
    assert_true(result.write_throughput_mbps > 0.0)
    assert_true(result.io_operations_count > 0)
    assert_true(result.io_efficiency_score > 0.0)
    assert_true(result.io_efficiency_score <= 1.0)
  }
  
  // 找出IO效率最高的策略
  let most_io_efficient = io_results
    .sort_by(fn(a, b) { a.io_efficiency_score - b.io_efficiency_score })
    .reverse()[0]
  
  // 验证最高IO效率
  assert_true(most_io_efficient.io_efficiency_score >= 0.7) // 至少70%效率
  
  // 找出读取吞吐量最高的策略
  let highest_read_throughput = io_results
    .sort_by(fn(a, b) { a.read_throughput_mbps - b.read_throughput_mbps })
    .reverse()[0]
  
  // 验证最高读取吞吐量
  assert_true(highest_read_throughput.read_throughput_mbps >= 100.0) // 至少100MB/s
  
  // 找出写入吞吐量最高的策略
  let highest_write_throughput = io_results
    .sort_by(fn(a, b) { a.write_throughput_mbps - b.write_throughput_mbps })
    .reverse()[0]
  
  // 验证最高写入吞吐量
  assert_true(highest_write_throughput.write_throughput_mbps >= 50.0) // 至少50MB/s
  
  // 测试IO缓存优化
  let cache_optimization_test = @azimuth.test_io_cache_optimization(
    io_intensive_data,
    100,  // 缓存大小(MB)
    1000  // 缓存条目数
  )
  
  // 验证IO缓存优化结果
  assert_true(cache_optimization_test.cache_configured)
  assert_true(cache_optimization_test.cache_hit_rate > 0.0)
  assert_true(cache_optimization_test.cache_hit_rate <= 1.0)
  assert_true(cache_optimization_test.io_operations_reduced > 0)
  
  // 验证缓存效果
  assert_true(cache_optimization_test.performance_improvement_percent > 0)
  assert_true(cache_optimization_test.latency_reduction_percent > 0)
  
  // 测试IO批处理优化
  let batch_io_test = @azimuth.test_io_batch_optimization(
    io_intensive_data,
    [
      10,   // 批大小10
      50,   // 批大小50
      100,  // 批大小100
      500   // 批大小500
    ]
  )
  
  // 验证IO批处理优化结果
  assert_eq(batch_io_test.batch_sizes_tested, 4)
  assert_true(batch_io_test.optimal_batch_size > 0)
  assert_true(batch_io_test.io_operations_reduced > 0)
  assert_true(batch_io_test.throughput_improvement_percent > 0)
  
  // 验证批处理效果
  assert_true(batch_io_test.latency_improvement_percent > 0)
  assert_true(batch_io_test.cpu_utilization_optimized)
}