// Azimuth Advanced Performance Test Suite
// 高级性能测试用例，专注于系统性能和资源优化

// 测试5: 高并发遥测数据处理
test "高并发遥测数据处理测试" {
  // 创建并发处理器
  let concurrent_processor = ConcurrentTelemetryProcessor::new({
    worker_count: 8,
    queue_size: 10000,
    batch_size: 100
  })
  
  // 启动处理器
  ConcurrentTelemetryProcessor::start(concurrent_processor)
  
  // 创建大量测试数据
  let test_data_count = 10000
  let start_time = Time::now()
  
  // 并发生成测试数据
  let producer_tasks = []
  for i in 0..=4 {
    let task = ConcurrentTask::spawn(fn() {
      for j in 0..=1999 { // 每个任务生成2000条数据
        let index = i * 2000 + j
        let telemetry_data = {
          trace_id: "trace-" + index.to_string(),
          span_id: "span-" + index.to_string(),
          service_name: "service-" + (index % 10).to_string(),
          operation_name: "operation-" + (index % 20).to_string(),
          start_time: Time::now(),
          duration: 50 + (index % 200),
          status: if index % 50 == 0 { "error" } else { "ok" },
          attributes: [
            ("http.method", if index % 2 == 0 { "GET" } else { "POST" }),
            ("http.status_code", (200 + (index % 5) * 100).to_string())
          ]
        }
        
        ConcurrentTelemetryProcessor::submit(concurrent_processor, telemetry_data)
      }
    })
    producer_tasks = producer_tasks.push(task)
  }
  
  // 等待所有生产者完成
  for task in producer_tasks {
    ConcurrentTask::join(task)
  }
  
  // 等待所有数据处理完成
  ConcurrentTelemetryProcessor::flush(concurrent_processor)
  
  let processing_time = Time::now() - start_time
  
  // 验证处理性能
  assert_true(processing_time < 30000) // 应在30秒内完成
  
  // 获取处理统计
  let stats = ConcurrentTelemetryProcessor::get_stats(concurrent_processor)
  
  // 验证处理统计
  assert_eq(stats.processed_count, test_data_count)
  assert_eq(stats.error_count, 0)
  assert_true(stats.average_processing_time > 0)
  assert_true(stats.throughput > 0)
  
  // 验证吞吐量
  let expected_min_throughput = test_data_count.to_float() / processing_time.to_float() * 1000.0
  assert_true(stats.throughput >= expected_min_throughput * 0.8) // 允许20%误差
  
  // 停止处理器
  ConcurrentTelemetryProcessor::stop(concurrent_processor)
}

// 测试6: 内存使用优化
test "内存使用优化测试" {
  // 创建内存优化管理器
  let memory_optimizer = MemoryOptimizer::new({
    max_heap_size: 536870912, // 512MB
    gc_threshold: 0.8,        // 80%堆使用率触发GC
    buffer_pool_size: 10485760 // 10MB缓冲池
  })
  
  // 模拟内存使用场景
  let memory_scenarios = [
    {
      name: "small_objects",
      object_size: 1024,      // 1KB对象
      object_count: 1000,     // 1000个对象
      expected_memory: 1048576 // 约1MB
    },
    {
      name: "medium_objects",
      object_size: 10240,     // 10KB对象
      object_count: 500,      // 500个对象
      expected_memory: 5242880 // 约5MB
    },
    {
      name: "large_objects",
      object_size: 102400,    // 100KB对象
      object_count: 100,      // 100个对象
      expected_memory: 10485760 // 约10MB
    }
  ]
  
  for scenario in memory_scenarios {
    // 记录初始内存使用
    let initial_memory = MemoryOptimizer::get_memory_usage(memory_optimizer)
    
    // 创建对象
    let objects = []
    for i in 0..=scenario.object_count {
      let object = MemoryOptimizer::allocate_object(memory_optimizer, scenario.object_size)
      objects = objects.push(object)
    }
    
    // 记录峰值内存使用
    let peak_memory = MemoryOptimizer::get_memory_usage(memory_optimizer)
    
    // 验证内存增长
    let memory_growth = peak_memory.used - initial_memory.used
    assert_true(memory_growth >= scenario.expected_memory * 0.8) // 允许20%误差
    assert_true(memory_growth <= scenario.expected_memory * 1.5) // 允许50%误差
    
    // 释放一半对象
    let remaining_objects = []
    for i in 0..=objects.length() - 1 {
      if i % 2 == 0 {
        MemoryOptimizer::deallocate_object(memory_optimizer, objects[i])
      } else {
        remaining_objects = remaining_objects.push(objects[i])
      }
    }
    
    // 强制垃圾回收
    MemoryOptimizer::force_gc(memory_optimizer)
    
    // 记录GC后内存使用
    let gc_memory = MemoryOptimizer::get_memory_usage(memory_optimizer)
    
    // 验证内存回收
    let memory_freed = peak_memory.used - gc_memory.used
    assert_true(memory_freed >= scenario.expected_memory * 0.3) // 至少回收30%
    
    // 释放剩余对象
    for object in remaining_objects {
      MemoryOptimizer::deallocate_object(memory_optimizer, object)
    }
    
    // 最终垃圾回收
    MemoryOptimizer::force_gc(memory_optimizer)
    
    // 记录最终内存使用
    let final_memory = MemoryOptimizer::get_memory_usage(memory_optimizer)
    
    // 验证内存接近初始状态
    let memory_difference = final_memory.used - initial_memory.used
    assert_true(memory_difference < 1048576) // 差异小于1MB
  }
  
  // 测试内存池
  let buffer_pool = MemoryOptimizer::get_buffer_pool(memory_optimizer)
  
  // 从池中获取缓冲区
  let buffers = []
  for i in 0..=100 {
    let buffer = BufferPool::acquire(buffer_pool, 10240) // 10KB缓冲区
    buffers = buffers.push(buffer)
  }
  
  // 验证池状态
  let pool_stats = BufferPool::get_stats(buffer_pool)
  assert_eq(pool_stats.acquired_count, 101)
  assert_true(pool_stats.pool_size > 0)
  
  // 归还缓冲区到池中
  for buffer in buffers {
    BufferPool::release(buffer_pool, buffer)
  }
  
  // 验证缓冲区归还
  let final_pool_stats = BufferPool::get_stats(buffer_pool)
  assert_eq(final_pool_stats.released_count, 100)
  assert_true(final_pool_stats.pool_size >= pool_stats.pool_size)
}

// 测试7: 自适应采样策略性能
test "自适应采样策略性能测试" {
  // 创建自适应采样管理器
  let adaptive_sampler = AdaptiveSampler::new({
    base_sampling_rate: 0.1,    // 基础10%采样率
    max_sampling_rate: 1.0,     // 最大100%采样率
    min_sampling_rate: 0.01,    // 最小1%采样率
    adjustment_window: 60000,   // 1分钟调整窗口
    throughput_target: 1000.0   // 目标吞吐量1000/秒
  })
  
  // 模拟不同负载场景
  let load_scenarios = [
    {
      name: "low_load",
      request_rate: 100.0,  // 100请求/秒
      duration: 10000,      // 10秒
      expected_sampling_rate: 0.1 // 保持基础采样率
    },
    {
      name: "medium_load",
      request_rate: 1000.0, // 1000请求/秒
      duration: 10000,      // 10秒
      expected_sampling_rate: 0.1 // 保持基础采样率
    },
    {
      name: "high_load",
      request_rate: 10000.0, // 10000请求/秒
      duration: 10000,       // 10秒
      expected_sampling_rate: 0.1 // 降低采样率
    },
    {
      name: "burst_load",
      request_rate: 50000.0, // 50000请求/秒
      duration: 5000,        // 5秒
      expected_sampling_rate: 0.02 // 大幅降低采样率
    }
  ]
  
  for scenario in load_scenarios {
    // 记录开始时间
    let start_time = Time::now()
    let end_time = start_time + scenario.duration
    
    // 重置采样器统计
    AdaptiveSampler::reset_stats(adaptive_sampler)
    
    // 模拟请求
    let request_interval = 1000.0 / scenario.request_rate // 毫秒
    let mut request_count = 0
    let mut sampled_count = 0
    
    while Time::now() < end_time {
      // 创建测试span
      let test_span = {
        trace_id: "trace-" + request_count.to_string(),
        span_id: "span-" + request_count.to_string(),
        service_name: "test.service",
        operation_name: "test.operation",
        timestamp: Time::now()
      }
      
      // 应用采样
      let should_sample = AdaptiveSampler::should_sample(adaptive_sampler, test_span)
      
      if should_sample {
        sampled_count = sampled_count + 1
      }
      
      request_count = request_count + 1
      
      // 控制请求频率
      Thread::sleep(request_interval.to_int())
    }
    
    // 计算实际采样率
    let actual_sampling_rate = sampled_count.to_float() / request_count.to_float()
    
    // 获取采样器状态
    let sampler_state = AdaptiveSampler::get_state(adaptive_sampler)
    
    // 验证采样率调整
    assert_true(actual_sampling_rate >= scenario.expected_sampling_rate * 0.8) // 允许20%误差
    assert_true(actual_sampling_rate <= scenario.expected_sampling_rate * 1.5) // 允许50%误差
    
    // 验证吞吐量控制
    let actual_throughput = sampled_count.to_float() / (scenario.duration / 1000.0)
    assert_true(actual_throughput <= scenario.throughput_target * 1.2) // 允许20%误差
    
    // 验证采样器状态
    assert_true(sampler_state.current_sampling_rate > 0)
    assert_true(sampler_state.current_sampling_rate <= 1.0)
    assert_eq(sampler_state.total_requests, request_count)
    assert_eq(sampler_state.sampled_requests, sampled_count)
  }
  
  // 测试采样器在错误率高的场景下的行为
  AdaptiveSampler::reset_stats(adaptive_sampler)
  
  // 模拟高错误率场景
  let error_scenarios = [
    { error_rate: 0.01, expected_sampling_adjustment: 0.0 },   // 1%错误率，不调整
    { error_rate: 0.05, expected_sampling_adjustment: 0.2 },   // 5%错误率，增加20%
    { error_rate: 0.1, expected_sampling_adjustment: 0.5 },    // 10%错误率，增加50%
    { error_rate: 0.2, expected_sampling_adjustment: 1.0 }     // 20%错误率，增加100%
  ]
  
  for scenario in error_scenarios {
    // 创建错误span
    for i in 0..=100 {
      let test_span = {
        trace_id: "error-trace-" + i.to_string(),
        span_id: "error-span-" + i.to_string(),
        service_name: "error.service",
        operation_name: "error.operation",
        timestamp: Time::now(),
        is_error: Random::next_float() < scenario.error_rate
      }
      
      // 应用采样
      AdaptiveSampler::should_sample(adaptive_sampler, test_span)
    }
    
    // 获取采样器状态
    let sampler_state = AdaptiveSampler::get_state(adaptive_sampler)
    
    // 验证错误率对采样率的影响
    let base_rate = 0.1
    let expected_rate = base_rate + scenario.expected_sampling_adjustment
    let adjusted_rate = sampler_state.current_sampling_rate
    
    assert_true(adjusted_rate >= base_rate)
    if expected_rate <= 1.0 {
      assert_true(adjusted_rate <= expected_rate * 1.2) // 允许20%误差
    } else {
      assert_true(adjusted_rate <= 1.0) // 不超过100%
    }
  }
}

// 测试8: 遥测数据压缩性能
test "遥测数据压缩性能测试" {
  // 创建压缩性能测试器
  let compression_benchmarker = CompressionBenchmarker::new()
  
  // 测试不同压缩算法
  let compression_algorithms = ["gzip", "lz4", "zstd", "snappy"]
  
  // 创建不同大小的测试数据集
  let test_datasets = [
    {
      name: "small_dataset",
      size: 10240,        // 10KB
      record_count: 100,
      data_pattern: "random"
    },
    {
      name: "medium_dataset",
      size: 102400,       // 100KB
      record_count: 1000,
      data_pattern: "random"
    },
    {
      name: "large_dataset",
      size: 1048576,      // 1MB
      record_count: 10000,
      data_pattern: "random"
    },
    {
      name: "repetitive_dataset",
      size: 102400,       // 100KB
      record_count: 1000,
      data_pattern: "repetitive"
    }
  ]
  
  for dataset in test_datasets {
    // 生成测试数据
    let test_data = CompressionBenchmarker::generate_test_data(
      compression_benchmarker,
      dataset.size,
      dataset.record_count,
      dataset.data_pattern
    )
    
    assert_eq(test_data.length(), dataset.size)
    
    // 测试每种压缩算法
    for algorithm in compression_algorithms {
      // 压缩性能测试
      let compression_start = Time::now()
      let compressed_data = CompressionBenchmarker::compress(
        compression_benchmarker,
        test_data,
        algorithm
      )
      let compression_time = Time::now() - compression_start
      
      // 验证压缩结果
      assert_true(compressed_data.length() < test_data.length())
      
      // 计算压缩比
      let compression_ratio = test_data.length().to_float() / compressed_data.length().to_float()
      
      // 解压性能测试
      let decompression_start = Time::now()
      let decompressed_data = CompressionBenchmarker::decompress(
        compression_benchmarker,
        compressed_data,
        algorithm
      )
      let decompression_time = Time::now() - decompression_start
      
      // 验证解压结果
      assert_eq(decompressed_data.length(), test_data.length())
      assert_true(decompressed_data == test_data)
      
      // 计算性能指标
      let compression_throughput = test_data.length().to_float() / compression_time.to_float() * 1000.0
      let decompression_throughput = test_data.length().to_float() / decompression_time.to_float() * 1000.0
      
      // 记录基准测试结果
      let benchmark_result = {
        algorithm: algorithm,
        dataset: dataset.name,
        original_size: test_data.length(),
        compressed_size: compressed_data.length(),
        compression_ratio: compression_ratio,
        compression_time: compression_time,
        decompression_time: decompression_time,
        compression_throughput: compression_throughput,
        decompression_throughput: decompression_throughput
      }
      
      CompressionBenchmarker::record_result(compression_benchmarker, benchmark_result)
      
      // 验证最低性能要求
      assert_true(compression_throughput > 1024)    // 至少1KB/s压缩速度
      assert_true(decompression_throughput > 2048)  // 至少2KB/s解压速度
      assert_true(compression_ratio > 1.1)          // 至少10%压缩率
    }
  }
  
  // 分析基准测试结果
  let analysis_results = CompressionBenchmarker::analyze_results(compression_benchmarker)
  
  // 验证分析结果
  assert_true(analysis_results.length() > 0)
  
  // 检查不同算法的性能特点
  let gzip_results = analysis_results.filter(fn(r) { r.algorithm == "gzip" })
  let lz4_results = analysis_results.filter(fn(r) { r.algorithm == "lz4" })
  
  // 验证gzip有更好的压缩比
  let avg_gzip_ratio = gzip_results.map(fn(r) { r.compression_ratio }).reduce(fn(a, b) { a + b }, 0.0) / gzip_results.length().to_float()
  let avg_lz4_ratio = lz4_results.map(fn(r) { r.compression_ratio }).reduce(fn(a, b) { a + b }, 0.0) / lz4_results.length().to_float()
  
  assert_true(avg_gzip_ratio > avg_lz4_ratio)
  
  // 验证lz4有更快的速度
  let avg_lz4_compression_throughput = lz4_results.map(fn(r) { r.compression_throughput }).reduce(fn(a, b) { a + b }, 0.0) / lz4_results.length().to_float()
  let avg_gzip_compression_throughput = gzip_results.map(fn(r) { r.compression_throughput }).reduce(fn(a, b) { a + b }, 0.0) / gzip_results.length().to_float()
  
  assert_true(avg_lz4_compression_throughput > avg_gzip_compression_throughput)
  
  // 测试自适应压缩策略选择
  let adaptive_compressor = AdaptiveCompressor::new(analysis_results)
  
  // 测试不同场景下的最佳算法选择
  let scenarios = [
    { 
      name: "network_transmission", 
      priorities: ["compression_ratio", "compression_speed"],
      expected_algorithm: "gzip" // 优先压缩比
    },
    { 
      name: "real_time_processing", 
      priorities: ["compression_speed", "decompression_speed"],
      expected_algorithm: "lz4" // 优先速度
    },
    { 
      name: "storage_optimization", 
      priorities: ["compression_ratio"],
      expected_algorithm: "gzip" // 只考虑压缩比
    }
  ]
  
  for scenario in scenarios {
    let selected_algorithm = AdaptiveCompressor::select_algorithm(
      adaptive_compressor,
      scenario.priorities
    )
    
    assert_eq(selected_algorithm, scenario.expected_algorithm)
  }
}