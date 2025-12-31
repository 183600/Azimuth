// 综合性能基准测试
// 测试遥测系统在各种负载条件下的性能表现

test "span_creation_performance_benchmark" {
  // 测试span创建的性能基准
  
  let tracer = create_optimized_tracer()
  let benchmark_iterations = 100000
  
  // 预热阶段
  for i = 0; i < 1000; i = i + 1 {
    let (_, span) = tracer.start_span(Context::empty(), "warmup-span")
    span.end()
  }
  
  // 基准测试阶段
  let start_time = get_high_precision_timestamp()
  let created_spans = []
  
  for i = 0; i < benchmark_iterations; i = i + 1 {
    let (_, span) = tracer.start_span(Context::empty(), "benchmark-span-" + i.to_string())
    span.set_attribute("iteration", i.to_int64())
    span.set_attribute("timestamp", get_current_timestamp())
    created_spans.push(span)
  }
  
  let creation_time = get_high_precision_timestamp() - start_time
  
  // 结束所有spans
  let end_start_time = get_high_precision_timestamp()
  for span in created_spans {
    span.end()
  }
  let end_time = get_high_precision_timestamp() - end_start_time
  
  // 计算性能指标
  let creation_throughput = benchmark_iterations.to_float() / (creation_time / 1000000.0) // ops/sec
  let end_throughput = benchmark_iterations.to_float() / (end_time / 1000000.0) // ops/sec
  let avg_creation_time_ns = creation_time / benchmark_iterations.to_int64()
  let avg_end_time_ns = end_time / benchmark_iterations.to_int64()
  
  // 验证性能基准
  assert_eq(creation_throughput >= 100000, true) // 至少10万ops/sec
  assert_eq(end_throughput >= 200000, true)      // 结束span至少20万ops/sec
  assert_eq(avg_creation_time_ns <= 10000, true) // 平均创建时间小于10μs
  assert_eq(avg_end_time_ns <= 5000, true)       // 平均结束时间小于5μs
  
  // 输出性能报告
  let performance_report = PerformanceReport{
    test_name: "span_creation_benchmark",
    iterations: benchmark_iterations,
    total_time_ms: creation_time / 1000000,
    throughput_ops_per_sec: creation_throughput,
    avg_operation_time_ns: avg_creation_time_ns,
    memory_usage_mb: get_current_memory_usage()
  }
  
  log_performance_report(performance_report)
}

test "metric_recording_performance_benchmark" {
  // 测试指标记录的性能基准
  
  let metric_recorder = create_high_performance_metric_recorder()
  let benchmark_metrics = 50000
  let metric_types = ["counter", "gauge", "histogram", "timer"]
  
  // 预热
  for i = 0; i < 1000; i = i + 1 {
    metric_recorder.record_metric("warmup", @random.float(100.0), [])
  }
  
  // 基准测试不同类型的指标
  let performance_results = []
  
  for metric_type in metric_types {
    let start_time = get_high_precision_timestamp()
    
    for i = 0; i < benchmark_metrics; i = i + 1 {
      let labels = [
        ("type", metric_type),
        ("iteration", i.to_string())
      ]
      
      match metric_type {
        "counter" => metric_recorder.record_counter("test_counter", 1.0, labels),
        "gauge" => metric_recorder.record_gauge("test_gauge", @random.float(100.0), labels),
        "histogram" => metric_recorder.record_histogram("test_histogram", @random.float(1000.0), labels),
        "timer" => metric_recorder.record_timer("test_timer", @random.float(100.0), labels),
        _ => {}
      }
    }
    
    let end_time = get_high_precision_timestamp()
    let duration = end_time - start_time
    let throughput = benchmark_metrics.to_float() / (duration / 1000000.0)
    
    performance_results.push((metric_type, throughput, duration / benchmark_metrics.to_int64()))
  }
  
  // 验证各类型指标的性能
  for (metric_type, throughput, avg_time_ns) in performance_results {
    assert_eq(throughput >= 50000, true) // 至少5万ops/sec
    assert_eq(avg_time_ns <= 20000, true) // 平均时间小于20μs
  }
  
  // 验证内存使用效率
  let memory_usage = metric_recorder.get_memory_usage()
  assert_eq(memory_usage.memory_per_metric_bytes <= 200, true) // 每个指标不超过200字节
  assert_eq(memory_usage.total_memory_mb <= 50.0, true)       // 总内存不超过50MB
}

test "log_writing_performance_benchmark" {
  // 测试日志写入的性能基准
  
  let log_writer = create_optimized_log_writer()
  let benchmark_logs = 100000
  let log_levels = ["DEBUG", "INFO", "WARN", "ERROR"]
  
  // 预热阶段
  for i = 0; i < 1000; i = i + 1 {
    log_writer.write_log(LogRecord{
      timestamp: get_current_timestamp(),
      level: "INFO",
      message: "warmup message " + i.to_string(),
      attributes: [("warmup", "true")]
    })
  }
  
  // 基准测试不同级别的日志
  let level_performance = []
  
  for level in log_levels {
    let start_time = get_high_precision_timestamp()
    
    for i = 0; i < benchmark_logs / log_levels.length(); i = i + 1 {
      let log_record = LogRecord{
        timestamp: get_current_timestamp(),
        level: level,
        message: "Benchmark log message " + i.to_string() + " at level " + level,
        attributes: [
          ("level", level),
          ("iteration", i.to_string()),
          ("thread_id", @thread.get_current_id().to_string()),
          ("random_data", @random.string(50)) // 50字符随机数据
        ]
      }
      
      log_writer.write_log(log_record)
    }
    
    // 强制刷新以确保所有日志都写入
    log_writer.flush()
    
    let end_time = get_high_precision_timestamp()
    let duration = end_time - start_time
    let log_count = benchmark_logs / log_levels.length()
    let throughput = log_count.to_float() / (duration / 1000000.0)
    
    level_performance.push((level, throughput, duration / log_count.to_int64()))
  }
  
  // 验证各级别日志的性能
  for (level, throughput, avg_time_ns) in level_performance {
    assert_eq(throughput >= 20000, true) // 至少2万logs/sec
    assert_eq(avg_time_ns <= 50000, true) // 平均时间小于50μs
  }
  
  // 验证批量写入性能
  let batch_start_time = get_high_precision_timestamp()
  let batch_logs = []
  
  for i = 0; i < 10000; i = i + 1 {
    batch_logs.push(LogRecord{
      timestamp: get_current_timestamp(),
      level: "INFO",
      message: "Batch log " + i.to_string(),
      attributes: [("batch", "true")]
    })
  }
  
  log_writer.write_batch(batch_logs)
  log_writer.flush()
  
  let batch_end_time = get_high_precision_timestamp()
  let batch_duration = batch_end_time - batch_start_time
  let batch_throughput = 10000.0 / (batch_duration / 1000000.0)
  
  assert_eq(batch_throughput >= 100000, true) // 批量写入至少10万logs/sec
}

test "context_propagation_performance_benchmark" {
  // 测试上下文传播的性能基准
  
  let context_manager = create_optimized_context_manager()
  let propagation_depths = [1, 5, 10, 20, 50]
  let iterations_per_depth = 10000
  
  // 预热
  let root_ctx = context_manager.create_root_context()
  for i = 0; i < 1000; i = i + 1 {
    let child_ctx = context_manager.create_child_context(root_ctx)
    child_ctx.set_attribute("warmup", i.to_string())
  }
  
  // 基准测试不同传播深度的性能
  let depth_performance = []
  
  for depth in propagation_depths {
    let start_time = get_high_precision_timestamp()
    
    for i = 0; i < iterations_per_depth; i = i + 1 {
      // 创建指定深度的上下文链
      let current_ctx = root_ctx
      
      for d = 0; d < depth; d = d + 1 {
        current_ctx = context_manager.create_child_context(current_ctx)
        current_ctx.set_attribute("depth", d.to_string())
        current_ctx.set_attribute("iteration", i.to_string())
      }
      
      // 在最深层上下文中读取所有属性
      let _ = current_ctx.get_attribute("depth")
      let _ = current_ctx.get_attribute("iteration")
      let _ = current_ctx.get_all_attributes()
    }
    
    let end_time = get_high_precision_timestamp()
    let duration = end_time - start_time
    let throughput = iterations_per_depth.to_float() / (duration / 1000000.0)
    let avg_time_per_operation = duration / iterations_per_depth.to_int64()
    
    depth_performance.push((depth, throughput, avg_time_per_operation))
  }
  
  // 验证不同深度的性能
  for (depth, throughput, avg_time_ns) in depth_performance {
    // 随着深度增加，性能会下降，但应该在可接受范围内
    let expected_min_throughput = 100000.0 / depth.to_float() // 递减的期望吞吐量
    assert_eq(throughput >= expected_min_throughput, true)
    assert_eq(avg_time_ns <= depth.to_int64() * 1000, true) // 每层深度不超过1μs
  }
  
  // 验证上下文存储的内存效率
  let memory_stats = context_manager.get_memory_statistics()
  assert_eq(memory_stats.memory_per_context_bytes <= 500, true) // 每个上下文不超过500字节
  assert_eq(memory_stats.attribute_storage_overhead_percent <= 20, true) // 属性存储开销不超过20%
}

test "serialization_performance_benchmark" {
  // 测试序列化/反序列化的性能基准
  
  let serializer = create_high_performance_serializer()
  let data_sizes = [100, 1000, 10000, 50000] // 不同大小的数据集
  
  for size in data_sizes {
    // 创建测试数据
    let test_data = generate_complex_telemetry_data(size)
    
    // 基准测试序列化
    let serialize_start = get_high_precision_timestamp()
    let serialized_results = []
    
    for data in test_data {
      let serialized = serializer.serialize(data)
      serialized_results.push(serialized)
    }
    
    let serialize_end = get_high_precision_timestamp()
    let serialize_duration = serialize_end - serialize_start
    let serialize_throughput = size.to_float() / (serialize_duration / 1000000.0)
    
    // 基准测试反序列化
    let deserialize_start = get_high_precision_timestamp()
    let deserialized_results = []
    
    for serialized in serialized_results {
      let deserialized = serializer.deserialize(serialized)
      deserialized_results.push(deserialized)
    }
    
    let deserialize_end = get_high_precision_timestamp()
    let deserialize_duration = deserialize_end - deserialize_start
    let deserialize_throughput = size.to_float() / (deserialize_duration / 1000000.0)
    
    // 验证性能
    assert_eq(serialize_throughput >= 1000, true)  // 序列化至少1000 ops/sec
    assert_eq(deserialize_throughput >= 1000, true) // 反序列化至少1000 ops/sec
    
    // 验证数据完整性
    assert_eq(deserialized_results.length(), test_data.length())
    for i = 0; i < test_data.length(); i = i + 1 {
      assert_eq(are_telemetry_data_equal(test_data[i], deserialized_results[i]), true)
    }
    
    // 验证压缩效率
    let original_size = calculate_data_size(test_data)
    let compressed_size = calculate_serialized_size(serialized_results)
    let compression_ratio = compressed_size.to_float() / original_size.to_float()
    
    assert_eq(compression_ratio <= 0.8, true) // 至少20%的压缩率
  }
}

test "memory_allocation_performance_benchmark" {
  // 测试内存分配的性能基准
  
  let allocation_iterations = 100000
  
  // 基准测试对象池化vs直接分配
  let object_pool = create_telemetry_object_pool()
  
  // 测试对象池化分配
  let pool_start_time = get_high_precision_timestamp()
  
  for i = 0; i < allocation_iterations; i = i + 1 {
    let span = object_pool.acquire_span()
    span.name = "pooled-span-" + i.to_string()
    object_pool.release_span(span)
  }
  
  let pool_end_time = get_high_precision_timestamp()
  let pool_duration = pool_end_time - pool_start_time
  let pool_throughput = allocation_iterations.to_float() / (pool_duration / 1000000.0)
  
  // 测试直接分配
  let direct_start_time = get_high_precision_timestamp()
  
  for i = 0; i < allocation_iterations; i = i + 1 {
    let span = create_span_directly()
    span.name = "direct-span-" + i.to_string()
    // span会被GC回收
  }
  
  let direct_end_time = get_high_precision_timestamp()
  let direct_duration = direct_end_time - direct_start_time
  let direct_throughput = allocation_iterations.to_float() / (direct_duration / 1000000.0)
  
  // 验证对象池化的性能优势
  let performance_improvement = (pool_throughput - direct_throughput) / direct_throughput
  assert_eq(performance_improvement >= 0.2, true) // 至少20%的性能提升
  
  // 验证内存效率
  let pool_memory_stats = object_pool.get_memory_statistics()
  assert_eq(pool_memory_stats.pool_hit_rate >= 0.95, true) // 池命中率应高于95%
  assert_eq(pool_memory_stats.memory_saved_mb > 0, true)   // 应该节省内存
  
  // 测试GC压力
  force_garbage_collection()
  let gc_before_memory = get_memory_usage()
  
  // 执行大量直接分配
  for i = 0; i < allocation_iterations; i = i + 1 {
    let _ = create_span_directly()
  }
  
  let gc_after_memory = get_memory_usage()
  force_garbage_collection()
  let gc_final_memory = get_memory_usage()
  
  // 验证GC效果
  let memory_growth = gc_after_memory - gc_before_memory
  let memory_recovered = gc_after_memory - gc_final_memory
  let gc_efficiency = memory_recovered.to_float() / memory_growth.to_float()
  
  assert_eq(gc_efficiency >= 0.8, true) // GC应该回收至少80%的内存
}