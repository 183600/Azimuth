// Azimuth 遥测系统性能基准测试
// 测试遥测系统的性能基准

// 测试1: 数据生成性能测试
test "数据生成性能测试" {
  // 创建性能基准测试器
  let benchmark_runner = BenchmarkRunner::new()
  
  // 配置基准测试参数
  let benchmark_config = {
    warmup_iterations: 100,
    measurement_iterations: 1000,
    data_sizes: [100, 1000, 10000],
    enable_memory_profiling: true,
    enable_cpu_profiling: true
  }
  
  // 测试Span生成性能
  let span_generation_benchmark = BenchmarkRunner::create_span_generation_benchmark(benchmark_runner, {
    services: ["payment-service", "user-service", "order-service", "notification-service"],
    operations: ["http.request", "database.query", "cache.get", "message.publish"],
    attribute_count: 5,
    event_count: 2
  })
  
  // 执行Span生成基准测试
  let span_results = BenchmarkRunner::run_benchmark(benchmark_runner, span_generation_benchmark, benchmark_config)
  
  // 验证Span生成性能结果
  assert_true(span_results.success)
  assert_eq(span_results.measurement_iterations, 1000)
  
  // 验证性能指标
  for size_result in span_results.size_results {
    assert_true(size_result.avg_time_per_item > 0)
    assert_true(size_result.throughput > 0)
    assert_true(size_result.memory_usage_per_item > 0)
    
    // 性能基准：每秒至少生成10000个Span
    assert_true(size_result.throughput >= 10000)
    
    // 内存基准：每个Span不超过1KB内存
    assert_true(size_result.memory_usage_per_item <= 1024)
  }
  
  // 测试Metric生成性能
  let metric_generation_benchmark = BenchmarkRunner::create_metric_generation_benchmark(benchmark_runner, {
    metric_types: ["counter", "gauge", "histogram", "summary"],
    services: ["payment-service", "user-service", "order-service", "notification-service"],
    attribute_count: 3
  })
  
  // 执行Metric生成基准测试
  let metric_results = BenchmarkRunner::run_benchmark(benchmark_runner, metric_generation_benchmark, benchmark_config)
  
  // 验证Metric生成性能结果
  assert_true(metric_results.success)
  
  // 验证性能指标
  for size_result in metric_results.size_results {
    // 性能基准：每秒至少生成20000个Metric
    assert_true(size_result.throughput >= 20000)
    
    // 内存基准：每个Metric不超过512B内存
    assert_true(size_result.memory_usage_per_item <= 512)
  }
  
  // 测试Log生成性能
  let log_generation_benchmark = BenchmarkRunner::create_log_generation_benchmark(benchmark_runner, {
    log_levels: ["debug", "info", "warn", "error"],
    services: ["payment-service", "user-service", "order-service", "notification-service"],
    message_length: 100
  })
  
  // 执行Log生成基准测试
  let log_results = BenchmarkRunner::run_benchmark(benchmark_runner, log_generation_benchmark, benchmark_config)
  
  // 验证Log生成性能结果
  assert_true(log_results.success)
  
  // 验证性能指标
  for size_result in log_results.size_results {
    // 性能基准：每秒至少生成50000个Log
    assert_true(size_result.throughput >= 50000)
    
    // 内存基准：每个Log不超过256B内存
    assert_true(size_result.memory_usage_per_item <= 256)
  }
  
  // 比较不同数据类型的生成性能
  let performance_comparison = BenchmarkRunner::compare_performance(benchmark_runner, [
    span_results,
    metric_results,
    log_results
  ])
  
  // 验证性能比较结果
  assert_eq(performance_comparison.length(), 3)
  
  // Log生成应该是最快的
  let fastest_type = performance_comparison[0]
  assert_eq(fastest_type.data_type, "log")
  
  // Span生成应该是最慢的
  let slowest_type = performance_comparison[2]
  assert_eq(slowest_type.data_type, "span")
}

// 测试2: 数据处理性能测试
test "数据处理性能测试" {
  // 创建性能基准测试器
  let benchmark_runner = BenchmarkRunner::new()
  
  // 创建测试数据集
  let test_datasets = []
  
  // 小数据集（1K项）
  let small_dataset = TestDataGenerator::generate_spans(1000, {
    services: 10,
    operations: 50,
    attributes_per_span: 5,
    events_per_span: 2
  })
  test_datasets = test_datasets.push({ size: 1000, data: small_dataset })
  
  // 中等数据集（10K项）
  let medium_dataset = TestDataGenerator::generate_spans(10000, {
    services: 20,
    operations: 100,
    attributes_per_span: 5,
    events_per_span: 2
  })
  test_datasets = test_datasets.push({ size: 10000, data: medium_dataset })
  
  // 大数据集（100K项）
  let large_dataset = TestDataGenerator::generate_spans(100000, {
    services: 50,
    operations: 200,
    attributes_per_span: 5,
    events_per_span: 2
  })
  test_datasets = test_datasets.push({ size: 100000, data: large_dataset })
  
  // 测试序列化性能
  for dataset in test_datasets {
    let serialization_benchmark = BenchmarkRunner::create_serialization_benchmark(benchmark_runner, {
      format: "json",
      compression: false
    })
    
    let serialization_result = BenchmarkRunner::run_benchmark_with_data(benchmark_runner, serialization_benchmark, dataset.data)
    
    // 验证序列化性能
    assert_true(serialization_result.success)
    assert_true(serialization_result.throughput > 0)
    
    // 性能基准：每秒至少序列化1000个项目
    assert_true(serialization_result.throughput >= 1000)
    
    // 测试压缩序列化性能
    let compressed_serialization_benchmark = BenchmarkRunner::create_serialization_benchmark(benchmark_runner, {
      format: "json",
      compression: true,
      compression_algorithm: "gzip"
    })
    
    let compressed_result = BenchmarkRunner::run_benchmark_with_data(benchmark_runner, compressed_serialization_benchmark, dataset.data)
    
    // 验证压缩序列化性能
    assert_true(compressed_result.success)
    
    // 压缩应该降低吞吐量但不超过50%
    let throughput_ratio = compressed_result.throughput / serialization_result.throughput
    assert_true(throughput_ratio >= 0.5)
    
    // 压缩应该显著减少输出大小
    let compression_ratio = compressed_result.output_size.to_float() / serialization_result.output_size.to_float()
    assert_true(compression_ratio <= 0.7)  // 至少30%压缩率
  }
  
  // 测试过滤性能
  for dataset in test_datasets {
    let filter_benchmark = BenchmarkRunner::create_filtering_benchmark(benchmark_runner, {
      filters: [
        {
          field: "service_name",
          operator: "equals",
          value: "payment-service"
        },
        {
          field: "status",
          operator: "equals",
          value: "error"
        }
      ]
    })
    
    let filter_result = BenchmarkRunner::run_benchmark_with_data(benchmark_runner, filter_benchmark, dataset.data)
    
    // 验证过滤性能
    assert_true(filter_result.success)
    
    // 性能基准：每秒至少过滤5000个项目
    assert_true(filter_result.throughput >= 5000)
    
    // 验证过滤结果正确性
    assert_true(filter_result.output_count < dataset.size)
  }
  
  // 测试聚合性能
  for dataset in test_datasets {
    let aggregation_benchmark = BenchmarkRunner::create_aggregation_benchmark(benchmark_runner, {
      group_by: ["service_name", "operation_name"],
      aggregations: [
        {
          function: "count",
          field: "span_id"
        },
        {
          function: "avg",
          field: "duration"
        },
        {
          function: "max",
          field: "duration"
        }
      ]
    })
    
    let aggregation_result = BenchmarkRunner::run_benchmark_with_data(benchmark_runner, aggregation_benchmark, dataset.data)
    
    // 验证聚合性能
    assert_true(aggregation_result.success)
    
    // 性能基准：每秒至少聚合2000个项目
    assert_true(aggregation_result.throughput >= 2000)
    
    // 验证聚合结果正确性
    assert_true(aggregation_result.output_count > 0)
    assert_true(aggregation_result.output_count <= dataset.size)
  }
}

// 测试3: 存储性能测试
test "存储性能测试" {
  // 创建存储性能测试器
  let storage_benchmark = StorageBenchmark::new()
  
  // 配置存储后端
  let storage_backends = [
    {
      name: "memory",
      type: "in_memory",
      config: { max_size: 1073741824 }  // 1GB
    },
    {
      name: "file",
      type: "file_based",
      config: { path: "/tmp/telemetry_benchmark", cache_size: 104857600 }  // 100MB缓存
    },
    {
      name: "database",
      type: "sql_database",
      config: { connection_string: "postgresql://localhost:5432/telemetry_benchmark", pool_size: 10 }
    }
  ]
  
  // 测试不同存储后端的写入性能
  let write_results = []
  
  for backend in storage_backends {
    let write_benchmark = StorageBenchmark::create_write_benchmark(storage_benchmark, {
      backend: backend,
      batch_sizes: [100, 500, 1000],
      data_types: ["span", "metric", "log"]
    })
    
    let result = StorageBenchmark::run_write_benchmark(storage_benchmark, write_benchmark)
    write_results = write_results.push(result)
  }
  
  // 验证写入性能结果
  for result in write_results {
    assert_true(result.success)
    
    for batch_result in result.batch_results {
      // 性能基准：内存存储每秒至少写入100000个项目
      if result.backend.name == "memory" {
        assert_true(batch_result.throughput >= 100000)
      }
      // 性能基准：文件存储每秒至少写入10000个项目
      else if result.backend.name == "file" {
        assert_true(batch_result.throughput >= 10000)
      }
      // 性能基准：数据库存储每秒至少写入5000个项目
      else if result.backend.name == "database" {
        assert_true(batch_result.throughput >= 5000)
      }
    }
  }
  
  // 测试不同存储后端的读取性能
  let read_results = []
  
  for backend in storage_backends {
    // 先写入测试数据
    let test_data = TestDataGenerator::generate_mixed_data(10000, {
      span_ratio: 0.6,
      metric_ratio: 0.3,
      log_ratio: 0.1
    })
    
    StorageBenchmark::prepare_test_data(storage_benchmark, backend, test_data)
    
    let read_benchmark = StorageBenchmark::create_read_benchmark(storage_benchmark, {
      backend: backend,
      query_types: ["by_id", "by_service", "by_time_range", "by_attributes"],
      result_sizes: [10, 100, 1000]
    })
    
    let result = StorageBenchmark::run_read_benchmark(storage_benchmark, read_benchmark)
    read_results = read_results.push(result)
  }
  
  // 验证读取性能结果
  for result in read_results {
    assert_true(result.success)
    
    for query_result in result.query_results {
      // 性能基准：内存存储每秒至少读取100000个项目
      if result.backend.name == "memory" {
        assert_true(query_result.throughput >= 100000)
      }
      // 性能基准：文件存储每秒至少读取20000个项目
      else if result.backend.name == "file" {
        assert_true(query_result.throughput >= 20000)
      }
      // 性能基准：数据库存储每秒至少读取10000个项目
      else if result.backend.name == "database" {
        assert_true(query_result.throughput >= 10000)
      }
      
      // 延迟基准：95%分位数延迟应低于100ms
      assert_true(query_result.latency_p95 <= 100)
    }
  }
  
  // 比较存储后端性能
  let storage_comparison = StorageBenchmark::compare_backends(storage_benchmark, write_results, read_results)
  
  // 验证存储比较结果
  assert_eq(storage_comparison.write_performance.length(), 3)
  assert_eq(storage_comparison.read_performance.length(), 3)
  
  // 内存存储应该有最好的写入性能
  let best_write_backend = storage_comparison.write_performance[0]
  assert_eq(best_write_backend.name, "memory")
  
  // 内存存储应该有最好的读取性能
  let best_read_backend = storage_comparison.read_performance[0]
  assert_eq(best_read_backend.name, "memory")
}

// 测试4: 网络传输性能测试
test "网络传输性能测试" {
  // 创建网络性能测试器
  let network_benchmark = NetworkBenchmark::new()
  
  // 配置传输协议
  let transport_protocols = [
    {
      name: "http",
      type: "http_json",
      config: { compression: false, batch_size: 100 }
    },
    {
      name: "http_compressed",
      type: "http_json",
      config: { compression: true, compression_algorithm: "gzip", batch_size: 100 }
    },
    {
      name: "grpc",
      type: "grpc_protobuf",
      config: { compression: false, batch_size: 100 }
    },
    {
      name: "kafka",
      type: "kafka_json",
      config: { compression: true, compression_algorithm: "snappy", batch_size: 100 }
    }
  ]
  
  // 创建测试数据
  let test_data = TestDataGenerator::generate_mixed_data(10000, {
    span_ratio: 0.6,
    metric_ratio: 0.3,
    log_ratio: 0.1
  })
  
  // 测试不同协议的传输性能
  let transmission_results = []
  
  for protocol in transport_protocols {
    let transmission_benchmark = NetworkBenchmark::create_transmission_benchmark(network_benchmark, {
      protocol: protocol,
      data_sizes: [1000, 5000, 10000],
      network_conditions: ["ideal", "high_latency", "packet_loss"]
    })
    
    let result = NetworkBenchmark::run_transmission_benchmark(network_benchmark, transmission_benchmark, test_data)
    transmission_results = transmission_results.push(result)
  }
  
  // 验证传输性能结果
  for result in transmission_results {
    assert_true(result.success)
    
    for size_result in result.size_results {
      // 在理想网络条件下的性能基准
      let ideal_result = size_result.network_conditions.find(fn(c) { c.condition == "ideal" })
      assert_true(ideal_result != None)
      
      match ideal_result {
        Some(condition) => {
          // gRPC应该有最高的吞吐量
          if result.protocol.name == "grpc" {
            assert_true(condition.throughput >= 5000)
          }
          // HTTP应该有合理的吞吐量
          else if result.protocol.name == "http" {
            assert_true(condition.throughput >= 2000)
          }
          // 压缩HTTP应该有略低的吞吐量但更高的效率
          else if result.protocol.name == "http_compressed" {
            assert_true(condition.throughput >= 1500)
          }
          // Kafka应该有良好的吞吐量
          else if result.protocol.name == "kafka" {
            assert_true(condition.throughput >= 3000)
          }
        }
        None => assert_true(false)
      }
      
      // 验证高延迟条件下的性能下降
      let high_latency_result = size_result.network_conditions.find(fn(c) { c.condition == "high_latency" })
      assert_true(high_latency_result != None)
      
      match high_latency_result {
        Some(condition) => {
          // 高延迟应该降低吞吐量但不应该完全失败
          assert_true(condition.throughput > 0)
          
          // 延迟应该显著增加
          assert_true(condition.avg_latency > condition.avg_latency * 2)
        }
        None => assert_true(false)
      }
      
      // 验证丢包条件下的容错性
      let packet_loss_result = size_result.network_conditions.find(fn(c) { c.condition == "packet_loss" })
      assert_true(packet_loss_result != None)
      
      match packet_loss_result {
        Some(condition) => {
          // 丢包情况下应该仍然能够传输数据
          assert_true(condition.throughput > 0)
          
          // 成功率应该合理（不是100%但也不应该太低）
          assert_true(condition.success_rate >= 0.9)  // 至少90%成功率
        }
        None => assert_true(false)
      }
    }
  }
  
  // 比较传输协议效率
  let efficiency_comparison = NetworkBenchmark::compare_efficiency(network_benchmark, transmission_results)
  
  // 验证效率比较结果
  assert_eq(efficiency_comparison.length(), 4)
  
  // 压缩协议应该有更好的带宽效率
  let compressed_protocols = efficiency_comparison.filter(fn(p) { 
    p.protocol.name.contains("compressed") 
  })
  
  for protocol in compressed_protocols {
    assert_true(protocol.bandwidth_efficiency > 0.8)  // 高带宽效率
  }
  
  // gRPC应该有最好的延迟性能
  let grpc_protocol = efficiency_comparison.find(fn(p) { p.protocol.name == "grpc" })
  assert_true(grpc_protocol != None)
  
  match grpc_protocol {
    Some(protocol) => {
      assert_true(protocol.latency_efficiency > 0.8)  // 高延迟效率
    }
    None => assert_true(false)
  }
}

// 测试5: 内存使用性能测试
test "内存使用性能测试" {
  // 创建内存性能测试器
  let memory_benchmark = MemoryBenchmark::new()
  
  // 配置内存测试参数
  let memory_test_config = {
    data_sizes: [1000, 10000, 100000],
    operations: ["create", "process", "store", "query"],
    gc_pressure: ["low", "medium", "high"]
  }
  
  // 测试Span内存使用
  let span_memory_benchmark = MemoryBenchmark::create_span_memory_benchmark(memory_benchmark, {
    attribute_count: 5,
    event_count: 2,
    link_count: 1
  })
  
  let span_memory_results = MemoryBenchmark::run_memory_benchmark(memory_benchmark, span_memory_benchmark, memory_test_config)
  
  // 验证Span内存使用结果
  assert_true(span_memory_results.success)
  
  for size_result in span_memory_results.size_results {
    // 内存基准：每个Span不应超过2KB
    assert_true(size_result.memory_per_item <= 2048)
    
    // 内存增长应该是线性的
    assert_true(size_result.memory_growth_rate <= 1.1)  // 不超过10%的非线性增长
    
    // GC压力测试
    for gc_result in size_result.gc_results {
      // 高GC压力下内存使用应该稳定
      if gc_result.gc_pressure == "high" {
        assert_true(gc_result.memory_stability >= 0.8)  // 80%稳定性
      }
    }
  }
  
  // 测试Metric内存使用
  let metric_memory_benchmark = MemoryBenchmark::create_metric_memory_benchmark(memory_benchmark, {
    attribute_count: 3,
    histogram_buckets: 10
  })
  
  let metric_memory_results = MemoryBenchmark::run_memory_benchmark(memory_benchmark, metric_memory_benchmark, memory_test_config)
  
  // 验证Metric内存使用结果
  assert_true(metric_memory_results.success)
  
  for size_result in metric_memory_results.size_results {
    // 内存基准：每个Metric不应超过1KB
    assert_true(size_result.memory_per_item <= 1024)
    
    // 直方图Metric应该使用更多内存
    let histogram_memory = size_result.type_breakdown.find(fn(b) { b.type == "histogram" })
    assert_true(histogram_memory != None)
    
    match histogram_memory {
      Some(breakdown) => {
        assert_true(breakdown.memory_per_item > size_result.memory_per_item * 0.5)  // 直方图占大部分内存
      }
      None => assert_true(false)
    }
  }
  
  // 测试内存泄漏
  let memory_leak_benchmark = MemoryBenchmark::create_leak_detection_benchmark(memory_benchmark, {
    iterations: 10000,
    operations: ["create", "update", "delete"],
    cleanup_interval: 1000
  })
  
  let leak_result = MemoryBenchmark::run_leak_detection(memory_benchmark, memory_leak_benchmark)
  
  // 验证内存泄漏检测结果
  assert_true(leak_result.success)
  
  // 内存使用应该稳定，不应该持续增长
  assert_true(leak_result.memory_growth_rate <= 0.05)  // 不超过5%增长
  
  // 最终内存使用应该接近初始内存使用
  assert_true(leak_result.final_memory <= leak_result.initial_memory * 1.5)  // 不超过50%增长
  
  // 测试内存池性能
  let memory_pool_benchmark = MemoryBenchmark::create_pool_benchmark(memory_benchmark, {
    pool_sizes: [100, 1000, 10000],
    object_sizes: [64, 256, 1024],
    allocation_patterns: ["random", "burst", "steady"]
  })
  
  let pool_result = MemoryBenchmark::run_pool_benchmark(memory_benchmark, memory_pool_benchmark)
  
  // 验证内存池性能结果
  assert_true(pool_result.success)
  
  for pool_size_result in pool_result.size_results {
    // 内存池应该比直接分配更快
    assert_true(pool_size_result.pool_allocation_time < pool_size_result.direct_allocation_time)
    
    // 内存池应该减少GC压力
    assert_true(pool_size_result.pool_gc_pressure < pool_size_result.direct_gc_pressure)
    
    // 内存池利用率应该合理
    assert_true(pool_size_result.utilization_rate >= 0.7)  // 至少70%利用率
  }
  
  // 生成内存使用报告
  let memory_report = MemoryBenchmark::generate_memory_report(memory_benchmark, [
    span_memory_results,
    metric_memory_results,
    leak_result,
    pool_result
  ])
  
  // 验证内存报告
  assert_true(memory_report.recommendations.length() > 0)
  
  // 检查是否有内存优化建议
  let optimization_suggestions = memory_report.recommendations.filter(fn(r) { 
    r.category == "optimization" 
  })
  assert_true(optimization_suggestions.length() > 0)
  
  // 检查是否有内存警告
  let warnings = memory_report.recommendations.filter(fn(r) { 
    r.category == "warning" 
  })
  // 可能没有警告，这是正常的
}

// 测试6: 并发性能测试
test "并发性能测试" {
  // 创建并发性能测试器
  let concurrency_benchmark = ConcurrencyBenchmark::new()
  
  // 配置并发测试参数
  let concurrency_config = {
    thread_counts: [1, 2, 4, 8, 16, 32],
    operations_per_thread: 1000,
    operations: ["create", "read", "update", "delete"],
    data_contention_levels: ["low", "medium", "high"]
  }
  
  // 测试并发数据生成
  let concurrent_generation_benchmark = ConcurrencyBenchmark::create_generation_benchmark(concurrency_benchmark, {
    services: 10,
    operations: 50,
    attributes_per_item: 5
  })
  
  let generation_results = ConcurrencyBenchmark::run_concurrency_benchmark(concurrency_benchmark, concurrent_generation_benchmark, concurrency_config)
  
  // 验证并发生成性能结果
  assert_true(generation_results.success)
  
  // 验证可扩展性
  for thread_result in generation_results.thread_results {
    // 吞吐量应该随着线程数增加而增加（至少到某个点）
    if thread_result.thread_count <= 8 {
      assert_true(thread_result.throughput >= generation_results.thread_results[0].throughput * thread_result.thread_count.to_float() * 0.7)
    }
  }
  
  // 查找最佳线程数
  let best_thread_result = generation_results.thread_results.reduce(fn(best, current) { 
    if current.throughput > best.throughput { current } else { best } 
  }, generation_results.thread_results[0])
  
  // 最佳性能不应该在最低或最高线程数
  assert_true(best_thread_result.thread_count > 1)
  assert_true(best_thread_result.thread_count < 32)
  
  // 测试并发数据处理
  let concurrent_processing_benchmark = ConcurrencyBenchmark::create_processing_benchmark(concurrency_benchmark, {
    operations: ["filter", "aggregate", "transform"],
    data_size: 10000
  })
  
  let processing_results = ConcurrencyBenchmark::run_concurrency_benchmark(concurrency_benchmark, concurrent_processing_benchmark, concurrency_config)
  
  // 验证并发处理性能结果
  assert_true(processing_results.success)
  
  // 验证不同操作的可扩展性
  for operation_result in processing_results.operation_results {
    // 过滤操作应该有良好的可扩展性
    if operation_result.operation == "filter" {
      assert_true(operation_result.scalability_factor >= 0.7)  // 至少70%的线性可扩展性
    }
    
    // 聚合操作可能有锁竞争，可扩展性较低
    if operation_result.operation == "aggregate" {
      assert_true(operation_result.scalability_factor >= 0.4)  // 至少40%的线性可扩展性
    }
  }
  
  // 测试并发存储访问
  let concurrent_storage_benchmark = ConcurrencyBenchmark::create_storage_benchmark(concurrency_benchmark, {
    storage_type: "in_memory",
    operations: ["write", "read", "query"],
    data_size: 5000
  })
  
  let storage_results = ConcurrencyBenchmark::run_concurrency_benchmark(concurrency_benchmark, concurrent_storage_benchmark, concurrency_config)
  
  // 验证并发存储性能结果
  assert_true(storage_results.success)
  
  // 验证读写操作的性能差异
  for thread_result in storage_results.thread_results {
    let read_throughput = thread_result.operation_throughputs.get("read")
    let write_throughput = thread_result.operation_throughputs.get("write")
    
    assert_true(read_throughput != None)
    assert_true(write_throughput != None)
    
    // 读操作通常比写操作快
    assert_true(read_throughput > write_throughput)
  }
  
  // 测试锁竞争
  let contention_benchmark = ConcurrencyBenchmark::create_contention_benchmark(concurrency_benchmark, {
    shared_resources: 10,
    critical_section_duration: 1,  // 1ms
    access_pattern: "random"
  })
  
  let contention_results = ConcurrencyBenchmark::run_contention_benchmark(concurrency_benchmark, contention_benchmark, {
    thread_counts: [1, 2, 4, 8, 16]
  })
  
  // 验证锁竞争结果
  assert_true(contention_results.success)
  
  // 验证锁竞争指标
  for thread_result in contention_results.thread_results {
    // 随着线程数增加，锁等待时间应该增加
    if thread_result.thread_count > 1 {
      assert_true(thread_result.avg_lock_wait_time > contention_results.thread_results[0].avg_lock_wait_time)
    }
    
    // 锁竞争不应该导致死锁
    assert_eq(thread_result.deadlock_count, 0)
    
    // 吞吐量下降应该在合理范围内
    if thread_result.thread_count == 16 {
      assert_true(thread_result.throughput >= contention_results.thread_results[1].throughput * 0.3)  // 至少30%的双线程性能
    }
  }
  
  // 生成并发性能报告
  let concurrency_report = ConcurrencyBenchmark::generate_concurrency_report(concurrency_benchmark, [
    generation_results,
    processing_results,
    storage_results,
    contention_results
  ])
  
  // 验证并发性能报告
  assert_true(concurrency_report.recommendations.length() > 0)
  
  // 检查是否有并发优化建议
  let concurrency_suggestions = concurrency_report.recommendations.filter(fn(r) { 
    r.category == "concurrency" 
  })
  assert_true(concurrency_suggestions.length() > 0)
  
  // 检查最佳实践建议
  let best_practices = concurrency_report.recommendations.filter(fn(r) { 
    r.category == "best_practice" 
  })
  assert_true(best_practices.length() > 0)
}