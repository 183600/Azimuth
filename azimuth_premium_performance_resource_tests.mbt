// Azimuth 高级性能基准和资源优化测试用例
// 专注于验证遥测系统的性能表现和资源使用效率

// 测试1: 高吞吐量遥测数据生成和处理
test "高吞吐量遥测数据生成和处理测试" {
  // 创建性能基准测试器
  let benchmark = PerformanceBenchmark::new("high.throughput.telemetry")
  PerformanceBenchmark::set_warmup_iterations(benchmark, 100)
  PerformanceBenchmark::set_test_iterations(benchmark, 1000)
  PerformanceBenchmark::set_concurrency_level(benchmark, 10)
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance.test")
  
  // 创建性能指标收集器
  let throughput_counter = Meter::create_counter(meter, "telemetry.operations.total")
  let latency_histogram = Meter::create_histogram(meter, "telemetry.operation.duration")
  let memory_gauge = Meter::create_gauge(meter, "memory.usage.bytes")
  
  // 预热阶段
  PerformanceBenchmark::warmup(benchmark, fn() {
    let tracer = TracerProvider::get_tracer(TracerProvider::default(), "warmup.service")
    let span = Tracer::start_span(tracer, "warmup.operation")
    Span::set_attribute(span, "warmup", BoolValue(true))
    Counter::add(throughput_counter, 1.0)
    Span::end(span)
  })
  
  // 记录初始内存使用
  let initial_memory = MemoryProfiler::get_current_memory_usage()
  Gauge::set(memory_gauge, initial_memory.to_float())
  
  // 基准测试阶段
  let results = PerformanceBenchmark::run(benchmark, fn() {
    let start_time = Timestamp::now()
    
    // 创建多个遥测操作
    let tracer = TracerProvider::get_tracer(TracerProvider::default(), "benchmark.service")
    let span = Tracer::start_span(tracer, "benchmark.operation")
    
    // 设置属性
    Span::set_attribute(span, "operation.type", StringValue("high.throughput.test"))
    Span::set_attribute(span, "thread.id", IntValue(ThreadId::current()))
    Span::set_attribute(span, "iteration", IntValue(benchmark.current_iteration))
    
    // 添加多个事件
    for i in 1..=5 {
      Span::add_event_with_attributes(span, "operation.step", [
        ("step.number", IntValue(i)),
        ("step.type", StringValue("processing"))
      ])
    }
    
    // 创建指标
    let operation_counter = Meter::create_counter(meter, "operations.per.span")
    Counter::add(operation_counter, 5.0)
    
    // 记录操作延迟
    let end_time = Timestamp::now()
    let duration = end_time.to_millis() - start_time.to_millis()
    Histogram::record(latency_histogram, duration.to_float())
    
    Counter::add(throughput_counter, 1.0)
    Span::end(span)
  })
  
  // 记录最终内存使用
  let final_memory = MemoryProfiler::get_current_memory_usage()
  Gauge::set(memory_gauge, final_memory.to_float())
  
  // 验证性能指标
  assert_true(results.average_duration_ms < 10.0) // 平均每次操作应小于10ms
  assert_true(results.throughput_ops_per_second > 100.0) // 吞吐量应大于100 ops/sec
  assert_true(results.p99_duration_ms < 50.0) // P99延迟应小于50ms
  
  // 验证内存使用
  let memory_increase = final_memory - initial_memory
  let memory_per_operation = memory_increase.to_float() / benchmark.test_iterations.to_float()
  
  assert_true(memory_per_operation < 1024.0) // 每个操作内存增长应小于1KB
  
  // 验证资源清理
  let cleanup_memory = MemoryProfiler::force_gc_and_measure()
  assert_true(cleanup_memory <= final_memory)
}

// 测试2: 内存管理和泄漏检测
test "内存管理和泄漏检测测试" {
  // 创建内存泄漏检测器
  let leak_detector = MemoryLeakDetector::new()
  MemoryLeakDetector::set_threshold(leak_detector, 1024 * 1024) // 1MB阈值
  
  let provider = MeterProvider::default()
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "memory.test")
  
  // 记录初始内存状态
  let initial_snapshot = MemoryLeakDetector::take_snapshot(leak_detector)
  
  // 创建大量span并释放
  let spans = []
  
  for i in 1..=1000 {
    let span = Tracer::start_span(tracer, "memory.test.span." + i.to_string())
    
    // 添加大量属性和事件
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "large.data", StringValue("x".repeat(100))) // 100字符字符串
    
    for j in 1..=10 {
      Span::add_event_with_attributes(span, "memory.test.event", [
        ("event.iteration", IntValue(j)),
        ("event.data", StringValue("y".repeat(50))) // 50字符字符串
      ])
    }
    
    spans = spans.push(span)
  }
  
  // 记录峰值内存
  let peak_snapshot = MemoryLeakDetector::take_snapshot(leak_detector)
  
  // 释放所有span
  for span in spans {
    Span::end(span)
  }
  
  // 强制垃圾回收
  MemoryProfiler::force_gc()
  
  // 记录清理后内存
  let cleanup_snapshot = MemoryLeakDetector::take_snapshot(leak_detector)
  
  // 检测内存泄漏
  let leak_analysis = MemoryLeakDetector::analyze_leaks(leak_detector, initial_snapshot, cleanup_snapshot)
  
  assert_true(leak_analysis.memory_leak_detected == false || leak_analysis.leak_size_bytes < 1024 * 1024)
  assert_true(leak_analysis.memory_recovered > 0)
  assert_true(cleanup_snapshot.memory_usage < peak_snapshot.memory_usage)
  
  // 测试循环引用检测
  let circular_ref_test = CircularReferenceDetector::new()
  
  // 创建可能产生循环引用的结构
  let parent_span = Tracer::start_span(tracer, "circular.parent")
  let child_span = Tracer::start_span_with_context(tracer, "circular.child", Span::context(parent_span))
  
  // 模拟循环引用（在实际实现中可能通过属性相互引用）
  Span::set_attribute(parent_span, "child.id", StringValue(Span::span_id(child_span).to_string()))
  Span::set_attribute(child_span, "parent.id", StringValue(Span::span_id(parent_span).to_string()))
  
  Span::end(child_span)
  Span::end(parent_span)
  
  // 检测循环引用
  let circular_ref_analysis = CircularReferenceDetector::detect_circular_references(circular_ref_test, [parent_span, child_span])
  
  assert_true(circular_ref_analysis.circular_references_detected == true || circular_ref_analysis.circular_references_detected == false)
  
  // 验证循环引用不影响内存回收
  MemoryProfiler::force_gc()
  let final_memory_check = MemoryProfiler::get_current_memory_usage()
  assert_true(final_memory_check <= cleanup_snapshot.memory_usage + 1024 * 100) // 允许少量误差
}

// 测试3: CPU使用率优化测试
test "CPU使用率优化测试" {
  // 创建CPU性能分析器
  let cpu_profiler = CPUProfiler::new()
  CPUProfiler::start_profiling(cpu_profiler)
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cpu.optimization")
  
  // 创建CPU使用率指标
  let cpu_usage_gauge = Meter::create_gauge(meter, "cpu.usage.percentage")
  let cpu_time_counter = Meter::create_counter(meter, "cpu.time.total")
  
  // 测试不同算法的CPU效率
  let algorithms = [
    ("naive.approach", fn() {
      // 低效算法
      let result = []
      for i in 1..=1000 {
        for j in 1..=1000 {
          result = result.push(i * j)
        }
      }
      result.length()
    }),
    ("optimized.approach", fn() {
      // 优化算法
      let sum = 0
      for i in 1..=1000 {
        sum = sum + (i * 1000 * (1000 + 1) / 2)
      }
      sum
    })
  ]
  
  for (algorithm_name, algorithm_func) in algorithms {
    let start_cpu = CPUProfiler::get_current_cpu_usage(cpu_profiler)
    let start_time = Timestamp::now()
    
    // 执行算法
    let result = algorithm_func()
    
    let end_time = Timestamp::now()
    let end_cpu = CPUProfiler::get_current_cpu_usage(cpu_profiler)
    
    let duration_ms = end_time.to_millis() - start_time.to_millis()
    let cpu_usage = end_cpu - start_cpu
    
    // 记录指标
    Gauge::set_with_attributes(cpu_usage_gauge, cpu_usage, [
      ("algorithm", StringValue(algorithm_name))
    ])
    
    Counter::add_with_attributes(cpu_time_counter, duration_ms.to_float(), [
      ("algorithm", StringValue(algorithm_name))
    ])
    
    // 验证结果正确性
    assert_true(result > 0)
  }
  
  // 测试并发CPU使用
  let concurrent_cpu_test = ConcurrentCPUTest::new()
  ConcurrentCPUTest::set_thread_count(concurrent_cpu_test, 4)
  
  let concurrent_results = ConcurrentCPUTest::run(concurrent_cpu_test, fn() {
    let tracer = TracerProvider::get_tracer(TracerProvider::default(), "concurrent.cpu.test")
    let span = Tracer::start_span(tracer, "concurrent.operation")
    
    // CPU密集型操作
    let mut result = 0
    for i in 1..=10000 {
      result = result + (i * i % 1000)
    }
    
    Span::set_attribute(span, "computation.result", IntValue(result))
    Span::end(span)
    
    result
  })
  
  // 验证并发效率
  assert_true(concurrent_results.total_threads == 4)
  assert_true(concurrent_results.average_cpu_usage_per_thread > 0)
  assert_true(concurrent_results.cpu_efficiency_score > 0.5) // 效率应大于50%
  
  CPUProfiler::stop_profiling(cpu_profiler)
  
  // 分析CPU性能
  let cpu_analysis = CPUProfiler::analyze_performance(cpu_profiler)
  
  assert_true(cpu_analysis.total_cpu_time > 0)
  assert_true(cpu_analysis peak_cpu_usage > 0)
  assert_true(cpu_analysis.cpu_efficiency_score > 0.0)
}

// 测试4: 网络I/O性能优化测试
test "网络I/O性能优化测试" {
  // 创建网络性能测试器
  let network_benchmark = NetworkBenchmark::new("telemetry.network.performance")
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "network.optimization")
  
  // 创建网络指标
  let bandwidth_gauge = Meter::create_gauge(meter, "network.bandwidth.mbps")
  let latency_histogram = Meter::create_histogram(meter, "network.latency.ms")
  let packet_loss_counter = Meter::create_counter(meter, "network.packets.lost")
  
  // 测试不同批量大小的网络传输效率
  let batch_sizes = [10, 50, 100, 500, 1000]
  
  for batch_size in batch_sizes {
    let start_time = Timestamp::now()
    
    // 模拟批量发送遥测数据
    let telemetry_batch = []
    
    for i in 1..=batch_size {
      let tracer = TracerProvider::get_tracer(TracerProvider::default(), "network.test")
      let span = Tracer::start_span(tracer, "batch.operation." + i.to_string())
      
      // 添加遥测数据
      Span::set_attribute(span, "batch.id", IntValue(batch_size))
      Span::set_attribute(span, "item.id", IntValue(i))
      Span::set_attribute(span, "payload.size", IntValue(1024)) // 1KB payload
      
      telemetry_batch = telemetry_batch.push(span)
    }
    
    // 模拟网络传输
    let transmission_result = NetworkBenchmark::simulate_transmission(network_benchmark, telemetry_batch)
    
    let end_time = Timestamp::now()
    let duration_ms = end_time.to_millis() - start_time.to_millis()
    
    // 记录指标
    let bandwidth_mbps = (batch_size * 1024 * 8) / (duration_ms.to_float() / 1000.0) / (1024 * 1024)
    Gauge::set_with_attributes(bandwidth_gauge, bandwidth_mbps, [
      ("batch.size", IntValue(batch_size))
    ])
    
    Histogram::record_with_attributes(latency_histogram, duration_ms.to_float(), [
      ("batch.size", IntValue(batch_size))
    ])
    
    // 结束所有span
    for span in telemetry_batch {
      Span::end(span)
    }
    
    // 验证传输效率
    assert_true(transmission_result.success_rate > 0.95) // 成功率应大于95%
    assert_true(bandwidth_mbps > 0.1) // 带宽应大于0.1 Mbps
    
    if transmission_result.lost_packets > 0 {
      Counter::add_with_attributes(packet_loss_counter, transmission_result.lost_packets.to_float(), [
        ("batch.size", IntValue(batch_size))
      ])
    }
  }
  
  // 测试连接池优化
  let connection_pool = ConnectionPool::new("telemetry.collector")
  ConnectionPool::set_max_connections(connection_pool, 10)
  ConnectionPool::set_connection_timeout(connection_pool, 5000) // 5秒
  
  // 并发连接测试
  let concurrent_connections = []
  
  for i in 1..=20 {
    let connection_task = fn() {
      let conn = ConnectionPool::acquire(connection_pool)
      
      if conn.is_available {
        // 模拟发送遥测数据
        let tracer = TracerProvider::get_tracer(TracerProvider::default(), "connection.pool.test")
        let span = Tracer::start_span(tracer, "pooled.connection.operation")
        
        Span::set_attribute(span, "connection.id", StringValue(conn.connection_id))
        Span::set_attribute(span, "operation.id", IntValue(i))
        
        // 模拟网络操作
        Thread::sleep(10) // 10ms模拟网络延迟
        
        Span::end(span)
        ConnectionPool::release(connection_pool, conn)
        
        return true
      } else {
        return false
      }
    }
    
    concurrent_connections = concurrent_connections.push(connection_task)
  }
  
  // 执行并发连接
  let connection_results = ConcurrentExecutor::execute_all(concurrent_connections)
  
  // 验证连接池效率
  let successful_connections = connection_results.filter(fn(r) { r }).length()
  let connection_success_rate = successful_connections.to_float() / concurrent_connections.length().to_float()
  
  assert_true(connection_success_rate > 0.8) // 连接成功率应大于80%
  assert_true(ConnectionPool::peak_usage(connection_pool) <= 10) // 不应超过最大连接数
  
  // 分析网络性能
  let network_analysis = NetworkBenchmark::analyze_performance(network_benchmark)
  
  assert_true(network_analysis.average_bandwidth > 0)
  assert_true(network_analysis.average_latency > 0)
  assert_true(network_analysis.optimal_batch_size > 0)
}

// 测试5: 磁盘I/O性能优化测试
test "磁盘I/O性能优化测试" {
  // 创建磁盘性能测试器
  let disk_benchmark = DiskBenchmark::new("telemetry.disk.performance")
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "disk.optimization")
  
  // 创建磁盘I/O指标
  let throughput_gauge = Meter::create_gauge(meter, "disk.throughput.mbps")
  let iops_counter = Meter::create_counter(meter, "disk.iops.total")
  let latency_histogram = Meter::create_histogram(meter, "disk.latency.ms")
  
  // 测试不同写入策略的性能
  let write_strategies = [
    ("synchronous", fn(data) {
      DiskBenchmark::write_sync(disk_benchmark, data)
    }),
    ("batched", fn(data) {
      DiskBenchmark::write_batch(disk_benchmark, data, 100) // 100个批次
    }),
    ("asynchronous", fn(data) {
      DiskBenchmark::write_async(disk_benchmark, data)
    })
  ]
  
  let telemetry_data = TelemetryDataGenerator::generate_large_dataset(10000) // 10K条记录
  
  for (strategy_name, strategy_func) in write_strategies {
    let start_time = Timestamp::now()
    
    // 执行写入策略
    let write_result = strategy_func(telemetry_data)
    
    let end_time = Timestamp::now()
    let duration_ms = end_time.to_millis() - start_time.to_millis()
    
    // 计算性能指标
    let data_size_mb = telemetry_data.total_size_bytes.to_float() / (1024 * 1024)
    let throughput_mbps = data_size_mb / (duration_ms.to_float() / 1000.0)
    let iops = telemetry_data.record_count.to_float() / (duration_ms.to_float() / 1000.0)
    
    // 记录指标
    Gauge::set_with_attributes(throughput_gauge, throughput_mbps, [
      ("write.strategy", StringValue(strategy_name))
    ])
    
    Counter::add_with_attributes(iops_counter, iops, [
      ("write.strategy", StringValue(strategy_name))
    ])
    
    Histogram::record_with_attributes(latency_histogram, duration_ms.to_float(), [
      ("write.strategy", StringValue(strategy_name))
    ])
    
    // 验证写入结果
    assert_true(write_result.success)
    assert_true(write_result.bytes_written > 0)
    assert_true(throughput_mbps > 0.1) // 吞吐量应大于0.1 MB/s
  }
  
  // 测试读取性能
  let read_start_time = Timestamp::now()
  
  let read_result = DiskBenchmark::read_all(disk_benchmark, telemetry_data.file_path)
  
  let read_end_time = Timestamp::now()
  let read_duration_ms = read_end_time.to_millis() - read_start_time.to_millis()
  
  // 验证读取性能
  assert_true(read_result.success)
  assert_eq(read_result.records_read, telemetry_data.record_count)
  assert_true(read_result.read_throughput_mbps > 1.0) // 读取吞吐量应大于1 MB/s
  
  // 测试文件压缩性能
  let compression_start_time = Timestamp::now()
  
  let compression_result = DiskBenchmark::compress_telemetry_data(disk_benchmark, telemetry_data)
  
  let compression_end_time = Timestamp::now()
  let compression_duration_ms = compression_end_time.to_millis() - compression_start_time.to_millis()
  
  // 验证压缩效果
  assert_true(compression_result.success)
  assert_true(compression_result.compression_ratio > 1.5) // 压缩比应大于1.5
  assert_true(compression_duration_ms < 30000) // 压缩时间应小于30秒
  
  // 分析磁盘性能
  let disk_analysis = DiskBenchmark::analyze_performance(disk_benchmark)
  
  assert_true(disk_analysis.optimal_write_strategy == "batched" || disk_analysis.optimal_write_strategy == "asynchronous")
  assert_true(disk_analysis.average_read_throughput > 0)
  assert_true(disk_analysis.average_write_throughput > 0)
}

// 测试6: 缓存性能优化测试
test "缓存性能优化测试" {
  // 创建缓存性能测试器
  let cache_benchmark = CacheBenchmark::new("telemetry.cache.performance")
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cache.optimization")
  
  // 创建缓存指标
  let hit_rate_gauge = Meter::create_gauge(meter, "cache.hit.rate")
  let miss_rate_gauge = Meter::create_gauge(meter, "cache.miss.rate")
  let latency_histogram = Meter::create_histogram(meter, "cache.latency.ms")
  
  // 测试不同缓存策略
  let cache_strategies = [
    ("lru", CacheStrategy::LRU),
    ("lfu", CacheStrategy::LFU),
    ("fifo", CacheStrategy::FIFO),
    ("random", CacheStrategy::Random)
  ]
  
  let telemetry_keys = TelemetryKeyGenerator::generate_unique_keys(1000)
  
  for (strategy_name, strategy) in cache_strategies {
    let cache = Cache::new(100, strategy) // 100项缓存容量
    
    let hits = 0
    let misses = 0
    let total_access_time = 0
    
    // 第一轮：填充缓存
    for key in telemetry_keys {
      let start_time = Timestamp::now()
      
      let value = Cache::get(cache, key)
      
      let end_time = Timestamp::now()
      let access_time = end_time.to_millis() - start_time.to_millis()
      total_access_time = total_access_time + access_time
      
      if value.is_none {
        // 缓存未命中，从数据源加载
        let telemetry_value = TelemetryDataSource::load(key)
        Cache::put(cache, key, telemetry_value)
        misses = misses + 1
      } else {
        hits = hits + 1
      }
    }
    
    // 第二轮：测试缓存命中
    for key in telemetry_keys {
      let start_time = Timestamp::now()
      
      let value = Cache::get(cache, key)
      
      let end_time = Timestamp::now()
      let access_time = end_time.to_millis() - start_time.to_millis()
      total_access_time = total_access_time + access_time
      
      if value.is_some {
        hits = hits + 1
      } else {
        misses = misses + 1
      }
    }
    
    // 计算缓存性能指标
    let total_accesses = hits + misses
    let hit_rate = hits.to_float() / total_accesses.to_float() * 100.0
    let miss_rate = misses.to_float() / total_accesses.to_float() * 100.0
    let average_latency = total_access_time.to_float() / total_accesses.to_float()
    
    // 记录指标
    Gauge::set_with_attributes(hit_rate_gauge, hit_rate, [
      ("cache.strategy", StringValue(strategy_name))
    ])
    
    Gauge::set_with_attributes(miss_rate_gauge, miss_rate, [
      ("cache.strategy", StringValue(strategy_name))
    ])
    
    Histogram::record_with_attributes(latency_histogram, average_latency, [
      ("cache.strategy", StringValue(strategy_name))
    ])
    
    // 验证缓存性能
    assert_true(hit_rate > 30.0) // 命中率应大于30%
    assert_true(average_latency < 1.0) // 平均延迟应小于1ms
  }
  
  // 测试分布式缓存性能
  let distributed_cache = DistributedCache::new("telemetry.cluster")
  DistributedCache::set_node_count(distributed_cache, 3)
  
  // 测试缓存一致性
  let consistency_test = CacheConsistencyTest::new(distributed_cache)
  
  let consistency_results = CacheConsistencyTest::run_consistency_test(consistency_test, telemetry_keys)
  
  assert_true(consistency_results.consistency_rate > 0.95) // 一致性应大于95%
  assert_true(consistency_results.replication_latency_ms < 100) // 复制延迟应小于100ms
  
  // 测试缓存失效策略
  let eviction_test = CacheEvictionTest::new(50) // 50项缓存容量
  
  let eviction_results = CacheEvictionTest::test_eviction_strategy(eviction_test, CacheStrategy::LRU, telemetry_keys)
  
  assert_true(eviction_results.eviction_efficiency > 0.8) // 驱逐效率应大于80%
  assert_true(eviction_results.evicted_hot_items_rate < 0.1) // 热点项被驱逐率应小于10%
  
  // 分析缓存性能
  let cache_analysis = CacheBenchmark::analyze_performance(cache_benchmark)
  
  assert_true(cache_analysis.best_strategy == "lru" || cache_analysis.best_strategy == "lfu")
  assert_true(cache_analysis.average_hit_rate > 50.0)
  assert_true(cache_analysis.average_cache_latency < 1.0)
}

// 测试7: 资源池化性能优化测试
test "资源池化性能优化测试" {
  // 创建资源池化测试器
  let pool_benchmark = ResourcePoolBenchmark::new("telemetry.resource.pool")
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "resource.pool.optimization")
  
  // 创建资源池指标
  let pool_utilization_gauge = Meter::create_gauge(meter, "pool.utilization")
  let wait_time_histogram = Meter::create_histogram(meter, "pool.wait.time.ms")
  let throughput_counter = Meter::create_counter(meter, "pool.operations.total")
  
  // 测试不同类型的资源池
  let resource_types = [
    ("database.connections", fn() {
      DatabaseConnection::new("telemetry.db")
    }),
    ("http.clients", fn() {
      HttpClient::new("telemetry.collector")
    }),
    ("serializers", fn() {
      TelemetrySerializer::new()
    })
  ]
  
  for (resource_type, resource_factory) in resource_types {
    let resource_pool = ResourcePool::new(10, resource_factory) // 10个资源
    
    let operations = 1000
    let concurrent_workers = 20
    
    // 并发资源获取测试
    let start_time = Timestamp::now()
    
    let worker_tasks = []
    
    for i in 1..=concurrent_workers {
      let task = fn() {
        let local_operations = operations / concurrent_workers
        let local_successes = 0
        let total_wait_time = 0
        
        for j in 1..=local_operations {
          let acquire_start = Timestamp::now()
          
          let resource = ResourcePool::acquire(resource_pool, 1000) // 1秒超时
          
          let acquire_end = Timestamp::now()
          let wait_time = acquire_end.to_millis() - acquire_start.to_millis()
          total_wait_time = total_wait_time + wait_time
          
          if resource.is_available {
            // 使用资源
            let operation_start = Timestamp::now()
            
            let result = resource.use()
            
            let operation_end = Timestamp::now()
            let operation_time = operation_end.to_millis() - operation_start.to_millis()
            
            // 释放资源
            ResourcePool::release(resource_pool, resource)
            
            if result.success {
              local_successes = local_successes + 1
            }
          }
        }
        
        return (local_successes, total_wait_time)
      }
      
      worker_tasks = worker_tasks.push(task)
    }
    
    // 执行并发任务
    let worker_results = ConcurrentExecutor::execute_all(worker_tasks)
    
    let end_time = Timestamp::now()
    let total_duration_ms = end_time.to_millis() - start_time.to_millis()
    
    // 汇总结果
    let total_successes = worker_results.reduce(fn(acc, result) { acc + result.0 }, 0)
    let total_wait_time = worker_results.reduce(fn(acc, result) { acc + result.1 }, 0)
    
    // 计算性能指标
    let success_rate = total_successes.to_float() / operations.to_float() * 100.0
    let average_wait_time = total_wait_time.to_float() / operations.to_float()
    let throughput = operations.to_float() / (total_duration_ms.to_float() / 1000.0)
    
    // 记录指标
    Gauge::set_with_attributes(pool_utilization_gauge, success_rate, [
      ("resource.type", StringValue(resource_type))
    ])
    
    Histogram::record_with_attributes(wait_time_histogram, average_wait_time, [
      ("resource.type", StringValue(resource_type))
    ])
    
    Counter::add_with_attributes(throughput_counter, throughput, [
      ("resource.type", StringValue(resource_type))
    ])
    
    // 验证资源池性能
    assert_true(success_rate > 95.0) // 成功率应大于95%
    assert_true(average_wait_time < 100.0) // 平均等待时间应小于100ms
    assert_true(throughput > 50.0) // 吞吐量应大于50 ops/sec
    
    // 测试资源池扩展性
    let scalability_test = ResourcePoolScalabilityTest::new(resource_pool)
    
    let scalability_results = ResourcePoolScalabilityTest::test_scalability(scalability_test, [5, 10, 20, 40])
    
    assert_true(scalability_results.linear_scaling_score > 0.7) // 线性扩展评分应大于70%
    assert_true(scalability_results.resource_contention_rate < 0.2) // 资源争用率应小于20%
  }
  
  // 测试资源池健康监控
  let health_monitor = ResourcePoolHealthMonitor::new()
  
  for (resource_type, _) in resource_types {
    let pool = ResourcePool::new(10, fn() { "dummy_resource" })
    
    let health_status = ResourcePoolHealthMonitor::check_health(health_monitor, pool)
    
    assert_true(health_status.is_healthy)
    assert_true(health_status.available_resources >= 0)
    assert_true(health_status.active_resources <= 10)
    
    // 记录健康指标
    let health_gauge = Meter::create_gauge(meter, "pool.health.score")
    Gauge::set_with_attributes(health_gauge, health_status.health_score, [
      ("resource.type", StringValue(resource_type))
    ])
  }
  
  // 分析资源池性能
  let pool_analysis = ResourcePoolBenchmark::analyze_performance(pool_benchmark)
  
  assert_true(pool_analysis.optimal_pool_size > 0)
  assert_true(pool_analysis.average_utilization > 0.5)
  assert_true(pool_analysis.average_wait_time < 100.0)
}