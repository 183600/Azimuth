// Azimuth 性能基准综合测试用例
// 专注于遥测系统的性能基准测试，包括延迟、吞吐量、资源使用等关键指标

// 测试1: Span创建性能基准
test "Span创建性能基准测试" {
  // 创建性能基准测试器
  let benchmark = PerformanceBenchmark::new("span_creation")
  
  // 设置测试参数
  let test_configs = [
    { name: "小规模", iterations: 1000, attributes: 5 },
    { name: "中规模", iterations: 10000, attributes: 10 },
    { name: "大规模", iterations: 100000, attributes: 20 }
  ]
  
  let results = []
  for config in test_configs {
    // 预热
    for i in 0..=100 {
      let _span = Span::new("warmup.operation", Server, 
                           TraceContext::new("warmup-trace", "warmup-span", true, ""))
    }
    
    // 基准测试
    let start_time = Time::now()
    let start_memory = MemoryManager::get_usage()
    
    for i in 0..=config.iterations {
      let span = Span::new("benchmark.operation", Server, 
                          TraceContext::new("benchmark-trace-" + i.to_string(), 
                                          "benchmark-span-" + i.to_string(), true, ""))
      
      // 添加属性
      for j in 0..=config.attributes {
        Span::set_attribute(span, "attr." + j.to_string(), StringValue("value." + j.to_string()))
      }
    }
    
    let end_time = Time::now()
    let end_memory = MemoryManager::get_usage()
    
    let duration = end_time - start_time
    let memory_delta = end_memory - start_memory
    let throughput = config.iterations.to_float() / duration.to_float() * 1000.0
    
    let result = {
      config_name: config.name,
      iterations: config.iterations,
      duration: duration,
      throughput: throughput,
      memory_delta: memory_delta,
      avg_latency: duration.to_float() / config.iterations.to_float()
    }
    
    results = results.push(result)
  }
  
  // 验证性能基准
  for result in results {
    // 验证吞吐量
    assert_true(result.throughput > 1000)  // 至少1000 spans/second
    
    // 验证平均延迟
    assert_true(result.avg_latency < 1.0)  // 平均延迟小于1ms
    
    // 验证内存使用
    assert_true(result.memory_delta < 100 * 1024 * 1024)  // 内存增长小于100MB
  }
  
  // 验证扩展性
  assert_true(results[1].throughput >= results[0].throughput * 0.8)  // 中规模吞吐量不低于小规模的80%
  assert_true(results[2].throughput >= results[1].throughput * 0.8)  // 大规模吞吐量不低于中规模的80%
}

// 测试2: 追踪上下文传播性能
test "追踪上下文传播性能测试" {
  let benchmark = PerformanceBenchmark::new("context_propagation")
  
  // 创建传播器
  let propagator = W3CTraceContextPropagator::new()
  let base_context = TraceContext::new("perf-trace", "perf-span", true, "key1=value1,key2=value2")
  
  // 测试注入性能
  let injection_iterations = 10000
  let injection_start = Time::now()
  
  for i in 0..=injection_iterations {
    let carrier = TextMapCarrier::new()
    DistributedTracer::inject_context(propagator, base_context, carrier)
  }
  
  let injection_end = Time::now()
  let injection_duration = injection_end - injection_start
  let injection_throughput = injection_iterations.to_float() / injection_duration.to_float() * 1000.0
  
  // 验证注入性能
  assert_true(injection_throughput > 50000)  // 至少50000次注入/秒
  
  // 测试提取性能
  let carrier = TextMapCarrier::new()
  DistributedTracer::inject_context(propagator, base_context, carrier)
  
  let extraction_iterations = 10000
  let extraction_start = Time::now()
  
  for i in 0..=extraction_iterations {
    let _extracted = DistributedTracer::extract_context(propagator, carrier)
  }
  
  let extraction_end = Time::now()
  let extraction_duration = extraction_end - extraction_start
  let extraction_throughput = extraction_iterations.to_float() / extraction_duration.to_float() * 1000.0
  
  // 验证提取性能
  assert_true(extraction_throughput > 50000)  // 至少50000次提取/秒
  
  // 测试往返性能
  let roundtrip_iterations = 10000
  let roundtrip_start = Time::now()
  
  for i in 0..=roundtrip_iterations {
    let temp_carrier = TextMapCarrier::new()
    DistributedTracer::inject_context(propagator, base_context, temp_carrier)
    let _extracted = DistributedTracer::extract_context(propagator, temp_carrier)
  }
  
  let roundtrip_end = Time::now()
  let roundtrip_duration = roundtrip_end - roundtrip_start
  let roundtrip_throughput = roundtrip_iterations.to_float() / roundtrip_duration.to_float() * 1000.0
  
  // 验证往返性能
  assert_true(roundtrip_throughput > 25000)  // 至少25000次往返/秒
  
  // 验证数据一致性
  let test_carrier = TextMapCarrier::new()
  DistributedTracer::inject_context(propagator, base_context, test_carrier)
  let extracted_context = DistributedTracer::extract_context(propagator, test_carrier)
  
  assert_eq(extracted_context.trace_id, base_context.trace_id)
  assert_eq(extracted_context.is_sampled, base_context.is_sampled)
}

// 测试3: 序列化性能基准
test "序列化性能基准测试" {
  let benchmark = PerformanceBenchmark::new("serialization")
  
  // 创建测试数据
  let test_spans = []
  for i in 0..=1000 {
    let span = Span::new("perf.operation", Server, 
                        TraceContext::new("perf-trace", "perf-span-" + i.to_string(), true, ""))
    Span::set_attribute(span, "service.name", StringValue("perf.service"))
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "payload", StringValue("performance test payload " + i.to_string()))
    test_spans = test_spans.push(span)
  }
  
  // JSON序列化性能测试
  let json_start = Time::now()
  let json_start_memory = MemoryManager::get_usage()
  
  let json_results = []
  for span in test_spans {
    let json = JsonSerializer::serialize_span(span)
    json_results = json_results.push(json)
  }
  
  let json_end = Time::now()
  let json_end_memory = MemoryManager::get_usage()
  
  let json_duration = json_end - json_start
  let json_memory_delta = json_end_memory - json_start_memory
  let json_throughput = test_spans.length().to_float() / json_duration.to_float() * 1000.0
  let json_avg_size = json_results.map(fn(r) { r.length() }).sum() / json_results.length()
  
  // 验证JSON序列化性能
  assert_true(json_throughput > 5000)  // 至少5000 spans/second
  assert_true(json_avg_size < 1000)    // 平均大小小于1KB
  assert_true(json_memory_delta < 50 * 1024 * 1024)  // 内存增长小于50MB
  
  // 二进制序列化性能测试
  let binary_start = Time::now()
  let binary_start_memory = MemoryManager::get_usage()
  
  let binary_results = []
  for span in test_spans {
    let binary = BinarySerializer::serialize_span(span)
    binary_results = binary_results.push(binary)
  }
  
  let binary_end = Time::now()
  let binary_end_memory = MemoryManager::get_usage()
  
  let binary_duration = binary_end - binary_start
  let binary_memory_delta = binary_end_memory - binary_start_memory
  let binary_throughput = test_spans.length().to_float() / binary_duration.to_float() * 1000.0
  let binary_avg_size = binary_results.map(fn(r) { r.length() }).sum() / binary_results.length()
  
  // 验证二进制序列化性能
  assert_true(binary_throughput > 10000)  // 至少10000 spans/second
  assert_true(binary_avg_size < json_avg_size)  // 二进制应该更小
  assert_true(binary_memory_delta < json_memory_delta)  // 二进制内存使用更少
  
  // 压缩序列化性能测试
  let compressed_start = Time::now()
  let compressed_start_memory = MemoryManager::get_usage()
  
  let compressed_results = []
  for span in test_spans {
    let compressed = CompressedSerializer::serialize_span(span, "gzip")
    compressed_results = compressed_results.push(compressed)
  }
  
  let compressed_end = Time::now()
  let compressed_end_memory = MemoryManager::get_usage()
  
  let compressed_duration = compressed_end - compressed_start
  let compressed_memory_delta = compressed_end_memory - compressed_start_memory
  let compressed_throughput = test_spans.length().to_float() / compressed_duration.to_float() * 1000.0
  let compressed_avg_size = compressed_results.map(fn(r) { r.length() }).sum() / compressed_results.length()
  
  // 验证压缩序列化性能
  assert_true(compressed_throughput > 2000)  // 至少2000 spans/second
  assert_true(compressed_avg_size < binary_avg_size)  // 压缩应该更小
  
  // 验证压缩比
  let compression_ratio = 1.0 - (compressed_avg_size.to_float() / json_avg_size.to_float())
  assert_true(compression_ratio > 0.5)  // 至少50%压缩率
}

// 测试4: 度量聚合性能
test "度量聚合性能测试" {
  let benchmark = PerformanceBenchmark::new("metrics_aggregation")
  
  // 创建度量管理器
  let metrics_manager = MetricsManager::new()
  
  // 创建各种类型的度量
  let counter = metrics_manager.create_counter("performance.counter", "性能测试计数器")
  let gauge = metrics_manager.create_gauge("performance.gauge", "性能测试仪表盘")
  let histogram = metrics_manager.create_histogram("performance.histogram", "性能测试直方图", 
                                                   [10.0, 50.0, 100.0, 500.0, 1000.0])
  
  // 测试度量记录性能
  let record_iterations = 100000
  let record_start = Time::now()
  
  for i in 0..=record_iterations {
    counter.add(1.0, [("service", StringValue("test"))])
    gauge.set(i.to_float() % 100.0, [("service", StringValue("test"))])
    histogram.observe(i.to_float() % 1000.0, [("service", StringValue("test"))])
  }
  
  let record_end = Time::now()
  let record_duration = record_end - record_start
  let record_throughput = (record_iterations * 3).to_float() / record_duration.to_float() * 1000.0
  
  // 验证度量记录性能
  assert_true(record_throughput > 100000)  // 至少100000次记录/秒
  
  // 测试度量聚合性能
  let aggregation_start = Time::now()
  
  let aggregated_metrics = metrics_manager.aggregate_all()
  
  let aggregation_end = Time::now()
  let aggregation_duration = aggregation_end - aggregation_start
  
  // 验证聚合性能
  assert_true(aggregation_duration < 100)  // 聚合应该在100ms内完成
  assert_eq(aggregated_metrics.length(), 3)  // 3种度量类型
  
  // 验证聚合结果
  let aggregated_counter = aggregated_metrics.find(fn(m) { m.name == "performance.counter" })
  assert_true(aggregated_counter != None)
  
  match aggregated_counter {
    Some(metric) => {
      assert_eq(metric.value, record_iterations.to_float())
    }
    None => assert_true(false)
  }
  
  let aggregated_histogram = aggregated_metrics.find(fn(m) { m.name == "performance.histogram" })
  assert_true(aggregated_histogram != None)
  
  match aggregated_histogram {
    Some(metric) => {
      assert_true(metric.buckets.length() > 0)
      let total_count = metric.buckets.map(fn(b) { b.count }).sum()
      assert_eq(total_count, record_iterations)
    }
    None => assert_true(false)
  }
}

// 测试5: 并发性能测试
test "并发性能测试" {
  let benchmark = PerformanceBenchmark::new("concurrency")
  
  // 创建线程池
  let thread_count = 10
  let operations_per_thread = 10000
  let thread_pool = ThreadPool::new(thread_count)
  
  // 测试并发span创建
  let concurrent_start = Time::now()
  let concurrent_start_memory = MemoryManager::get_usage()
  
  let futures = []
  for i in 0..=thread_count {
    let future = thread_pool.spawn(fn() {
      let local_spans = []
      for j in 0..=operations_per_thread {
        let span = Span::new("concurrent.operation", Server, 
                            TraceContext::new("concurrent-trace-" + i.to_string(), 
                                            "concurrent-span-" + i.to_string() + "-" + j.to_string(), 
                                            true, ""))
        Span::set_attribute(span, "thread.id", IntValue(i))
        Span::set_attribute(span, "operation.id", IntValue(j))
        local_spans = local_spans.push(span)
      }
      local_spans.length()
    })
    futures = futures.push(future)
  }
  
  // 等待所有线程完成
  let mut total_spans = 0
  for future in futures {
    let span_count = future.wait()
    total_spans = total_spans + span_count
  }
  
  let concurrent_end = Time::now()
  let concurrent_end_memory = MemoryManager::get_usage()
  
  let concurrent_duration = concurrent_end - concurrent_start
  let concurrent_memory_delta = concurrent_end_memory - concurrent_start_memory
  let concurrent_throughput = total_spans.to_float() / concurrent_duration.to_float() * 1000.0
  
  // 验证并发性能
  assert_eq(total_spans, thread_count * (operations_per_thread + 1))  // 验证所有span都被创建
  assert_true(concurrent_throughput > 50000)  // 至少50000 spans/second
  assert_true(concurrent_memory_delta < 200 * 1024 * 1024)  // 内存增长小于200MB
  
  // 测试并发序列化
  let serialization_spans = []
  for i in 0..=1000 {
    let span = Span::new("serialization.test", Server, 
                        TraceContext::new("serialization-trace", "serialization-span-" + i.to_string(), true, ""))
    serialization_spans = serialization_spans.push(span)
  }
  
  let serialization_futures = []
  for i in 0..=thread_count {
    let spans_per_thread = serialization_spans.length() / thread_count
    let thread_spans = serialization_spans.slice(i * spans_per_thread, spans_per_thread)
    
    let future = thread_pool.spawn(fn() {
      let json_results = []
      for span in thread_spans {
        let json = JsonSerializer::serialize_span(span)
        json_results = json_results.push(json)
      }
      json_results.length()
    })
    serialization_futures = serialization_futures.push(future)
  }
  
  let serialization_start = Time::now()
  
  let mut total_serialized = 0
  for future in serialization_futures {
    let serialized_count = future.wait()
    total_serialized = total_serialized + serialized_count
  }
  
  let serialization_end = Time::now()
  let serialization_duration = serialization_end - serialization_start
  let serialization_throughput = total_serialized.to_float() / serialization_duration.to_float() * 1000.0
  
  // 验证并发序列化性能
  assert_eq(total_serialized, serialization_spans.length())
  assert_true(serialization_throughput > 10000)  // 至少10000序列化/秒
}

// 测试6: 内存使用性能
test "内存使用性能测试" {
  let benchmark = PerformanceBenchmark::new("memory_usage")
  
  // 记录初始内存使用
  let initial_memory = MemoryManager::get_usage()
  
  // 测试大量span创建的内存影响
  let span_count = 100000
  let spans = []
  
  let creation_start = Time::now()
  for i in 0..=span_count {
    let span = Span::new("memory.test", Server, 
                        TraceContext::new("memory-trace", "memory-span-" + i.to_string(), true, ""))
    Span::set_attribute(span, "large.data", StringValue("large payload data " + i.to_string()))
    spans = spans.push(span)
  }
  let creation_end = Time::now()
  
  let peak_memory = MemoryManager::get_usage()
  let memory_increase = peak_memory - initial_memory
  let memory_per_span = memory_increase.to_float() / span_count.to_float()
  
  // 验证内存使用
  assert_true(memory_per_span < 1024)  // 每个span内存使用小于1KB
  assert_true(memory_increase < 200 * 1024 * 1024)  // 总内存增长小于200MB
  
  // 测试内存释放
  let spans = []  // 释放引用
  
  // 强制垃圾回收
  MemoryManager::force_gc()
  
  let gc_memory = MemoryManager::get_usage()
  let memory_recovered = peak_memory - gc_memory
  let recovery_rate = memory_recovered.to_float() / memory_increase.to_float()
  
  // 验证内存回收
  assert_true(recovery_rate > 0.7)  // 至少回收70%的内存
  
  // 测试内存泄漏
  let leak_test_iterations = 10
  let baseline_memory = gc_memory
  
  for iteration in 0..=leak_test_iterations {
    let temp_spans = []
    for i in 0..=10000 {
      let span = Span::new("leak.test", Server, 
                          TraceContext::new("leak-trace", "leak-span-" + i.to_string(), true, ""))
      temp_spans = temp_spans.push(span)
    }
    // temp_spans在循环结束时自动释放
  }
  
  MemoryManager::force_gc()
  let final_memory = MemoryManager::get_usage()
  let leak_increase = final_memory - baseline_memory
  
  // 验证无内存泄漏
  assert_true(leak_increase < 10 * 1024 * 1024)  // 内存增长应该小于10MB
}

// 测试7: 批处理性能
test "批处理性能测试" {
  let benchmark = PerformanceBenchmark::new("batch_processing")
  
  // 创建测试数据
  let batch_sizes = [10, 100, 1000, 10000]
  
  for batch_size in batch_sizes {
    // 创建批次数据
    let batch_spans = []
    for i in 0..=batch_size {
      let span = Span::new("batch.operation", Server, 
                          TraceContext::new("batch-trace", "batch-span-" + i.to_string(), true, ""))
      Span::set_attribute(span, "batch.size", IntValue(batch_size))
      Span::set_attribute(span, "item.index", IntValue(i))
      batch_spans = batch_spans.push(span)
    }
    
    // 测试单个处理性能
    let individual_start = Time::now()
    let individual_results = []
    for span in batch_spans {
      let json = JsonSerializer::serialize_span(span)
      individual_results = individual_results.push(json)
    }
    let individual_end = Time::now()
    let individual_duration = individual_end - individual_start
    
    // 测试批处理性能
    let batch_start = Time::now()
    let batch_result = JsonSerializer::serialize_spans(batch_spans)
    let batch_end = Time::now()
    let batch_duration = batch_end - batch_start
    
    // 验证批处理性能优势
    let speedup = individual_duration.to_float() / batch_duration.to_float()
    assert_true(speedup > 1.5)  // 批处理应该至少快50%
    
    // 验证结果一致性
    let individual_size = individual_results.map(fn(r) { r.length() }).sum()
    assert_eq(batch_result.length(), individual_size + batch_size.to_string().length() + 20)  // 允许JSON数组结构开销
    
    // 验证批处理扩展性
    if batch_size >= 1000 {
      assert_true(speedup > 2.0)  // 大批次应该有更明显的性能优势
    }
  }
}

// 测试8: 网络传输性能
test "网络传输性能测试" {
  let benchmark = PerformanceBenchmark::new("network_transport")
  
  // 创建网络传输管理器
  let transport_manager = NetworkTransportManager::new()
  
  // 创建测试数据
  let data_sizes = [1024, 10240, 102400, 1024000]  // 1KB, 10KB, 100KB, 1MB
  
  for data_size in data_sizes {
    // 创建测试span数据
    let test_spans = []
    let span_count = data_size / 100  // 假设每个span约100字节
    for i in 0..=span_count {
      let span = Span::new("network.test", Server, 
                          TraceContext::new("network-trace", "network-span-" + i.to_string(), true, ""))
      Span::set_attribute(span, "payload", StringValue("x".repeat(50)))  // 50字节payload
      test_spans = test_spans.push(span)
    }
    
    // 序列化数据
    let serialized_data = JsonSerializer::serialize_spans(test_spans)
    assert_eq(serialized_data.length(), data_size)
    
    // 测试传输性能
    let transport_start = Time::now()
    let send_result = transport_manager.send_data(serialized_data, "http://localhost:8080/telemetry")
    let transport_end = Time::now()
    
    // 验证传输结果
    assert_true(send_result.success)
    
    let transport_duration = transport_end - transport_start
    let throughput = data_size.to_float() / transport_duration.to_float() * 1000.0  // bytes/second
    
    // 验证传输性能
    assert_true(throughput > 1024 * 1024)  // 至少1MB/s
    
    // 测试压缩传输
    let compressed_data = CompressedSerializer::compress(serialized_data, "gzip")
    let compressed_transport_start = Time::now()
    let compressed_send_result = transport_manager.send_data(compressed_data, "http://localhost:8080/telemetry")
    let compressed_transport_end = Time::now()
    
    assert_true(compressed_send_result.success)
    
    let compressed_transport_duration = compressed_transport_end - compressed_transport_start
    let compressed_throughput = data_size.to_float() / compressed_transport_duration.to_float() * 1000.0
    
    // 验证压缩传输性能
    assert_true(compressed_throughput > throughput * 0.8)  // 压缩传输吞吐量不应显著降低
    
    // 验证压缩效果
    let compression_ratio = 1.0 - (compressed_data.length().to_float() / serialized_data.length().to_float())
    assert_true(compression_ratio > 0.5)  // 至少50%压缩率
  }
}

// 测试9: 负载均衡性能
test "负载均衡性能测试" {
  let benchmark = PerformanceBenchmark::new("load_balancing")
  
  // 创建负载均衡器
  let load_balancer = LoadBalancer::new()
  
  // 添加后端节点
  let backends = [
    "http://backend1:8080",
    "http://backend2:8080", 
    "http://backend3:8080",
    "http://backend4:8080"
  ]
  
  for backend in backends {
    load_balancer.add_backend(backend)
  }
  
  // 测试不同负载均衡策略
  let strategies = ["round_robin", "weighted_round_robin", "least_connections", "random"]
  
  for strategy in strategies {
    load_balancer.set_strategy(strategy)
    
    // 模拟请求分发
    let request_count = 10000
    let distribution_start = Time::now()
    
    let backend_counts = @{
      "http://backend1:8080": 0,
      "http://backend2:8080": 0,
      "http://backend3:8080": 0,
      "http://backend4:8080": 0
    }
    
    for i in 0..=request_count {
      let selected_backend = load_balancer.select_backend()
      backend_counts[selected_backend] = backend_counts[selected_backend] + 1
    }
    
    let distribution_end = Time::now()
    let distribution_duration = distribution_end - distribution_start
    let distribution_throughput = request_count.to_float() / distribution_duration.to_float() * 1000.0
    
    // 验证分发性能
    assert_true(distribution_throughput > 100000)  // 至少100000次分发/秒
    
    // 验证负载分布
    if strategy == "round_robin" or strategy == "weighted_round_robin" {
      // 轮询策略应该均匀分布
      let expected_per_backend = request_count / backends.length()
      for backend in backends {
        let count = backend_counts[backend]
        let variance = (count - expected_per_backend).to_float() / expected_per_backend.to_float()
        assert_true(variance.abs() < 0.1)  // 变异系数小于10%
      }
    }
    
    // 验证所有后端都被使用
    for backend in backends {
      assert_true(backend_counts[backend] > 0)
    }
  }
}

// 测试10: 资源限制性能
test "资源限制性能测试" {
  let benchmark = PerformanceBenchmark::new("resource_limits")
  
  // 创建资源限制管理器
  let resource_limiter = ResourceLimiter::new()
  
  // 设置资源限制
  resource_limiter.set_limits({
    max_memory: 100 * 1024 * 1024,     // 100MB
    max_cpu_percent: 80.0,             // 80%
    max_file_descriptors: 1000,        // 1000个文件描述符
    max_network_connections: 100       // 100个网络连接
  })
  
  // 测试内存限制
  let memory_test_start = Time::now()
  let memory_exceeded = false
  let spans_created = 0
  
  while not memory_exceeded {
    let span = Span::new("memory.limit.test", Server, 
                        TraceContext::new("memory-limit-trace", "memory-limit-span-" + spans_created.to_string(), true, ""))
    Span::set_attribute(span, "large.data", StringValue("x".repeat(1024)))  // 1KB per span
    spans_created = spans_created + 1
    
    if resource_limiter.check_memory_limit() {
      memory_exceeded = true
    }
  }
  
  let memory_test_end = Time::now()
  let memory_test_duration = memory_test_end - memory_test_start
  
  // 验证内存限制效果
  assert_true(memory_exceeded)
  assert_true(spans_created > 100)  // 至少创建100个span
  assert_true(memory_test_duration < 10000)  // 测试应在10秒内完成
  
  // 测试CPU限制
  let cpu_test_start = Time::now()
  let cpu_exceeded = false
  let iterations = 0
  
  while not cpu_exceeded and iterations < 1000000 {
    // CPU密集型操作
    let result = 0
    for i in 0..=1000 {
      result = result + i * i
    }
    iterations = iterations + 1
    
    if resource_limiter.check_cpu_limit() {
      cpu_exceeded = true
    }
  }
  
  let cpu_test_end = Time::now()
  let cpu_test_duration = cpu_test_end - cpu_test_start
  
  // 验证CPU限制效果
  assert_true(cpu_test_duration < 30000)  // CPU限制测试应在30秒内完成
  
  // 测试资源限制下的性能
  let limited_performance_start = Time::now()
  let limited_spans = []
  
  for i in 0..=10000 {
    if resource_limiter.can_create_resource() {
      let span = Span::new("limited.performance.test", Server, 
                          TraceContext::new("limited-perf-trace", "limited-perf-span-" + i.to_string(), true, ""))
      limited_spans = limited_spans.push(span)
    }
  }
  
  let limited_performance_end = Time::now()
  let limited_performance_duration = limited_performance_end - limited_performance_start
  let limited_throughput = limited_spans.length().to_float() / limited_performance_duration.to_float() * 1000.0
  
  // 验证资源限制下的性能
  assert_true(limited_spans.length() > 0)  // 应该能够创建一些span
  assert_true(limited_throughput > 100)    // 至少100 spans/second
}