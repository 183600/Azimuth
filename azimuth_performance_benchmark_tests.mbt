// Azimuth Performance Benchmark Tests
// 性能基准测试用例 - 测试系统的性能表现，包括各种操作的性能基准和资源使用情况

// Test 1: 跨度创建和操作性能基准
test "span creation and operations performance benchmark" {
  let benchmark = PerformanceBenchmark::new("span_operations")
  let num_iterations = 10000
  
  // 基准测试：跨度创建
  let span_creation_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let trace_id = "trace_" + i.to_string()
      let span_id = "span_" + i.to_string()
      let span_ctx = SpanContext::new(trace_id, span_id, true, "")
      let span = Span::new("benchmark_span", Internal, span_ctx)
      // 跨度会在作用域结束时自动清理
    }
  })
  
  // 验证跨度创建性能
  assert_true(span_creation_time.total_time > 0L)
  assert_true(span_creation_time.average_time > 0.0)
  let spans_per_second = num_iterations.to_float() / (span_creation_time.total_time.to_float() / 1000000000.0)
  assert_true(spans_per_second > 10000.0) // 至少每秒创建10000个跨度
  
  // 基准测试：跨度事件添加
  let span = Span::new("event_benchmark_span", Internal, SpanContext::new("test_trace", "test_span", true, ""))
  let event_addition_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      Span::add_event(span, "event_" + i.to_string(), Some([
        ("iteration", IntValue(i)),
        ("timestamp", IntValue(Time::now()))
      ]))
    }
  })
  
  // 验证事件添加性能
  assert_true(event_addition_time.total_time > 0L)
  assert_true(event_addition_time.average_time > 0.0)
  let events_per_second = num_iterations.to_float() / (event_addition_time.total_time.to_float() / 1000000000.0)
  assert_true(events_per_second > 50000.0) // 至少每秒添加50000个事件
  
  // 基准测试：跨度状态设置
  let status_span = Span::new("status_benchmark_span", Internal, SpanContext::new("test_trace", "test_span", true, ""))
  let status_setting_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      if i % 3 == 0 {
        Span::set_status(status_span, Ok, Some("Success " + i.to_string()))
      } else if i % 3 == 1 {
        Span::set_status(status_span, Error, Some("Error " + i.to_string()))
      } else {
        Span::set_status(status_span, Unset, None)
      }
    }
  })
  
  // 验证状态设置性能
  assert_true(status_setting_time.total_time > 0L)
  assert_true(status_setting_time.average_time > 0.0)
  let status_updates_per_second = num_iterations.to_float() / (status_setting_time.total_time.to_float() / 1000000000.0)
  assert_true(status_updates_per_second > 100000.0) // 至少每秒设置100000个状态
  
  // 基准测试：跨度结束
  let end_span = Span::new("end_benchmark_span", Internal, SpanContext::new("test_trace", "test_span", true, ""))
  let span_end_time = benchmark.measure(|| {
    // 注意：这里只能测试一次，因为跨度结束后不能再次结束
    Span::end(end_span)
  })
  
  // 验证跨度结束性能
  assert_true(span_end_time.total_time > 0L)
  assert_true(span_end_time.total_time < 1000000L) // 结束操作应该在1ms内完成
  
  // 输出性能报告
  let performance_report = benchmark.generate_report()
  assert_true(performance_report.contains("span_operations"))
  assert_true(performance_report.contains("spans_per_second"))
  assert_true(performance_report.contains("events_per_second"))
  assert_true(performance_report.contains("status_updates_per_second"))
}

// Test 2: 指标操作性能基准
test "metrics operations performance benchmark" {
  let benchmark = PerformanceBenchmark::new("metrics_operations")
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "benchmark_meter")
  let num_iterations = 50000
  
  // 创建各种类型的指标
  let counter = Meter::create_counter(meter, "benchmark_counter", Some("Benchmark counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "benchmark_histogram", Some("Benchmark histogram"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "benchmark_gauge", Some("Benchmark gauge"), Some("value"))
  
  // 基准测试：计数器操作
  let counter_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let attrs = Attributes::new()
      Attributes::set(attrs, "iteration", IntValue(i))
      Counter::add(counter, 1.0, Some(attrs))
    }
  })
  
  // 验证计数器性能
  assert_true(counter_time.total_time > 0L)
  assert_true(counter_time.average_time > 0.0)
  let counter_ops_per_second = num_iterations.to_float() / (counter_time.total_time.to_float() / 1000000000.0)
  assert_true(counter_ops_per_second > 100000.0) // 至少每秒100000次计数器操作
  
  // 基准测试：直方图操作
  let histogram_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let value = (i % 1000).to_float()
      Histogram::record(histogram, value)
    }
  })
  
  // 验证直方图性能
  assert_true(histogram_time.total_time > 0L)
  assert_true(histogram_time.average_time > 0.0)
  let histogram_ops_per_second = num_iterations.to_float() / (histogram_time.total_time.to_float() / 1000000000.0)
  assert_true(histogram_ops_per_second > 50000.0) // 至少每秒50000次直方图操作
  
  // 基准测试：仪表操作
  let gauge_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let value = (i % 100).to_float()
      Gauge::record(gauge, value)
    }
  })
  
  // 验证仪表性能
  assert_true(gauge_time.total_time > 0L)
  assert_true(gauge_time.average_time > 0.0)
  let gauge_ops_per_second = num_iterations.to_float() / (gauge_time.total_time.to_float() / 1000000000.0)
  assert_true(gauge_ops_per_second > 75000.0) // 至少每秒75000次仪表操作
  
  // 基准测试：指标聚合
  let aggregation_time = benchmark.measure(|| {
    let counter_value = Counter::get_value(counter)
    let histogram_stats = Histogram::get_statistics(histogram)
    let gauge_value = Gauge::get_value(gauge)
    
    // 确保编译器不会优化掉这些计算
    assert_true(counter_value > 0.0)
    assert_true(histogram_stats.count > 0L)
    assert_true(gauge_value >= 0.0)
  })
  
  // 验证聚合性能
  assert_true(aggregation_time.total_time > 0L)
  assert_true(aggregation_time.total_time < 10000000L) // 聚合操作应该在10ms内完成
  
  // 输出性能报告
  let performance_report = benchmark.generate_report()
  assert_true(performance_report.contains("metrics_operations"))
  assert_true(performance_report.contains("counter_ops_per_second"))
  assert_true(performance_report.contains("histogram_ops_per_second"))
  assert_true(performance_report.contains("gauge_ops_per_second"))
}

// Test 3: 属性操作性能基准
test "attributes operations performance benchmark" {
  let benchmark = PerformanceBenchmark::new("attributes_operations")
  let num_iterations = 25000
  
  // 基准测试：属性设置
  let attrs = Attributes::new()
  let set_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      Attributes::set(attrs, "key_" + i.to_string(), StringValue("value_" + i.to_string()))
    }
  })
  
  // 验证属性设置性能
  assert_true(set_time.total_time > 0L)
  assert_true(set_time.average_time > 0.0)
  let sets_per_second = num_iterations.to_float() / (set_time.total_time.to_float() / 1000000000.0)
  assert_true(sets_per_second > 100000.0) // 至少每秒设置100000个属性
  
  // 基准测试：属性获取
  let get_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let key = "key_" + i.to_string()
      let value = Attributes::get(attrs, key)
      // 确保编译器不会优化掉这个操作
      match value {
        Some(_) => assert_true(true)
        None => assert_true(false)
      }
    }
  })
  
  // 验证属性获取性能
  assert_true(get_time.total_time > 0L)
  assert_true(get_time.average_time > 0.0)
  let gets_per_second = num_iterations.to_float() / (get_time.total_time.to_float() / 1000000000.0)
  assert_true(gets_per_second > 200000.0) // 至少每秒获取200000个属性
  
  // 基准测试：属性删除
  let remove_attrs = Attributes::new()
  // 先设置一些属性
  for i in 1..=1000 {
    Attributes::set(remove_attrs, "remove_key_" + i.to_string(), StringValue("remove_value_" + i.to_string()))
  }
  
  let remove_time = benchmark.measure(|| {
    for i in 1..=1000 {
      let key = "remove_key_" + i.to_string()
      Attributes::remove(remove_attrs, key)
    }
  })
  
  // 验证属性删除性能
  assert_true(remove_time.total_time > 0L)
  assert_true(remove_time.average_time > 0.0)
  let removes_per_second = 1000.0 / (remove_time.total_time.to_float() / 1000000000.0)
  assert_true(removes_per_second > 50000.0) // 至少每秒删除50000个属性
  
  // 基准测试：属性迭代
  let iteration_attrs = Attributes::new()
  for i in 1..=1000 {
    Attributes::set(iteration_attrs, "iter_key_" + i.to_string(), StringValue("iter_value_" + i.to_string()))
  }
  
  let iteration_time = benchmark.measure(|| {
    let mut count = 0
    Attributes::for_each(iteration_attrs, || {
      count = count + 1
    })
    assert_eq(count, 1000)
  })
  
  // 验证属性迭代性能
  assert_true(iteration_time.total_time > 0L)
  assert_true(iteration_time.total_time < 1000000L) // 1000个属性的迭代应该在1ms内完成
  
  // 输出性能报告
  let performance_report = benchmark.generate_report()
  assert_true(performance_report.contains("attributes_operations"))
  assert_true(performance_report.contains("sets_per_second"))
  assert_true(performance_report.contains("gets_per_second"))
  assert_true(performance_report.contains("removes_per_second"))
}

// Test 4: 序列化和反序列化性能基准
test "serialization and deserialization performance benchmark" {
  let benchmark = PerformanceBenchmark::new("serialization_operations")
  let num_iterations = 5000
  
  // 准备测试数据
  let trace_id = "benchmark_trace_12345"
  let span_id = "benchmark_span_67890"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("benchmark_span", Internal, span_ctx)
  
  // 添加事件和属性
  for i in 1..=100 {
    Span::add_event(span, "event_" + i.to_string(), Some([
      ("iteration", IntValue(i)),
      ("data", StringValue("test_data_" + i.to_string()))
    ]))
  }
  
  // 基准测试：JSON序列化
  let json_serializer = JsonSerializer::new()
  let json_serialization_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let serialized = JsonSerializer::serialize(json_serializer, span)
      // 确保编译器不会优化掉这个操作
      assert_true(serialized.length() > 0)
    }
  })
  
  // 验证JSON序列化性能
  assert_true(json_serialization_time.total_time > 0L)
  assert_true(json_serialization_time.average_time > 0.0)
  let json_serializations_per_second = num_iterations.to_float() / (json_serialization_time.total_time.to_float() / 1000000000.0)
  assert_true(json_serializations_per_second > 1000.0) // 至少每秒序列化1000个跨度
  
  // 基准测试：JSON反序列化
  let serialized_span = JsonSerializer::serialize(json_serializer, span)
  let json_deserializer = SpanDeserializer::new()
  let json_deserialization_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let deserialized = SpanDeserializer::deserialize(json_deserializer, serialized_span)
      // 确保编译器不会优化掉这个操作
      assert_eq(Span::name(deserialized), "benchmark_span")
    }
  })
  
  // 验证JSON反序列化性能
  assert_true(json_deserialization_time.total_time > 0L)
  assert_true(json_deserialization_time.average_time > 0.0)
  let json_deserializations_per_second = num_iterations.to_float() / (json_deserialization_time.total_time.to_float() / 1000000000.0)
  assert_true(json_deserializations_per_second > 500.0) // 至少每秒反序列化500个跨度
  
  // 基准测试：二进制序列化
  let binary_serializer = BinarySerializer::new()
  let binary_serialization_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let serialized = BinarySerializer::serialize_span(binary_serializer, span)
      // 确保编译器不会优化掉这个操作
      assert_true(serialized.length() > 0)
    }
  })
  
  // 验证二进制序列化性能
  assert_true(binary_serialization_time.total_time > 0L)
  assert_true(binary_serialization_time.average_time > 0.0)
  let binary_serializations_per_second = num_iterations.to_float() / (binary_serialization_time.total_time.to_float() / 1000000000.0)
  assert_true(binary_serializations_per_second > 2000.0) // 至少每秒序列化2000个跨度
  
  // 基准测试：压缩序列化
  let compressed_serializer = CompressedSerializer::new(CompressionAlgorithm::Gzip)
  let compressed_serialization_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let serialized = CompressedSerializer::serialize(compressed_serializer, span)
      // 确保编译器不会优化掉这个操作
      assert_true(serialized.length() > 0)
    }
  })
  
  // 验证压缩序列化性能
  assert_true(compressed_serialization_time.total_time > 0L)
  assert_true(compressed_serialization_time.average_time > 0.0)
  let compressed_serializations_per_second = num_iterations.to_float() / (compressed_serialization_time.total_time.to_float() / 1000000000.0)
  assert_true(compressed_serializations_per_second > 100.0) // 至少每秒压缩序列化100个跨度
  
  // 比较不同序列化方法的性能
  assert_true(binary_serialization_time.total_time < json_serialization_time.total_time) // 二进制应该比JSON快
  assert_true(compressed_serialization_time.total_time > json_serialization_time.total_time) // 压缩应该比JSON慢
  
  // 输出性能报告
  let performance_report = benchmark.generate_report()
  assert_true(performance_report.contains("serialization_operations"))
  assert_true(performance_report.contains("json_serializations_per_second"))
  assert_true(performance_report.contains("json_deserializations_per_second"))
  assert_true(performance_report.contains("binary_serializations_per_second"))
  assert_true(performance_report.contains("compressed_serializations_per_second"))
}

// Test 5: 上下文传播性能基准
test "context propagation performance benchmark" {
  let benchmark = PerformanceBenchmark::new("context_propagation")
  let num_iterations = 10000
  
  // 基准测试：上下文创建
  let context_creation_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let trace_id = "trace_" + i.to_string()
      let span_id = "span_" + i.to_string()
      let span_ctx = SpanContext::new(trace_id, span_id, true, "")
      // 上下文会在作用域结束时自动清理
    }
  })
  
  // 验证上下文创建性能
  assert_true(context_creation_time.total_time > 0L)
  assert_true(context_creation_time.average_time > 0.0)
  let contexts_per_second = num_iterations.to_float() / (context_creation_time.total_time.to_float() / 1000000000.0)
  assert_true(contexts_per_second > 100000.0) // 至少每秒创建100000个上下文
  
  // 基准测试：追踪传播注入
  let propagator = TracePropagator::new()
  let injection_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let trace_id = "trace_" + i.to_string()
      let span_id = "span_" + i.to_string()
      let span_ctx = SpanContext::new(trace_id, span_id, true, "")
      let headers = TracePropagator::inject_to_headers(propagator, span_ctx)
      // 确保编译器不会优化掉这个操作
      assert_true(headers.contains("traceparent"))
    }
  })
  
  // 验证追踪传播注入性能
  assert_true(injection_time.total_time > 0L)
  assert_true(injection_time.average_time > 0.0)
  let injections_per_second = num_iterations.to_float() / (injection_time.total_time.to_float() / 1000000000.0)
  assert_true(injections_per_second > 50000.0) // 至少每秒注入50000个上下文
  
  // 基准测试：追踪传播提取
  let test_headers = TracePropagator::inject_to_headers(propagator, SpanContext::new("test_trace", "test_span", true, ""))
  let extraction_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let extracted_ctx = TracePropagator::extract_from_headers(propagator, test_headers)
      // 确保编译器不会优化掉这个操作
      assert_true(SpanContext::is_valid(extracted_ctx))
    }
  })
  
  // 验证追踪传播提取性能
  assert_true(extraction_time.total_time > 0L)
  assert_true(extraction_time.average_time > 0.0)
  let extractions_per_second = num_iterations.to_float() / (extraction_time.total_time.to_float() / 1000000000.0)
  assert_true(extractions_per_second > 75000.0) // 至少每秒提取75000个上下文
  
  // 基准测试：行李操作
  let baggage = Baggage::new()
  let baggage_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let key = "baggage_key_" + i.to_string()
      let value = "baggage_value_" + i.to_string()
      let updated_baggage = Baggage::set_entry(baggage, key, value)
      let retrieved_value = Baggage::get_entry(updated_baggage, key)
      // 确保编译器不会优化掉这个操作
      match retrieved_value {
        Some(v) => assert_eq(v, value)
        None => assert_true(false)
      }
    }
  })
  
  // 验证行李操作性能
  assert_true(baggage_time.total_time > 0L)
  assert_true(baggage_time.average_time > 0.0)
  let baggage_ops_per_second = num_iterations.to_float() / (baggage_time.total_time.to_float() / 1000000000.0)
  assert_true(baggage_ops_per_second > 25000.0) // 至少每秒25000次行李操作
  
  // 输出性能报告
  let performance_report = benchmark.generate_report()
  assert_true(performance_report.contains("context_propagation"))
  assert_true(performance_report.contains("contexts_per_second"))
  assert_true(performance_report.contains("injections_per_second"))
  assert_true(performance_report.contains("extractions_per_second"))
  assert_true(performance_report.contains("baggage_ops_per_second"))
}

// Test 6: 内存使用性能基准
test "memory usage performance benchmark" {
  let benchmark = PerformanceBenchmark::new("memory_usage")
  let memory_profiler = MemoryProfiler::new()
  
  // 基准测试：跨度创建的内存使用
  let initial_memory = MemoryProfiler::get_current_usage(memory_profiler)
  let spans = []
  
  for i in 1..=10000 {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("memory_test_span", Internal, span_ctx)
    
    // 添加事件增加内存使用
    for j in 1..=10 {
      Span::add_event(span, "event_" + j.to_string(), Some([
        ("iteration", IntValue(j)),
        ("data", StringValue("test_data_" + j.to_string()))
      ]))
    }
    
    spans.push(span)
  }
  
  let spans_memory = MemoryProfiler::get_current_usage(memory_profiler)
  let spans_memory_usage = spans_memory - initial_memory
  
  // 验证跨度内存使用
  assert_true(spans_memory_usage > 0)
  let memory_per_span = spans_memory_usage.to_float() / 10000.0
  assert_true(memory_per_span < 10240.0) // 每个跨度应该使用少于10KB内存
  
  // 清理跨度
  spans.clear()
  
  // 强制垃圾回收
  MemoryProfiler::force_gc(memory_profiler)
  
  let after_gc_memory = MemoryProfiler::get_current_usage(memory_profiler)
  let memory_recovered = spans_memory - after_gc_memory
  
  // 验证内存回收
  assert_true(memory_recovered > spans_memory_usage * 0.8) // 至少回收80%的内存
  
  // 基准测试：属性创建的内存使用
  let attrs_memory_start = MemoryProfiler::get_current_usage(memory_profiler)
  let attributes_list = []
  
  for i in 1..=5000 {
    let attrs = Attributes::new()
    for j in 1..=20 {
      Attributes::set(attrs, "key_" + j.to_string(), StringValue("value_" + j.to_string()))
    }
    attributes_list.push(attrs)
  }
  
  let attrs_memory_end = MemoryProfiler::get_current_usage(memory_profiler)
  let attrs_memory_usage = attrs_memory_end - attrs_memory_start
  
  // 验证属性内存使用
  assert_true(attrs_memory_usage > 0)
  let memory_per_attrs = attrs_memory_usage.to_float() / 5000.0
  assert_true(memory_per_attrs < 5120.0) // 每个属性集应该使用少于5KB内存
  
  // 清理属性
  attributes_list.clear()
  
  // 强制垃圾回收
  MemoryProfiler::force_gc(memory_profiler)
  
  let attrs_after_gc_memory = MemoryProfiler::get_current_usage(memory_profiler)
  let attrs_memory_recovered = attrs_memory_end - attrs_after_gc_memory
  
  // 验证属性内存回收
  assert_true(attrs_memory_recovered > attrs_memory_usage * 0.8) // 至少回收80%的内存
  
  // 基准测试：内存分配速度
  let allocation_time = benchmark.measure(|| {
    let temp_objects = []
    for i in 1..=10000 {
      let obj = LargeObject::new(1024) // 1KB对象
      temp_objects.push(obj)
    }
    // 对象会在作用域结束时自动清理
  })
  
  // 验证内存分配速度
  assert_true(allocation_time.total_time > 0L)
  let allocations_per_second = 10000.0 / (allocation_time.total_time.to_float() / 1000000000.0)
  assert_true(allocations_per_second > 1000.0) // 至少每秒分配1000个1KB对象
  
  // 输出性能报告
  let performance_report = benchmark.generate_report()
  assert_true(performance_report.contains("memory_usage"))
  assert_true(performance_report.contains("memory_per_span"))
  assert_true(performance_report.contains("memory_per_attrs"))
  assert_true(performance_report.contains("allocations_per_second"))
  
  // 输出内存分析报告
  let memory_report = MemoryProfiler::generate_report(memory_profiler)
  assert_true(memory_report.contains("memory_usage"))
  assert_true(memory_report.contains("memory_recovered"))
  assert_true(memory_report.contains("allocation_speed"))
}

// Test 7: 并发性能基准
test "concurrent performance benchmark" {
  let benchmark = PerformanceBenchmark::new("concurrent_operations")
  let num_threads = 8
  let operations_per_thread = 5000
  
  // 基准测试：并发跨度创建
  let concurrent_span_time = benchmark.measure(|| {
    let threads = []
    for thread_id in 1..=num_threads {
      let thread = Thread::spawn(|| {
        for i in 1..=operations_per_thread {
          let trace_id = "trace_" + thread_id.to_string() + "_" + i.to_string()
          let span_id = "span_" + thread_id.to_string() + "_" + i.to_string()
          let span_ctx = SpanContext::new(trace_id, span_id, true, "")
          let span = Span::new("concurrent_span", Internal, span_ctx)
          Span::add_event(span, "concurrent_event", Some([
            ("thread_id", IntValue(thread_id)),
            ("iteration", IntValue(i))
          ]))
          Span::end(span)
        }
      })
      threads.push(thread)
    }
    
    for thread in threads {
      Thread::join(thread)
    }
  })
  
  // 验证并发跨度创建性能
  assert_true(concurrent_span_time.total_time > 0L)
  let total_operations = (num_threads * operations_per_thread).to_float()
  let concurrent_spans_per_second = total_operations / (concurrent_span_time.total_time.to_float() / 1000000000.0)
  assert_true(concurrent_spans_per_second > 10000.0) // 至少每秒创建10000个并发跨度
  
  // 基准测试：并发指标操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_meter")
  let counter = Meter::create_counter(meter, "concurrent_counter", Some("Concurrent counter"), Some("count"))
  
  let concurrent_metrics_time = benchmark.measure(|| {
    let threads = []
    for thread_id in 1..=num_threads {
      let thread = Thread::spawn(|| {
        for i in 1..=operations_per_thread {
          let attrs = Attributes::new()
          Attributes::set(attrs, "thread_id", IntValue(thread_id))
          Attributes::set(attrs, "iteration", IntValue(i))
          Counter::add(counter, 1.0, Some(attrs))
        }
      })
      threads.push(thread)
    }
    
    for thread in threads {
      Thread::join(thread)
    }
  })
  
  // 验证并发指标操作性能
  assert_true(concurrent_metrics_time.total_time > 0L)
  let concurrent_metrics_ops_per_second = total_operations / (concurrent_metrics_time.total_time.to_float() / 1000000000.0)
  assert_true(concurrent_metrics_ops_per_second > 50000.0) // 至少每秒执行50000次并发指标操作
  
  // 基准测试：并发属性操作
  let shared_attributes = ThreadSafeAttributes::new()
  
  let concurrent_attrs_time = benchmark.measure(|| {
    let threads = []
    for thread_id in 1..=num_threads {
      let thread = Thread::spawn(|| {
        for i in 1..=operations_per_thread {
          let key = "thread_" + thread_id.to_string() + "_key_" + i.to_string()
          let value = "thread_" + thread_id.to_string() + "_value_" + i.to_string()
          
          ThreadSafeAttributes::set(shared_attributes, key, StringValue(value))
          let retrieved = ThreadSafeAttributes::get(shared_attributes, key)
          
          match retrieved {
            Some(StringValue(v)) => assert_eq(v, value)
            _ => assert_true(false)
          }
        }
      })
      threads.push(thread)
    }
    
    for thread in threads {
      Thread::join(thread)
    }
  })
  
  // 验证并发属性操作性能
  assert_true(concurrent_attrs_time.total_time > 0L)
  let concurrent_attrs_ops_per_second = total_operations / (concurrent_attrs_time.total_time.to_float() / 1000000000.0)
  assert_true(concurrent_attrs_ops_per_second > 25000.0) // 至少每秒执行25000次并发属性操作
  
  // 输出性能报告
  let performance_report = benchmark.generate_report()
  assert_true(performance_report.contains("concurrent_operations"))
  assert_true(performance_report.contains("concurrent_spans_per_second"))
  assert_true(performance_report.contains("concurrent_metrics_ops_per_second"))
  assert_true(performance_report.contains("concurrent_attrs_ops_per_second"))
}

// Test 8: 资源限制下的性能基准
test "performance under resource constraints" {
  let benchmark = PerformanceBenchmark::new("resource_constraints")
  let num_iterations = 5000
  
  // 设置内存限制
  let memory_limiter = MemoryLimiter::new(50 * 1024 * 1024) // 50MB限制
  MemoryLimiter::activate(memory_limiter)
  
  // 基准测试：内存限制下的跨度创建
  let memory_constrained_span_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let trace_id = "trace_" + i.to_string()
      let span_id = "span_" + i.to_string()
      let span_ctx = SpanContext::new(trace_id, span_id, true, "")
      let span = Span::new("memory_constrained_span", Internal, span_ctx)
      
      // 添加更多事件增加内存压力
      for j in 1..=50 {
        Span::add_event(span, "memory_pressure_event_" + j.to_string(), Some([
          ("iteration", IntValue(j)),
          ("data", StringValue("large_test_data_" + j.to_string()))
        ]))
      }
      
      Span::end(span)
      
      // 偶尔检查内存使用
      if i % 1000 == 0 {
        let current_usage = MemoryLimiter::get_current_usage(memory_limiter)
        if current_usage > MemoryLimiter::get_limit(memory_limiter) * 0.8 {
          MemoryLimiter::trigger_cleanup(memory_limiter)
        }
      }
    }
  })
  
  // 验证内存限制下的性能
  assert_true(memory_constrained_span_time.total_time > 0L)
  let memory_constrained_spans_per_second = num_iterations.to_float() / (memory_constrained_span_time.total_time.to_float() / 1000000000.0)
  assert_true(memory_constrained_spans_per_second > 1000.0) // 即使在内存限制下，至少每秒创建1000个跨度
  
  // 设置CPU限制
  let cpu_limiter = CPULimiter::new(50.0) // 限制CPU使用率为50%
  CPULimiter::activate(cpu_limiter)
  
  // 基准测试：CPU限制下的指标操作
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cpu_constrained_meter")
  let histogram = Meter::create_histogram(meter, "cpu_constrained_histogram", Some("CPU constrained histogram"), Some("ms"))
  
  let cpu_constrained_metrics_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      // 添加一些计算密集型操作
      let value = (i % 1000).to_float()
      for j in 1..=10 {
        let calculated_value = Math::sin(value) * Math::cos(value) + Math::sqrt(value)
        Histogram::record(histogram, calculated_value)
      }
      
      // 偶尔让出CPU
      if i % 500 == 0 {
        CPULimiter::yield_cpu(cpu_limiter)
      }
    }
  })
  
  // 验证CPU限制下的性能
  assert_true(cpu_constrained_metrics_time.total_time > 0L)
  let cpu_constrained_metrics_ops_per_second = (num_iterations * 10).to_float() / (cpu_constrained_metrics_time.total_time.to_float() / 1000000000.0)
  assert_true(cpu_constrained_metrics_ops_per_second > 10000.0) // 即使在CPU限制下，至少每秒执行10000次指标操作
  
  // 设置IO限制
  let io_limiter = IOLimiter::new(1000) // 限制为1000次IO操作/秒
  IOLimiter::activate(io_limiter)
  
  // 基准测试：IO限制下的序列化操作
  let trace_id = "io_constrained_trace"
  let span_id = "io_constrained_span"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("io_constrained_span", Internal, span_ctx)
  
  let io_constrained_serialization_time = benchmark.measure(|| {
    for i in 1..=num_iterations {
      let serializer = JsonSerializer::new()
      let serialized = JsonSerializer::serialize(serializer, span)
      
      // 模拟IO写入
      IOLimiter::perform_io(io_limiter, || {
        // 模拟写入操作
        assert_true(serialized.length() > 0)
      })
      
      // 模拟IO读取
      IOLimiter::perform_io(io_limiter, || {
        // 模拟读取操作
        let deserializer = SpanDeserializer::new()
        let deserialized = SpanDeserializer::deserialize(deserializer, serialized)
        assert_eq(Span::name(deserialized), "io_constrained_span")
      })
    }
  })
  
  // 验证IO限制下的性能
  assert_true(io_constrained_serialization_time.total_time > 0L)
  let io_constrained_serializations_per_second = num_iterations.to_float() / (io_constrained_serialization_time.total_time.to_float() / 1000000000.0)
  assert_true(io_constrained_serializations_per_second > 100.0) // 即使在IO限制下，至少每秒完成100次序列化操作
  
  // 清理资源限制
  MemoryLimiter::deactivate(memory_limiter)
  CPULimiter::deactivate(cpu_limiter)
  IOLimiter::deactivate(io_limiter)
  
  // 输出性能报告
  let performance_report = benchmark.generate_report()
  assert_true(performance_report.contains("resource_constraints"))
  assert_true(performance_report.contains("memory_constrained_spans_per_second"))
  assert_true(performance_report.contains("cpu_constrained_metrics_ops_per_second"))
  assert_true(performance_report.contains("io_constrained_serializations_per_second"))
  
  // 输出资源限制报告
  let constraint_report = ResourceLimiter::generate_combined_report(memory_limiter, cpu_limiter, io_limiter)
  assert_true(constraint_report.contains("memory_limit"))
  assert_true(constraint_report.contains("cpu_limit"))
  assert_true(constraint_report.contains("io_limit"))
}