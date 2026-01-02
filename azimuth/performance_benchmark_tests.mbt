// Performance Benchmark Tests
// 验证遥测系统在各种负载下的性能表现

test "span creation performance benchmark" {
  // 测试大量span创建的性能
  let start_time = azimuth::Clock::now()
  let span_count = 10000
  
  for i in 0..span_count {
    let trace_id = "trace-" + i.to_string()
    let span_id = "span-" + i.to_string()
    let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
    let span = azimuth::Span::new("operation-" + i.to_string(), azimuth::Internal, span_ctx)
    
    // 设置一些属性
    azimuth::Span::set_attribute(span, "index", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "name", azimuth::StringValue("test-" + i.to_string()))
  }
  
  let end_time = azimuth::Clock::now()
  let duration = end_time - start_time
  
  // 验证性能在合理范围内（每秒至少1000个span）
  let spans_per_second = span_count.to_double() / (duration.to_double() / 1000000000.0)
  assert_true(spans_per_second >= 1000.0, "Span creation should be at least 1000 spans/second")
}

test "metrics recording performance benchmark" {
  // 测试指标记录的性能
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "benchmark-meter")
  let counter = azimuth::Meter::create_counter(meter, "benchmark.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "benchmark.histogram")
  
  let start_time = azimuth::Clock::now()
  let operation_count = 50000
  
  // 记录大量指标数据
  for i in 0..operation_count {
    azimuth::Counter::add(counter, 1, [{"operation", azimuth::StringValue("op-" + (i % 10).to_string())}])
    azimuth::Histogram::record(histogram, (i % 1000).to_double(), [{"type", azimuth::StringValue("benchmark")}])
  }
  
  let end_time = azimuth::Clock::now()
  let duration = end_time - start_time
  
  // 验证性能（每秒至少10000次操作）
  let operations_per_second = operation_count.to_double() / (duration.to_double() / 1000000000.0)
  assert_true(operations_per_second >= 10000.0, "Metrics recording should be at least 10000 ops/second")
}

test "context propagation performance benchmark" {
  // 测试上下文传播的性能
  let propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let composite_propagator = azimuth::CompositePropagator::new([propagator, baggage_propagator])
  
  let span_ctx = azimuth::SpanContext::new("benchmark-trace", "benchmark-span", true, "")
  let baggage = azimuth::Baggage::new()
  azimuth::Baggage::set_entry(baggage, "benchmark.key", "benchmark.value")
  
  let start_time = azimuth::Clock::now()
  let propagation_count = 10000
  
  // 重复进行注入和提取操作
  for i in 0..propagation_count {
    let carrier = azimuth::TextMapCarrier::new()
    
    // 注入
    azimuth::CompositePropagator::inject(composite_propagator, span_ctx, carrier)
    azimuth::CompositePropagator::inject_baggage(composite_propagator, baggage, carrier)
    
    // 提取
    let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
    let extracted_baggage = azimuth::CompositePropagator::extract_baggage(composite_propagator, carrier)
    
    // 验证提取的数据正确
    assert_eq(azimuth::SpanContext::trace_id(extracted_ctx), "benchmark-trace")
  }
  
  let end_time = azimuth::Clock::now()
  let duration = end_time - start_time
  
  // 验证性能（每秒至少1000次传播）
  let propagations_per_second = propagation_count.to_double() / (duration.to_double() / 1000000000.0)
  assert_true(propagations_per_second >= 1000.0, "Context propagation should be at least 1000 propagations/second")
}

test "attribute operations performance benchmark" {
  // 测试属性操作的性能
  let attributes = azimuth::Attributes::new()
  let start_time = azimuth::Clock::now()
  let operation_count = 20000
  
  // 大量属性设置和获取操作
  for i in 0..operation_count {
    let key = "attr-" + (i % 1000).to_string()
    let value = azimuth::StringValue("value-" + i.to_string())
    
    azimuth::Attributes::set(attributes, key, value)
    
    // 随机获取一些属性
    if (i % 10 == 0) {
      let lookup_key = "attr-" + (i % 1000).to_string()
      let retrieved = azimuth::Attributes::get(attributes, lookup_key)
      assert_true(retrieved.length > 0)
    }
  }
  
  let end_time = azimuth::Clock::now()
  let duration = end_time - start_time
  
  // 验证性能（每秒至少20000次操作）
  let operations_per_second = operation_count.to_double() / (duration.to_double() / 1000000000.0)
  assert_true(operations_per_second >= 20000.0, "Attribute operations should be at least 20000 ops/second")
}

test "memory allocation efficiency test" {
  // 测试内存分配效率
  let initial_memory = azimuth::Memory::allocated()
  
  // 创建大量临时对象
  let spans = []
  for i in 0..1000 {
    let span_ctx = azimuth::SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, "")
    let span = azimuth::Span::new("operation-" + i.to_string(), azimuth::Internal, span_ctx)
    spans.push(span)
  }
  
  let peak_memory = azimuth::Memory::allocated()
  
  // 清理对象
  spans = []
  
  let final_memory = azimuth::Memory::allocated()
  
  // 验证内存使用合理（每个span不超过1KB）
  let memory_per_span = (peak_memory - initial_memory).to_double() / 1000.0
  assert_true(memory_per_span < 1024.0, "Each span should use less than 1KB memory")
  
  // 验证内存回收（大部分内存应该被回收）
  let memory_recovered = (peak_memory - final_memory).to_double() / (peak_memory - initial_memory).to_double()
  assert_true(memory_recovered > 0.8, "At least 80% of memory should be recovered")
}